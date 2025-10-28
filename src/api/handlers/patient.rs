use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response, Html},
    Json,
};
use percent_encoding::percent_decode_str;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

use crate::api::content_negotiation::{*, preferred_format_with_query};
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::extractors::FhirJson;
use crate::repository::PatientRepository;
use crate::validation::validate_fhir_id;

pub type SharedPatientRepo = Arc<PatientRepository>;

/// Search patients
pub async fn search_patients(
    State(repo): State<SharedPatientRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Always include total in search results per FHIR R5 spec
    let include_total = params.get("_total").map(|t| t != "none").unwrap_or(true);

    let (patients, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add patient entries with id and meta fields
    for r in &patients {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&r.content)?
        ));
    }

    // Process _include parameter
    if let Some(include_param) = params.get("_include") {

        // Parse include parameter: Patient:general-practitioner
        if let Some((resource_type, field)) = include_param.split_once(':') {
            if resource_type == "Patient" && field == "general-practitioner" {
                // Extract practitioner IDs from matched patients
                let mut practitioner_ids = std::collections::HashSet::new();
                for patient in &patients {
                    if let Some(gp_array) = patient.content.get("generalPractitioner").and_then(|v| v.as_array()) {
                        for gp in gp_array {
                            if let Some(reference) = gp.get("reference").and_then(|r| r.as_str()) {
                                if let Some(id) = reference.strip_prefix("Practitioner/") {
                                    practitioner_ids.insert(id.to_string());
                                }
                            }
                        }
                    }
                }
                // Fetch practitioners by IDs
                if !practitioner_ids.is_empty() {
                    let ids: Vec<String> = practitioner_ids.into_iter().collect();
                    let practitioners = repo.find_practitioners_by_ids(&ids).await?;
                    for prac in practitioners {
                        entries.push(format!(
                            r#"{{"resource":{},"search":{{"mode":"include"}}}}"#,
                            serde_json::to_string(&prac.content)?
                        ));
                    }
                }
            }
        }
    }

    // Process _revinclude parameter
    if let Some(revinclude_param) = params.get("_revinclude") {

        // Parse revinclude parameter: Observation:subject
        if let Some((resource_type, field)) = revinclude_param.split_once(':') {
            if resource_type == "Observation" && field == "subject" {
                // For each matched patient, find observations that reference them
                for patient in &patients {
                    let observations = repo.find_observations_by_patient(&patient.id).await?;
                    for obs in observations {
                        entries.push(format!(
                            r#"{{"resource":{},"search":{{"mode":"include"}}}}"#,
                            serde_json::to_string(&obs.content)?
                        ));
                    }
                }
            }
        }
    }

    // Build pagination links
    let base_url = format!("http://localhost:3000{}", uri.path());
    let query_params = uri.query().unwrap_or("");

    // Parse _count and _offset from params
    let count = params
        .get("_count")
        .and_then(|c| c.parse::<i64>().ok())
        .unwrap_or(50);
    let offset = params
        .get("_offset")
        .and_then(|o| o.parse::<i64>().ok())
        .unwrap_or(0);

    // Build self link
    let self_link = if query_params.is_empty() {
        base_url.clone()
    } else {
        format!("{}?{}", base_url, query_params)
    };

    // Build next link if there are more results
    let next_link = if let Some(total_count) = total {
        if offset + count < total_count {
            // Build next URL with updated offset
            let next_offset = offset + count;
            let mut next_params: Vec<String> = params
                .iter()
                .filter(|(k, _)| k.as_str() != "_offset")
                .map(|(k, v)| format!("{}={}", k, v))
                .collect();
            next_params.push(format!("_offset={}", next_offset));
            Some(format!("{}?{}", base_url, next_params.join("&")))
        } else {
            None
        }
    } else {
        None
    };

    // Build links array
    let mut links = vec![format!(r#"{{"relation":"self","url":"{}"}}"#, self_link)];
    if let Some(next_url) = next_link {
        links.push(format!(r#"{{"relation":"next","url":"{}"}}"#, next_url));
    }
    let links_str = links.join(",");

    // Build final Bundle JSON string
    let entries_str = entries.join(",");
    let bundle_str = if let Some(total_count) = total {
        format!(
            r#"{{"resourceType":"Bundle","type":"searchset","total":{},"link":[{}],"entry":[{}]}}"#,
            total_count, links_str, entries_str
        )
    } else {
        format!(
            r#"{{"resourceType":"Bundle","type":"searchset","link":[{}],"entry":[{}]}}"#,
            links_str, entries_str
        )
    };

    // Parse string back to Value for Json response
    let bundle: Value = serde_json::from_str(&bundle_str)?;

    // Handle response format
    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            // Build HTML template data
            let rows: Vec<PatientRow> = patients
                .iter()
                .map(|r| PatientRow {
                    id: r.id.clone(),
                    version_id: r.version_id.to_string(),
                    name: extract_patient_name(&r.content),
                    gender: r.content
                        .get("gender")
                        .and_then(|g| g.as_str())
                        .unwrap_or("Unknown")
                        .to_string(),
                    birth_date: r.content
                        .get("birthDate")
                        .and_then(|b| b.as_str())
                        .unwrap_or("Unknown")
                        .to_string(),
                    active: r.content
                        .get("active")
                        .and_then(|a| a.as_bool())
                        .unwrap_or(true),
                    last_updated: r.last_updated.to_rfc3339(),
                })
                .collect();

            let template = PatientListTemplate {
                patients: rows,
                total: total.unwrap_or(0) as usize,
                current_url: self_link,
            };

            let html = template.render()
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok(Html(html).into_response())
        }
        ResponseFormat::Json => {
            Ok(Json(bundle).into_response())
        }
        ResponseFormat::Xml => {
            let xml_string = json_to_xml(&bundle)
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok((
                [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                xml_string,
            )
                .into_response())
        }
    }
}


/// Create a new patient
pub async fn create_patient(
    State(repo): State<SharedPatientRepo>,
    headers: HeaderMap,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    // Check for If-None-Exist header (conditional create)
    if let Some(if_none_exist) = headers.get("if-none-exist") {
        let query_string = if_none_exist.to_str()
            .map_err(|_| crate::error::FhirError::BadRequest("Invalid If-None-Exist header".to_string()))?;

        // Parse query string into HashMap
        let mut search_params = HashMap::new();
        for pair in query_string.split('&') {
            if let Some((key, value)) = pair.split_once('=') {
                // URL decode the value - handle all percent-encoded characters
                let with_spaces = value.replace('+', " ");
                let decoded = percent_decode_str(&with_spaces)
                    .decode_utf8_lossy()
                    .into_owned();

                // No need to split identifier - let the search parser handle it
                search_params.insert(key.to_string(), decoded);
            }
        }

        // Search for existing resource
        let (existing, _) = repo.search(&search_params, false).await?;

        if !existing.is_empty() {
            // Check for multiple matches - return 412 Precondition Failed per FHIR spec
            if existing.len() > 1 {
                return Err(crate::error::FhirError::PreconditionFailed(
                    format!("Multiple matches found ({} resources)", existing.len())
                ));
            }

            // Found exactly one existing resource - return 200 with existing resource
            let existing_patient = &existing[0];
            let mut response_headers = HeaderMap::new();
            response_headers.insert(
                axum::http::header::LOCATION,
                format!("/fhir/Patient/{}", existing_patient.id).parse().unwrap()
            );
            return Ok((StatusCode::OK, response_headers, Json(existing_patient.content.clone())));
        }
        // No match found - continue with create
    }

    let patient = repo.create(content).await?;

    // Build Location header per FHIR spec
    let location = format!("/fhir/Patient/{}", patient.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((StatusCode::CREATED, response_headers, Json(patient.content)))
}


/// Read a patient by ID
pub async fn read_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let patient = repo.read(&id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", patient.version_id);

    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            let template = PatientEditTemplate {
                id: id.clone(),
                resource_json: serde_json::to_string(&patient.content)?,
            };
            Ok(Html(template.render().unwrap()).into_response())
        }
        ResponseFormat::Json => {
            let mut response_headers = HeaderMap::new();
            response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());
            Ok((response_headers, Json(patient.content)).into_response())
        }
        ResponseFormat::Xml => {
            let xml_string = json_to_xml(&patient.content)
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok((
                [
                    (axum::http::header::CONTENT_TYPE, "application/fhir+xml"),
                    (axum::http::header::ETAG, &etag),
                ],
                xml_string
            ).into_response())
        }
    }
}


/// Update a patient (JSON) - Uses upsert semantics per FHIR spec
pub async fn update_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Check if resource exists first for If-Match version checking
    let existing = repo.read(&id).await;

    // Check If-Match header for version conflict (only if resource exists)
    if let Ok(existing_patient) = &existing {
        if let Some(if_match) = headers.get("if-match") {
            let if_match_str = if_match.to_str()
                .map_err(|_| crate::error::FhirError::BadRequest("Invalid If-Match header".to_string()))?;

            // Parse ETag format: W/"version" or just "version"
            let requested_version = if_match_str
                .trim_start_matches("W/")
                .trim_matches('"')
                .parse::<i32>()
                .map_err(|_| crate::error::FhirError::BadRequest("Invalid version in If-Match header".to_string()))?;

            if requested_version != existing_patient.version_id {
                // Version conflict - return 409
                return Err(crate::error::FhirError::Conflict(
                    format!("Version conflict: expected {}, got {}", existing_patient.version_id, requested_version)
                ));
            }
        }
    }

    // Use upsert to handle both create and update cases per FHIR PUT semantics
    let patient = repo.upsert(&id, content).await?;

    // Return 201 if newly created, 200 if updated
    let status = if existing.is_err() {
        StatusCode::CREATED
    } else {
        StatusCode::OK
    };

    // Build Location header per FHIR spec
    let location = format!("/fhir/Patient/{}", patient.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((status, response_headers, Json(patient.content)))
}


/// Delete a patient
pub async fn delete_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<String>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}


/// Get patient history
pub async fn get_patient_history(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<String>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Parse _count parameter
    let count = params
        .get("_count")
        .and_then(|c| c.parse::<i64>().ok());

    let history = repo.history(&id, count).await?;

    // Build Bundle using efficient string concatenation
    let total = history.len();
    let entries: Vec<String> = history
        .iter()
        .map(|h| {
            format!(
                r#"{{"resource":{},"request":{{"method":"{}","url":"Patient/{}"}},"response":{{"status":"200","lastModified":"{}"}}}}"#,
                serde_json::to_string(&h.content).unwrap_or_default(),
                h.history_operation,
                h.id,
                h.last_updated.to_rfc3339()
            )
        })
        .collect();

    let entries_str = entries.join(",");
    let bundle_str = format!(
        r#"{{"resourceType":"Bundle","type":"history","total":{},"entry":[{}]}}"#,
        total, entries_str
    );

    // Parse string back to Value for Json response
    let bundle: Value = serde_json::from_str(&bundle_str)?;

    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            let history_rows: Vec<HistoryRow> = history
                .iter()
                .map(|h| {
                    let operation = h.content
                        .get("request")
                        .and_then(|r| r.get("method"))
                        .and_then(|m| m.as_str())
                        .unwrap_or("UPDATE")
                        .to_string();

                    let fhir_json = h.content.get("resource").unwrap_or(&h.content);
                    let summary = extract_patient_name(fhir_json);

                    HistoryRow {
                        version_id: h.version_id.to_string(),
                        operation,
                        last_updated: h.last_updated.to_rfc3339(),
                        summary,
                    }
                })
                .collect();

            let template = PatientHistoryTemplate {
                id: id.clone(),
                history: history_rows,
                total,
            };
            let html = template.render()
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok(Html(html).into_response())
        }
        ResponseFormat::Json => Ok(Json(bundle).into_response()),
        ResponseFormat::Xml => {
            let xml_string = json_to_xml(&bundle)
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok((
                [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                xml_string
            ).into_response())
        }
    }
}


/// Get specific version of a patient
pub async fn read_patient_version(
    State(repo): State<SharedPatientRepo>,
    Path((id, version_id)): Path<(String, i32)>,
) -> Result<(HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let patient = repo.read_version(&id, version_id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", patient.version_id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());

    Ok((response_headers, Json(patient.content)))
}


/// Update a patient via form submission
pub async fn update_patient_form(
    State(_repo): State<SharedPatientRepo>,
    Path(_id): Path<String>,
    _headers: HeaderMap,
    _body: axum::body::Bytes,
) -> Result<StatusCode> {
    // TODO: Implement HTML form handling when UI is ready
    Err(crate::error::FhirError::BadRequest(
        "HTML form submission not yet implemented".to_string()
    ))
}

/// Delete multiple patients (for testing)
pub async fn delete_patients(
    State(repo): State<SharedPatientRepo>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<StatusCode> {
    // Check if _id parameter is present for batch delete
    if let Some(id_param) = params.get("_id") {
        // Split comma-separated IDs
        let ids: Vec<&str> = id_param.split(',').collect();

        // Delete each patient
        for id_str in ids {
            repo.delete(id_str.trim()).await?;
        }
    } else {
        // Delete all patients matching the search criteria
        let (patients, _) = repo.search(&params, false).await?;

        for patient in patients {
            repo.delete(&patient.id).await?;
        }
    }

    Ok(StatusCode::NO_CONTENT)
}

/// Rollback a patient to a specific version
pub async fn rollback_patient(
    State(repo): State<SharedPatientRepo>,
    Path((id, version)): Path<(String, i32)>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.rollback(&id, version).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Get type-level history for all patients
pub async fn get_patient_type_history(
    State(repo): State<SharedPatientRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Parse _count parameter
    let count = params
        .get("_count")
        .and_then(|c| c.parse::<i64>().ok());

    let history = repo.type_history(count).await?;

    // Build Bundle using efficient string concatenation
    let total = history.len();
    let entries: Vec<String> = history
        .iter()
        .map(|h| {
            format!(
                r#"{{"resource":{},"request":{{"method":"{}","url":"Patient/{}"}},"response":{{"status":"200","lastModified":"{}"}}}}"#,
                serde_json::to_string(&h.content).unwrap_or_default(),
                h.history_operation,
                h.id,
                h.last_updated.to_rfc3339()
            )
        })
        .collect();

    let entries_str = entries.join(",");
    let bundle_str = format!(
        r#"{{"resourceType":"Bundle","type":"history","total":{},"entry":[{}]}}"#,
        total, entries_str
    );

    // Parse string back to Value for Json response
    let bundle: Value = serde_json::from_str(&bundle_str)?;

    match preferred_format_with_query(&uri, &headers) {
        crate::api::content_negotiation::ResponseFormat::Json |
        crate::api::content_negotiation::ResponseFormat::Html => Ok(Json(bundle).into_response()),
        crate::api::content_negotiation::ResponseFormat::Xml => {
            let xml_string = json_to_xml(&bundle)
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok((
                [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                xml_string
            ).into_response())
        }
    }
}

