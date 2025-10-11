use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Redirect, Response},
    Form, Json,
};
use serde::Deserialize;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

use crate::api::content_negotiation::{*, preferred_format_with_query};
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::repository::PatientRepository;
use askama::Template;
use percent_encoding::percent_decode_str;

#[derive(Debug, Deserialize)]
pub struct PatientFormData {
    pub family: String,
    pub given: String,
    pub gender: String,
    #[serde(rename = "birthDate")]
    pub birth_date: String,
    pub active: String,
}

pub type SharedPatientRepo = Arc<PatientRepository>;

/// Create a new patient
pub async fn create_patient(
    State(repo): State<SharedPatientRepo>,
    headers: HeaderMap,
    Json(content): Json<Value>,
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

                // Special handling for identifier parameter: system|value format
                if key == "identifier" && decoded.contains('|') {
                    if let Some((system, val)) = decoded.split_once('|') {
                        search_params.insert("identifier_system".to_string(), system.to_string());
                        search_params.insert("identifier_value".to_string(), val.to_string());
                        continue;
                    }
                }

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
    Path(id_str): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Invalid IDs should return 404 per FHIR spec, not 400
    let id = Uuid::parse_str(&id_str)
        .map_err(|_| crate::error::FhirError::NotFound)?;

    // Try to read the patient - if it doesn't exist, return empty template for new resource
    match repo.read(&id).await {
        Ok(patient) => {
            // Build ETag header with version
            let etag = format!("W/\"{}\"", patient.version_id);

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = PatientEditTemplate {
                        id: id.to_string(),
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
        Err(crate::error::FhirError::NotFound) => {
            // For HTML requests, return empty template for creating new resource
            // For JSON/XML (FHIR API), return 404 as per FHIR spec
            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let empty_patient = serde_json::json!({
                        "resourceType": "Patient",
                        "id": id.to_string(),
                        "active": true,
                        "name": [],
                        "telecom": [],
                        "address": [],
                        "identifier": []
                    });
                    let template = PatientEditTemplate {
                        id: id.to_string(),
                        resource_json: serde_json::to_string(&empty_patient)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json | ResponseFormat::Xml => {
                    // For FHIR API, return 404 Not Found
                    Err(crate::error::FhirError::NotFound)
                }
            }
        }
        Err(e) => Err(e),
    }
}

/// Update a patient (JSON) - Uses update semantics with version checking
pub async fn update_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    headers: HeaderMap,
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> {
    // Check if resource exists first
    let existing = repo.read(&id).await;

    if existing.is_err() {
        // Resource doesn't exist - return 404 per FHIR spec
        return Err(crate::error::FhirError::NotFound);
    }

    let existing_patient = existing.unwrap();

    // Check If-Match header for version conflict
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

    let patient = repo.update(&id, content).await?;
    Ok((StatusCode::OK, Json(patient.content)))
}

/// Update a patient (HTML form) - Uses upsert semantics per FHIR spec
pub async fn update_patient_form(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    Form(form_data): Form<PatientFormData>,
) -> Result<Redirect> {
    // Convert form data to FHIR JSON
    let content = serde_json::json!({
        "resourceType": "Patient",
        "name": [{
            "family": form_data.family,
            "given": [form_data.given]
        }],
        "gender": form_data.gender,
        "birthDate": form_data.birth_date,
        "active": form_data.active == "true"
    });

    repo.upsert(&id, content).await?;

    // Redirect back to the patient detail page
    Ok(Redirect::to(&format!("/fhir/Patient/{}", id)))
}

/// Delete a patient
pub async fn delete_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
) -> Result<StatusCode> {
    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Search patients
pub async fn search_patients(
    State(repo): State<SharedPatientRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Always include total in search results per FHIR R5 spec
    // (can be disabled with _total=none in the future if needed)
    let include_total = params
        .get("_total")
        .map(|t| t != "none")
        .unwrap_or(true);

    let (patients, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add patient entries with id and meta fields
    for p in &patients {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&p.content)?
        ));
    }

    // Handle _revinclude parameter for Observation references
    if let Some(revinclude_param) = params.get("_revinclude") {
        if revinclude_param == "Observation:patient" || revinclude_param == "Observation:subject" {
            // For each patient, find observations that reference it
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
    let mut links = vec![
        format!(r#"{{"relation":"self","url":"{}"}}"#, self_link)
    ];
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

    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            // Convert patients to HTML table rows
            let patient_rows: Vec<PatientRow> = patients
                .iter()
                .map(|p| {
                    PatientRow {
                        id: p.id.to_string(),
                        version_id: p.version_id.to_string(),
                        name: extract_patient_name(&p.content),
                        gender: p.content
                            .get("gender")
                            .and_then(|g| g.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        birth_date: p.content
                            .get("birthDate")
                            .and_then(|b| b.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        active: p.content
                            .get("active")
                            .and_then(|a| a.as_bool())
                            .unwrap_or(true),
                        last_updated: p.last_updated.to_rfc3339(),
                    }
                })
                .collect();

            let template = PatientListTemplate {
                patients: patient_rows,
                total: total.map(|t| t as usize).unwrap_or(patients.len()),
                current_url: "/fhir/Patient?".to_string(),
            };

            Ok(Html(template.render().unwrap()).into_response())
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

/// Get patient history
pub async fn get_patient_history(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
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
            // Convert history to HTML table rows
            let history_rows: Vec<HistoryRow> = history
                .iter()
                .map(|h| {
                    let fhir_json = &h.content;
                    HistoryRow {
                        version_id: h.version_id.to_string(),
                        operation: h.history_operation.clone(),
                        last_updated: h.last_updated.to_rfc3339(),
                        summary: extract_patient_name(fhir_json),
                    }
                })
                .collect();

            let template = PatientHistoryTemplate {
                id: id.to_string(),
                history: history_rows,
                total,
            };

            Ok(Html(template.render().unwrap()).into_response())
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
        ResponseFormat::Json => Ok(Json(bundle).into_response()),
        ResponseFormat::Xml => {
            let xml_string = json_to_xml(&bundle)
                .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
            Ok((
                [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                xml_string
            ).into_response())
        }
        ResponseFormat::Html => {
            // For type-level history, just return JSON (no HTML template for this)
            Ok(Json(bundle).into_response())
        }
    }
}

/// Get specific version of a patient
pub async fn read_patient_version(
    State(repo): State<SharedPatientRepo>,
    Path((id, version_id)): Path<(Uuid, i32)>,
) -> Result<(HeaderMap, Json<Value>)> {
    let patient = repo.read_version(&id, version_id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", patient.version_id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());

    Ok((response_headers, Json(patient.content)))
}

/// Delete patients (batch delete)
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
            if let Ok(id) = Uuid::parse_str(id_str.trim()) {
                repo.delete(&id).await?;
            }
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

/// Rollback a patient to a specific version (destructive operation for dev/test)
pub async fn rollback_patient(
    State(repo): State<SharedPatientRepo>,
    Path((id, version)): Path<(Uuid, i32)>,
) -> Result<StatusCode> {
    repo.rollback(&id, version).await?;
    Ok(StatusCode::NO_CONTENT)
}

