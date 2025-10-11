use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Redirect, Response},
    Json,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

use crate::api::content_negotiation::*;
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::extractors::FhirJson;
use crate::repository::ObservationRepository;
use crate::validation::validate_fhir_id;
use percent_encoding::percent_decode_str;
use askama::Template;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct ObservationFormData {
    pub code: String,
    pub status: String,
    pub value: String,
    pub subject: String,
    #[serde(rename = "effectiveDateTime")]
    pub effective_date_time: String,
}

pub type SharedObservationRepo = Arc<ObservationRepository>;

/// Create a new observation
pub async fn create_observation(
    State(repo): State<SharedObservationRepo>,
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
            // Found existing resource - return 200 with existing resource
            let existing_observation = &existing[0];
            let mut response_headers = HeaderMap::new();
            response_headers.insert(
                axum::http::header::LOCATION,
                format!("/fhir/Observation/{}", existing_observation.id).parse().unwrap()
            );
            return Ok((StatusCode::OK, response_headers, Json(existing_observation.content.clone())));
        }
        // No match found - continue with create
    }

    let observation = repo.create(content).await?;

    // Build Location header per FHIR spec
    let location = format!("/fhir/Observation/{}", observation.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((StatusCode::CREATED, response_headers, Json(observation.content)))
}

/// Read an observation by ID
pub async fn read_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Try to read the observation - if it doesn't exist, return empty template for new resource
    match repo.read(&id).await {
        Ok(observation) => {
            // Build ETag header with version
            let etag = format!("W/\"{}\"", observation.version_id);

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = ObservationEditTemplate {
                        id: id.clone(),
                        resource_json: serde_json::to_string(&observation.content)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => {
                    let mut response_headers = HeaderMap::new();
                    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());
                    Ok((response_headers, Json(observation.content)).into_response())
                }
                ResponseFormat::Xml => {
                    let xml_string = json_to_xml(&observation.content)
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
                    let empty_observation = serde_json::json!({
                        "resourceType": "Observation",
                        "id": id.clone(),
                        "status": "final",
                        "code": { "text": "", "coding": [] },
                        "subject": { "reference": "", "display": "" },
                        "valueQuantity": { "value": null, "unit": "", "system": "http://unitsofmeasure.org", "code": "" }
                    });
                    let template = ObservationEditTemplate {
                        id: id.clone(),
                        resource_json: serde_json::to_string(&empty_observation)?,
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

/// Update an observation - Uses upsert semantics per FHIR spec
pub async fn update_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Check if resource exists for If-Match version checking
    let existing = repo.read(&id).await.ok();
    let is_new_resource = existing.is_none();

    // Check If-Match header for version conflict (only if resource exists)
    if let (Some(if_match), Some(existing_observation)) = (headers.get("if-match"), &existing) {
        let if_match_str = if_match.to_str()
            .map_err(|_| crate::error::FhirError::BadRequest("Invalid If-Match header".to_string()))?;

        // Parse ETag format: W/"version" or just "version"
        let requested_version = if_match_str
            .trim_start_matches("W/")
            .trim_matches('"')
            .parse::<i32>()
            .map_err(|_| crate::error::FhirError::BadRequest("Invalid version in If-Match header".to_string()))?;

        if requested_version != existing_observation.version_id {
            // Version conflict - return 409
            return Err(crate::error::FhirError::Conflict(
                format!("Version conflict: expected {}, got {}", existing_observation.version_id, requested_version)
            ));
        }
    }

    // Use upsert to create or update
    let observation = repo.upsert(&id, content).await?;

    // Return 201 Created if new resource, 200 OK if updated
    let status = if is_new_resource {
        StatusCode::CREATED
    } else {
        StatusCode::OK
    };

    // Add Location header for newly created resources
    let mut response_headers = HeaderMap::new();
    if is_new_resource {
        let location = format!("/fhir/Observation/{}", observation.id);
        response_headers.insert(
            axum::http::header::LOCATION,
            location.parse().unwrap()
        );
    }

    Ok((status, response_headers, Json(observation.content)))
}


/// Update an observation (HTML form)
/// Note: For FHIR API (JSON), POST to instance endpoint returns 400 Bad Request
pub async fn update_observation_form(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    body: axum::body::Bytes,
) -> Result<impl IntoResponse> {
    // Check content type to determine if this is a FHIR API request or HTML form
    let content_type = headers
        .get(axum::http::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    // For FHIR API requests (application/fhir+json or application/json), POST to instance is not allowed
    if content_type.contains("application/fhir+json") || content_type.contains("application/json") {
        return Err(crate::error::FhirError::BadRequest(
            "POST to instance endpoint not allowed. Use PUT for updates.".to_string()
        ));
    }

    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Parse form data
    let body_str = String::from_utf8_lossy(&body);
    let form_data: ObservationFormData = serde_urlencoded::from_str(&body_str)
        .map_err(|e| crate::error::FhirError::BadRequest(format!("Invalid form data: {}", e)))?;

    // Convert form data to FHIR JSON
    let content = serde_json::json!({
        "resourceType": "Observation",
        "status": form_data.status,
        "code": {
            "text": form_data.code
        },
        "subject": {
            "reference": form_data.subject
        },
        "effectiveDateTime": form_data.effective_date_time,
        "valueString": form_data.value
    });

    repo.upsert(&id, content).await?;

    // Redirect back to the observation detail page
    Ok(Redirect::to(&format!("/fhir/Observation/{}", id)))
}

/// Delete an observation
pub async fn delete_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Get observation history
pub async fn get_observation_history(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let history = repo.history(&id).await?;

    // Build Bundle using efficient string concatenation
    let total = history.len();
    let entries: Vec<String> = history
        .iter()
        .map(|h| {
            format!(
                r#"{{"resource":{},"request":{{"method":"{}","url":"Observation/{}"}},"response":{{"status":"200","lastModified":"{}"}}}}"#,
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
                        summary: extract_observation_code(fhir_json),
                    }
                })
                .collect();

            let template = ObservationHistoryTemplate {
                id: id.clone(),
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

/// Get specific version of an observation
pub async fn read_observation_version(
    State(repo): State<SharedObservationRepo>,
    Path((id, version_id)): Path<(String, i32)>,
) -> Result<(HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let observation = repo.read_version(&id, version_id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", observation.version_id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());

    Ok((response_headers, Json(observation.content)))
}

/// Delete observations (batch delete)
pub async fn delete_observations(
    State(repo): State<SharedObservationRepo>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<StatusCode> {
    // Check if _id parameter is present for batch delete
    if let Some(id_param) = params.get("_id") {
        // Split comma-separated IDs
        let ids: Vec<&str> = id_param.split(',').collect();

        // Delete each observation
        for id_str in ids {
            repo.delete(id_str.trim()).await?;
        }
    } else {
        // Delete all observations matching the search criteria
        let (observations, _) = repo.search(&params, false).await?;

        for observation in observations {
            repo.delete(&observation.id).await?;
        }
    }

    Ok(StatusCode::NO_CONTENT)
}

/// Rollback an observation to a specific version (destructive operation for dev/test)
pub async fn rollback_observation(
    State(repo): State<SharedObservationRepo>,
    Path((id, version)): Path<(String, i32)>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.rollback(&id, version).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Search observations
pub async fn search_observations(
    State(repo): State<SharedObservationRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Always include total in search results per FHIR R5 spec
    let include_total = params
        .get("_total")
        .map(|t| t != "none")
        .unwrap_or(true);
    let (observations, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add observation entries with id and meta fields
    for obs in &observations {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&obs.content)?
        ));
    }

    // Handle _include parameter for Patient references
    if let Some(include_param) = params.get("_include") {
        if include_param == "Observation:patient" || include_param == "Observation:subject" {
            // Collect unique patient IDs from observations
            let mut patient_ids = std::collections::HashSet::<String>::new();
            for obs in &observations {
                // Extract patient reference from content JSON
                if let Some(patient_ref) = obs
                    .content
                    .get("subject")
                    .and_then(|s| s.get("reference"))
                    .and_then(|r| r.as_str())
                {
                    if let Some(id_str) = patient_ref.strip_prefix("Patient/") {
                        patient_ids.insert(id_str.to_string());
                    }
                }
            }

            // Fetch and include patient resources
            for patient_id in patient_ids {
                if let Ok(patient) = repo.read_patient(&patient_id).await {
                    entries.push(format!(
                        r#"{{"resource":{},"search":{{"mode":"include"}}}}"#,
                        serde_json::to_string(&patient.content)?
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
            // Convert observations to HTML table rows
            let observation_rows: Vec<ObservationRow> = observations
                .iter()
                .map(|obs| {
                    ObservationRow {
                        id: obs.id.clone(),
                        version_id: obs.version_id.to_string(),
                        code: extract_observation_code(&obs.content),
                        status: obs.content
                            .get("status")
                            .and_then(|s| s.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        value: extract_observation_value(&obs.content),
                        subject: obs.content
                            .get("subject")
                            .and_then(|s| s.get("reference"))
                            .and_then(|r| r.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        effective_date: obs.content
                            .get("effectiveDateTime")
                            .and_then(|e| e.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        last_updated: obs.last_updated.to_rfc3339(),
                    }
                })
                .collect();

            let template = ObservationListTemplate {
                observations: observation_rows,
                total: total.map(|t| t as usize).unwrap_or(observations.len()),
                current_url: "/fhir/Observation?".to_string(),
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

