use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Response},
    Json,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

use crate::api::content_negotiation::{
    extract_practitioner_name, preferred_format_with_query, PractitionerEditTemplate,
    PractitionerListTemplate, PractitionerRow, ResponseFormat,
};
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::extractors::FhirJson;
use crate::repository::PractitionerRepository;
use crate::validation::validate_fhir_id;
use askama::Template;

pub type SharedPractitionerRepo = Arc<PractitionerRepository>;

/// Search practitioners
pub async fn search_practitioners(
    State(repo): State<SharedPractitionerRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Always include total in search results per FHIR R5 spec
    let include_total = params.get("_total").map(|t| t != "none").unwrap_or(true);

    let (practitioners, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add practitioner entries with id and meta fields
    for p in &practitioners {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&p.content)?
        ));
    }

    // Handle _revinclude parameter for Patient references
    if let Some(revinclude_param) = params.get("_revinclude") {
        if revinclude_param == "Patient:general-practitioner" {
            // For each practitioner, find patients that reference it
            for practitioner in &practitioners {
                let patients = repo.find_patients_by_practitioner(&practitioner.id).await?;
                for patient in patients {
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

    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            // Convert practitioners to HTML table rows
            let practitioner_rows: Vec<PractitionerRow> = practitioners
                .iter()
                .map(|p| PractitionerRow {
                    id: p.id.clone(),
                    version_id: p.version_id.to_string(),
                    name: extract_practitioner_name(&p.content),
                    active: p
                        .content
                        .get("active")
                        .and_then(|a| a.as_bool())
                        .unwrap_or(true),
                    last_updated: p.last_updated.to_rfc3339(),
                })
                .collect();

            let template = PractitionerListTemplate {
                practitioners: practitioner_rows,
                total: total.map(|t| t as usize).unwrap_or(practitioners.len()),
                current_url: "/fhir/Practitioner?".to_string(),
            };

            Ok(Html(template.render().unwrap()).into_response())
        }
        ResponseFormat::Json => Ok(Json(bundle).into_response()),
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

/// Create a new practitioner
pub async fn create_practitioner(
    State(repo): State<SharedPractitionerRepo>,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    let practitioner = repo.create(content).await?;

    // Build Location header per FHIR spec
    let location = format!("/fhir/Practitioner/{}", practitioner.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((StatusCode::CREATED, response_headers, Json(practitioner.content)))
}

/// Read a practitioner by ID
pub async fn read_practitioner(
    State(repo): State<SharedPractitionerRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Try to read the practitioner - if it doesn't exist, return empty template for new resource
    match repo.read(&id).await {
        Ok(practitioner) => {
            // Build ETag header with version
            let etag = format!("W/\"{}\"", practitioner.version_id);

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = PractitionerEditTemplate {
                        id: id.clone(),
                        resource_json: serde_json::to_string(&practitioner.content)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => {
                    let mut response_headers = HeaderMap::new();
                    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());
                    Ok((response_headers, Json(practitioner.content)).into_response())
                }
                ResponseFormat::Xml => {
                    let xml_string = json_to_xml(&practitioner.content)
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
                    let empty_practitioner = serde_json::json!({
                        "resourceType": "Practitioner",
                        "id": id.clone(),
                        "active": true,
                        "name": [],
                        "telecom": [],
                        "address": [],
                        "identifier": [],
                        "qualification": []
                    });
                    let template = PractitionerEditTemplate {
                        id: id.clone(),
                        resource_json: serde_json::to_string(&empty_practitioner)?,
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

/// Update a practitioner (JSON) - Uses upsert semantics per FHIR spec
pub async fn update_practitioner(
    State(repo): State<SharedPractitionerRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Check if resource exists first for If-Match version checking
    let existing = repo.read(&id).await;

    // Check If-Match header for version conflict (only if resource exists)
    if let Ok(existing_practitioner) = &existing {
        if let Some(if_match) = headers.get("if-match") {
            let if_match_str = if_match.to_str()
                .map_err(|_| crate::error::FhirError::BadRequest("Invalid If-Match header".to_string()))?;

            // Parse ETag format: W/"version" or just "version"
            let requested_version = if_match_str
                .trim_start_matches("W/")
                .trim_matches('"')
                .parse::<i32>()
                .map_err(|_| crate::error::FhirError::BadRequest("Invalid version in If-Match header".to_string()))?;

            if requested_version != existing_practitioner.version_id {
                // Version conflict - return 409
                return Err(crate::error::FhirError::Conflict(
                    format!("Version conflict: expected {}, got {}", existing_practitioner.version_id, requested_version)
                ));
            }
        }
    }

    // Use upsert to handle both create and update cases per FHIR PUT semantics
    let practitioner = repo.upsert(&id, content).await?;

    // Return 201 if newly created, 200 if updated
    let status = if existing.is_err() {
        StatusCode::CREATED
    } else {
        StatusCode::OK
    };

    // Build Location header per FHIR spec
    let location = format!("/fhir/Practitioner/{}", practitioner.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((status, response_headers, Json(practitioner.content)))
}

/// Delete a practitioner
pub async fn delete_practitioner(
    State(repo): State<SharedPractitionerRepo>,
    Path(id): Path<String>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Get practitioner history
pub async fn get_practitioner_history(
    State(repo): State<SharedPractitionerRepo>,
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
                r#"{{"resource":{},"request":{{"method":"{}","url":"Practitioner/{}"}},"response":{{"status":"200","lastModified":"{}"}}}}"#,
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
        ResponseFormat::Json | ResponseFormat::Html => Ok(Json(bundle).into_response()),
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

/// Get specific version of a practitioner
pub async fn read_practitioner_version(
    State(repo): State<SharedPractitionerRepo>,
    Path((id, version_id)): Path<(String, i32)>,
) -> Result<(HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let practitioner = repo.read_version(&id, version_id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", practitioner.version_id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());

    Ok((response_headers, Json(practitioner.content)))
}
