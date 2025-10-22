use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    Json,
};
use percent_encoding::percent_decode_str;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

use crate::api::content_negotiation::preferred_format_with_query;
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::extractors::FhirJson;
use crate::repository::ObservationRepository;
use crate::validation::validate_fhir_id;

pub type SharedObservationRepo = Arc<ObservationRepository>;

/// Search observations
pub async fn search_observations(
    State(repo): State<SharedObservationRepo>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Always include total in search results per FHIR R5 spec
    let include_total = params.get("_total").map(|t| t != "none").unwrap_or(true);

    let (observations, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add observation entries with id and meta fields
    for r in &observations {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&r.content)?
        ));
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

    // For now, only support JSON and XML (HTML rendering requires templates)
    match preferred_format_with_query(&uri, &headers) {
        crate::api::content_negotiation::ResponseFormat::Json |
        crate::api::content_negotiation::ResponseFormat::Html => Ok(Json(bundle).into_response()),
        crate::api::content_negotiation::ResponseFormat::Xml => {
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


/// Read a observation by ID
pub async fn read_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    let observation = repo.read(&id).await?;

    // Build ETag header with version
    let etag = format!("W/\"{}\"", observation.version_id);

    match preferred_format_with_query(&uri, &headers) {
        crate::api::content_negotiation::ResponseFormat::Json |
        crate::api::content_negotiation::ResponseFormat::Html => {
            let mut response_headers = HeaderMap::new();
            response_headers.insert(axum::http::header::ETAG, etag.parse().unwrap());
            Ok((response_headers, Json(observation.content)).into_response())
        }
        crate::api::content_negotiation::ResponseFormat::Xml => {
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


/// Update a observation (JSON) - Uses upsert semantics per FHIR spec
pub async fn update_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<String>,
    headers: HeaderMap,
    FhirJson(content): FhirJson<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    // Check if resource exists first for If-Match version checking
    let existing = repo.read(&id).await;

    // Check If-Match header for version conflict (only if resource exists)
    if let Ok(existing_observation) = &existing {
        if let Some(if_match) = headers.get("if-match") {
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
    }

    // Use upsert to handle both create and update cases per FHIR PUT semantics
    let observation = repo.upsert(&id, content).await?;

    // Return 201 if newly created, 200 if updated
    let status = if existing.is_err() {
        StatusCode::CREATED
    } else {
        StatusCode::OK
    };

    // Build Location header per FHIR spec
    let location = format!("/fhir/Observation/{}", observation.id);
    let mut response_headers = HeaderMap::new();
    response_headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((status, response_headers, Json(observation.content)))
}


/// Delete a observation
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


/// Get specific version of a observation
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


/// Update a observation via form submission
pub async fn update_observation_form(
    State(_repo): State<SharedObservationRepo>,
    Path(_id): Path<String>,
    _headers: HeaderMap,
    _body: axum::body::Bytes,
) -> Result<StatusCode> {
    // TODO: Implement HTML form handling when UI is ready
    Err(crate::error::FhirError::BadRequest(
        "HTML form submission not yet implemented".to_string()
    ))
}

/// Delete multiple observations (for testing)
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

/// Rollback a observation to a specific version
pub async fn rollback_observation(
    State(repo): State<SharedObservationRepo>,
    Path((id, version)): Path<(String, i32)>,
) -> Result<StatusCode> {
    // Validate FHIR ID format
    validate_fhir_id(&id)?;

    repo.rollback(&id, version).await?;
    Ok(StatusCode::NO_CONTENT)
}

