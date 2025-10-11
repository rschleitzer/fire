use axum::{extract::State, Json};
use serde_json::Value;
use std::sync::Arc;

use crate::error::{FhirError, Result};
use crate::extractors::FhirJson;
use crate::repository::{ObservationRepository, PatientRepository};

pub struct BundleState {
    pub patient_repo: Arc<PatientRepository>,
    pub observation_repo: Arc<ObservationRepository>,
}

/// Process a transaction bundle
pub async fn process_bundle(
    State(state): State<Arc<BundleState>>,
    FhirJson(bundle): FhirJson<Value>,
) -> Result<Json<Value>> {
    // Validate bundle structure
    if bundle.get("resourceType").and_then(|r| r.as_str()) != Some("Bundle") {
        return Err(FhirError::BadRequest(
            "Resource must be a Bundle".to_string(),
        ));
    }

    let bundle_type = bundle
        .get("type")
        .and_then(|t| t.as_str())
        .ok_or_else(|| FhirError::BadRequest("Bundle must have a type".to_string()))?;

    if bundle_type != "transaction" && bundle_type != "batch" {
        return Err(FhirError::BadRequest(
            "Only transaction and batch bundles are supported".to_string(),
        ));
    }

    let entries = bundle
        .get("entry")
        .and_then(|e| e.as_array())
        .ok_or_else(|| FhirError::BadRequest("Bundle must have entries".to_string()))?;

    let mut response_entries = Vec::new();

    // For transactions, validate all entries first before executing any
    // This allows proper rollback behavior
    if bundle_type == "transaction" {
        for entry in entries {
            validate_entry(entry)?;
        }
    }

    // Process each entry
    for entry in entries {
        // For batch: continue on errors, wrapping them in response entries
        // For transaction: stop on first error and rollback
        let response = match process_entry(&state, entry).await {
            Ok(resp) => resp,
            Err(err) => {
                // For transactions, return error immediately
                if bundle_type == "transaction" {
                    return Err(err);
                }

                // For batch, wrap error in response entry
                create_error_response(&err)
            }
        };

        response_entries.push(response);
    }

    // Build response bundle
    let response_bundle = serde_json::json!({
        "resourceType": "Bundle",
        "type": format!("{}-response", bundle_type),
        "entry": response_entries
    });

    Ok(Json(response_bundle))
}

/// Validate an entry without executing it (for transaction pre-validation)
fn validate_entry(entry: &Value) -> Result<()> {
    let request = entry
        .get("request")
        .ok_or_else(|| FhirError::BadRequest("Entry must have a request".to_string()))?;

    let method = request
        .get("method")
        .and_then(|m| m.as_str())
        .ok_or_else(|| FhirError::BadRequest("Request must have a method".to_string()))?;

    let url = request
        .get("url")
        .and_then(|u| u.as_str())
        .ok_or_else(|| FhirError::BadRequest("Request must have a url".to_string()))?;

    let resource = entry.get("resource");

    // Validate based on method
    match method {
        "POST" | "PUT" => {
            let resource = resource
                .ok_or_else(|| FhirError::BadRequest(format!("{} request must have a resource", method)))?;

            let resource_type = resource
                .get("resourceType")
                .and_then(|r| r.as_str())
                .ok_or_else(|| FhirError::BadRequest("Resource must have resourceType".to_string()))?;

            // Validate the resource structure
            match resource_type {
                "Patient" => {
                    crate::services::validate_patient(resource)?;
                }
                "Observation" => {
                    crate::services::validate_observation(resource)?;
                }
                _ => {
                    return Err(FhirError::BadRequest(format!(
                        "Unsupported resource type: {}",
                        resource_type
                    )));
                }
            }

            // For POST, validate URL matches resource type
            if method == "POST" {
                if url != resource_type {
                    return Err(FhirError::BadRequest(format!(
                        "URL '{}' does not match resource type '{}'",
                        url, resource_type
                    )));
                }
            }

            // For PUT, validate URL format
            if method == "PUT" {
                let parts: Vec<&str> = url.split('/').collect();
                if parts.len() != 2 {
                    return Err(FhirError::BadRequest(format!(
                        "Invalid URL format for PUT: {}",
                        url
                    )));
                }
                if parts[0] != resource_type {
                    return Err(FhirError::BadRequest(format!(
                        "URL resource type '{}' does not match resource type '{}'",
                        parts[0], resource_type
                    )));
                }
            }
        }
        "GET" | "DELETE" => {
            // These don't need resource validation
        }
        _ => {
            return Err(FhirError::BadRequest(format!(
                "Unsupported method: {}",
                method
            )));
        }
    }

    Ok(())
}

async fn process_entry(state: &Arc<BundleState>, entry: &Value) -> Result<Value> {
    let request = entry
        .get("request")
        .ok_or_else(|| FhirError::BadRequest("Entry must have a request".to_string()))?;

    let method = request
        .get("method")
        .and_then(|m| m.as_str())
        .ok_or_else(|| FhirError::BadRequest("Request must have a method".to_string()))?;

    let url = request
        .get("url")
        .and_then(|u| u.as_str())
        .ok_or_else(|| FhirError::BadRequest("Request must have a url".to_string()))?;

    let resource = entry.get("resource");

    // Check for conditional create (ifNoneExist)
    let if_none_exist = request.get("ifNoneExist").and_then(|v| v.as_str());

    // Process based on method
    match method {
        "POST" => process_post(state, url, resource, if_none_exist).await,
        "PUT" => process_put(state, url, resource).await,
        "GET" => process_get(state, url).await,
        "DELETE" => process_delete(state, url).await,
        _ => Err(FhirError::BadRequest(format!(
            "Unsupported method: {}",
            method
        ))),
    }
}

fn create_error_response(err: &FhirError) -> Value {
    let (status_code, diagnostics) = match err {
        FhirError::NotFound => (404, "Resource not found"),
        FhirError::InvalidResource(msg) => (400, msg.as_str()),
        FhirError::ValidationError(msg) => (400, msg.as_str()),
        FhirError::BadRequest(msg) => (400, msg.as_str()),
        FhirError::VersionConflict => (409, "Version conflict"),
        FhirError::Conflict(msg) => (409, msg.as_str()),
        FhirError::PreconditionFailed(msg) => (412, msg.as_str()),
        _ => (500, "Internal server error"),
    };

    serde_json::json!({
        "response": {
            "status": format!("{} {}", status_code, get_status_text(status_code)),
            "outcome": {
                "resourceType": "OperationOutcome",
                "issue": [{
                    "severity": "error",
                    "code": "processing",
                    "diagnostics": diagnostics
                }]
            }
        }
    })
}

fn get_status_text(code: u16) -> &'static str {
    match code {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        400 => "Bad Request",
        404 => "Not Found",
        409 => "Conflict",
        412 => "Precondition Failed",
        500 => "Internal Server Error",
        _ => "Unknown",
    }
}

async fn process_post(
    state: &Arc<BundleState>,
    url: &str,
    resource: Option<&Value>,
    if_none_exist: Option<&str>,
) -> Result<Value> {
    let resource = resource
        .ok_or_else(|| FhirError::BadRequest("POST request must have a resource".to_string()))?;

    let resource_type = resource
        .get("resourceType")
        .and_then(|r| r.as_str())
        .ok_or_else(|| FhirError::BadRequest("Resource must have resourceType".to_string()))?;

    match (url, resource_type) {
        ("Patient", "Patient") => {
            // Handle conditional create if present
            if let Some(search_params_str) = if_none_exist {
                let mut params = std::collections::HashMap::new();
                for param in search_params_str.split('&') {
                    if let Some((key, value)) = param.split_once('=') {
                        params.insert(key.to_string(), value.to_string());
                    }
                }

                let (existing, _) = state.patient_repo.search(&params, false).await?;

                if !existing.is_empty() {
                    // Resource already exists - return 200 with existing resource
                    let existing_patient = &existing[0];
                    return Ok(serde_json::json!({
                        "response": {
                            "status": "200 OK",
                            "location": format!("Patient/{}", existing_patient.id),
                            "etag": format!("W/\"{}\"", existing_patient.version_id),
                            "lastModified": existing_patient.last_updated
                        },
                        "resource": existing_patient.content
                    }));
                }
            }

            // Create new resource
            let patient = state.patient_repo.create(resource.clone()).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "201 Created",
                    "location": format!("Patient/{}", patient.id),
                    "etag": format!("W/\"{}\"", patient.version_id),
                    "lastModified": patient.last_updated
                },
                "resource": patient.content
            }))
        }
        ("Observation", "Observation") => {
            let observation = state.observation_repo.create(resource.clone()).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "201 Created",
                    "location": format!("Observation/{}", observation.id),
                    "etag": format!("W/\"{}\"", observation.version_id),
                    "lastModified": observation.last_updated
                },
                "resource": observation.content
            }))
        }
        _ => Err(FhirError::BadRequest(format!(
            "Unsupported resource type or URL mismatch: {} / {}",
            url, resource_type
        ))),
    }
}

async fn process_put(
    state: &Arc<BundleState>,
    url: &str,
    resource: Option<&Value>,
) -> Result<Value> {
    let resource = resource
        .ok_or_else(|| FhirError::BadRequest("PUT request must have a resource".to_string()))?;

    // Parse URL like "Patient/{id}" or "Observation/{id}"
    let parts: Vec<&str> = url.split('/').collect();
    if parts.len() != 2 {
        return Err(FhirError::BadRequest(format!(
            "Invalid URL format: {}",
            url
        )));
    }

    let resource_type = parts[0];
    let id = parts[1];

    match resource_type {
        "Patient" => {
            let patient = state.patient_repo.update(id, resource.clone()).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "200 OK",
                    "location": format!("Patient/{}", patient.id),
                    "etag": format!("W/\"{}\"", patient.version_id),
                    "lastModified": patient.last_updated
                },
                "resource": patient.content
            }))
        }
        "Observation" => {
            let observation = state.observation_repo.update(id, resource.clone()).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "200 OK",
                    "location": format!("Observation/{}", observation.id),
                    "etag": format!("W/\"{}\"", observation.version_id),
                    "lastModified": observation.last_updated
                },
                "resource": observation.content
            }))
        }
        _ => Err(FhirError::BadRequest(format!(
            "Unsupported resource type: {}",
            resource_type
        ))),
    }
}

async fn process_get(state: &Arc<BundleState>, url: &str) -> Result<Value> {
    // Check if this is a search (has query parameters)
    if url.contains('?') {
        let parts: Vec<&str> = url.splitn(2, '?').collect();
        let resource_type = parts[0];
        let query_string = parts.get(1).unwrap_or(&"");

        // Parse query parameters
        let mut params = std::collections::HashMap::new();
        for param in query_string.split('&') {
            if let Some((key, value)) = param.split_once('=') {
                params.insert(key.to_string(), value.to_string());
            }
        }

        match resource_type {
            "Patient" => {
                let (patients, total) = state.patient_repo.search(&params, true).await?;

                // Build search bundle
                let entries: Vec<Value> = patients.into_iter().map(|p| {
                    serde_json::json!({
                        "fullUrl": format!("Patient/{}", p.id),
                        "resource": p.content
                    })
                }).collect();

                let search_bundle = serde_json::json!({
                    "resourceType": "Bundle",
                    "type": "searchset",
                    "total": total.unwrap_or(0),
                    "entry": entries
                });

                Ok(serde_json::json!({
                    "response": {
                        "status": "200 OK"
                    },
                    "resource": search_bundle
                }))
            }
            _ => Err(FhirError::BadRequest(format!(
                "Unsupported resource type for search: {}",
                resource_type
            ))),
        }
    } else {
        // Parse URL like "Patient/{id}" or "Observation/{id}"
        let parts: Vec<&str> = url.split('/').collect();
        if parts.len() != 2 {
            return Err(FhirError::BadRequest(format!(
                "Invalid URL format: {}",
                url
            )));
        }

        let resource_type = parts[0];
        let id = parts[1];

        match resource_type {
            "Patient" => {
                let patient = state.patient_repo.read(id).await?;
                Ok(serde_json::json!({
                    "response": {
                        "status": "200 OK",
                        "etag": format!("W/\"{}\"", patient.version_id),
                        "lastModified": patient.last_updated
                    },
                    "resource": patient.content
                }))
            }
            "Observation" => {
                let observation = state.observation_repo.read(id).await?;
                Ok(serde_json::json!({
                    "response": {
                        "status": "200 OK",
                        "etag": format!("W/\"{}\"", observation.version_id),
                        "lastModified": observation.last_updated
                    },
                    "resource": observation.content
                }))
            }
            _ => Err(FhirError::BadRequest(format!(
                "Unsupported resource type: {}",
                resource_type
            ))),
        }
    }
}

async fn process_delete(state: &Arc<BundleState>, url: &str) -> Result<Value> {
    // Parse URL like "Patient/{id}" or "Observation/{id}"
    let parts: Vec<&str> = url.split('/').collect();
    if parts.len() != 2 {
        return Err(FhirError::BadRequest(format!(
            "Invalid URL format: {}",
            url
        )));
    }

    let resource_type = parts[0];
    let id = parts[1];

    match resource_type {
        "Patient" => {
            state.patient_repo.delete(id).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "204 No Content"
                }
            }))
        }
        "Observation" => {
            state.observation_repo.delete(id).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "204 No Content"
                }
            }))
        }
        _ => Err(FhirError::BadRequest(format!(
            "Unsupported resource type: {}",
            resource_type
        ))),
    }
}
