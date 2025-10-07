use axum::{extract::State, http::StatusCode, Json};
use serde_json::Value;
use std::sync::Arc;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::repository::{ObservationRepository, PatientRepository};

pub struct BundleState {
    pub patient_repo: Arc<PatientRepository>,
    pub observation_repo: Arc<ObservationRepository>,
}

/// Process a transaction bundle
pub async fn process_bundle(
    State(state): State<Arc<BundleState>>,
    Json(bundle): Json<Value>,
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

    // Process each entry
    for entry in entries {
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

        // Process based on method
        let response = match method {
            "POST" => process_post(&state, url, resource).await?,
            "PUT" => process_put(&state, url, resource).await?,
            "GET" => process_get(&state, url).await?,
            "DELETE" => process_delete(&state, url).await?,
            _ => {
                return Err(FhirError::BadRequest(format!(
                    "Unsupported method: {}",
                    method
                )))
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

async fn process_post(
    state: &Arc<BundleState>,
    url: &str,
    resource: Option<&Value>,
) -> Result<Value> {
    let resource = resource
        .ok_or_else(|| FhirError::BadRequest("POST request must have a resource".to_string()))?;

    let resource_type = resource
        .get("resourceType")
        .and_then(|r| r.as_str())
        .ok_or_else(|| FhirError::BadRequest("Resource must have resourceType".to_string()))?;

    match (url, resource_type) {
        ("Patient", "Patient") => {
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
        return Err(FhirError::BadRequest(format!("Invalid URL format: {}", url)));
    }

    let resource_type = parts[0];
    let id = Uuid::parse_str(parts[1])
        .map_err(|_| FhirError::BadRequest(format!("Invalid UUID: {}", parts[1])))?;

    match resource_type {
        "Patient" => {
            let patient = state.patient_repo.update(&id, resource.clone()).await?;
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
            let observation = state.observation_repo.update(&id, resource.clone()).await?;
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
    // Parse URL like "Patient/{id}" or "Observation/{id}"
    let parts: Vec<&str> = url.split('/').collect();
    if parts.len() != 2 {
        return Err(FhirError::BadRequest(format!("Invalid URL format: {}", url)));
    }

    let resource_type = parts[0];
    let id = Uuid::parse_str(parts[1])
        .map_err(|_| FhirError::BadRequest(format!("Invalid UUID: {}", parts[1])))?;

    match resource_type {
        "Patient" => {
            let patient = state.patient_repo.read(&id).await?;
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
            let observation = state.observation_repo.read(&id).await?;
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

async fn process_delete(state: &Arc<BundleState>, url: &str) -> Result<Value> {
    // Parse URL like "Patient/{id}" or "Observation/{id}"
    let parts: Vec<&str> = url.split('/').collect();
    if parts.len() != 2 {
        return Err(FhirError::BadRequest(format!("Invalid URL format: {}", url)));
    }

    let resource_type = parts[0];
    let id = Uuid::parse_str(parts[1])
        .map_err(|_| FhirError::BadRequest(format!("Invalid UUID: {}", parts[1])))?;

    match resource_type {
        "Patient" => {
            state.patient_repo.delete(&id).await?;
            Ok(serde_json::json!({
                "response": {
                    "status": "204 No Content"
                }
            }))
        }
        "Observation" => {
            state.observation_repo.delete(&id).await?;
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
