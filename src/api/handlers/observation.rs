use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    Json,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

use crate::error::Result;
use crate::repository::ObservationRepository;

pub type SharedObservationRepo = Arc<ObservationRepository>;

/// Create a new observation
pub async fn create_observation(
    State(repo): State<SharedObservationRepo>,
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> {
    let observation = repo.create(content).await?;
    Ok((StatusCode::CREATED, Json(observation.content.clone())))
}

/// Read an observation by ID
pub async fn read_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
) -> Result<Json<Value>> {
    let observation = repo.read(&id).await?;
    Ok(Json(observation.content))
}

/// Update an observation
pub async fn update_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
    Json(content): Json<Value>,
) -> Result<Json<Value>> {
    let observation = repo.update(&id, content).await?;
    Ok(Json(observation.content))
}

/// Delete an observation
pub async fn delete_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
) -> Result<StatusCode> {
    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}

/// Get observation history
pub async fn get_observation_history(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
) -> Result<Json<Value>> {
    let history = repo.history(&id).await?;

    let entries: Vec<Value> = history
        .into_iter()
        .map(|h| {
            serde_json::json!({
                "resource": h.content,
                "request": {
                    "method": h.history_operation,
                    "url": format!("Observation/{}", h.id)
                },
                "response": {
                    "status": "200",
                    "lastModified": h.last_updated
                }
            })
        })
        .collect();

    let bundle = serde_json::json!({
        "resourceType": "Bundle",
        "type": "history",
        "total": entries.len(),
        "entry": entries
    });

    Ok(Json(bundle))
}

/// Get specific version of an observation
pub async fn read_observation_version(
    State(repo): State<SharedObservationRepo>,
    Path((id, version_id)): Path<(Uuid, i32)>,
) -> Result<Json<Value>> {
    let observation = repo.read_version(&id, version_id).await?;
    Ok(Json(observation.content))
}

/// Search observations
pub async fn search_observations(
    State(repo): State<SharedObservationRepo>,
    Query(params): Query<HashMap<String, String>>,
) -> Result<Json<Value>> {
    let include_total = params.get("_total").map_or(false, |v| v == "accurate");
    let (observations, total) = repo.search(&params, include_total).await?;

    let mut entries: Vec<Value> = observations
        .iter()
        .map(|obs| {
            serde_json::json!({
                "resource": obs.content,
                "search": {
                    "mode": "match"
                }
            })
        })
        .collect();

    // Handle _include parameter for Patient references
    if let Some(include_param) = params.get("_include") {
        if include_param == "Observation:patient" || include_param == "Observation:subject" {
            // Collect unique patient IDs from observations
            let mut patient_ids = std::collections::HashSet::new();
            for obs in &observations {
                if let Some(patient_ref) = &obs.patient_reference {
                    if let Some(id_str) = patient_ref.strip_prefix("Patient/") {
                        if let Ok(patient_id) = Uuid::parse_str(id_str) {
                            patient_ids.insert(patient_id);
                        }
                    }
                }
            }

            // Fetch and include patient resources
            for patient_id in patient_ids {
                if let Ok(patient) = repo.read_patient(&patient_id).await {
                    entries.push(serde_json::json!({
                        "resource": patient.content,
                        "search": {
                            "mode": "include"
                        }
                    }));
                }
            }
        }
    }

    let mut bundle = serde_json::json!({
        "resourceType": "Bundle",
        "type": "searchset",
        "entry": entries
    });

    if let Some(count) = total {
        bundle["total"] = serde_json::json!(count);
    }

    Ok(Json(bundle))
}
