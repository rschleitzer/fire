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
use crate::repository::PatientRepository;

pub type SharedPatientRepo = Arc<PatientRepository>;

/// Create a new patient
pub async fn create_patient(
    State(repo): State<SharedPatientRepo>,
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> {
    let patient = repo.create(content).await?;
    Ok((StatusCode::CREATED, Json(patient.content.clone())))
}

/// Read a patient by ID
pub async fn read_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
) -> Result<Json<Value>> {
    let patient = repo.read(&id).await?;
    Ok(Json(patient.content))
}

/// Update a patient
pub async fn update_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    Json(content): Json<Value>,
) -> Result<Json<Value>> {
    let patient = repo.update(&id, content).await?;
    Ok(Json(patient.content))
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
) -> Result<Json<Value>> {
    // Check if _total parameter is requested
    let include_total = params.get("_total").map(|t| t == "accurate").unwrap_or(false);

    let (patients, total) = repo.search(&params, include_total).await?;

    // Build FHIR Bundle response
    let mut entries: Vec<Value> = patients
        .iter()
        .map(|p| {
            serde_json::json!({
                "resource": p.content,
                "search": {
                    "mode": "match"
                }
            })
        })
        .collect();

    // Handle _revinclude parameter for Observation references
    if let Some(revinclude_param) = params.get("_revinclude") {
        if revinclude_param == "Observation:patient" || revinclude_param == "Observation:subject" {
            // For each patient, find observations that reference it
            for patient in &patients {
                let observations = repo.find_observations_by_patient(&patient.id).await?;
                for obs in observations {
                    entries.push(serde_json::json!({
                        "resource": obs.content,
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

    // Add total if requested or if we have it
    if let Some(total_count) = total {
        bundle["total"] = serde_json::json!(total_count);
    }

    Ok(Json(bundle))
}

/// Get patient history
pub async fn get_patient_history(
    State(repo): State<SharedPatientRepo>,
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
                    "url": format!("Patient/{}", h.id)
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

/// Get specific version of a patient
pub async fn read_patient_version(
    State(repo): State<SharedPatientRepo>,
    Path((id, version_id)): Path<(Uuid, i32)>,
) -> Result<Json<Value>> {
    let patient = repo.read_version(&id, version_id).await?;
    Ok(Json(patient.content))
}
