use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Response},
    Json,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

use crate::api::content_negotiation::*;
use crate::error::Result;
use crate::repository::PatientRepository;
use askama::Template;

pub type SharedPatientRepo = Arc<PatientRepository>;

/// Create a new patient
pub async fn create_patient(
    State(repo): State<SharedPatientRepo>,
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> {
    let patient = repo.create(content).await?;
    Ok((StatusCode::CREATED, Json(patient.to_fhir_json())))
}

/// Read a patient by ID
pub async fn read_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    headers: HeaderMap,
) -> Result<Response> {
    let patient = repo.read(&id).await?;
    let fhir_json = patient.to_fhir_json();

    match preferred_format(&headers) {
        ResponseFormat::Html => {
            // Extract fields for HTML template
            let name = extract_patient_name(&fhir_json);
            let gender = fhir_json.get("gender")
                .and_then(|g| g.as_str())
                .unwrap_or("Unknown")
                .to_string();
            let birth_date = fhir_json.get("birthDate")
                .and_then(|b| b.as_str())
                .unwrap_or("Unknown")
                .to_string();
            let active = fhir_json.get("active")
                .and_then(|a| a.as_bool())
                .unwrap_or(true);

            let template = PatientDetailTemplate {
                id: id.to_string(),
                version_id: patient.version_id.to_string(),
                last_updated: patient.last_updated.to_rfc3339(),
                name,
                gender,
                birth_date,
                active,
                resource_json: serde_json::to_string_pretty(&fhir_json)?,
            };

            Ok(Html(template.render().unwrap()).into_response())
        }
        ResponseFormat::Json => Ok(Json(fhir_json).into_response()),
    }
}

/// Update a patient
pub async fn update_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    Json(content): Json<Value>,
) -> Result<Json<Value>> {
    let patient = repo.update(&id, content).await?;
    Ok(Json(patient.to_fhir_json()))
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
) -> Result<Response> {
    // Check if _total parameter is requested
    let include_total = params.get("_total").map(|t| t == "accurate").unwrap_or(false);

    let (patients, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add patient entries with id and meta fields
    for p in &patients {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&p.to_fhir_json())?
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
                        serde_json::to_string(&obs.to_fhir_json())?
                    ));
                }
            }
        }
    }

    // Build final Bundle JSON string
    let entries_str = entries.join(",");
    let bundle_str = if let Some(total_count) = total {
        format!(
            r#"{{"resourceType":"Bundle","type":"searchset","total":{},"entry":[{}]}}"#,
            total_count, entries_str
        )
    } else {
        format!(
            r#"{{"resourceType":"Bundle","type":"searchset","entry":[{}]}}"#,
            entries_str
        )
    };

    // Parse string back to Value for Json response
    let bundle: Value = serde_json::from_str(&bundle_str)?;

    match preferred_format(&headers) {
        ResponseFormat::Html => {
            // Convert patients to HTML table rows
            let patient_rows: Vec<PatientRow> = patients
                .iter()
                .map(|p| {
                    let fhir_json = p.to_fhir_json();
                    PatientRow {
                        id: p.id.to_string(),
                        name: extract_patient_name(&fhir_json),
                        gender: fhir_json.get("gender")
                            .and_then(|g| g.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        birth_date: fhir_json.get("birthDate")
                            .and_then(|b| b.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        active: fhir_json.get("active")
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
    }
}

/// Get patient history
pub async fn get_patient_history(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
) -> Result<Json<Value>> {
    let history = repo.history(&id).await?;

    // Build Bundle using efficient string concatenation
    let total = history.len();
    let entries: Vec<String> = history
        .into_iter()
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
