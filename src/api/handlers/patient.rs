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
use crate::error::Result;
use crate::repository::PatientRepository;
use askama::Template;

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
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> {
    let patient = repo.create(content).await?;
    Ok((StatusCode::CREATED, Json(patient.to_fhir_json())))
}

/// Read a patient by ID
pub async fn read_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id_str): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    let id = Uuid::parse_str(&id_str)
        .map_err(|_| crate::error::FhirError::BadRequest(format!("Invalid patient ID: {}", id_str)))?;

    // Try to read the patient - if it doesn't exist, return empty template for new resource
    match repo.read(&id).await {
        Ok(patient) => {
            let fhir_json = patient.to_fhir_json();

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = PatientEditTemplate {
                        id: id.to_string(),
                        resource_json: serde_json::to_string(&fhir_json)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => Ok(Json(fhir_json).into_response()),
            }
        }
        Err(crate::error::FhirError::NotFound) => {
            // Resource doesn't exist - return empty template for new resource
            let empty_patient = serde_json::json!({
                "resourceType": "Patient",
                "id": id.to_string(),
                "active": true,
                "name": [],
                "telecom": [],
                "address": [],
                "identifier": []
            });

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = PatientEditTemplate {
                        id: id.to_string(),
                        resource_json: serde_json::to_string(&empty_patient)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => Ok(Json(empty_patient).into_response()),
            }
        }
        Err(e) => Err(e),
    }
}

/// Update a patient (JSON) - Uses upsert semantics per FHIR spec
pub async fn update_patient(
    State(repo): State<SharedPatientRepo>,
    Path(id): Path<Uuid>,
    Json(content): Json<Value>,
) -> Result<Json<Value>> {
    let patient = repo.upsert(&id, content).await?;
    Ok(Json(patient.to_fhir_json()))
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
    // Check if _total parameter is requested
    let include_total = params
        .get("_total")
        .map(|t| t == "accurate")
        .unwrap_or(false);

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

    match preferred_format_with_query(&uri, &headers) {
        ResponseFormat::Html => {
            // Convert patients to HTML table rows
            let patient_rows: Vec<PatientRow> = patients
                .iter()
                .map(|p| {
                    let fhir_json = p.to_fhir_json();
                    PatientRow {
                        id: p.id.to_string(),
                        version_id: p.version_id.to_string(),
                        name: extract_patient_name(&fhir_json),
                        gender: fhir_json
                            .get("gender")
                            .and_then(|g| g.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        birth_date: fhir_json
                            .get("birthDate")
                            .and_then(|b| b.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        active: fhir_json
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

