use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Redirect, Response},
    Form, Json,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use uuid::Uuid;

use crate::api::content_negotiation::*;
use crate::api::xml_serializer::json_to_xml;
use crate::error::Result;
use crate::repository::ObservationRepository;
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
    Json(content): Json<Value>,
) -> Result<(StatusCode, HeaderMap, Json<Value>)> {
    let observation = repo.create(content).await?;

    // Build Location header per FHIR spec
    let location = format!("/fhir/Observation/{}", observation.id);
    let mut headers = HeaderMap::new();
    headers.insert(
        axum::http::header::LOCATION,
        location.parse().unwrap()
    );

    Ok((StatusCode::CREATED, headers, Json(observation.to_fhir_json())))
}

/// Read an observation by ID
pub async fn read_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id_str): Path<String>,
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
    let id = Uuid::parse_str(&id_str)
        .map_err(|_| crate::error::FhirError::BadRequest(format!("Invalid observation ID: {}", id_str)))?;

    // Try to read the observation - if it doesn't exist, return empty template for new resource
    match repo.read(&id).await {
        Ok(observation) => {
            let fhir_json = observation.to_fhir_json();

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = ObservationEditTemplate {
                        id: id.to_string(),
                        resource_json: serde_json::to_string(&fhir_json)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => Ok(Json(fhir_json).into_response()),
                ResponseFormat::Xml => {
                    let xml_string = json_to_xml(&fhir_json)
                        .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
                    Ok((
                        [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                        xml_string
                    ).into_response())
                }
            }
        }
        Err(crate::error::FhirError::NotFound) => {
            // Resource doesn't exist - return empty template for new resource
            let empty_observation = serde_json::json!({
                "resourceType": "Observation",
                "id": id.to_string(),
                "status": "final",
                "code": { "text": "", "coding": [] },
                "subject": { "reference": "", "display": "" },
                "valueQuantity": { "value": null, "unit": "", "system": "http://unitsofmeasure.org", "code": "" }
            });

            match preferred_format_with_query(&uri, &headers) {
                ResponseFormat::Html => {
                    let template = ObservationEditTemplate {
                        id: id.to_string(),
                        resource_json: serde_json::to_string(&empty_observation)?,
                    };
                    Ok(Html(template.render().unwrap()).into_response())
                }
                ResponseFormat::Json => Ok(Json(empty_observation).into_response()),
                ResponseFormat::Xml => {
                    let xml_string = json_to_xml(&empty_observation)
                        .map_err(|e| crate::error::FhirError::Internal(anyhow::anyhow!(e)))?;
                    Ok((
                        [(axum::http::header::CONTENT_TYPE, "application/fhir+xml")],
                        xml_string
                    ).into_response())
                }
            }
        }
        Err(e) => Err(e),
    }
}

/// Update an observation
pub async fn update_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
    Json(content): Json<Value>,
) -> Result<Json<Value>> {
    let observation = repo.update(&id, content).await?;
    Ok(Json(observation.to_fhir_json()))
}

/// Update an observation (HTML form)
pub async fn update_observation_form(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
    Form(form_data): Form<ObservationFormData>,
) -> Result<Redirect> {
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

    repo.update(&id, content).await?;

    // Redirect back to the observation detail page
    Ok(Redirect::to(&format!("/fhir/Observation/{}", id)))
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
    headers: HeaderMap,
    uri: axum::http::Uri,
) -> Result<Response> {
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

/// Get specific version of an observation
pub async fn read_observation_version(
    State(repo): State<SharedObservationRepo>,
    Path((id, version_id)): Path<(Uuid, i32)>,
) -> Result<Json<Value>> {
    let observation = repo.read_version(&id, version_id).await?;
    Ok(Json(observation.content))
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
            if let Ok(id) = Uuid::parse_str(id_str.trim()) {
                repo.delete(&id).await?;
            }
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
    Path((id, version)): Path<(Uuid, i32)>,
) -> Result<StatusCode> {
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
    let include_total = params.get("_total").is_some_and(|v| v == "accurate");
    let (observations, total) = repo.search(&params, include_total).await?;

    // Build Bundle using efficient string concatenation
    let mut entries = Vec::new();

    // Add observation entries with id and meta fields
    for obs in &observations {
        entries.push(format!(
            r#"{{"resource":{},"search":{{"mode":"match"}}}}"#,
            serde_json::to_string(&obs.to_fhir_json())?
        ));
    }

    // Handle _include parameter for Patient references
    if let Some(include_param) = params.get("_include") {
        if include_param == "Observation:patient" || include_param == "Observation:subject" {
            // Collect unique patient IDs from observations
            let mut patient_ids = std::collections::HashSet::new();
            for obs in &observations {
                // Extract patient reference from content JSON
                if let Some(patient_ref) = obs
                    .content
                    .get("subject")
                    .and_then(|s| s.get("reference"))
                    .and_then(|r| r.as_str())
                {
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
                    entries.push(format!(
                        r#"{{"resource":{},"search":{{"mode":"include"}}}}"#,
                        serde_json::to_string(&patient.to_fhir_json())?
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
            // Convert observations to HTML table rows
            let observation_rows: Vec<ObservationRow> = observations
                .iter()
                .map(|obs| {
                    let fhir_json = obs.to_fhir_json();
                    ObservationRow {
                        id: obs.id.to_string(),
                        version_id: obs.version_id.to_string(),
                        code: extract_observation_code(&fhir_json),
                        status: fhir_json
                            .get("status")
                            .and_then(|s| s.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        value: extract_observation_value(&fhir_json),
                        subject: fhir_json
                            .get("subject")
                            .and_then(|s| s.get("reference"))
                            .and_then(|r| r.as_str())
                            .unwrap_or("Unknown")
                            .to_string(),
                        effective_date: fhir_json
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

