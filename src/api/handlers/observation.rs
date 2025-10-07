use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use serde_json::Value;
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

/// Delete an observation
pub async fn delete_observation(
    State(repo): State<SharedObservationRepo>,
    Path(id): Path<Uuid>,
) -> Result<StatusCode> {
    repo.delete(&id).await?;
    Ok(StatusCode::NO_CONTENT)
}
