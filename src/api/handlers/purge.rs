use axum::{extract::{Path, State}, http::StatusCode};
use std::sync::Arc;

use crate::repository::{ObservationRepository, PatientRepository};

/// State for purge endpoints
pub struct PurgeState {
    pub patient_repo: Arc<PatientRepository>,
    pub observation_repo: Arc<ObservationRepository>,
}

/// Purge schema endpoint - FOR TESTING ONLY
/// Deletes all records (current and history) for a given resource type
/// Requires a matching secret key to prevent accidental data deletion
pub async fn purge_schema(
    State(state): State<Arc<PurgeState>>,
    Path((resource_type, key)): Path<(String, String)>,
) -> StatusCode {
    // Verify the secret key matches (same as used in test fixtures)
    const EXPECTED_KEY: &str = "c522382c-8656-463e-8277-b913e4466f53";

    if key != EXPECTED_KEY {
        return StatusCode::UNAUTHORIZED;
    }

    // Purge based on resource type
    let result = match resource_type.as_str() {
        "Patient" => state.patient_repo.purge().await,
        "Observation" => state.observation_repo.purge().await,
        _ => {
            // Unsupported resource type
            return StatusCode::BAD_REQUEST;
        }
    };

    match result {
        Ok(_) => StatusCode::OK,
        Err(_) => StatusCode::INTERNAL_SERVER_ERROR,
    }
}
