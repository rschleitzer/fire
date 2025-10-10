use axum::{
    routing::{get, post},
    Router,
};
use std::sync::Arc;

use super::handlers::bundle::{process_bundle, BundleState};
use super::handlers::health::{health_check, liveness_check, readiness_check, SharedPool};
use super::handlers::metadata::capability_statement;
use super::handlers::observation::{
    create_observation, delete_observation, get_observation_history, read_observation,
    read_observation_version, search_observations, update_observation, update_observation_form,
    SharedObservationRepo,
};
use super::handlers::patient::{
    create_patient, delete_patient, get_patient_history, read_patient, read_patient_version,
    search_patients, update_patient, update_patient_form, SharedPatientRepo,
};

pub fn patient_routes(repo: SharedPatientRepo) -> Router {
    Router::new()
        .route("/fhir/Patient", get(search_patients).post(create_patient))
        .route(
            "/fhir/Patient/:id",
            get(read_patient)
                .post(update_patient_form)
                .put(update_patient)
                .delete(delete_patient),
        )
        .route("/fhir/Patient/:id/_history", get(get_patient_history))
        .route(
            "/fhir/Patient/:id/_history/:version_id",
            get(read_patient_version),
        )
        .with_state(repo)
}

pub fn observation_routes(repo: SharedObservationRepo) -> Router {
    Router::new()
        .route(
            "/fhir/Observation",
            get(search_observations).post(create_observation),
        )
        .route(
            "/fhir/Observation/:id",
            get(read_observation)
                .post(update_observation_form)
                .put(update_observation)
                .delete(delete_observation),
        )
        .route(
            "/fhir/Observation/:id/_history",
            get(get_observation_history),
        )
        .route(
            "/fhir/Observation/:id/_history/:version_id",
            get(read_observation_version),
        )
        .with_state(repo)
}

pub fn bundle_routes(
    patient_repo: SharedPatientRepo,
    observation_repo: SharedObservationRepo,
) -> Router {
    let state = Arc::new(BundleState {
        patient_repo,
        observation_repo,
    });

    Router::new()
        .route("/fhir", post(process_bundle))
        .with_state(state)
}

pub fn health_routes(pool: SharedPool) -> Router {
    Router::new()
        .route("/health", get(health_check))
        .route("/health/ready", get(readiness_check))
        .route("/health/live", get(liveness_check))
        .with_state(pool)
}

pub fn metadata_routes() -> Router {
    Router::new().route("/fhir/metadata", get(capability_statement))
}
