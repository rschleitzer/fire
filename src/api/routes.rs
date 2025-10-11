use axum::{
    response::Html,
    routing::{get, post},
    Router,
};
use std::sync::Arc;

use super::handlers::bundle::{process_bundle, BundleState};
use super::handlers::health::{health_check, liveness_check, readiness_check, SharedPool};
use super::handlers::metadata::capability_statement;

async fn index() -> Html<String> {
    Html(std::fs::read_to_string("static/index.html").unwrap_or_else(|_| String::from("<h1>Fire FHIR Server</h1>")))
}
use super::handlers::observation::{
    create_observation, delete_observation, delete_observations,
    get_observation_history, read_observation, read_observation_version, rollback_observation,
    search_observations, update_observation, update_observation_form, SharedObservationRepo,
};
use super::handlers::patient::{
    create_patient, delete_patient, delete_patients, get_patient_history, get_patient_type_history,
    read_patient, read_patient_version, rollback_patient, search_patients, update_patient,
    update_patient_form, SharedPatientRepo,
};

pub fn patient_routes(repo: SharedPatientRepo) -> Router {
    Router::new()
        .route(
            "/fhir/Patient",
            get(search_patients)
                .post(create_patient)
                .delete(delete_patients),
        )
        // Type-level history MUST be before instance routes to match correctly
        .route("/fhir/Patient/_history", get(get_patient_type_history))
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
        .route(
            "/fhir/Patient/:id/_rollback/:version",
            post(rollback_patient),
        )
        .with_state(repo)
}

pub fn observation_routes(repo: SharedObservationRepo) -> Router {
    Router::new()
        .route(
            "/fhir/Observation",
            get(search_observations)
                .post(create_observation)
                .delete(delete_observations),
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
        .route(
            "/fhir/Observation/:id/_rollback/:version",
            post(rollback_observation),
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

pub fn root_routes() -> Router {
    Router::new().route("/", get(index))
}
