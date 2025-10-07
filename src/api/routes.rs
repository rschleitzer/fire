use axum::{
    routing::get,
    Router,
};

use super::handlers::observation::{
    create_observation, delete_observation, read_observation, SharedObservationRepo,
};
use super::handlers::patient::{
    create_patient, delete_patient, get_patient_history, read_patient, read_patient_version,
    search_patients, update_patient, SharedPatientRepo,
};

pub fn patient_routes(repo: SharedPatientRepo) -> Router {
    Router::new()
        .route("/fhir/Patient", get(search_patients).post(create_patient))
        .route("/fhir/Patient/:id", get(read_patient).put(update_patient).delete(delete_patient))
        .route("/fhir/Patient/:id/_history", get(get_patient_history))
        .route("/fhir/Patient/:id/_history/:version_id", get(read_patient_version))
        .with_state(repo)
}

pub fn observation_routes(repo: SharedObservationRepo) -> Router {
    Router::new()
        .route("/fhir/Observation", get(|| async { "Observation search not yet implemented" }).post(create_observation))
        .route("/fhir/Observation/:id", get(read_observation).delete(delete_observation))
        .with_state(repo)
}
