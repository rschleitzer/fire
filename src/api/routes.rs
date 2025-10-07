use axum::{
    routing::get,
    Router,
};

use super::handlers::observation::{
    create_observation, delete_observation, get_observation_history, read_observation,
    read_observation_version, search_observations, update_observation, SharedObservationRepo,
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
        .route("/fhir/Observation", get(search_observations).post(create_observation))
        .route("/fhir/Observation/:id", get(read_observation).put(update_observation).delete(delete_observation))
        .route("/fhir/Observation/:id/_history", get(get_observation_history))
        .route("/fhir/Observation/:id/_history/:version_id", get(read_observation_version))
        .with_state(repo)
}
