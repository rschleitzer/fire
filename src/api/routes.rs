use axum::{
    routing::get,
    Router,
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
