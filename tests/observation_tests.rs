mod common;

use fire::models::observation::extract_observation_search_params;
use fire::repository::{ObservationRepository, PatientRepository};
use fire::services::validate_observation;

#[tokio::test]
async fn test_create_observation() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    // Create patient first
    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    // Create observation
    let obs_json = common::test_observation_json(&patient.id.to_string());
    let observation = obs_repo
        .create(obs_json)
        .await
        .expect("Failed to create observation");

    assert_eq!(observation.version_id, 1);
    assert_eq!(observation.content["resourceType"], "Observation");
    assert_eq!(observation.status, Some("final".to_string()));

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_read_observation() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    let obs_json = common::test_observation_json(&patient.id.to_string());
    let created = obs_repo.create(obs_json).await.expect("Failed to create");

    // Read observation
    let observation = obs_repo
        .read(&created.id)
        .await
        .expect("Failed to read observation");

    assert_eq!(observation.id, created.id);
    assert_eq!(observation.version_id, 1);
    assert_eq!(observation.status, Some("final".to_string()));

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_update_observation() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    let obs_json = common::test_observation_json(&patient.id.to_string());
    let created = obs_repo.create(obs_json).await.expect("Failed to create");

    // Update observation
    let mut updated_json = created.content.clone();
    updated_json["status"] = serde_json::json!("amended");

    let updated = obs_repo
        .update(&created.id, updated_json)
        .await
        .expect("Failed to update");

    assert_eq!(updated.id, created.id);
    assert_eq!(updated.version_id, 2);
    assert_eq!(updated.status, Some("amended".to_string()));

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_delete_observation() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    let obs_json = common::test_observation_json(&patient.id.to_string());
    let created = obs_repo.create(obs_json).await.expect("Failed to create");

    // Delete observation
    obs_repo
        .delete(&created.id)
        .await
        .expect("Failed to delete");

    // Try to read deleted observation should fail
    let result = obs_repo.read(&created.id).await;
    assert!(result.is_err());

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_observation_history() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    let obs_json = common::test_observation_json(&patient.id.to_string());
    let created = obs_repo.create(obs_json).await.expect("Failed to create");

    // Update to create history
    let mut updated_json = created.content.clone();
    updated_json["status"] = serde_json::json!("amended");
    obs_repo
        .update(&created.id, updated_json)
        .await
        .expect("Failed to update");

    // Get history
    let history = obs_repo
        .history(&created.id)
        .await
        .expect("Failed to get history");

    assert_eq!(history.len(), 2);
    assert_eq!(history[0].version_id, 2);
    assert_eq!(history[1].version_id, 1);
    assert_eq!(history[0].history_operation, "UPDATE");
    assert_eq!(history[1].history_operation, "CREATE");

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_observation_validation() {
    let patient_json = common::test_patient_json();
    let obs_json = common::test_observation_json("test-id");

    // Valid observation
    assert!(validate_observation(&obs_json).is_ok());

    // Invalid - missing status
    let mut invalid = obs_json.clone();
    invalid.as_object_mut().unwrap().remove("status");
    assert!(validate_observation(&invalid).is_err());

    // Invalid - missing code
    let mut invalid = obs_json.clone();
    invalid.as_object_mut().unwrap().remove("code");
    assert!(validate_observation(&invalid).is_err());

    // Invalid - invalid status
    let mut invalid = obs_json.clone();
    invalid["status"] = serde_json::json!("invalid");
    assert!(validate_observation(&invalid).is_err());
}

#[tokio::test]
async fn test_observation_search_params() {
    let obs_json = common::test_observation_json("test-patient-id");
    let params = extract_observation_search_params(&obs_json);

    assert_eq!(params.status, Some("final".to_string()));
    assert_eq!(params.code_code, Some("8867-4".to_string()));
    assert_eq!(params.code_system, Some("http://loinc.org".to_string()));
    assert_eq!(
        params.subject_reference,
        Some("Patient/test-patient-id".to_string())
    );
    assert!(params.value_quantity_value.is_some());
}

#[tokio::test]
async fn test_search_observation_by_status() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    let patient = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient");

    // Create observations with different statuses
    let mut obs1 = common::test_observation_json(&patient.id.to_string());
    obs1["status"] = serde_json::json!("final");
    obs_repo.create(obs1).await.expect("Failed to create");

    let mut obs2 = common::test_observation_json(&patient.id.to_string());
    obs2["status"] = serde_json::json!("preliminary");
    obs_repo.create(obs2).await.expect("Failed to create");

    // Search by status
    let mut params = std::collections::HashMap::new();
    params.insert("status".to_string(), "final".to_string());

    let (results, _) = obs_repo.search(&params, false).await.expect("Failed to search");

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].status, Some("final".to_string()));

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_search_observation_by_patient() {
    let pool = common::setup_test_db().await;
    let patient_repo = PatientRepository::new(pool.clone());
    let obs_repo = ObservationRepository::new(pool.clone());

    // Create two patients
    let patient1 = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient1");

    let patient2 = patient_repo
        .create(common::test_patient_json())
        .await
        .expect("Failed to create patient2");

    // Create observations for each patient
    obs_repo
        .create(common::test_observation_json(&patient1.id.to_string()))
        .await
        .expect("Failed to create obs1");

    obs_repo
        .create(common::test_observation_json(&patient2.id.to_string()))
        .await
        .expect("Failed to create obs2");

    // Search by patient
    let mut params = std::collections::HashMap::new();
    params.insert("patient".to_string(), patient1.id.to_string());

    let (results, _) = obs_repo.search(&params, false).await.expect("Failed to search");

    assert_eq!(results.len(), 1);
    assert_eq!(
        results[0].patient_reference,
        Some(format!("Patient/{}", patient1.id))
    );

    common::cleanup_test_db(&pool).await;
}
