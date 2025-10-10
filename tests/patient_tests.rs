mod common;

use fire::models::patient::extract_patient_search_params;
use fire::repository::PatientRepository;
use fire::services::validate_patient;
use uuid::Uuid;

#[tokio::test]
async fn test_create_patient() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    let patient_json = common::test_patient_json();

    // Create patient
    let patient = repo
        .create(patient_json.clone())
        .await
        .expect("Failed to create patient");

    assert_eq!(patient.version_id, 1);
    assert_eq!(patient.content["resourceType"], "Patient");
    assert_eq!(patient.content["name"][0]["family"], "TestFamily");

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_read_patient() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    let patient_json = common::test_patient_json();
    let created = repo.create(patient_json).await.expect("Failed to create");

    // Read patient
    let patient = repo
        .read(&created.id)
        .await
        .expect("Failed to read patient");

    assert_eq!(patient.id, created.id);
    assert_eq!(patient.version_id, 1);
    assert_eq!(patient.content["name"][0]["family"], "TestFamily");

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_update_patient() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    let patient_json = common::test_patient_json();
    let created = repo.create(patient_json).await.expect("Failed to create");

    // Update patient
    let mut updated_json = created.content.clone();
    updated_json["name"][0]["family"] = serde_json::json!("UpdatedFamily");

    let updated = repo
        .update(&created.id, updated_json)
        .await
        .expect("Failed to update");

    assert_eq!(updated.id, created.id);
    assert_eq!(updated.version_id, 2);
    assert_eq!(updated.content["name"][0]["family"], "UpdatedFamily");

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_delete_patient() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    let patient_json = common::test_patient_json();
    let created = repo.create(patient_json).await.expect("Failed to create");

    // Delete patient
    repo.delete(&created.id)
        .await
        .expect("Failed to delete patient");

    // Try to read deleted patient should fail
    let result = repo.read(&created.id).await;
    assert!(result.is_err());

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_patient_history() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    let patient_json = common::test_patient_json();
    let created = repo.create(patient_json).await.expect("Failed to create");

    // Update to create history
    let mut updated_json = created.content.clone();
    updated_json["name"][0]["family"] = serde_json::json!("UpdatedFamily");
    repo.update(&created.id, updated_json)
        .await
        .expect("Failed to update");

    // Get history
    let history = repo
        .history(&created.id)
        .await
        .expect("Failed to get history");

    assert_eq!(history.len(), 2); // CREATE + UPDATE
    assert_eq!(history[0].version_id, 2); // Most recent first
    assert_eq!(history[1].version_id, 1);
    assert_eq!(history[0].history_operation, "UPDATE");
    assert_eq!(history[1].history_operation, "CREATE");

    common::cleanup_test_db(&pool).await;
}

#[tokio::test]
async fn test_patient_validation() {
    // Valid patient
    let valid = common::test_patient_json();
    assert!(validate_patient(&valid).is_ok());

    // Invalid - missing resourceType
    let mut invalid = valid.clone();
    invalid.as_object_mut().unwrap().remove("resourceType");
    assert!(validate_patient(&invalid).is_err());

    // Invalid - wrong resourceType
    let mut invalid = valid.clone();
    invalid["resourceType"] = serde_json::json!("Observation");
    assert!(validate_patient(&invalid).is_err());

    // Invalid - invalid gender
    let mut invalid = valid.clone();
    invalid["gender"] = serde_json::json!("invalid");
    assert!(validate_patient(&invalid).is_err());
}

#[tokio::test]
async fn test_patient_search_params() {
    let patient_json = common::test_patient_json();
    let params = extract_patient_search_params(&patient_json);

    assert_eq!(params.family_name, vec!["TestFamily".to_string()]);
    assert_eq!(params.given_name, vec!["TestGiven".to_string()]);
    assert_eq!(params.gender, Some("male".to_string()));
    assert_eq!(
        params.birthdate.map(|d| d.to_string()),
        Some("1990-01-01".to_string())
    );
    assert_eq!(params.active, Some(true));
}

#[tokio::test]
async fn test_search_patient_by_name() {
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());

    // Create test patients
    let mut patient1 = common::test_patient_json();
    patient1["name"][0]["family"] = serde_json::json!("Smith");
    repo.create(patient1).await.expect("Failed to create");

    let mut patient2 = common::test_patient_json();
    patient2["name"][0]["family"] = serde_json::json!("Jones");
    repo.create(patient2).await.expect("Failed to create");

    // Search by name
    let mut params = std::collections::HashMap::new();
    params.insert("family".to_string(), "Smith".to_string());

    let (results, _) = repo.search(&params, false).await.expect("Failed to search");

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].content["name"][0]["family"], "Smith");

    common::cleanup_test_db(&pool).await;
}
