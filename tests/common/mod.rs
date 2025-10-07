use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;
use std::sync::Arc;

/// Test helper to create a test database pool
pub async fn setup_test_db() -> PgPool {
    let database_url = std::env::var("DATABASE_URL")
        .unwrap_or_else(|_| "postgres://postgres:postgres@localhost/fhir_test".to_string());

    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await
        .expect("Failed to connect to test database");

    // Run migrations
    sqlx::migrate!("./migrations")
        .run(&pool)
        .await
        .expect("Failed to run migrations");

    pool
}

/// Clean up test data
pub async fn cleanup_test_db(pool: &PgPool) {
    sqlx::query("TRUNCATE TABLE patient_history CASCADE")
        .execute(pool)
        .await
        .expect("Failed to truncate patient_history");

    sqlx::query("TRUNCATE TABLE patient CASCADE")
        .execute(pool)
        .await
        .expect("Failed to truncate patient");

    sqlx::query("TRUNCATE TABLE observation_history CASCADE")
        .execute(pool)
        .await
        .expect("Failed to truncate observation_history");

    sqlx::query("TRUNCATE TABLE observation CASCADE")
        .execute(pool)
        .await
        .expect("Failed to truncate observation");
}

/// Create a test patient JSON
pub fn test_patient_json() -> serde_json::Value {
    serde_json::json!({
        "resourceType": "Patient",
        "name": [{
            "family": "TestFamily",
            "given": ["TestGiven"]
        }],
        "gender": "male",
        "birthDate": "1990-01-01",
        "active": true
    })
}

/// Create a test observation JSON
pub fn test_observation_json(patient_id: &str) -> serde_json::Value {
    serde_json::json!({
        "resourceType": "Observation",
        "status": "final",
        "code": {
            "coding": [{
                "system": "http://loinc.org",
                "code": "8867-4",
                "display": "Heart rate"
            }]
        },
        "subject": {
            "reference": format!("Patient/{}", patient_id)
        },
        "effectiveDateTime": "2024-01-15T10:30:00Z",
        "valueQuantity": {
            "value": 72,
            "unit": "beats/minute",
            "system": "http://unitsofmeasure.org",
            "code": "/min"
        }
    })
}
