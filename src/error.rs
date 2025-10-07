use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde_json::json;

#[derive(Debug, thiserror::Error)]
pub enum FhirError {
    #[error("Resource not found")]
    NotFound,

    #[error("Invalid resource: {0}")]
    InvalidResource(String),

    #[error("Version conflict")]
    VersionConflict,

    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),

    #[error("Internal error: {0}")]
    Internal(#[from] anyhow::Error),

    #[error("Invalid search parameter: {0}")]
    InvalidSearchParameter(String),

    #[error("Validation error: {0}")]
    ValidationError(String),
}

impl IntoResponse for FhirError {
    fn into_response(self) -> Response {
        let (status, severity, code, diagnostics) = match self {
            FhirError::NotFound => (
                StatusCode::NOT_FOUND,
                "error",
                "not-found",
                self.to_string(),
            ),
            FhirError::InvalidResource(_) => (
                StatusCode::BAD_REQUEST,
                "error",
                "invalid",
                self.to_string(),
            ),
            FhirError::VersionConflict => (
                StatusCode::CONFLICT,
                "error",
                "conflict",
                self.to_string(),
            ),
            FhirError::InvalidSearchParameter(_) => (
                StatusCode::BAD_REQUEST,
                "error",
                "invalid",
                self.to_string(),
            ),
            FhirError::ValidationError(_) => (
                StatusCode::BAD_REQUEST,
                "error",
                "invalid",
                self.to_string(),
            ),
            FhirError::DatabaseError(_) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                "error",
                "exception",
                "Database error occurred".to_string(),
            ),
            FhirError::Internal(_) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                "error",
                "exception",
                "Internal server error".to_string(),
            ),
        };

        let operation_outcome = create_operation_outcome(severity, code, &diagnostics);
        (status, Json(operation_outcome)).into_response()
    }
}

fn create_operation_outcome(severity: &str, code: &str, diagnostics: &str) -> serde_json::Value {
    json!({
        "resourceType": "OperationOutcome",
        "issue": [{
            "severity": severity,
            "code": code,
            "diagnostics": diagnostics
        }]
    })
}

pub type Result<T> = std::result::Result<T, FhirError>;
