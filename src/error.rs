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

    #[error("Conflict: {0}")]
    Conflict(String),

    #[error("Bad request: {0}")]
    BadRequest(String),

    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),

    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),

    #[error("Internal error: {0}")]
    Internal(#[from] anyhow::Error),

    #[error("Invalid search parameter: {0}")]
    InvalidSearchParameter(String),

    #[error("Validation error: {0}")]
    ValidationError(String),
}

impl IntoResponse for FhirError {
    fn into_response(self) -> Response {
        let (status, severity, code, diagnostics) = match &self {
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
            FhirError::VersionConflict => {
                (StatusCode::CONFLICT, "error", "conflict", self.to_string())
            }
            FhirError::Conflict(_) => {
                (StatusCode::CONFLICT, "error", "conflict", self.to_string())
            }
            FhirError::BadRequest(_) => (
                StatusCode::BAD_REQUEST,
                "error",
                "invalid",
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
            FhirError::DatabaseError(err) => {
                // Log database errors with details
                tracing::error!("Database error: {:?}", err);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "error",
                    "exception",
                    "Database error occurred".to_string(),
                )
            }
            FhirError::JsonError(err) => {
                // Log JSON errors with details
                tracing::error!("JSON error: {:?}", err);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "error",
                    "exception",
                    "JSON processing error".to_string(),
                )
            }
            FhirError::Internal(err) => {
                // Log internal errors with details
                tracing::error!("Internal error: {:?}", err);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "error",
                    "exception",
                    "Internal server error".to_string(),
                )
            }
        };

        // Log all errors
        match status {
            StatusCode::INTERNAL_SERVER_ERROR => {
                tracing::error!(status = %status, diagnostics = %diagnostics, "FHIR error");
            }
            StatusCode::BAD_REQUEST => {
                tracing::warn!(status = %status, diagnostics = %diagnostics, "FHIR client error");
            }
            _ => {
                tracing::info!(status = %status, diagnostics = %diagnostics, "FHIR error");
            }
        }

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
