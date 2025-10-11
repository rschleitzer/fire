use axum::{
    async_trait,
    extract::{rejection::JsonRejection, FromRequest, Request},
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde::de::DeserializeOwned;
use serde_json::json;

/// Custom JSON extractor that returns FHIR-compliant OperationOutcome on parsing errors
pub struct FhirJson<T>(pub T);

#[async_trait]
impl<T, S> FromRequest<S> for FhirJson<T>
where
    T: DeserializeOwned,
    S: Send + Sync,
{
    type Rejection = Response;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        match Json::<T>::from_request(req, state).await {
            Ok(Json(value)) => Ok(FhirJson(value)),
            Err(rejection) => {
                // Convert Axum's JSON rejection into a FHIR OperationOutcome
                let (status, code, diagnostics) = match rejection {
                    JsonRejection::JsonDataError(err) => (
                        StatusCode::BAD_REQUEST,
                        "invalid",
                        format!("Invalid JSON data: {}", err),
                    ),
                    JsonRejection::JsonSyntaxError(err) => (
                        StatusCode::BAD_REQUEST,
                        "invalid",
                        format!("Failed to parse the request body as JSON: {}", err),
                    ),
                    JsonRejection::MissingJsonContentType(err) => (
                        StatusCode::UNSUPPORTED_MEDIA_TYPE,
                        "not-supported",
                        format!("Missing JSON Content-Type: {}", err),
                    ),
                    JsonRejection::BytesRejection(err) => (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        "exception",
                        format!("Failed to read request body: {}", err),
                    ),
                    _ => (
                        StatusCode::BAD_REQUEST,
                        "invalid",
                        "Failed to parse JSON".to_string(),
                    ),
                };

                let operation_outcome = json!({
                    "resourceType": "OperationOutcome",
                    "issue": [{
                        "severity": "error",
                        "code": code,
                        "diagnostics": diagnostics
                    }]
                });

                Err((status, Json(operation_outcome)).into_response())
            }
        }
    }
}
