use axum::{
    body::Body,
    http::{Request, Response},
};
use std::time::Instant;
use tower::{Layer, Service};

#[derive(Clone)]
pub struct LoggingLayer;

impl<S> Layer<S> for LoggingLayer {
    type Service = LoggingService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        LoggingService { inner }
    }
}

#[derive(Clone)]
pub struct LoggingService<S> {
    inner: S,
}

impl<S> Service<Request<Body>> for LoggingService<S>
where
    S: Service<Request<Body>, Response = Response<Body>> + Clone + Send + 'static,
    S::Future: Send + 'static,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = std::pin::Pin<
        Box<dyn std::future::Future<Output = Result<Self::Response, Self::Error>> + Send>,
    >;

    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, req: Request<Body>) -> Self::Future {
        let start = Instant::now();
        let method = req.method().clone();
        let uri = req.uri().clone();
        let request_id = req
            .extensions()
            .get::<String>()
            .cloned()
            .unwrap_or_else(|| "unknown".to_string());

        let mut inner = self.inner.clone();

        Box::pin(async move {
            let response = inner.call(req).await?;
            let duration = start.elapsed();
            let status = response.status();

            // Log based on status code
            if status.is_server_error() {
                tracing::error!(
                    request_id = %request_id,
                    method = %method,
                    uri = %uri,
                    status = %status,
                    duration_ms = %duration.as_millis(),
                    "Request completed with server error"
                );
            } else if status.is_client_error() {
                tracing::warn!(
                    request_id = %request_id,
                    method = %method,
                    uri = %uri,
                    status = %status,
                    duration_ms = %duration.as_millis(),
                    "Request completed with client error"
                );
            } else {
                tracing::info!(
                    request_id = %request_id,
                    method = %method,
                    uri = %uri,
                    status = %status,
                    duration_ms = %duration.as_millis(),
                    "Request completed successfully"
                );
            }

            Ok(response)
        })
    }
}
