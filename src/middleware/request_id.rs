use axum::{
    body::Body,
    http::{Request, Response},
    middleware::Next,
};
use tower::{Layer, Service};
use uuid::Uuid;

#[derive(Clone)]
pub struct RequestIdLayer;

impl<S> Layer<S> for RequestIdLayer {
    type Service = RequestIdService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        RequestIdService { inner }
    }
}

#[derive(Clone)]
pub struct RequestIdService<S> {
    inner: S,
}

impl<S> Service<Request<Body>> for RequestIdService<S>
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

    fn call(&mut self, mut req: Request<Body>) -> Self::Future {
        let request_id = Uuid::new_v4().to_string();

        // Insert request ID into extensions for use in handlers
        req.extensions_mut().insert(request_id.clone());

        let mut inner = self.inner.clone();

        Box::pin(async move {
            let mut response = inner.call(req).await?;

            // Add request ID to response headers
            response
                .headers_mut()
                .insert("X-Request-ID", request_id.parse().unwrap());

            Ok(response)
        })
    }
}

/// Middleware function to add request ID
pub async fn add_request_id(mut req: Request<Body>, next: Next) -> Response<Body> {
    let request_id = Uuid::new_v4().to_string();

    // Add to extensions
    req.extensions_mut().insert(request_id.clone());

    // Log request
    tracing::info!(
        request_id = %request_id,
        method = %req.method(),
        uri = %req.uri(),
        "Incoming request"
    );

    let mut response = next.run(req).await;

    // Add request ID to response headers
    response
        .headers_mut()
        .insert("X-Request-ID", request_id.parse().unwrap());

    response
}
