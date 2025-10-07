use axum::{extract::State, http::StatusCode, Json};
use serde_json::{json, Value};
use sqlx::PgPool;
use std::sync::Arc;

use crate::error::Result;

pub type SharedPool = Arc<PgPool>;

/// Health check endpoint - checks database connectivity
pub async fn health_check(State(pool): State<SharedPool>) -> Result<(StatusCode, Json<Value>)> {
    // Check database connection
    match sqlx::query("SELECT 1").fetch_one(pool.as_ref()).await {
        Ok(_) => {
            tracing::debug!("Health check passed - database connection OK");
            Ok((
                StatusCode::OK,
                Json(json!({
                    "status": "healthy",
                    "database": "connected",
                    "version": env!("CARGO_PKG_VERSION")
                })),
            ))
        }
        Err(e) => {
            tracing::error!("Health check failed - database error: {}", e);
            Ok((
                StatusCode::SERVICE_UNAVAILABLE,
                Json(json!({
                    "status": "unhealthy",
                    "database": "disconnected",
                    "error": "Database connection failed"
                })),
            ))
        }
    }
}

/// Readiness check endpoint - indicates if server is ready to accept traffic
pub async fn readiness_check(State(pool): State<SharedPool>) -> Result<(StatusCode, Json<Value>)> {
    // For now, readiness is the same as health - checks database
    match sqlx::query("SELECT 1").fetch_one(pool.as_ref()).await {
        Ok(_) => {
            tracing::debug!("Readiness check passed");
            Ok((
                StatusCode::OK,
                Json(json!({
                    "status": "ready",
                    "database": "connected"
                })),
            ))
        }
        Err(e) => {
            tracing::warn!("Readiness check failed: {}", e);
            Ok((
                StatusCode::SERVICE_UNAVAILABLE,
                Json(json!({
                    "status": "not ready",
                    "database": "disconnected"
                })),
            ))
        }
    }
}

/// Liveness check endpoint - indicates if server process is alive
pub async fn liveness_check() -> (StatusCode, Json<Value>) {
    tracing::debug!("Liveness check passed");
    (
        StatusCode::OK,
        Json(json!({
            "status": "alive"
        })),
    )
}
