use axum::{middleware, Router};
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tower_http::{cors::CorsLayer, services::ServeDir, trace::TraceLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use fire::api::{
    bundle_routes, health_routes, metadata_routes, observation_routes, patient_routes,
};
use fire::config::Config;
use fire::middleware::request_id::add_request_id;
use fire::repository::{ObservationRepository, PatientRepository};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing with JSON formatting for structured logs
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "fire=info,tower_http=info,axum=info,sqlx=warn".into()),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(true)
                .with_thread_ids(true)
                .with_line_number(true),
        )
        .init();

    tracing::info!("Initializing Fire FHIR Server");

    // Load configuration
    let config = Config::from_env()?;
    tracing::info!("Starting FHIR server on {}", config.server_addr());

    // Create database connection pool with configured settings
    let pool = PgPoolOptions::new()
        .max_connections(config.db_max_connections)
        .min_connections(config.db_min_connections)
        .acquire_timeout(std::time::Duration::from_secs(
            config.db_connection_timeout_secs,
        ))
        .idle_timeout(std::time::Duration::from_secs(config.db_idle_timeout_secs))
        .connect(&config.database_url)
        .await?;

    tracing::info!(
        max_connections = config.db_max_connections,
        min_connections = config.db_min_connections,
        connection_timeout_secs = config.db_connection_timeout_secs,
        idle_timeout_secs = config.db_idle_timeout_secs,
        "Connected to database with connection pool"
    );

    // Run migrations
    tracing::info!("Running database migrations...");
    match sqlx::migrate!("./migrations").run(&pool).await {
        Ok(_) => tracing::info!("Database migrations completed successfully"),
        Err(e) => {
            tracing::error!("Failed to run migrations: {}", e);
            return Err(e.into());
        }
    }

    // Create repositories
    let patient_repo = Arc::new(PatientRepository::new(pool.clone()));
    let observation_repo = Arc::new(ObservationRepository::new(pool.clone()));

    // Build application routes
    let app = Router::new()
        .merge(health_routes(Arc::new(pool.clone())))
        .merge(metadata_routes())
        .merge(patient_routes(patient_repo.clone()))
        .merge(observation_routes(observation_repo.clone()))
        .merge(bundle_routes(patient_repo, observation_repo))
        .nest_service("/static", ServeDir::new("static"))
        .layer(middleware::from_fn(add_request_id))
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http());

    tracing::info!("Application routes configured with middleware");

    // Start server
    let listener = tokio::net::TcpListener::bind(&config.server_addr()).await?;
    tracing::info!(
        addr = %config.server_addr(),
        "Fire FHIR Server started and listening for connections"
    );

    match axum::serve(listener, app).await {
        Ok(_) => {
            tracing::info!("Server shutdown gracefully");
            Ok(())
        }
        Err(e) => {
            tracing::error!("Server error: {}", e);
            Err(e.into())
        }
    }
}
