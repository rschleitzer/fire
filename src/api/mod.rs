pub mod content_negotiation;
pub mod handlers;
pub mod routes;

pub use routes::{
    bundle_routes, health_routes, metadata_routes, observation_routes, patient_routes,
};
