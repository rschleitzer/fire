pub mod content_negotiation;
pub mod handlers;
pub mod routes;
pub mod xml_serializer;

pub use routes::{
    bundle_routes, health_routes, metadata_routes, observation_routes, patient_routes, root_routes,
};
