use axum::{http::StatusCode, Json};
use serde_json::{json, Value};

/// FHIR Capability Statement endpoint
/// Returns server capabilities and supported features
pub async fn capability_statement() -> (StatusCode, Json<Value>) {
    tracing::debug!("Serving capability statement");

    let capability = json!({
        "resourceType": "CapabilityStatement",
        "status": "active",
        "date": "2024-01-01",
        "kind": "instance",
        "software": {
            "name": "Fire FHIR Server",
            "version": env!("CARGO_PKG_VERSION")
        },
        "implementation": {
            "description": "Fire - A high-performance FHIR server in Rust",
            "url": "http://localhost:3000/fhir"
        },
        "fhirVersion": "4.0.1",
        "format": ["json"],
        "rest": [{
            "mode": "server",
            "resource": [
                {
                    "type": "Patient",
                    "interaction": [
                        {"code": "read"},
                        {"code": "vread"},
                        {"code": "update"},
                        {"code": "delete"},
                        {"code": "history-instance"},
                        {"code": "create"},
                        {"code": "search-type"}
                    ],
                    "versioning": "versioned",
                    "readHistory": true,
                    "updateCreate": false,
                    "conditionalCreate": false,
                    "conditionalRead": "not-supported",
                    "conditionalUpdate": false,
                    "conditionalDelete": "not-supported",
                    "searchParam": [
                        {
                            "name": "name",
                            "type": "string",
                            "documentation": "Search by family or given name"
                        },
                        {
                            "name": "family",
                            "type": "string",
                            "documentation": "Search by family name"
                        },
                        {
                            "name": "given",
                            "type": "string",
                            "documentation": "Search by given name"
                        },
                        {
                            "name": "identifier",
                            "type": "token",
                            "documentation": "Search by identifier"
                        },
                        {
                            "name": "birthdate",
                            "type": "date",
                            "documentation": "Search by birthdate"
                        },
                        {
                            "name": "gender",
                            "type": "token",
                            "documentation": "Search by gender"
                        },
                        {
                            "name": "active",
                            "type": "token",
                            "documentation": "Search by active status"
                        },
                        {
                            "name": "_revinclude",
                            "type": "special",
                            "documentation": "Reverse include (e.g., Observation:patient)"
                        }
                    ]
                },
                {
                    "type": "Observation",
                    "interaction": [
                        {"code": "read"},
                        {"code": "vread"},
                        {"code": "update"},
                        {"code": "delete"},
                        {"code": "history-instance"},
                        {"code": "create"},
                        {"code": "search-type"}
                    ],
                    "versioning": "versioned",
                    "readHistory": true,
                    "updateCreate": false,
                    "conditionalCreate": false,
                    "conditionalRead": "not-supported",
                    "conditionalUpdate": false,
                    "conditionalDelete": "not-supported",
                    "searchParam": [
                        {
                            "name": "status",
                            "type": "token",
                            "documentation": "Search by observation status"
                        },
                        {
                            "name": "code",
                            "type": "token",
                            "documentation": "Search by observation code"
                        },
                        {
                            "name": "category",
                            "type": "token",
                            "documentation": "Search by category"
                        },
                        {
                            "name": "patient",
                            "type": "reference",
                            "documentation": "Search by patient reference"
                        },
                        {
                            "name": "subject",
                            "type": "reference",
                            "documentation": "Search by subject reference"
                        },
                        {
                            "name": "date",
                            "type": "date",
                            "documentation": "Search by effective date"
                        },
                        {
                            "name": "_include",
                            "type": "special",
                            "documentation": "Include referenced resources (e.g., Observation:patient)"
                        }
                    ]
                }
            ],
            "interaction": [
                {
                    "code": "transaction",
                    "documentation": "Support for transaction bundles"
                },
                {
                    "code": "batch",
                    "documentation": "Support for batch bundles"
                }
            ]
        }]
    });

    (StatusCode::OK, Json(capability))
}
