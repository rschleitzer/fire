use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::FromRow;

use super::traits::VersionedResource;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Patient {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is, returned without deserialization
}

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct PatientHistory {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is

    // History metadata
    pub history_operation: String,
    pub history_timestamp: DateTime<Utc>,
}

impl VersionedResource for Patient {
    type History = PatientHistory;

    const RESOURCE_TYPE: &'static str = "Patient";
    const TABLE_NAME: &'static str = "patient";
    const HISTORY_TABLE_NAME: &'static str = "patient_history";

    fn get_id(&self) -> &String {
        &self.id
    }

    fn get_version_id(&self) -> i32 {
        self.version_id
    }

    fn get_last_updated(&self) -> &DateTime<Utc> {
        &self.last_updated
    }

    fn get_content(&self) -> &Value {
        &self.content
    }
}

/// Inject id and meta fields into a FHIR Patient resource
/// This is used at storage time to ensure all stored resources have complete id/meta
pub fn inject_id_meta(content: &Value, id: &str, version_id: i32, last_updated: &DateTime<Utc>) -> Value {
    if let Some(content_obj) = content.as_object() {
        // Create new object with fields in FHIR-standard order
        let mut resource = serde_json::Map::new();

        // 1. resourceType (if present in content)
        if let Some(resource_type) = content_obj.get("resourceType") {
            resource.insert("resourceType".to_string(), resource_type.clone());
        }

        // 2. id (always inject)
        resource.insert("id".to_string(), serde_json::json!(id));

        // 3. meta (always inject)
        resource.insert("meta".to_string(), serde_json::json!({
            "versionId": version_id.to_string(),
            "lastUpdated": last_updated.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        }));

        // 4. All other fields from content (skip resourceType, id, meta if client sent them)
        for (key, value) in content_obj {
            if key != "resourceType" && key != "id" && key != "meta" {
                resources.insert(key.clone(), value.clone());
            }
        }

        Value::Object(resource)
    } else {
        // Fallback if content is not an object - just return as-is
        content.clone()
    }
}
