use chrono::{DateTime, NaiveDate, Utc};
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

/// Inject id and meta fields into a FHIR resource
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
                resource.insert(key.clone(), value.clone());
            }
        }

        Value::Object(resource)
    } else {
        // Fallback if content is not an object - just return as-is
        content.clone()
    }
}

/// Extract search parameters from FHIR Patient JSON
pub fn extract_patient_search_params(content: &Value) -> PatientSearchParams {
    let mut params = PatientSearchParams::default();

    // Extract names
    if let Some(names) = content.get("name").and_then(|n| n.as_array()) {
        for name in names {
            // Extract family
            if let Some(family) = name.get("family").and_then(|f| f.as_str()) {
                params.family_name.push(family.to_string());
            }

            // Extract given names
            if let Some(given) = name.get("given").and_then(|g| g.as_array()) {
                for g in given {
                    if let Some(given_str) = g.as_str() {
                        params.given_name.push(given_str.to_string());
                    }
                }
            }

            // Extract prefix (Dr., Mr., Ms., etc.)
            if let Some(prefix) = name.get("prefix").and_then(|p| p.as_array()) {
                for p in prefix {
                    if let Some(prefix_str) = p.as_str() {
                        params.prefix.push(prefix_str.to_string());
                    }
                }
            }

            // Extract suffix (Jr., Sr., III, etc.)
            if let Some(suffix) = name.get("suffix").and_then(|s| s.as_array()) {
                for s in suffix {
                    if let Some(suffix_str) = s.as_str() {
                        params.suffix.push(suffix_str.to_string());
                    }
                }
            }

            // Extract text (full name as text)
            if let Some(text) = name.get("text").and_then(|t| t.as_str()) {
                params.name_text.push(text.to_string());
            }
        }
    }

    // Extract identifiers
    if let Some(identifiers) = content.get("identifier").and_then(|i| i.as_array()) {
        for identifier in identifiers {
            if let Some(system) = identifier.get("system").and_then(|s| s.as_str()) {
                params.identifier_system.push(system.to_string());
            }
            if let Some(value) = identifier.get("value").and_then(|v| v.as_str()) {
                params.identifier_value.push(value.to_string());
            }
        }
    }

    // Extract birthdate
    if let Some(birthdate) = content.get("birthDate").and_then(|b| b.as_str()) {
        if let Ok(date) = NaiveDate::parse_from_str(birthdate, "%Y-%m-%d") {
            params.birthdate = Some(date);
        }
    }

    // Extract gender
    if let Some(gender) = content.get("gender").and_then(|g| g.as_str()) {
        params.gender = Some(gender.to_string());
    }

    // Extract active
    if let Some(active) = content.get("active").and_then(|a| a.as_bool()) {
        params.active = Some(active);
    }

    // Extract general practitioner references
    if let Some(gps) = content.get("generalPractitioner").and_then(|g| g.as_array()) {
        for gp in gps {
            if let Some(reference) = gp.get("reference").and_then(|r| r.as_str()) {
                params.general_practitioner_reference.push(reference.to_string());
            }
        }
    }

    params
}

#[derive(Debug, Default)]
pub struct PatientSearchParams {
    pub family_name: Vec<String>,
    pub given_name: Vec<String>,
    pub prefix: Vec<String>,
    pub suffix: Vec<String>,
    pub name_text: Vec<String>,
    pub identifier_system: Vec<String>,
    pub identifier_value: Vec<String>,
    pub birthdate: Option<NaiveDate>,
    pub gender: Option<String>,
    pub active: Option<bool>,
    pub general_practitioner_reference: Vec<String>,
}
