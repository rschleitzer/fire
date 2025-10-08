use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::FromRow;
use uuid::Uuid;

use super::traits::VersionedResource;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Patient {
    pub id: Uuid,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value,  // Raw JSON stored as-is, returned without deserialization
}

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct PatientHistory {
    pub id: Uuid,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value,  // Raw JSON stored as-is

    // History metadata
    pub history_operation: String,
    pub history_timestamp: DateTime<Utc>,
}

impl VersionedResource for Patient {
    type History = PatientHistory;

    const RESOURCE_TYPE: &'static str = "Patient";
    const TABLE_NAME: &'static str = "patient";
    const HISTORY_TABLE_NAME: &'static str = "patient_history";

    fn get_id(&self) -> &Uuid {
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

/// Extract search parameters from FHIR Patient JSON
pub fn extract_patient_search_params(content: &Value) -> PatientSearchParams {
    let mut params = PatientSearchParams::default();

    // Extract names
    if let Some(names) = content.get("name").and_then(|n| n.as_array()) {
        for name in names {
            if let Some(family) = name.get("family").and_then(|f| f.as_str()) {
                params.family_name.push(family.to_string());
            }
            if let Some(given) = name.get("given").and_then(|g| g.as_array()) {
                for g in given {
                    if let Some(given_str) = g.as_str() {
                        params.given_name.push(given_str.to_string());
                    }
                }
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

    params
}

#[derive(Debug, Default)]
pub struct PatientSearchParams {
    pub family_name: Vec<String>,
    pub given_name: Vec<String>,
    pub identifier_system: Vec<String>,
    pub identifier_value: Vec<String>,
    pub birthdate: Option<NaiveDate>,
    pub gender: Option<String>,
    pub active: Option<bool>,
}
