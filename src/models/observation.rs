use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::FromRow;

use super::traits::VersionedResource;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Observation {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is, returned without deserialization
}

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct ObservationHistory {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is

    // History metadata
    pub history_operation: String,
    pub history_timestamp: DateTime<Utc>,
}

impl VersionedResource for Observation {
    type History = ObservationHistory;

    const RESOURCE_TYPE: &'static str = "Observation";
    const TABLE_NAME: &'static str = "observation";
    const HISTORY_TABLE_NAME: &'static str = "observation_history";

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

/// Inject id and meta fields into a FHIR Observation resource
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
/// Extract search parameters from FHIR Observation JSON
pub fn extract_observation_search_params(content: &Value) -> ObservationSearchParams {
    let mut params = ObservationSearchParams::default();

    // Extract status
    if let Some(status) = content.get("status").and_then(|s| s.as_str()) {
        params.status = Some(status.to_string());
    }

    // Extract category
    if let Some(categories) = content.get("category").and_then(|c| c.as_array()) {
        for category in categories {
            if let Some(codings) = category.get("coding").and_then(|c| c.as_array()) {
                for coding in codings {
                    if let Some(system) = coding.get("system").and_then(|s| s.as_str()) {
                        params.category_system.push(system.to_string());
                    }
                    if let Some(code) = coding.get("code").and_then(|c| c.as_str()) {
                        params.category_code.push(code.to_string());
                    }
                }
            }
        }
    }

    // Extract code
    if let Some(code_obj) = content.get("code") {
        if let Some(codings) = code_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.code_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.code_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract subject
    if let Some(subject) = content
        .get("subject")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        params.subject_reference = Some(subject.to_string());
        if subject.starts_with("Patient/") {
            params.patient_reference = Some(subject.to_string());
        }
    }

    // Extract encounter
    if let Some(encounter) = content
        .get("encounter")
        .and_then(|e| e.get("reference"))
        .and_then(|r| r.as_str())
    {
        params.encounter_reference = Some(encounter.to_string());
    }

    // Extract effectiveDateTime
    if let Some(effective) = content.get("effectiveDateTime").and_then(|e| e.as_str()) {
        if let Ok(dt) = DateTime::parse_from_rfc3339(effective) {
            params.effective_datetime = Some(dt.with_timezone(&Utc));
        }
    }

    // Extract effectivePeriod
    if let Some(period) = content.get("effectivePeriod") {
        if let Some(start) = period.get("start").and_then(|s| s.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(start) {
                params.effective_period_start = Some(dt.with_timezone(&Utc));
            }
        }
        if let Some(end) = period.get("end").and_then(|e| e.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(end) {
                params.effective_period_end = Some(dt.with_timezone(&Utc));
            }
        }
    }

    // Extract issued
    if let Some(issued) = content.get("issued").and_then(|i| i.as_str()) {
        if let Ok(dt) = DateTime::parse_from_rfc3339(issued) {
            params.issued = Some(dt.with_timezone(&Utc));
        }
    }

    // Extract valueQuantity
    if let Some(value_qty) = content.get("valueQuantity") {
        if let Some(value) = value_qty.get("value").and_then(|v| v.as_f64()) {
            params.value_quantity_value = Some(value);
        }
        if let Some(unit) = value_qty.get("unit").and_then(|u| u.as_str()) {
            params.value_quantity_unit = Some(unit.to_string());
        }
        if let Some(system) = value_qty.get("system").and_then(|s| s.as_str()) {
            params.value_quantity_system = Some(system.to_string());
        }
    }

    // Extract valueCodeableConcept
    if let Some(value_cc) = content.get("valueCodeableConcept") {
        if let Some(codings) = value_cc.get("coding").and_then(|c| c.as_array()) {
            for coding in codings {
                if let Some(code) = coding.get("code").and_then(|c| c.as_str()) {
                    params.value_codeable_concept_code.push(code.to_string());
                }
            }
        }
    }

    // Extract valueString
    if let Some(value_str) = content.get("valueString").and_then(|v| v.as_str()) {
        params.value_string = Some(value_str.to_string());
    }

    // Extract performer
    if let Some(performers) = content.get("performer").and_then(|p| p.as_array()) {
        for performer in performers {
            if let Some(reference) = performer.get("reference").and_then(|r| r.as_str()) {
                params.performer_reference.push(reference.to_string());
            }
        }
    }

    // R5: Extract triggeredBy
    if let Some(triggered_by) = content.get("triggeredBy").and_then(|t| t.as_array()) {
        for trigger in triggered_by {
            if let Some(obs_ref) = trigger
                .get("observation")
                .and_then(|o| o.get("reference"))
                .and_then(|r| r.as_str())
            {
                params.triggered_by_observation.push(obs_ref.to_string());
            }
            if let Some(trigger_type) = trigger.get("type").and_then(|t| t.as_str()) {
                params.triggered_by_type.push(trigger_type.to_string());
            }
        }
    }

    // R5: Extract focus
    if let Some(focus) = content.get("focus").and_then(|f| f.as_array()) {
        for focus_item in focus {
            if let Some(reference) = focus_item.get("reference").and_then(|r| r.as_str()) {
                params.focus_reference.push(reference.to_string());
            }
        }
    }

    // R5: Extract bodyStructure
    if let Some(body_structure) = content
        .get("bodyStructure")
        .and_then(|b| b.get("reference"))
        .and_then(|r| r.as_str())
    {
        params.body_structure_reference = Some(body_structure.to_string());
    }

    params
}

#[derive(Debug, Default)]
pub struct ObservationSearchParams {
    pub status: Option<String>,
    pub category_system: Vec<String>,
    pub category_code: Vec<String>,
    pub code_system: Option<String>,
    pub code_code: Option<String>,
    pub subject_reference: Option<String>,
    pub patient_reference: Option<String>,
    pub encounter_reference: Option<String>,
    pub effective_datetime: Option<DateTime<Utc>>,
    pub effective_period_start: Option<DateTime<Utc>>,
    pub effective_period_end: Option<DateTime<Utc>>,
    pub issued: Option<DateTime<Utc>>,
    pub value_quantity_value: Option<f64>,
    pub value_quantity_unit: Option<String>,
    pub value_quantity_system: Option<String>,
    pub value_codeable_concept_code: Vec<String>,
    pub value_string: Option<String>,
    pub performer_reference: Vec<String>,

    // R5 specific fields
    pub triggered_by_observation: Vec<String>,
    pub triggered_by_type: Vec<String>,
    pub focus_reference: Vec<String>,
    pub body_structure_reference: Option<String>,
}
