use askama::Template;
use axum::http::HeaderMap;
use serde_json::Value;

// Re-export Template trait for use in handlers
pub use askama::Template as AskamaTemplate;

/// Determine the preferred response format based on Accept header
pub fn preferred_format(headers: &HeaderMap) -> ResponseFormat {
    if let Some(accept) = headers.get("accept") {
        if let Ok(accept_str) = accept.to_str() {
            // Check for HTML preference (browser requests)
            if accept_str.contains("text/html") || accept_str.contains("text/*") {
                return ResponseFormat::Html;
            }
            // Check for explicit JSON request
            if accept_str.contains("application/json") || accept_str.contains("application/fhir+json") {
                return ResponseFormat::Json;
            }
        }
    }

    // Default to JSON for API clients
    ResponseFormat::Json
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResponseFormat {
    Html,
    Json,
}

// Template structs for rendering HTML

#[derive(Template)]
#[template(path = "patient_list.html")]
pub struct PatientListTemplate {
    pub patients: Vec<PatientRow>,
    pub total: usize,
    pub current_url: String,
}

#[derive(Clone)]
pub struct PatientRow {
    pub id: String,
    pub name: String,
    pub gender: String,
    pub birth_date: String,
    pub active: bool,
    pub last_updated: String,
}

#[derive(Template)]
#[template(path = "patient_detail.html")]
pub struct PatientDetailTemplate {
    pub id: String,
    pub version_id: String,
    pub last_updated: String,
    pub name: String,
    pub family: String,
    pub given: String,
    pub gender: String,
    pub birth_date: String,
    pub active: bool,
    pub resource_json: String,
}

#[derive(Template)]
#[template(path = "observation_list.html")]
pub struct ObservationListTemplate {
    pub observations: Vec<ObservationRow>,
    pub total: usize,
    pub current_url: String,
}

#[derive(Clone)]
pub struct ObservationRow {
    pub id: String,
    pub code: String,
    pub status: String,
    pub value: String,
    pub subject: String,
    pub effective_date: String,
    pub last_updated: String,
}

#[derive(Template)]
#[template(path = "observation_detail.html")]
pub struct ObservationDetailTemplate {
    pub id: String,
    pub version_id: String,
    pub last_updated: String,
    pub code: String,
    pub status: String,
    pub value: String,
    pub subject: String,
    pub effective_date: String,
    pub resource_json: String,
}

/// Extract a human-readable name from Patient FHIR JSON
pub fn extract_patient_name(content: &Value) -> String {
    if let Some(names) = content.get("name").and_then(|n| n.as_array()) {
        if let Some(first_name) = names.first() {
            let family = first_name
                .get("family")
                .and_then(|f| f.as_str())
                .unwrap_or("");

            let given = if let Some(given_arr) = first_name.get("given").and_then(|g| g.as_array()) {
                given_arr
                    .iter()
                    .filter_map(|g| g.as_str())
                    .collect::<Vec<_>>()
                    .join(" ")
            } else {
                String::new()
            };

            if !given.is_empty() && !family.is_empty() {
                return format!("{} {}", given, family);
            } else if !family.is_empty() {
                return family.to_string();
            } else if !given.is_empty() {
                return given;
            }
        }
    }
    "Unknown".to_string()
}

/// Extract family name from Patient FHIR JSON
pub fn extract_patient_family(content: &Value) -> String {
    if let Some(names) = content.get("name").and_then(|n| n.as_array()) {
        if let Some(first_name) = names.first() {
            if let Some(family) = first_name.get("family").and_then(|f| f.as_str()) {
                return family.to_string();
            }
        }
    }
    String::new()
}

/// Extract given name from Patient FHIR JSON
pub fn extract_patient_given(content: &Value) -> String {
    if let Some(names) = content.get("name").and_then(|n| n.as_array()) {
        if let Some(first_name) = names.first() {
            if let Some(given_arr) = first_name.get("given").and_then(|g| g.as_array()) {
                return given_arr
                    .iter()
                    .filter_map(|g| g.as_str())
                    .collect::<Vec<_>>()
                    .join(" ");
            }
        }
    }
    String::new()
}

/// Extract observation code display text from FHIR JSON
pub fn extract_observation_code(content: &Value) -> String {
    if let Some(code) = content.get("code") {
        if let Some(text) = code.get("text").and_then(|t| t.as_str()) {
            return text.to_string();
        }
        if let Some(coding) = code.get("coding").and_then(|c| c.as_array()).and_then(|c| c.first()) {
            if let Some(display) = coding.get("display").and_then(|d| d.as_str()) {
                return display.to_string();
            }
            if let Some(code_val) = coding.get("code").and_then(|c| c.as_str()) {
                return code_val.to_string();
            }
        }
    }
    "Unknown".to_string()
}

/// Extract observation value as a display string
pub fn extract_observation_value(content: &Value) -> String {
    if let Some(value_qty) = content.get("valueQuantity") {
        let value = value_qty.get("value").and_then(|v| v.as_f64()).unwrap_or(0.0);
        let unit = value_qty.get("unit").and_then(|u| u.as_str()).unwrap_or("");
        return format!("{} {}", value, unit);
    }
    if let Some(value_str) = content.get("valueString").and_then(|v| v.as_str()) {
        return value_str.to_string();
    }
    if let Some(value_cc) = content.get("valueCodeableConcept") {
        if let Some(text) = value_cc.get("text").and_then(|t| t.as_str()) {
            return text.to_string();
        }
    }
    "N/A".to_string()
}
