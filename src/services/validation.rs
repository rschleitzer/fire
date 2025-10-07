use serde_json::Value;

use crate::error::{FhirError, Result};

/// Validates a FHIR Patient resource
pub fn validate_patient(content: &Value) -> Result<()> {
    // Validate resourceType
    let resource_type = content
        .get("resourceType")
        .and_then(|r| r.as_str())
        .ok_or_else(|| FhirError::InvalidResource("Missing resourceType".to_string()))?;

    if resource_type != "Patient" {
        return Err(FhirError::InvalidResource(format!(
            "Expected resourceType 'Patient', got '{}'",
            resource_type
        )));
    }

    // Validate name structure if present
    if let Some(names) = content.get("name") {
        if !names.is_array() {
            return Err(FhirError::ValidationError(
                "name must be an array".to_string(),
            ));
        }

        for (i, name) in names.as_array().unwrap().iter().enumerate() {
            if !name.is_object() {
                return Err(FhirError::ValidationError(format!(
                    "name[{}] must be an object",
                    i
                )));
            }

            // At least one of family or given should be present
            let has_family = name.get("family").is_some();
            let has_given = name.get("given").is_some();

            if !has_family && !has_given {
                return Err(FhirError::ValidationError(format!(
                    "name[{}] must have either family or given name",
                    i
                )));
            }

            // Validate given is array if present
            if let Some(given) = name.get("given") {
                if !given.is_array() {
                    return Err(FhirError::ValidationError(format!(
                        "name[{}].given must be an array",
                        i
                    )));
                }
            }
        }
    }

    // Validate identifier structure if present
    if let Some(identifiers) = content.get("identifier") {
        if !identifiers.is_array() {
            return Err(FhirError::ValidationError(
                "identifier must be an array".to_string(),
            ));
        }

        for (i, identifier) in identifiers.as_array().unwrap().iter().enumerate() {
            if !identifier.is_object() {
                return Err(FhirError::ValidationError(format!(
                    "identifier[{}] must be an object",
                    i
                )));
            }

            // Identifier must have a value
            if identifier.get("value").is_none() {
                return Err(FhirError::ValidationError(format!(
                    "identifier[{}] must have a value",
                    i
                )));
            }

            // System should be present
            if identifier.get("system").is_none() {
                return Err(FhirError::ValidationError(format!(
                    "identifier[{}] should have a system",
                    i
                )));
            }
        }
    }

    // Validate gender if present
    if let Some(gender) = content.get("gender") {
        if let Some(gender_str) = gender.as_str() {
            if !["male", "female", "other", "unknown"].contains(&gender_str) {
                return Err(FhirError::ValidationError(format!(
                    "Invalid gender value '{}'. Must be one of: male, female, other, unknown",
                    gender_str
                )));
            }
        } else {
            return Err(FhirError::ValidationError(
                "gender must be a string".to_string(),
            ));
        }
    }

    // Validate birthDate format if present
    if let Some(birthdate) = content.get("birthDate") {
        if let Some(date_str) = birthdate.as_str() {
            // Basic date format validation (YYYY-MM-DD)
            if !is_valid_date_format(date_str) {
                return Err(FhirError::ValidationError(format!(
                    "Invalid birthDate format '{}'. Must be YYYY-MM-DD",
                    date_str
                )));
            }
        } else {
            return Err(FhirError::ValidationError(
                "birthDate must be a string".to_string(),
            ));
        }
    }

    // Validate active if present
    if let Some(active) = content.get("active") {
        if !active.is_boolean() {
            return Err(FhirError::ValidationError(
                "active must be a boolean".to_string(),
            ));
        }
    }

    Ok(())
}

/// Basic date format validation (YYYY-MM-DD)
fn is_valid_date_format(date_str: &str) -> bool {
    use chrono::NaiveDate;

    // FHIR supports YYYY, YYYY-MM, or YYYY-MM-DD
    if date_str.len() == 4 {
        // Just year
        date_str.parse::<i32>().is_ok()
    } else if date_str.len() == 7 {
        // Year-month
        let parts: Vec<&str> = date_str.split('-').collect();
        parts.len() == 2
            && parts[0].parse::<i32>().is_ok()
            && parts[1].parse::<u32>().is_ok()
    } else {
        // Full date
        NaiveDate::parse_from_str(date_str, "%Y-%m-%d").is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_valid_patient() {
        let patient = json!({
            "resourceType": "Patient",
            "name": [{
                "family": "Smith",
                "given": ["John"]
            }],
            "gender": "male",
            "birthDate": "1980-01-01"
        });

        assert!(validate_patient(&patient).is_ok());
    }

    #[test]
    fn test_missing_resource_type() {
        let patient = json!({
            "name": [{
                "family": "Smith"
            }]
        });

        assert!(validate_patient(&patient).is_err());
    }

    #[test]
    fn test_invalid_gender() {
        let patient = json!({
            "resourceType": "Patient",
            "gender": "invalid"
        });

        assert!(validate_patient(&patient).is_err());
    }

    #[test]
    fn test_invalid_date_format() {
        let patient = json!({
            "resourceType": "Patient",
            "birthDate": "01-01-1980"
        });

        assert!(validate_patient(&patient).is_err());
    }

    #[test]
    fn test_valid_partial_date() {
        let patient = json!({
            "resourceType": "Patient",
            "birthDate": "1980"
        });

        assert!(validate_patient(&patient).is_ok());
    }
}

/// Validates a FHIR Observation resource
pub fn validate_observation(content: &Value) -> Result<()> {
    // Validate resourceType
    let resource_type = content
        .get("resourceType")
        .and_then(|r| r.as_str())
        .ok_or_else(|| FhirError::InvalidResource("Missing resourceType".to_string()))?;

    if resource_type != "Observation" {
        return Err(FhirError::InvalidResource(format!(
            "Expected resourceType 'Observation', got '{}'",
            resource_type
        )));
    }

    // Status is required
    let status = content
        .get("status")
        .and_then(|s| s.as_str())
        .ok_or_else(|| FhirError::ValidationError("status is required".to_string()))?;

    // Validate status values
    let valid_statuses = [
        "registered",
        "preliminary",
        "final",
        "amended",
        "corrected",
        "cancelled",
        "entered-in-error",
        "unknown",
    ];
    if !valid_statuses.contains(&status) {
        return Err(FhirError::ValidationError(format!(
            "Invalid status '{}'. Must be one of: {}",
            status,
            valid_statuses.join(", ")
        )));
    }

    // Code is required
    if content.get("code").is_none() {
        return Err(FhirError::ValidationError(
            "code is required".to_string(),
        ));
    }

    // Validate code structure
    if let Some(code) = content.get("code") {
        if !code.is_object() {
            return Err(FhirError::ValidationError(
                "code must be an object".to_string(),
            ));
        }

        // Should have coding array
        if let Some(coding) = code.get("coding") {
            if !coding.is_array() {
                return Err(FhirError::ValidationError(
                    "code.coding must be an array".to_string(),
                ));
            }
        }
    }

    // Subject is required
    if content.get("subject").is_none() {
        return Err(FhirError::ValidationError(
            "subject is required".to_string(),
        ));
    }

    // Validate subject has reference
    if let Some(subject) = content.get("subject") {
        if subject.get("reference").is_none() {
            return Err(FhirError::ValidationError(
                "subject.reference is required".to_string(),
            ));
        }
    }

    // Validate effective[x] - at least one should be present for most observations
    let has_effective = content.get("effectiveDateTime").is_some()
        || content.get("effectivePeriod").is_some()
        || content.get("effectiveTiming").is_some()
        || content.get("effectiveInstant").is_some();

    if !has_effective {
        // Not strictly required by FHIR, but recommended
        // Just log a warning in real implementation
    }

    // Validate value[x] structure if present
    if let Some(value_qty) = content.get("valueQuantity") {
        if !value_qty.is_object() {
            return Err(FhirError::ValidationError(
                "valueQuantity must be an object".to_string(),
            ));
        }
        // Value is required in Quantity
        if value_qty.get("value").is_none() {
            return Err(FhirError::ValidationError(
                "valueQuantity.value is required".to_string(),
            ));
        }
    }

    Ok(())
}
