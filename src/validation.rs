use crate::error::{FhirError, Result};

/// Validates a FHIR resource ID according to the FHIR specification.
///
/// FHIR IDs must match the pattern: [A-Za-z0-9\-\.]{1,64}
/// - Only alphanumeric characters, hyphens, and periods allowed
/// - Length must be between 1 and 64 characters
pub fn validate_fhir_id(id: &str) -> Result<()> {
    // Check length
    if id.is_empty() || id.len() > 64 {
        return Err(FhirError::BadRequest(
            format!("Resource ID must be between 1 and 64 characters, got {}", id.len())
        ));
    }

    // Check characters - must match [A-Za-z0-9\-\.]+
    for ch in id.chars() {
        if !ch.is_ascii_alphanumeric() && ch != '-' && ch != '.' {
            return Err(FhirError::BadRequest(
                format!("Resource ID contains invalid character '{}'. Only alphanumeric characters, hyphens, and periods are allowed.", ch)
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_ids() {
        assert!(validate_fhir_id("123").is_ok());
        assert!(validate_fhir_id("abc").is_ok());
        assert!(validate_fhir_id("ABC").is_ok());
        assert!(validate_fhir_id("a1b2c3").is_ok());
        assert!(validate_fhir_id("test-id").is_ok());
        assert!(validate_fhir_id("test.id").is_ok());
        assert!(validate_fhir_id("test-id.123").is_ok());
        assert!(validate_fhir_id("a").is_ok());
        assert!(validate_fhir_id(&"a".repeat(64)).is_ok()); // Max length
    }

    #[test]
    fn test_invalid_ids() {
        // Empty
        assert!(validate_fhir_id("").is_err());

        // Too long
        assert!(validate_fhir_id(&"a".repeat(65)).is_err());

        // Invalid characters
        assert!(validate_fhir_id("test id").is_err()); // space
        assert!(validate_fhir_id("test/id").is_err()); // slash
        assert!(validate_fhir_id("test_id").is_err()); // underscore
        assert!(validate_fhir_id("test@id").is_err()); // @
        assert!(validate_fhir_id("test#id").is_err()); // #
    }
}
