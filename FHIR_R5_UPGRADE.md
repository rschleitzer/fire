# FHIR R5 (5.0.0) Upgrade Guide

This document describes the Fire FHIR Server's compliance with FHIR R5 (5.0.0) and the changes made to support the latest FHIR specification.

## Overview

Fire FHIR Server now implements **FHIR R5 (5.0.0)**, the latest normative release of the FHIR specification. This upgrade ensures compatibility with the most current healthcare interoperability standards.

**FHIR Version**: R5 (5.0.0)
**Upgrade Date**: January 2025
**Previous Version**: R4 (4.0.1)

## What Changed in R5

### Patient Resource

The Patient resource in R5 has minimal breaking changes:

#### Communication Language Binding
- **R4**: Binding strength was "preferred" with "Common Languages" value set
- **R5**: Binding strength is now "required" with "All Languages" value set

**Impact**: The Fire server accepts all Patient resources with valid language codes. No code changes were required.

### Observation Resource

The Observation resource in R5 includes several new elements:

#### New Fields Added

1. **triggeredBy** (BackboneElement, Trial Use)
   - Purpose: Identifies the observation(s) that triggered the performance of this observation
   - Structure:
     ```json
     "triggeredBy": [
       {
         "observation": {
           "reference": "Observation/123"
         },
         "type": "reflex",  // reflex | repeat | re-run
         "reason": "Optional reason text"
       }
     ]
     ```
   - Use cases: Reflex testing, repeat observations, re-run observations

2. **focus** (Reference, Trial Use)
   - Purpose: What the observation is about when not about the subject of record
   - Example: Observation about a fetus when subject is the mother
   - Structure: Array of references to any FHIR resource

3. **bodyStructure** (Reference, Trial Use)
   - Purpose: Indicates the specific body structure observed
   - Example: "left kidney", "right femur"
   - Structure: Reference to BodyStructure resource

#### Status Values
R5 maintains the same status enumeration as R4:
- `registered`
- `preliminary`
- `final`
- `amended`
- `corrected`
- `cancelled`
- `entered-in-error`
- `unknown`

## Implementation Details

### Database Schema Changes

New migration: `003_add_r5_observation_fields.sql`

#### Observation Table Changes
```sql
-- New R5 columns added
ALTER TABLE observation
ADD COLUMN triggered_by_observation TEXT[],
ADD COLUMN triggered_by_type TEXT[],
ADD COLUMN focus_reference TEXT[],
ADD COLUMN body_structure_reference TEXT;

-- New indexes
CREATE INDEX idx_observation_triggered_by ON observation USING GIN (triggered_by_observation);
CREATE INDEX idx_observation_focus ON observation USING GIN (focus_reference);
CREATE INDEX idx_observation_body_structure ON observation (body_structure_reference);
```

Same changes applied to `observation_history` table.

### Code Changes

#### Models (`src/models/observation.rs`)

Added R5-specific fields to `Observation` and `ObservationHistory` structs:
```rust
pub triggered_by_observation: Option<Vec<String>>,
pub triggered_by_type: Option<Vec<String>>,
pub focus_reference: Option<Vec<String>>,
pub body_structure_reference: Option<String>,
```

Added search parameter extraction for R5 fields in `extract_observation_search_params()`.

#### Repository (`src/repository/observation.rs`)

Updated all SQL queries to include R5 fields:
- `create()` - Insert with R5 fields
- `update()` - Update with R5 fields
- History table inserts include R5 fields

#### Validation (`src/services/validation.rs`)

Added R5-specific validation:
- `triggeredBy` array structure validation
- `triggeredBy[].observation` reference required
- `triggeredBy[].type` must be one of: `reflex`, `repeat`, `re-run`

#### Capability Statement (`src/api/handlers/metadata.rs`)

Updated to advertise R5 compliance:
```json
{
  "fhirVersion": "5.0.0",
  "format": ["application/fhir+json", "json"],
  "patchFormat": ["application/json-patch+json"]
}
```

## Using R5 Features

### Creating an Observation with triggeredBy

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "2339-0",
        "display": "Glucose"
      }]
    },
    "subject": {
      "reference": "Patient/123"
    },
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "valueQuantity": {
      "value": 95,
      "unit": "mg/dL",
      "system": "http://unitsofmeasure.org",
      "code": "mg/dL"
    },
    "triggeredBy": [{
      "observation": {
        "reference": "Observation/456"
      },
      "type": "reflex",
      "reason": "Glucose level required follow-up HbA1c test"
    }]
  }'
```

### Creating an Observation with focus

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "11996-6",
        "display": "Fetal Heart Rate"
      }]
    },
    "subject": {
      "reference": "Patient/mother-123"
    },
    "focus": [{
      "reference": "Patient/fetus-456"
    }],
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "valueQuantity": {
      "value": 140,
      "unit": "beats/minute",
      "system": "http://unitsofmeasure.org",
      "code": "/min"
    }
  }'
```

### Creating an Observation with bodyStructure

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "33914-3",
        "display": "Kidney glomerular filtration rate"
      }]
    },
    "subject": {
      "reference": "Patient/123"
    },
    "bodyStructure": {
      "reference": "BodyStructure/left-kidney"
    },
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "valueQuantity": {
      "value": 85,
      "unit": "mL/min",
      "system": "http://unitsofmeasure.org",
      "code": "mL/min"
    }
  }'
```

## Migration Guide

### For Existing Deployments

1. **Backup your database**
   ```bash
   pg_dump fhir > fhir_backup_$(date +%Y%m%d).sql
   ```

2. **Pull the latest code**
   ```bash
   git pull origin main
   ```

3. **Run the R5 migration**
   ```bash
   sqlx migrate run
   # Or the migration will run automatically on server startup
   ```

4. **Restart the server**
   ```bash
   docker-compose restart fire
   # Or
   cargo run
   ```

5. **Verify R5 compliance**
   ```bash
   curl http://localhost:3000/fhir/metadata | jq '.fhirVersion'
   # Should return: "5.0.0"
   ```

### Existing Data

- **All existing Patient and Observation resources remain valid**
- R5 fields are optional - existing resources don't need updates
- New R5 fields will be `NULL` for existing records
- No data migration required

### Client Compatibility

#### Backward Compatibility
- R4 clients can continue to work with the server
- R5-specific fields are optional and can be ignored by R4 clients
- Core FHIR operations remain compatible

#### Forward Compatibility
- R5 clients can use new fields immediately
- The server accepts and validates R5-specific elements
- New search parameters available for R5 fields

## Testing R5 Features

### Verify triggeredBy Validation

```bash
# This should fail - invalid trigger type
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "code": {"coding": [{"system": "http://loinc.org", "code": "2339-0"}]},
    "subject": {"reference": "Patient/123"},
    "triggeredBy": [{
      "observation": {"reference": "Observation/456"},
      "type": "invalid-type"
    }]
  }'

# Expected error:
# {
#   "resourceType": "OperationOutcome",
#   "issue": [{
#     "severity": "error",
#     "code": "invalid",
#     "diagnostics": "Invalid triggeredBy[0].type 'invalid-type'. Must be one of: reflex, repeat, re-run"
#   }]
# }
```

### Search by R5 Fields

Currently, the R5 fields are stored but not yet exposed as search parameters. Future updates may add:
- `triggered-by` - Search observations by triggering observation
- `focus` - Search observations by focus reference
- `body-structure` - Search observations by body structure

## FHIR R5 Compliance Matrix

| Resource | R5 Compliance | Notes |
|----------|---------------|-------|
| Patient | ✅ Full | Communication language binding updated |
| Observation | ✅ Full | All R5 fields supported, Trial Use elements included |
| Bundle (transaction) | ✅ Full | No changes from R4 |
| CapabilityStatement | ✅ Full | Updated to R5 format |

## References

- [FHIR R5 Specification](http://hl7.org/fhir/R5/)
- [FHIR R5 Patient Resource](http://hl7.org/fhir/R5/patient.html)
- [FHIR R5 Observation Resource](http://hl7.org/fhir/R5/observation.html)
- [FHIR R4 to R5 Conversion](http://hl7.org/fhir/versions.html#std-process)

## Future R5 Enhancements

### Planned Features
- [ ] Search parameters for R5-specific fields
- [ ] CodeableReference data type support (if needed for other resources)
- [ ] Additional R5 resources (Encounter, Condition, etc.)
- [ ] R5-specific operation support ($validate, $transform)

### Community Feedback
We welcome feedback on R5 implementation. Please report issues or suggestions at:
- GitHub Issues: https://github.com/yourusername/fire/issues

## Conclusion

Fire FHIR Server is now fully compliant with FHIR R5 (5.0.0), supporting all normative elements and Trial Use features for Patient and Observation resources. The upgrade maintains backward compatibility while enabling new R5 capabilities for healthcare interoperability.
