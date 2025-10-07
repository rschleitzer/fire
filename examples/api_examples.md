# Fire FHIR Server - API Examples

This document provides comprehensive examples of using the Fire FHIR Server API.

## Table of Contents

- [Patient Examples](#patient-examples)
- [Observation Examples](#observation-examples)
- [Search Examples](#search-examples)
- [Transaction Bundle Examples](#transaction-bundle-examples)
- [History Examples](#history-examples)
- [Error Handling Examples](#error-handling-examples)

## Prerequisites

```bash
# Start the server
docker-compose up -d

# Or run locally
cargo run

# Server should be running on http://localhost:3000
```

## Patient Examples

### Create a Patient

```bash
curl -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "name": [{
      "use": "official",
      "family": "Smith",
      "given": ["John", "Jacob"]
    }],
    "gender": "male",
    "birthDate": "1980-05-15",
    "active": true,
    "identifier": [{
      "system": "http://hospital.example.org/patients",
      "value": "12345"
    }],
    "telecom": [{
      "system": "phone",
      "value": "(555) 123-4567",
      "use": "home"
    }, {
      "system": "email",
      "value": "john.smith@example.com"
    }],
    "address": [{
      "use": "home",
      "line": ["123 Main St", "Apt 4B"],
      "city": "Springfield",
      "state": "IL",
      "postalCode": "62701",
      "country": "USA"
    }]
  }'
```

Response:
```json
{
  "resourceType": "Patient",
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "meta": {
    "versionId": "1",
    "lastUpdated": "2024-01-15T10:30:00Z"
  },
  "name": [{"family": "Smith", "given": ["John", "Jacob"]}],
  "gender": "male",
  "birthDate": "1980-05-15"
}
```

### Read a Patient

```bash
# Replace {id} with actual patient ID
curl http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000
```

### Update a Patient

```bash
curl -X PUT http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000 \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "name": [{
      "family": "Smith",
      "given": ["John", "Jacob", "James"]
    }],
    "gender": "male",
    "birthDate": "1980-05-15",
    "active": true,
    "telecom": [{
      "system": "phone",
      "value": "(555) 999-8888",
      "use": "home"
    }]
  }'
```

### Delete a Patient

```bash
curl -X DELETE http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000
```

## Observation Examples

### Create a Heart Rate Observation

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "category": [{
      "coding": [{
        "system": "http://terminology.hl7.org/CodeSystem/observation-category",
        "code": "vital-signs",
        "display": "Vital Signs"
      }]
    }],
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "8867-4",
        "display": "Heart rate"
      }],
      "text": "Heart rate"
    },
    "subject": {
      "reference": "Patient/550e8400-e29b-41d4-a716-446655440000",
      "display": "John Smith"
    },
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "issued": "2024-01-15T10:35:00Z",
    "performer": [{
      "reference": "Practitioner/example",
      "display": "Dr. Jane Doe"
    }],
    "valueQuantity": {
      "value": 72,
      "unit": "beats/minute",
      "system": "http://unitsofmeasure.org",
      "code": "/min"
    },
    "interpretation": [{
      "coding": [{
        "system": "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation",
        "code": "N",
        "display": "Normal"
      }]
    }]
  }'
```

### Create a Blood Pressure Observation

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "category": [{
      "coding": [{
        "system": "http://terminology.hl7.org/CodeSystem/observation-category",
        "code": "vital-signs"
      }]
    }],
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "85354-9",
        "display": "Blood pressure panel"
      }]
    },
    "subject": {
      "reference": "Patient/550e8400-e29b-41d4-a716-446655440000"
    },
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "component": [{
      "code": {
        "coding": [{
          "system": "http://loinc.org",
          "code": "8480-6",
          "display": "Systolic blood pressure"
        }]
      },
      "valueQuantity": {
        "value": 120,
        "unit": "mmHg",
        "system": "http://unitsofmeasure.org",
        "code": "mm[Hg]"
      }
    }, {
      "code": {
        "coding": [{
          "system": "http://loinc.org",
          "code": "8462-4",
          "display": "Diastolic blood pressure"
        }]
      },
      "valueQuantity": {
        "value": 80,
        "unit": "mmHg",
        "system": "http://unitsofmeasure.org",
        "code": "mm[Hg]"
      }
    }]
  }'
```

### Create a Lab Result Observation

```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "category": [{
      "coding": [{
        "system": "http://terminology.hl7.org/CodeSystem/observation-category",
        "code": "laboratory"
      }]
    }],
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "2339-0",
        "display": "Glucose [Mass/volume] in Blood"
      }]
    },
    "subject": {
      "reference": "Patient/550e8400-e29b-41d4-a716-446655440000"
    },
    "effectiveDateTime": "2024-01-15T08:00:00Z",
    "valueQuantity": {
      "value": 95,
      "unit": "mg/dL",
      "system": "http://unitsofmeasure.org",
      "code": "mg/dL"
    },
    "referenceRange": [{
      "low": {
        "value": 70,
        "unit": "mg/dL"
      },
      "high": {
        "value": 100,
        "unit": "mg/dL"
      },
      "text": "70-100 mg/dL"
    }]
  }'
```

## Search Examples

### Search Patients

#### By Name (Partial Match)

```bash
# Search by family name
curl "http://localhost:3000/fhir/Patient?family=Smith"

# Search by given name
curl "http://localhost:3000/fhir/Patient?given=John"

# Search by any name
curl "http://localhost:3000/fhir/Patient?name=Smith"
```

#### By Name with Exact Match

```bash
curl "http://localhost:3000/fhir/Patient?family:exact=Smith"
```

#### By Birth Date

```bash
# Exact date
curl "http://localhost:3000/fhir/Patient?birthdate=1980-05-15"

# After date
curl "http://localhost:3000/fhir/Patient?birthdate=gt1980-01-01"

# Before date
curl "http://localhost:3000/fhir/Patient?birthdate=lt1990-12-31"

# Date range
curl "http://localhost:3000/fhir/Patient?birthdate=ge1980-01-01&birthdate=le1990-12-31"
```

#### By Gender

```bash
curl "http://localhost:3000/fhir/Patient?gender=male"
```

#### By Active Status

```bash
curl "http://localhost:3000/fhir/Patient?active=true"
```

#### Combined Search with Sorting

```bash
# Multiple criteria with sorting
curl "http://localhost:3000/fhir/Patient?gender=male&birthdate=gt1980-01-01&_sort=-birthdate,family"

# With pagination
curl "http://localhost:3000/fhir/Patient?gender=male&_count=10&_offset=0"

# With total count
curl "http://localhost:3000/fhir/Patient?active=true&_total=accurate"
```

#### Search with Reverse Include

```bash
# Get patients with their observations
curl "http://localhost:3000/fhir/Patient?name=Smith&_revinclude=Observation:patient"
```

### Search Observations

#### By Status

```bash
curl "http://localhost:3000/fhir/Observation?status=final"
```

#### By Code

```bash
# Search by LOINC code
curl "http://localhost:3000/fhir/Observation?code=8867-4"
```

#### By Patient

```bash
# Replace {patient-id} with actual ID
curl "http://localhost:3000/fhir/Observation?patient=550e8400-e29b-41d4-a716-446655440000"
```

#### By Date Range

```bash
curl "http://localhost:3000/fhir/Observation?date=ge2024-01-01&date=le2024-12-31"
```

#### By Category

```bash
curl "http://localhost:3000/fhir/Observation?category=vital-signs"
```

#### Combined Search with Include

```bash
# Get observations with patient details included
curl "http://localhost:3000/fhir/Observation?status=final&patient=550e8400-e29b-41d4-a716-446655440000&_include=Observation:patient"
```

#### Search with Sorting

```bash
# Most recent first
curl "http://localhost:3000/fhir/Observation?patient=550e8400-e29b-41d4-a716-446655440000&_sort=-date"

# Multiple sort criteria
curl "http://localhost:3000/fhir/Observation?_sort=-date,code"
```

## Transaction Bundle Examples

### Create Multiple Resources in One Transaction

```bash
curl -X POST http://localhost:3000/fhir \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Bundle",
    "type": "transaction",
    "entry": [
      {
        "request": {
          "method": "POST",
          "url": "Patient"
        },
        "resource": {
          "resourceType": "Patient",
          "name": [{
            "family": "Johnson",
            "given": ["Emily"]
          }],
          "gender": "female",
          "birthDate": "1985-03-20"
        }
      },
      {
        "request": {
          "method": "POST",
          "url": "Patient"
        },
        "resource": {
          "resourceType": "Patient",
          "name": [{
            "family": "Williams",
            "given": ["Michael"]
          }],
          "gender": "male",
          "birthDate": "1975-11-10"
        }
      }
    ]
  }'
```

### Mixed Operations Bundle

```bash
curl -X POST http://localhost:3000/fhir \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Bundle",
    "type": "transaction",
    "entry": [
      {
        "request": {
          "method": "POST",
          "url": "Patient"
        },
        "resource": {
          "resourceType": "Patient",
          "name": [{"family": "Brown", "given": ["Sarah"]}],
          "gender": "female",
          "birthDate": "1990-07-25"
        }
      },
      {
        "request": {
          "method": "GET",
          "url": "Patient/550e8400-e29b-41d4-a716-446655440000"
        }
      },
      {
        "request": {
          "method": "PUT",
          "url": "Patient/550e8400-e29b-41d4-a716-446655440000"
        },
        "resource": {
          "resourceType": "Patient",
          "name": [{"family": "Smith-Updated", "given": ["John"]}],
          "gender": "male",
          "birthDate": "1980-05-15"
        }
      }
    ]
  }'
```

### Batch Bundle (Non-Atomic)

```bash
curl -X POST http://localhost:3000/fhir \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Bundle",
    "type": "batch",
    "entry": [
      {
        "request": {
          "method": "GET",
          "url": "Patient?name=Smith"
        }
      },
      {
        "request": {
          "method": "GET",
          "url": "Observation?status=final"
        }
      }
    ]
  }'
```

## History Examples

### Get Patient History

```bash
# Get all versions of a patient
curl "http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000/_history"
```

Response:
```json
{
  "resourceType": "Bundle",
  "type": "history",
  "total": 3,
  "entry": [
    {
      "resource": {
        "resourceType": "Patient",
        "id": "550e8400-e29b-41d4-a716-446655440000",
        "meta": {"versionId": "3"}
      },
      "request": {
        "method": "UPDATE",
        "url": "Patient/550e8400-e29b-41d4-a716-446655440000"
      },
      "response": {
        "status": "200",
        "lastModified": "2024-01-15T12:00:00Z"
      }
    }
  ]
}
```

### Get Specific Version

```bash
# Get version 2 of a patient
curl "http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000/_history/2"
```

### Get Observation History

```bash
curl "http://localhost:3000/fhir/Observation/a1b2c3d4-e5f6-4a5b-8c9d-0e1f2a3b4c5d/_history"
```

## Error Handling Examples

### Validation Error

```bash
curl -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "gender": "invalid-gender"
  }'
```

Response (400 Bad Request):
```json
{
  "resourceType": "OperationOutcome",
  "issue": [{
    "severity": "error",
    "code": "invalid",
    "diagnostics": "Validation error: Invalid gender value"
  }]
}
```

### Not Found Error

```bash
curl "http://localhost:3000/fhir/Patient/non-existent-id"
```

Response (404 Not Found):
```json
{
  "resourceType": "OperationOutcome",
  "issue": [{
    "severity": "error",
    "code": "not-found",
    "diagnostics": "Resource not found"
  }]
}
```

## System Endpoints

### Get Server Capability Statement

```bash
curl "http://localhost:3000/fhir/metadata"
```

### Health Checks

```bash
# Overall health
curl "http://localhost:3000/health"

# Readiness probe
curl "http://localhost:3000/health/ready"

# Liveness probe
curl "http://localhost:3000/health/live"
```

## Advanced Examples

### Pagination

```bash
# First page (10 results)
curl "http://localhost:3000/fhir/Patient?_count=10&_offset=0"

# Second page
curl "http://localhost:3000/fhir/Patient?_count=10&_offset=10"

# Third page with total count
curl "http://localhost:3000/fhir/Patient?_count=10&_offset=20&_total=accurate"
```

### Complex Search

```bash
# Find male patients born in the 1980s with observations
curl "http://localhost:3000/fhir/Patient?gender=male&birthdate=ge1980-01-01&birthdate=lt1990-01-01&_revinclude=Observation:patient&_sort=-birthdate&_count=20"
```

### Using with curl and jq

```bash
# Pretty print JSON response
curl "http://localhost:3000/fhir/Patient?name=Smith" | jq '.'

# Extract just the patient names
curl "http://localhost:3000/fhir/Patient?name=Smith" | jq '.entry[].resource.name[0]'

# Count total results
curl "http://localhost:3000/fhir/Patient?_total=accurate" | jq '.total'
```

## Testing Tips

### Save Response to File

```bash
curl "http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000" > patient.json
```

### Use Response in Next Request

```bash
# Get a patient
PATIENT_ID=$(curl -s -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{"resourceType":"Patient","name":[{"family":"Test"}],"gender":"male","birthDate":"1990-01-01"}' \
  | jq -r '.id')

# Create observation for that patient
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d "{
    \"resourceType\": \"Observation\",
    \"status\": \"final\",
    \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"8867-4\"}]},
    \"subject\": {\"reference\": \"Patient/$PATIENT_ID\"},
    \"effectiveDateTime\": \"2024-01-15T10:30:00Z\",
    \"valueQuantity\": {\"value\": 72, \"unit\": \"beats/minute\"}
  }"
```

### Measure Response Time

```bash
curl -w "@-" -o /dev/null -s "http://localhost:3000/fhir/Patient?name=Smith" <<'EOF'
    time_namelookup:  %{time_namelookup}\n
       time_connect:  %{time_connect}\n
    time_appconnect:  %{time_appconnect}\n
   time_pretransfer:  %{time_pretransfer}\n
      time_redirect:  %{time_redirect}\n
 time_starttransfer:  %{time_starttransfer}\n
                    ----------\n
         time_total:  %{time_total}\n
EOF
```

## See Also

- [README.md](../README.md) - Main documentation
- [DEPLOYMENT.md](../DEPLOYMENT.md) - Deployment guides
- [CONTRIBUTING.md](../CONTRIBUTING.md) - How to contribute
