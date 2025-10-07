#!/bin/bash
# Load sample data into Fire FHIR Server

set -e

BASE_URL="${FHIR_SERVER_URL:-http://localhost:3000}"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Loading sample data into Fire FHIR Server at $BASE_URL"
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if server is running
echo "Checking server health..."
if ! curl -sf "$BASE_URL/health/live" > /dev/null; then
    echo "❌ Error: Server is not responding at $BASE_URL"
    echo "Please start the server first with: docker-compose up -d"
    exit 1
fi
echo -e "${GREEN}✓${NC} Server is running"
echo ""

# Function to create a resource
create_resource() {
    local resource_type=$1
    local data=$2
    local name=$3

    echo -n "Creating $resource_type: $name... "

    response=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/fhir/$resource_type" \
        -H "Content-Type: application/json" \
        -d "$data")

    http_code=$(echo "$response" | tail -n1)
    body=$(echo "$response" | sed '$d')

    if [ "$http_code" -eq 201 ]; then
        resource_id=$(echo "$body" | jq -r '.id')
        echo -e "${GREEN}✓${NC} Created with ID: $resource_id"
        echo "$resource_id"
    else
        echo -e "❌ Failed (HTTP $http_code)"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        return 1
    fi
}

# Store patient IDs
declare -A PATIENT_IDS

echo -e "${BLUE}Creating Patients...${NC}"
echo "===================="

# Create patients
PATIENT_IDS["PAT-001"]=$(create_resource "Patient" '{
  "resourceType": "Patient",
  "name": [{"family": "Smith", "given": ["John", "Jacob"]}],
  "gender": "male",
  "birthDate": "1980-05-15",
  "active": true,
  "identifier": [{"system": "http://hospital.example.org/patients", "value": "PAT-001"}]
}' "John Smith")

PATIENT_IDS["PAT-002"]=$(create_resource "Patient" '{
  "resourceType": "Patient",
  "name": [{"family": "Johnson", "given": ["Emily", "Rose"]}],
  "gender": "female",
  "birthDate": "1985-03-20",
  "active": true,
  "identifier": [{"system": "http://hospital.example.org/patients", "value": "PAT-002"}]
}' "Emily Johnson")

PATIENT_IDS["PAT-003"]=$(create_resource "Patient" '{
  "resourceType": "Patient",
  "name": [{"family": "Williams", "given": ["Michael", "David"]}],
  "gender": "male",
  "birthDate": "1975-11-10",
  "active": true,
  "identifier": [{"system": "http://hospital.example.org/patients", "value": "PAT-003"}]
}' "Michael Williams")

PATIENT_IDS["PAT-004"]=$(create_resource "Patient" '{
  "resourceType": "Patient",
  "name": [{"family": "Brown", "given": ["Sarah", "Marie"]}],
  "gender": "female",
  "birthDate": "1990-07-25",
  "active": true,
  "identifier": [{"system": "http://hospital.example.org/patients", "value": "PAT-004"}]
}' "Sarah Brown")

PATIENT_IDS["PAT-005"]=$(create_resource "Patient" '{
  "resourceType": "Patient",
  "name": [{"family": "Davis", "given": ["Robert", "James"]}],
  "gender": "male",
  "birthDate": "1965-09-08",
  "active": true,
  "identifier": [{"system": "http://hospital.example.org/patients", "value": "PAT-005"}]
}' "Robert Davis")

echo ""
echo -e "${BLUE}Creating Observations...${NC}"
echo "========================"

# Create observations
create_resource "Observation" "{
  \"resourceType\": \"Observation\",
  \"status\": \"final\",
  \"category\": [{\"coding\": [{\"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\", \"code\": \"vital-signs\"}]}],
  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"8867-4\", \"display\": \"Heart rate\"}]},
  \"subject\": {\"reference\": \"Patient/${PATIENT_IDS[PAT-001]}\"},
  \"effectiveDateTime\": \"2024-01-15T10:30:00Z\",
  \"valueQuantity\": {\"value\": 72, \"unit\": \"beats/minute\", \"system\": \"http://unitsofmeasure.org\", \"code\": \"/min\"}
}" "Heart rate for John Smith"

create_resource "Observation" "{
  \"resourceType\": \"Observation\",
  \"status\": \"final\",
  \"category\": [{\"coding\": [{\"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\", \"code\": \"vital-signs\"}]}],
  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"8480-6\", \"display\": \"Systolic BP\"}]},
  \"subject\": {\"reference\": \"Patient/${PATIENT_IDS[PAT-001]}\"},
  \"effectiveDateTime\": \"2024-01-15T10:30:00Z\",
  \"valueQuantity\": {\"value\": 120, \"unit\": \"mmHg\", \"system\": \"http://unitsofmeasure.org\", \"code\": \"mm[Hg]\"}
}" "Blood pressure for John Smith"

create_resource "Observation" "{
  \"resourceType\": \"Observation\",
  \"status\": \"final\",
  \"category\": [{\"coding\": [{\"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\", \"code\": \"laboratory\"}]}],
  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"2339-0\", \"display\": \"Glucose\"}]},
  \"subject\": {\"reference\": \"Patient/${PATIENT_IDS[PAT-002]}\"},
  \"effectiveDateTime\": \"2024-01-14T08:00:00Z\",
  \"valueQuantity\": {\"value\": 95, \"unit\": \"mg/dL\", \"system\": \"http://unitsofmeasure.org\", \"code\": \"mg/dL\"}
}" "Glucose for Emily Johnson"

create_resource "Observation" "{
  \"resourceType\": \"Observation\",
  \"status\": \"final\",
  \"category\": [{\"coding\": [{\"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\", \"code\": \"vital-signs\"}]}],
  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"29463-7\", \"display\": \"Body weight\"}]},
  \"subject\": {\"reference\": \"Patient/${PATIENT_IDS[PAT-003]}\"},
  \"effectiveDateTime\": \"2024-01-13T09:00:00Z\",
  \"valueQuantity\": {\"value\": 85.5, \"unit\": \"kg\", \"system\": \"http://unitsofmeasure.org\", \"code\": \"kg\"}
}" "Weight for Michael Williams"

create_resource "Observation" "{
  \"resourceType\": \"Observation\",
  \"status\": \"final\",
  \"category\": [{\"coding\": [{\"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\", \"code\": \"vital-signs\"}]}],
  \"code\": {\"coding\": [{\"system\": \"http://loinc.org\", \"code\": \"8310-5\", \"display\": \"Body temperature\"}]},
  \"subject\": {\"reference\": \"Patient/${PATIENT_IDS[PAT-004]}\"},
  \"effectiveDateTime\": \"2024-01-16T11:15:00Z\",
  \"valueQuantity\": {\"value\": 37.0, \"unit\": \"Cel\", \"system\": \"http://unitsofmeasure.org\", \"code\": \"Cel\"}
}" "Temperature for Sarah Brown"

echo ""
echo -e "${GREEN}✓ Sample data loaded successfully!${NC}"
echo ""
echo "Try these queries:"
echo "  curl '$BASE_URL/fhir/Patient?name=Smith'"
echo "  curl '$BASE_URL/fhir/Observation?status=final'"
echo "  curl '$BASE_URL/fhir/Patient/${PATIENT_IDS[PAT-001]}'"
echo ""
