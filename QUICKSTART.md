# Fire FHIR R5 Server - Quick Start Guide

Get up and running with Fire FHIR R5 Server in 5 minutes!

**FHIR Version**: R5 (5.0.0) - Latest normative release

## Option 1: Docker (Recommended)

### 1. Start the Server

```bash
# Clone the repository
git clone https://github.com/yourusername/fire.git
cd fire

# Start with Docker Compose
docker-compose up -d

# Check server is running
curl http://localhost:3000/health
```

### 2. Load Sample Data

```bash
# Make script executable (if needed)
chmod +x examples/load_sample_data.sh

# Load sample patients and observations
./examples/load_sample_data.sh
```

### 3. Try Your First Queries

```bash
# Search for patients
curl "http://localhost:3000/fhir/Patient?name=Smith"

# Get all observations
curl "http://localhost:3000/fhir/Observation?status=final"

# Get server capabilities
curl "http://localhost:3000/fhir/metadata"
```

**Done!** 🎉 You now have a running FHIR server with sample data.

## Option 2: Local Development

### 1. Install Prerequisites

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install sqlx-cli
cargo install sqlx-cli --no-default-features --features postgres

# Install PostgreSQL (macOS)
brew install postgresql@15
brew services start postgresql@15

# Or on Ubuntu
sudo apt-get install postgresql-15
```

### 2. Setup Database

```bash
# Create databases
createdb fhir
createdb fhir_test
```

### 3. Run the Server

```bash
# Clone and enter directory
git clone https://github.com/yourusername/fire.git
cd fire

# Run migrations
sqlx migrate run

# Start the server
cargo run

# Or build and run release version
cargo build --release
./target/release/fire
```

Server will start on http://localhost:3000

### 4. Load Sample Data

```bash
./examples/load_sample_data.sh
```

## Quick API Tour

### Create a Patient

```bash
curl -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "name": [{"family": "Doe", "given": ["Jane"]}],
    "gender": "female",
    "birthDate": "1985-01-01"
  }'
```

**Response:**
```json
{
  "resourceType": "Patient",
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "meta": {
    "versionId": "1",
    "lastUpdated": "2024-01-15T10:30:00Z"
  },
  "name": [{"family": "Doe", "given": ["Jane"]}],
  "gender": "female",
  "birthDate": "1985-01-01"
}
```

### Read a Patient

```bash
# Replace {id} with the ID from the create response
curl http://localhost:3000/fhir/Patient/550e8400-e29b-41d4-a716-446655440000
```

### Search Patients

```bash
# By name
curl "http://localhost:3000/fhir/Patient?name=Doe"

# By gender
curl "http://localhost:3000/fhir/Patient?gender=female"

# By birth date (after 1980)
curl "http://localhost:3000/fhir/Patient?birthdate=gt1980-01-01"

# Multiple criteria with sorting
curl "http://localhost:3000/fhir/Patient?gender=female&_sort=-birthdate"
```

### Create an Observation

```bash
# First, save the patient ID
PATIENT_ID="550e8400-e29b-41d4-a716-446655440000"

# Create a vital signs observation
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d "{
    \"resourceType\": \"Observation\",
    \"status\": \"final\",
    \"category\": [{
      \"coding\": [{
        \"system\": \"http://terminology.hl7.org/CodeSystem/observation-category\",
        \"code\": \"vital-signs\"
      }]
    }],
    \"code\": {
      \"coding\": [{
        \"system\": \"http://loinc.org\",
        \"code\": \"8867-4\",
        \"display\": \"Heart rate\"
      }]
    },
    \"subject\": {
      \"reference\": \"Patient/$PATIENT_ID\"
    },
    \"effectiveDateTime\": \"2024-01-15T10:30:00Z\",
    \"valueQuantity\": {
      \"value\": 72,
      \"unit\": \"beats/minute\",
      \"system\": \"http://unitsofmeasure.org\",
      \"code\": \"/min\"
    }
  }"
```

### Search Observations

```bash
# By patient
curl "http://localhost:3000/fhir/Observation?patient=$PATIENT_ID"

# By status
curl "http://localhost:3000/fhir/Observation?status=final"

# With patient details included
curl "http://localhost:3000/fhir/Observation?patient=$PATIENT_ID&_include=Observation:patient"
```

### Transaction Bundle

Create multiple resources at once:

```bash
curl -X POST http://localhost:3000/fhir \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Bundle",
    "type": "transaction",
    "entry": [
      {
        "request": {"method": "POST", "url": "Patient"},
        "resource": {
          "resourceType": "Patient",
          "name": [{"family": "Test", "given": ["User"]}],
          "gender": "other",
          "birthDate": "1990-01-01"
        }
      },
      {
        "request": {"method": "POST", "url": "Patient"},
        "resource": {
          "resourceType": "Patient",
          "name": [{"family": "Another", "given": ["Test"]}],
          "gender": "male",
          "birthDate": "1995-06-15"
        }
      }
    ]
  }'
```

## Common Operations

### Update a Resource

```bash
curl -X PUT http://localhost:3000/fhir/Patient/$PATIENT_ID \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "name": [{"family": "Doe-Updated", "given": ["Jane"]}],
    "gender": "female",
    "birthDate": "1985-01-01"
  }'
```

### Delete a Resource (Soft Delete)

```bash
curl -X DELETE http://localhost:3000/fhir/Patient/$PATIENT_ID
```

### Get Resource History

```bash
# View all versions
curl "http://localhost:3000/fhir/Patient/$PATIENT_ID/_history"

# Get specific version
curl "http://localhost:3000/fhir/Patient/$PATIENT_ID/_history/1"
```

### Search with Pagination

```bash
# First page (10 results)
curl "http://localhost:3000/fhir/Patient?_count=10&_offset=0"

# Second page
curl "http://localhost:3000/fhir/Patient?_count=10&_offset=10"

# With total count
curl "http://localhost:3000/fhir/Patient?_count=10&_total=accurate"
```

## System Endpoints

### Check Server Health

```bash
# Overall health (includes database)
curl http://localhost:3000/health

# Liveness probe (is process running)
curl http://localhost:3000/health/live

# Readiness probe (ready to serve traffic)
curl http://localhost:3000/health/ready
```

### Get Capability Statement

```bash
# What can this server do?
curl http://localhost:3000/fhir/metadata | jq '.'
```

## Tips and Tricks

### Pretty Print JSON

```bash
# Install jq if you haven't
brew install jq  # macOS
sudo apt-get install jq  # Ubuntu

# Use with curl
curl "http://localhost:3000/fhir/Patient?name=Smith" | jq '.'
```

### Extract Specific Fields

```bash
# Get just patient names
curl "http://localhost:3000/fhir/Patient" | jq '.entry[].resource.name[0]'

# Get total count
curl "http://localhost:3000/fhir/Patient?_total=accurate" | jq '.total'

# Get patient IDs
curl "http://localhost:3000/fhir/Patient" | jq '.entry[].resource.id'
```

### Save Response to File

```bash
curl "http://localhost:3000/fhir/Patient/$PATIENT_ID" > patient.json
```

### Measure Response Time

```bash
curl -w "Time: %{time_total}s\n" -o /dev/null -s \
  "http://localhost:3000/fhir/Patient?name=Smith"
```

### Automated Testing

```bash
# Create test script
cat > test.sh << 'EOF'
#!/bin/bash
BASE_URL="http://localhost:3000"

# Test health
curl -sf "$BASE_URL/health" || exit 1
echo "✓ Health check passed"

# Test create patient
PATIENT=$(curl -sf -X POST "$BASE_URL/fhir/Patient" \
  -H "Content-Type: application/json" \
  -d '{"resourceType":"Patient","name":[{"family":"Test"}],"gender":"male","birthDate":"1990-01-01"}')
PATIENT_ID=$(echo "$PATIENT" | jq -r '.id')
echo "✓ Created patient: $PATIENT_ID"

# Test read patient
curl -sf "$BASE_URL/fhir/Patient/$PATIENT_ID" > /dev/null
echo "✓ Read patient"

# Test search
curl -sf "$BASE_URL/fhir/Patient?name=Test" > /dev/null
echo "✓ Search patients"

echo "All tests passed!"
EOF

chmod +x test.sh
./test.sh
```

## Environment Configuration

### Configure Database

```bash
# Set custom database URL
export DATABASE_URL=postgres://user:pass@localhost:5432/fhir

# Configure connection pool
export DB_MAX_CONNECTIONS=50
export DB_MIN_CONNECTIONS=10
```

### Configure Logging

```bash
# Set log level
export RUST_LOG=fire=debug,tower_http=debug

# Run with debug logging
RUST_LOG=debug cargo run
```

### Configure Server

```bash
# Change server port
export SERVER_PORT=8080

# Bind to all interfaces
export SERVER_HOST=0.0.0.0
```

## Troubleshooting

### Server won't start

```bash
# Check if port is already in use
lsof -i :3000

# Check database connection
psql postgres://postgres:postgres@localhost/fhir -c "SELECT 1"

# Check Docker logs
docker-compose logs fire
```

### Database connection errors

```bash
# Verify PostgreSQL is running
pg_isready

# Check database exists
psql -l | grep fhir

# Recreate database
dropdb fhir && createdb fhir
sqlx migrate run
```

### Reset everything

```bash
# With Docker
docker-compose down -v
docker-compose up -d

# Local development
dropdb fhir && createdb fhir
cargo run
```

## Next Steps

- **📖 Read the full [API Examples](examples/api_examples.md)**
- **🚀 See [DEPLOYMENT.md](DEPLOYMENT.md) for production deployment**
- **🤝 Check [CONTRIBUTING.md](CONTRIBUTING.md) to contribute**
- **📚 Read [README.md](README.md) for complete documentation**

## Getting Help

- **Issues**: https://github.com/yourusername/fire/issues
- **Discussions**: https://github.com/yourusername/fire/discussions
- **FHIR Spec**: http://hl7.org/fhir/

## Resources

- [FHIR R5 Specification](http://hl7.org/fhir/R5/)
- [LOINC Codes](https://loinc.org/)
- [FHIR Test Data](https://github.com/smart-on-fhir/sample-patients)

---

**Happy FHIR-ing!** 🔥
