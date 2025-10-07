# Changelog

All notable changes to the Fire FHIR Server project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- **BREAKING**: Updated to FHIR R5 (5.0.0) from R4 (4.0.1)
- Updated capability statement to advertise FHIR R5 compliance
- Updated all documentation to reflect R5 specification

### Added
- **FHIR R5 Support**: Complete implementation of FHIR R5 (5.0.0)
- R5-specific Observation fields:
  - `triggeredBy` - Reflex/repeat testing support
  - `focus` - Observation focus references
  - `bodyStructure` - Body structure references
- Database migration `003_add_r5_observation_fields.sql`
- R5 validation for `triggeredBy` structure and types
- Comprehensive R5 upgrade guide (`FHIR_R5_UPGRADE.md`)
- R5 field extraction in search parameter processing
- Database indexes for R5 fields

## [0.1.0] - 2024-01-01

### Added

#### Phase 1: Core Foundation
- Project setup with Cargo and Rust toolchain
- PostgreSQL database integration with sqlx
- Patient resource implementation with CRUD operations
- Database migrations for patient tables (current + history)
- Version tracking and soft deletes
- Basic search functionality
- `.env` configuration support
- Initial README documentation

#### Phase 2: Advanced Search & Validation
- Search modifiers: `:exact`, `:contains`, `:missing`
- `_sort` parameter with multiple fields and directions
- `_total` parameter for accurate result counts
- Dynamic SQL query building with injection protection
- Comprehensive FHIR resource validation
  - Resource type validation
  - Required field validation
  - Data type validation (gender, dates, identifiers)
  - Structure validation (names, identifiers)
- Search parameter extraction for Patient
- Optimized database indexes

#### Phase 3: Observation Resource
- Complete Observation resource implementation
- CRUD operations (create, read, update, delete)
- 18+ search parameters:
  - status, category, code
  - subject/patient references
  - effective dates and periods
  - value types (quantity, codeable concept, string)
  - performer references
- Observation-specific validation
- History tracking and versioning
- Database schema with optimized indexes

#### Phase 4: Advanced Search Features
- Observation search implementation
- `_include` parameter for forward references (Observation:patient)
- Support for `_count`, `_offset` pagination
- FHIR-compliant Bundle responses for search results
- Search result sorting and filtering

#### Phase 5: Transaction Bundles & Reverse Includes
- Transaction bundle support (atomic multi-resource operations)
- Batch bundle support (non-atomic operations)
- Bundle operations: POST, PUT, GET, DELETE
- `_revinclude` parameter for reverse references
- Observation:patient and Observation:subject reverse includes
- Bundle response formatting

#### Phase 6: Production Readiness & Testing
- Comprehensive structured logging with `tracing`
- Request ID tracking (X-Request-ID header)
- Request/response timing and performance logging
- Enhanced error handling with detailed context
- FHIR OperationOutcome error responses
- Configurable log levels via RUST_LOG
- Health check endpoints:
  - `/health` - Overall health with database check
  - `/health/ready` - Readiness probe for Kubernetes
  - `/health/live` - Liveness probe for Kubernetes
- FHIR capability statement endpoint (`/fhir/metadata`)
- Configurable database connection pooling:
  - Max/min connections
  - Connection timeout
  - Idle timeout
- Production-grade configuration management
- Comprehensive test suite:
  - Unit tests for validation logic
  - Integration tests for CRUD operations
  - Repository tests with real database
  - Search parameter tests
  - History tracking tests
- Test helpers and fixtures

#### Phase 7: Deployment & DevOps
- Multi-stage Dockerfile for optimized images
- Docker Compose configuration for local development
- Docker Compose production configuration
- GitHub Actions CI/CD pipelines:
  - Automated testing on push/PR
  - Clippy linting
  - Format checking
  - Release builds
- Kubernetes deployment manifests:
  - Deployments with health probes
  - Services and ConfigMaps
  - Secrets management
  - Horizontal Pod Autoscaling (HPA)
- Comprehensive deployment documentation (DEPLOYMENT.md)
- Production configuration examples
- Backup and recovery procedures
- Nginx reverse proxy configuration
- Performance tuning guidelines

### Documentation
- Comprehensive README.md with API documentation
- CLAUDE.md with architectural decisions
- DEPLOYMENT.md with deployment guides
- PROJECT_SUMMARY.md with project overview
- CONTRIBUTING.md with contribution guidelines
- CHANGELOG.md (this file)

### Infrastructure
- `.dockerignore` for optimized builds
- `.gitignore` for Rust projects
- CI/CD workflows for GitHub Actions
- Database migration scripts
- Environment variable templates

## API Endpoints

### FHIR Endpoints (v0.1.0)

**Patient Resource**
- `POST /fhir/Patient` - Create patient
- `GET /fhir/Patient/:id` - Read patient
- `PUT /fhir/Patient/:id` - Update patient
- `DELETE /fhir/Patient/:id` - Delete patient (soft)
- `GET /fhir/Patient` - Search patients
- `GET /fhir/Patient/:id/_history` - Patient history
- `GET /fhir/Patient/:id/_history/:version_id` - Patient version

**Observation Resource**
- `POST /fhir/Observation` - Create observation
- `GET /fhir/Observation/:id` - Read observation
- `PUT /fhir/Observation/:id` - Update observation
- `DELETE /fhir/Observation/:id` - Delete observation (soft)
- `GET /fhir/Observation` - Search observations
- `GET /fhir/Observation/:id/_history` - Observation history
- `GET /fhir/Observation/:id/_history/:version_id` - Observation version

**Bundle Operations**
- `POST /fhir` - Transaction/Batch bundle

**System Endpoints**
- `GET /fhir/metadata` - FHIR capability statement
- `GET /health` - Health check
- `GET /health/ready` - Readiness probe
- `GET /health/live` - Liveness probe

## Breaking Changes

N/A - Initial release

## Migration Guide

N/A - Initial release

## Known Issues

- OAuth2/SMART on FHIR authentication not yet implemented
- Limited to Patient and Observation resources
- No caching layer implemented yet
- No GraphQL API support yet

## Future Roadmap

### Phase 8: Authentication & Authorization
- OAuth2 support
- SMART on FHIR implementation
- JWT token validation
- Role-based access control (RBAC)
- Audit logging enhancements

### Phase 9: Additional FHIR Resources
- Encounter
- Condition
- Procedure
- MedicationRequest
- DiagnosticReport
- Practitioner
- Organization

### Phase 10: Performance Optimization
- Redis caching layer
- Connection pooling optimization
- Database read replicas
- Bulk operations support
- Query optimization

### Phase 11: Advanced Features
- GraphQL API
- WebSocket notifications
- FHIR Subscriptions
- Advanced search ($everything)
- Terminology services (CodeSystem, ValueSet)

## Security Advisories

None at this time.

## Contributors

- Initial implementation by Fire FHIR Server Team

## Links

- [GitHub Repository](https://github.com/yourusername/fire)
- [Issue Tracker](https://github.com/yourusername/fire/issues)
- [Documentation](https://github.com/yourusername/fire/blob/main/README.md)

---

[Unreleased]: https://github.com/yourusername/fire/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/yourusername/fire/releases/tag/v0.1.0
