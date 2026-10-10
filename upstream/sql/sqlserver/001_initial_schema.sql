-- Fire FHIR Server - Binary Schema
-- Resource table in public, history/search tables in per-resource schema

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'Binary') EXEC('CREATE SCHEMA [Binary]');

-- Main resource table
CREATE TABLE [Binary] (
    id VARCHAR(64) PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    content VARBINARY(MAX) NOT NULL
);

CREATE INDEX ix_Binary_last_updated ON [Binary] (last_updated);

-- History table
CREATE TABLE [Binary].resource_history (
    id VARCHAR(64) NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated DATETIME2 NOT NULL,
    content VARBINARY(MAX) NOT NULL,
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    PRIMARY KEY (id, version_id)
);

CREATE INDEX ix_Binary_resource_history_id ON [Binary].resource_history (id);
CREATE INDEX ix_Binary_resource_history_timestamp ON [Binary].resource_history (history_timestamp);
CREATE INDEX ix_Binary_resource_history_last_updated ON [Binary].resource_history (last_updated);

-- Fire FHIR Server - Observation Schema
-- Resource table in public, history/search tables in per-resource schema

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'Observation') EXEC('CREATE SCHEMA [Observation]');

-- Main resource table
CREATE TABLE [Observation] (
    id VARCHAR(64) PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    content VARBINARY(MAX) NOT NULL,
    -- Inline: non-collection, non-variant, non-object, non-date
    [status] NVARCHAR(MAX)
);

CREATE INDEX ix_Observation_last_updated ON [Observation] (last_updated);
CREATE INDEX ix_Observation_status ON [Observation] ([status]);

-- History table
CREATE TABLE [Observation].resource_history (
    id VARCHAR(64) NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated DATETIME2 NOT NULL,
    content VARBINARY(MAX) NOT NULL,
    [status] NVARCHAR(MAX),
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    PRIMARY KEY (id, version_id)
);

CREATE INDEX ix_Observation_resource_history_id ON [Observation].resource_history (id);
CREATE INDEX ix_Observation_resource_history_timestamp ON [Observation].resource_history (history_timestamp);
CREATE INDEX ix_Observation_resource_history_last_updated ON [Observation].resource_history (last_updated);

-- =============================================================================
-- String search tables (value TEXT)
-- =============================================================================

CREATE TABLE [Observation].search_value_markdown (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_value_markdown_value ON [Observation].search_value_markdown (value);

-- =============================================================================
-- Token search tables (system TEXT, value TEXT, display TEXT)
-- =============================================================================

CREATE TABLE [Observation].search_identifier (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_identifier_value ON [Observation].search_identifier (value);
CREATE INDEX ix_Observation_search_identifier_system ON [Observation].search_identifier (system);

CREATE TABLE [Observation].search_code (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_code_value ON [Observation].search_code (value);

CREATE TABLE [Observation].search_category (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_category_value ON [Observation].search_category (value);

CREATE TABLE [Observation].search_combo_code (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_combo_code_value ON [Observation].search_combo_code (value);

CREATE TABLE [Observation].search_combo_data_absent_reason (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_combo_data_absent_reason_value ON [Observation].search_combo_data_absent_reason (value);

CREATE TABLE [Observation].search_combo_value_concept (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_combo_value_concept_value ON [Observation].search_combo_value_concept (value);

CREATE TABLE [Observation].search_component_code (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_component_code_value ON [Observation].search_component_code (value);

CREATE TABLE [Observation].search_component_data_absent_reason (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_component_data_absent_reason_value ON [Observation].search_component_data_absent_reason (value);

CREATE TABLE [Observation].search_component_value_concept (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_component_value_concept_value ON [Observation].search_component_value_concept (value);

CREATE TABLE [Observation].search_data_absent_reason (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_data_absent_reason_value ON [Observation].search_data_absent_reason (value);

CREATE TABLE [Observation].search_method (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_method_value ON [Observation].search_method (value);

CREATE TABLE [Observation].search_value_concept (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_value_concept_value ON [Observation].search_value_concept (value);

-- =============================================================================
-- Date search tables
-- =============================================================================

CREATE TABLE [Observation].search_date (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    value_low DATETIME2,
    value_high DATETIME2,
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_date_value_low ON [Observation].search_date (value_low);
CREATE INDEX ix_Observation_search_date_value_high ON [Observation].search_date (value_high);

CREATE TABLE [Observation].search_value_date (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    value_low DATETIME2,
    value_high DATETIME2,
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_value_date_value_low ON [Observation].search_value_date (value_low);
CREATE INDEX ix_Observation_search_value_date_value_high ON [Observation].search_value_date (value_high);

-- =============================================================================
-- Reference search tables (target_type TEXT, target_id TEXT, target_version TEXT)
-- =============================================================================

CREATE TABLE [Observation].search_patient (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_patient_reference ON [Observation].search_patient (target_id, target_type);

CREATE TABLE [Observation].search_encounter (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_encounter_reference ON [Observation].search_encounter (target_id, target_type);

CREATE TABLE [Observation].search_based_on (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_based_on_reference ON [Observation].search_based_on (target_id, target_type);

CREATE TABLE [Observation].search_component_value_reference (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_component_value_reference_reference ON [Observation].search_component_value_reference (target_id, target_type);

CREATE TABLE [Observation].search_derived_from (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_derived_from_reference ON [Observation].search_derived_from (target_id, target_type);

CREATE TABLE [Observation].search_device (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_device_reference ON [Observation].search_device (target_id, target_type);

CREATE TABLE [Observation].search_focus (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_focus_reference ON [Observation].search_focus (target_id, target_type);

CREATE TABLE [Observation].search_has_member (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_has_member_reference ON [Observation].search_has_member (target_id, target_type);

CREATE TABLE [Observation].search_part_of (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_part_of_reference ON [Observation].search_part_of (target_id, target_type);

CREATE TABLE [Observation].search_performer (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_performer_reference ON [Observation].search_performer (target_id, target_type);

CREATE TABLE [Observation].search_specimen (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_specimen_reference ON [Observation].search_specimen (target_id, target_type);

CREATE TABLE [Observation].search_subject (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_subject_reference ON [Observation].search_subject (target_id, target_type);

CREATE TABLE [Observation].search_value_reference (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Observation](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Observation_search_value_reference_reference ON [Observation].search_value_reference (target_id, target_type);

-- Fire FHIR Server - Patient Schema
-- Resource table in public, history/search tables in per-resource schema

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'Patient') EXEC('CREATE SCHEMA [Patient]');

-- Main resource table
CREATE TABLE [Patient] (
    id VARCHAR(64) PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    content VARBINARY(MAX) NOT NULL,
    -- Inline: non-collection, non-variant, non-object, non-date
    [active] BIT,
    [gender] NVARCHAR(MAX)
);

CREATE INDEX ix_Patient_last_updated ON [Patient] (last_updated);
CREATE INDEX ix_Patient_active ON [Patient] ([active]);
CREATE INDEX ix_Patient_gender ON [Patient] ([gender]);

-- History table
CREATE TABLE [Patient].resource_history (
    id VARCHAR(64) NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated DATETIME2 NOT NULL,
    content VARBINARY(MAX) NOT NULL,
    [active] BIT,
    [gender] NVARCHAR(MAX),
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    PRIMARY KEY (id, version_id)
);

CREATE INDEX ix_Patient_resource_history_id ON [Patient].resource_history (id);
CREATE INDEX ix_Patient_resource_history_timestamp ON [Patient].resource_history (history_timestamp);
CREATE INDEX ix_Patient_resource_history_last_updated ON [Patient].resource_history (last_updated);

-- =============================================================================
-- String search tables (value TEXT)
-- =============================================================================

CREATE TABLE [Patient].search_address (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_value ON [Patient].search_address (value);

CREATE TABLE [Patient].search_address_city (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_city_value ON [Patient].search_address_city (value);

CREATE TABLE [Patient].search_address_country (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_country_value ON [Patient].search_address_country (value);

CREATE TABLE [Patient].search_address_postalcode (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_postalcode_value ON [Patient].search_address_postalcode (value);

CREATE TABLE [Patient].search_address_state (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_state_value ON [Patient].search_address_state (value);

CREATE TABLE [Patient].search_family (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_family_value ON [Patient].search_family (value);

CREATE TABLE [Patient].search_given (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_given_value ON [Patient].search_given (value);

CREATE TABLE [Patient].search_name (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_name_value ON [Patient].search_name (value);

CREATE TABLE [Patient].search_phonetic (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_phonetic_value ON [Patient].search_phonetic (value);

-- =============================================================================
-- Token search tables (system TEXT, value TEXT, display TEXT)
-- =============================================================================

CREATE TABLE [Patient].search_address_use (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_address_use_value ON [Patient].search_address_use (value);

CREATE TABLE [Patient].search_deceased (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_deceased_value ON [Patient].search_deceased (value);

CREATE TABLE [Patient].search_email (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_email_value ON [Patient].search_email (value);

CREATE TABLE [Patient].search_identifier (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_identifier_value ON [Patient].search_identifier (value);
CREATE INDEX ix_Patient_search_identifier_system ON [Patient].search_identifier (system);

CREATE TABLE [Patient].search_language (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_language_value ON [Patient].search_language (value);
CREATE INDEX ix_Patient_search_language_system ON [Patient].search_language (system);

CREATE TABLE [Patient].search_phone (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_phone_value ON [Patient].search_phone (value);

CREATE TABLE [Patient].search_telecom (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_telecom_value ON [Patient].search_telecom (value);

-- =============================================================================
-- Date search tables
-- =============================================================================

CREATE TABLE [Patient].search_birthdate (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    value_low DATETIME2,
    value_high DATETIME2,
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_birthdate_value_low ON [Patient].search_birthdate (value_low);
CREATE INDEX ix_Patient_search_birthdate_value_high ON [Patient].search_birthdate (value_high);

-- =============================================================================
-- Reference search tables (target_type TEXT, target_id TEXT, target_version TEXT)
-- =============================================================================

CREATE TABLE [Patient].search_general_practitioner (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_general_practitioner_reference ON [Patient].search_general_practitioner (target_id, target_type);

CREATE TABLE [Patient].search_link (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_link_reference ON [Patient].search_link (target_id, target_type);

CREATE TABLE [Patient].search_organization (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Patient](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Patient_search_organization_reference ON [Patient].search_organization (target_id, target_type);

-- Fire FHIR Server - Practitioner Schema
-- Resource table in public, history/search tables in per-resource schema

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'Practitioner') EXEC('CREATE SCHEMA [Practitioner]');

-- Main resource table
CREATE TABLE [Practitioner] (
    id VARCHAR(64) PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    content VARBINARY(MAX) NOT NULL,
    -- Inline: non-collection, non-variant, non-object, non-date
    [gender] NVARCHAR(MAX),
    [active] BIT
);

CREATE INDEX ix_Practitioner_last_updated ON [Practitioner] (last_updated);
CREATE INDEX ix_Practitioner_gender ON [Practitioner] ([gender]);
CREATE INDEX ix_Practitioner_active ON [Practitioner] ([active]);

-- History table
CREATE TABLE [Practitioner].resource_history (
    id VARCHAR(64) NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated DATETIME2 NOT NULL,
    content VARBINARY(MAX) NOT NULL,
    [gender] NVARCHAR(MAX),
    [active] BIT,
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    PRIMARY KEY (id, version_id)
);

CREATE INDEX ix_Practitioner_resource_history_id ON [Practitioner].resource_history (id);
CREATE INDEX ix_Practitioner_resource_history_timestamp ON [Practitioner].resource_history (history_timestamp);
CREATE INDEX ix_Practitioner_resource_history_last_updated ON [Practitioner].resource_history (last_updated);

-- =============================================================================
-- String search tables (value TEXT)
-- =============================================================================

CREATE TABLE [Practitioner].search_address (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_value ON [Practitioner].search_address (value);

CREATE TABLE [Practitioner].search_address_city (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_city_value ON [Practitioner].search_address_city (value);

CREATE TABLE [Practitioner].search_address_country (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_country_value ON [Practitioner].search_address_country (value);

CREATE TABLE [Practitioner].search_address_postalcode (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_postalcode_value ON [Practitioner].search_address_postalcode (value);

CREATE TABLE [Practitioner].search_address_state (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_state_value ON [Practitioner].search_address_state (value);

CREATE TABLE [Practitioner].search_family (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_family_value ON [Practitioner].search_family (value);

CREATE TABLE [Practitioner].search_given (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_given_value ON [Practitioner].search_given (value);

CREATE TABLE [Practitioner].search_phonetic (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_phonetic_value ON [Practitioner].search_phonetic (value);

CREATE TABLE [Practitioner].search_name (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value NVARCHAR(450),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_name_value ON [Practitioner].search_name (value);

-- =============================================================================
-- Token search tables (system TEXT, value TEXT, display TEXT)
-- =============================================================================

CREATE TABLE [Practitioner].search_address_use (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_address_use_value ON [Practitioner].search_address_use (value);

CREATE TABLE [Practitioner].search_email (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_email_value ON [Practitioner].search_email (value);

CREATE TABLE [Practitioner].search_phone (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_phone_value ON [Practitioner].search_phone (value);

CREATE TABLE [Practitioner].search_telecom (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_telecom_value ON [Practitioner].search_telecom (value);

CREATE TABLE [Practitioner].search_communication (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_communication_value ON [Practitioner].search_communication (value);

CREATE TABLE [Practitioner].search_deceased (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_deceased_value ON [Practitioner].search_deceased (value);

CREATE TABLE [Practitioner].search_identifier (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_identifier_value ON [Practitioner].search_identifier (value);
CREATE INDEX ix_Practitioner_search_identifier_system ON [Practitioner].search_identifier (system);

-- =============================================================================
-- Date search tables
-- =============================================================================

CREATE TABLE [Practitioner].search_qualification_period (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Practitioner](id) ON DELETE CASCADE,
    value_low DATETIME2,
    value_high DATETIME2,
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Practitioner_search_qualification_period_value_low ON [Practitioner].search_qualification_period (value_low);
CREATE INDEX ix_Practitioner_search_qualification_period_value_high ON [Practitioner].search_qualification_period (value_high);

-- Fire FHIR Server - Provenance Schema
-- Resource table in public, history/search tables in per-resource schema

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'Provenance') EXEC('CREATE SCHEMA [Provenance]');

-- Main resource table
CREATE TABLE [Provenance] (
    id VARCHAR(64) PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    content VARBINARY(MAX) NOT NULL,
    -- Inline: non-collection, non-variant, non-object, non-date
    [recorded] DATETIME2
);

CREATE INDEX ix_Provenance_last_updated ON [Provenance] (last_updated);
CREATE INDEX ix_Provenance_recorded ON [Provenance] ([recorded]);

-- History table
CREATE TABLE [Provenance].resource_history (
    id VARCHAR(64) NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated DATETIME2 NOT NULL,
    content VARBINARY(MAX) NOT NULL,
    [recorded] DATETIME2,
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    PRIMARY KEY (id, version_id)
);

CREATE INDEX ix_Provenance_resource_history_id ON [Provenance].resource_history (id);
CREATE INDEX ix_Provenance_resource_history_timestamp ON [Provenance].resource_history (history_timestamp);
CREATE INDEX ix_Provenance_resource_history_last_updated ON [Provenance].resource_history (last_updated);

-- =============================================================================
-- Token search tables (system TEXT, value TEXT, display TEXT)
-- =============================================================================

CREATE TABLE [Provenance].search_activity (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_activity_value ON [Provenance].search_activity (value);

CREATE TABLE [Provenance].search_agent_role (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_agent_role_value ON [Provenance].search_agent_role (value);

CREATE TABLE [Provenance].search_agent_type (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_agent_type_value ON [Provenance].search_agent_type (value);

CREATE TABLE [Provenance].search_signature_type (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    system NVARCHAR(450),
    value NVARCHAR(450),
    display NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_signature_type_value ON [Provenance].search_signature_type (value);

-- =============================================================================
-- Reference search tables (target_type TEXT, target_id TEXT, target_version TEXT)
-- =============================================================================

CREATE TABLE [Provenance].search_patient (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_patient_reference ON [Provenance].search_patient (target_id, target_type);

CREATE TABLE [Provenance].search_encounter (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_encounter_reference ON [Provenance].search_encounter (target_id, target_type);

CREATE TABLE [Provenance].search_agent (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_agent_reference ON [Provenance].search_agent (target_id, target_type);

CREATE TABLE [Provenance].search_based_on (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_based_on_reference ON [Provenance].search_based_on (target_id, target_type);

CREATE TABLE [Provenance].search_entity (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_entity_reference ON [Provenance].search_entity (target_id, target_type);

CREATE TABLE [Provenance].search_location (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_location_reference ON [Provenance].search_location (target_id, target_type);

CREATE TABLE [Provenance].search_target (
    id INT NOT NULL,
    resource_id VARCHAR(64) NOT NULL REFERENCES [Provenance](id) ON DELETE CASCADE,
    target_type NVARCHAR(450),
    target_id NVARCHAR(450),
    target_version NVARCHAR(MAX),
    PRIMARY KEY (id, resource_id)
);
CREATE INDEX ix_Provenance_search_target_reference ON [Provenance].search_target (target_id, target_type);

