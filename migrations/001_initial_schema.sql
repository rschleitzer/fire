-- Create observation current table
CREATE TABLE observation (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    code_system TEXT,
    code_code TEXT,
    effective_datetime TIMESTAMPTZ,
    effective_period_start TIMESTAMPTZ,
    effective_period_end TIMESTAMPTZ,
    encounter_reference TEXT,
    category_system TEXT,
    category_code TEXT,
    focus_reference TEXT,
    performer_reference TEXT,
    status TEXT,
    subject_reference TEXT,
    value_quantity_value NUMERIC(20, 6),
    value_quantity_unit TEXT,
    value_quantity_system TEXT,
    value_codeable_concept_code TEXT,
    patient_reference TEXT,
    issued TIMESTAMPTZ,
    value_string TEXT,
    triggered_by_observation TEXT,
    triggered_by_type TEXT,
    body_structure_reference TEXT,
);

-- Create indexes for current table
CREATE INDEX idx_observation_status ON observation (status);
CREATE INDEX idx_observation_category_code ON observation USING GIN (category_code);
CREATE INDEX idx_observation_code_code ON observation (code_code);
CREATE INDEX idx_observation_subject ON observation (subject_reference);
CREATE INDEX idx_observation_patient ON observation (patient_reference);
CREATE INDEX idx_observation_encounter ON observation (encounter_reference);
CREATE INDEX idx_observation_effective_datetime ON observation (effective_datetime);
CREATE INDEX idx_observation_effective_period ON observation (effective_period_start, effective_period_end);
CREATE INDEX idx_observation_issued ON observation (issued);
CREATE INDEX idx_observation_value_quantity ON observation (value_quantity_value);
CREATE INDEX idx_observation_last_updated ON observation (last_updated);
CREATE INDEX idx_observation_triggered_by ON observation USING GIN (triggered_by_observation);
CREATE INDEX idx_observation_focus ON observation USING GIN (focus_reference);
CREATE INDEX idx_observation_body_structure ON observation (body_structure_reference);

-- Create GIN index for JSONB content
CREATE INDEX idx_observation_content ON observation USING GIN (content);

-- Create observation history table
CREATE TABLE observation_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    code_system TEXT,
    code_code TEXT,
    effective_datetime TIMESTAMPTZ,
    effective_period_start TIMESTAMPTZ,
    effective_period_end TIMESTAMPTZ,
    encounter_reference TEXT,
    category_system TEXT[],
    category_code TEXT[],
    focus_reference TEXT[],
    performer_reference TEXT[],
    status TEXT,
    subject_reference TEXT,
    value_quantity_value NUMERIC(20, 6),
    value_quantity_unit TEXT,
    value_quantity_system TEXT,
    value_codeable_concept_code TEXT[],
    patient_reference TEXT,
    issued TIMESTAMPTZ,
    value_string TEXT,
    triggered_by_observation TEXT[],
    triggered_by_type TEXT[],
    body_structure_reference TEXT,

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_observation_history_id ON observation_history (id);
CREATE INDEX idx_observation_history_timestamp ON observation_history (history_timestamp);
CREATE INDEX idx_observation_history_last_updated ON observation_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_observation_history_content ON observation_history USING GIN (content);

-- Create patient current table
CREATE TABLE patient (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    family_name TEXT[],
    given_name TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    birthdate DATE,
    gender TEXT,
    active BOOLEAN,
    general_practitioner_reference TEXT[] DEFAULT '{}'
);

-- Create indexes for current table
CREATE INDEX idx_patient_family_name ON patient USING GIN (family_name);
CREATE INDEX idx_patient_given_name ON patient USING GIN (given_name);
CREATE INDEX idx_patient_prefix ON patient USING GIN (prefix);
CREATE INDEX idx_patient_suffix ON patient USING GIN (suffix);
CREATE INDEX idx_patient_name_text ON patient USING GIN (name_text);
CREATE INDEX idx_patient_identifier_value ON patient USING GIN (identifier_value);
CREATE INDEX idx_patient_birthdate ON patient (birthdate);
CREATE INDEX idx_patient_gender ON patient (gender);
CREATE INDEX idx_patient_active ON patient (active);
CREATE INDEX idx_patient_last_updated ON patient (last_updated);
CREATE INDEX idx_patient_general_practitioner ON patient USING GIN (general_practitioner_reference);

-- Create GIN index for JSONB content (for advanced queries)
CREATE INDEX idx_patient_content ON patient USING GIN (content);

-- Create patient history table
CREATE TABLE patient_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    family_name TEXT[],
    given_name TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    birthdate DATE,
    gender TEXT,
    active BOOLEAN,
    general_practitioner_reference TEXT[] DEFAULT '{}',

    -- History metadata
    history_operation VARCHAR(10),
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table (lighter than current)
CREATE INDEX idx_patient_history_id ON patient_history (id);
CREATE INDEX idx_patient_history_timestamp ON patient_history (history_timestamp);
CREATE INDEX idx_patient_history_last_updated ON patient_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_patient_history_content ON patient_history USING GIN (content);


-- Create practitioner current table
CREATE TABLE practitioner (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    family_name TEXT[],
    given_name TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    telecom_value TEXT[],
    active BOOLEAN
);

-- Create indexes for current table
CREATE INDEX idx_practitioner_last_updated ON practitioner(last_updated);
CREATE INDEX idx_practitioner_family_name ON practitioner USING GIN(family_name);
CREATE INDEX idx_practitioner_given_name ON practitioner USING GIN(given_name);
CREATE INDEX idx_practitioner_identifier_value ON practitioner USING GIN(identifier_value);
CREATE INDEX idx_practitioner_telecom_value ON practitioner USING GIN(telecom_value);
CREATE INDEX idx_practitioner_active ON practitioner(active);

-- Create GIN index for JSONB content
CREATE INDEX idx_practitioner_content ON practitioner USING GIN (content);

-- Create practitioner history table
CREATE TABLE practitioner_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    family_name TEXT[],
    given_name TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    telecom_value TEXT[],
    active BOOLEAN,

    -- History metadata
    history_operation VARCHAR(10),
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_practitioner_history_id ON practitioner_history (id);
CREATE INDEX idx_practitioner_history_timestamp ON practitioner_history(history_timestamp);
CREATE INDEX idx_practitioner_history_last_updated ON practitioner_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_practitioner_history_content ON practitioner_history USING GIN (content);
