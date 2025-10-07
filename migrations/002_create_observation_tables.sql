-- Create observation current table
CREATE TABLE observation (
    id UUID PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted BOOLEAN NOT NULL DEFAULT FALSE,
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    status TEXT,                           -- registered | preliminary | final | amended +
    category_system TEXT[],                -- e.g., ["http://terminology.hl7.org/CodeSystem/observation-category"]
    category_code TEXT[],                  -- e.g., ["vital-signs", "laboratory"]
    code_system TEXT,                      -- e.g., "http://loinc.org"
    code_code TEXT,                        -- e.g., "8867-4" (heart rate)
    subject_reference TEXT,                -- e.g., "Patient/123"
    patient_reference TEXT,                -- Same as subject for Patient
    encounter_reference TEXT,              -- e.g., "Encounter/456"
    effective_datetime TIMESTAMPTZ,        -- When observation was made
    effective_period_start TIMESTAMPTZ,
    effective_period_end TIMESTAMPTZ,
    issued TIMESTAMPTZ,                    -- When observation was issued
    value_quantity_value NUMERIC(20, 6),  -- Numeric value
    value_quantity_unit TEXT,              -- Unit of measure
    value_quantity_system TEXT,            -- Unit system
    value_codeable_concept_code TEXT[],    -- For coded values
    value_string TEXT,                     -- For string values
    performer_reference TEXT[]             -- Who performed observation
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
CREATE INDEX idx_observation_deleted ON observation (deleted);

-- Create GIN index for JSONB content
CREATE INDEX idx_observation_content ON observation USING GIN (content);

-- Create observation history table
CREATE TABLE observation_history (
    id UUID NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT FALSE,
    content JSONB NOT NULL,

    -- Same search parameters as current
    status TEXT,
    category_system TEXT[],
    category_code TEXT[],
    code_system TEXT,
    code_code TEXT,
    subject_reference TEXT,
    patient_reference TEXT,
    encounter_reference TEXT,
    effective_datetime TIMESTAMPTZ,
    effective_period_start TIMESTAMPTZ,
    effective_period_end TIMESTAMPTZ,
    issued TIMESTAMPTZ,
    value_quantity_value NUMERIC(20, 6),
    value_quantity_unit TEXT,
    value_quantity_system TEXT,
    value_codeable_concept_code TEXT[],
    value_string TEXT,
    performer_reference TEXT[],

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
