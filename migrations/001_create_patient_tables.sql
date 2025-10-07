-- Create patient current table
CREATE TABLE patient (
    id UUID PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted BOOLEAN NOT NULL DEFAULT FALSE,
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    family_name TEXT[],
    given_name TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    birthdate DATE,
    gender TEXT,
    active BOOLEAN
);

-- Create indexes for current table
CREATE INDEX idx_patient_family_name ON patient USING GIN (family_name);
CREATE INDEX idx_patient_given_name ON patient USING GIN (given_name);
CREATE INDEX idx_patient_identifier_value ON patient USING GIN (identifier_value);
CREATE INDEX idx_patient_birthdate ON patient (birthdate);
CREATE INDEX idx_patient_gender ON patient (gender);
CREATE INDEX idx_patient_active ON patient (active);
CREATE INDEX idx_patient_last_updated ON patient (last_updated);
CREATE INDEX idx_patient_deleted ON patient (deleted);

-- Create GIN index for JSONB content (for advanced queries)
CREATE INDEX idx_patient_content ON patient USING GIN (content);

-- Create patient history table
CREATE TABLE patient_history (
    id UUID NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT FALSE,
    content JSONB NOT NULL,

    -- Same search parameters as current
    family_name TEXT[],
    given_name TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    birthdate DATE,
    gender TEXT,
    active BOOLEAN,

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,  -- CREATE, UPDATE, DELETE
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table (lighter than current)
CREATE INDEX idx_patient_history_id ON patient_history (id);
CREATE INDEX idx_patient_history_timestamp ON patient_history (history_timestamp);
CREATE INDEX idx_patient_history_last_updated ON patient_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_patient_history_content ON patient_history USING GIN (content);
