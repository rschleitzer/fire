-- Create observation current table
CREATE TABLE observation (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    _lastUpdated DATE,
    identifier_system TEXT[],
    identifier_value TEXT[],
    code_system TEXT,
    code_code TEXT,
    date_datetime TIMESTAMPTZ,
    date_period_start TIMESTAMPTZ,
    date_period_end TIMESTAMPTZ,
    encounter_reference TEXT DEFAULT '{}',
    based_on_reference TEXT[] DEFAULT '{}',
    category_system TEXT[],
    category_code TEXT[],
    combo_code_system TEXT,
    combo_code_code TEXT,
    combo_data_absent_reason_system TEXT,
    combo_data_absent_reason_code TEXT,
    combo_value_concept_system TEXT,
    combo_value_concept_code TEXT,
    combo_value_quantity_value NUMERIC(20, 6),
    combo_value_quantity_unit TEXT,
    combo_value_quantity_system TEXT,
    component_code_system TEXT[],
    component_code_code TEXT[],
    component_data_absent_reason_system TEXT[],
    component_data_absent_reason_code TEXT[],
    component_value_canonical TEXT,
    component_value_concept_system TEXT[],
    component_value_concept_code TEXT[],
    component_value_quantity_value NUMERIC(20, 6),
    component_value_quantity_unit TEXT,
    component_value_quantity_system TEXT,
    component_value_reference_reference TEXT[] DEFAULT '{}',
    data_absent_reason_system TEXT,
    data_absent_reason_code TEXT,
    derived_from_reference TEXT[] DEFAULT '{}',
    device_reference TEXT DEFAULT '{}',
    focus_reference TEXT[] DEFAULT '{}',
    has_member_reference TEXT[] DEFAULT '{}',
    method_system TEXT,
    method_code TEXT,
    part_of_reference TEXT[] DEFAULT '{}',
    performer_reference TEXT[] DEFAULT '{}',
    specimen_reference TEXT DEFAULT '{}',
    status TEXT,
    subject_reference TEXT DEFAULT '{}',
    value_canonical TEXT,
    value_concept_system TEXT,
    value_concept_code TEXT,
    value_date_datetime TIMESTAMPTZ,
    value_date_period_start TIMESTAMPTZ,
    value_date_period_end TIMESTAMPTZ,
    value_markdown_name TEXT,
    value_quantity_value NUMERIC(20, 6),
    value_quantity_unit TEXT,
    value_quantity_system TEXT,
    value_reference_reference TEXT DEFAULT '{}'
);

-- Create indexes for current table
CREATE INDEX idx_observation_last_updated ON observation (last_updated);
CREATE INDEX idx_observation__lastUpdated ON observation (_lastUpdated);
CREATE INDEX idx_observation_identifier_value ON observation USING GIN (identifier_value);
CREATE INDEX idx_observation_code_code ON observation (code_code);
CREATE INDEX idx_observation_date_datetime ON observation (date_datetime);
CREATE INDEX idx_observation_date_period ON observation (date_period_start, date_period_end);
CREATE INDEX idx_observation_encounter_reference ON observation (encounter_reference);
CREATE INDEX idx_observation_based_on_reference ON observation USING GIN (based_on_reference);
CREATE INDEX idx_observation_category_code ON observation USING GIN (category_code);
CREATE INDEX idx_observation_combo_code_code ON observation (combo_code_code);
CREATE INDEX idx_observation_combo_data_absent_reason_code ON observation (combo_data_absent_reason_code);
CREATE INDEX idx_observation_combo_value_concept_code ON observation (combo_value_concept_code);
CREATE INDEX idx_observation_combo_value_quantity_value ON observation (combo_value_quantity_value);
CREATE INDEX idx_observation_component_code_code ON observation USING GIN (component_code_code);
CREATE INDEX idx_observation_component_data_absent_reason_code ON observation USING GIN (component_data_absent_reason_code);
CREATE INDEX idx_observation_component_value_canonical ON observation (component_value_canonical);
CREATE INDEX idx_observation_component_value_concept_code ON observation USING GIN (component_value_concept_code);
CREATE INDEX idx_observation_component_value_quantity_value ON observation (component_value_quantity_value);
CREATE INDEX idx_observation_component_value_reference_reference ON observation USING GIN (component_value_reference_reference);
CREATE INDEX idx_observation_data_absent_reason_code ON observation (data_absent_reason_code);
CREATE INDEX idx_observation_derived_from_reference ON observation USING GIN (derived_from_reference);
CREATE INDEX idx_observation_device_reference ON observation (device_reference);
CREATE INDEX idx_observation_focus_reference ON observation USING GIN (focus_reference);
CREATE INDEX idx_observation_has_member_reference ON observation USING GIN (has_member_reference);
CREATE INDEX idx_observation_method_code ON observation (method_code);
CREATE INDEX idx_observation_part_of_reference ON observation USING GIN (part_of_reference);
CREATE INDEX idx_observation_performer_reference ON observation USING GIN (performer_reference);
CREATE INDEX idx_observation_specimen_reference ON observation (specimen_reference);
CREATE INDEX idx_observation_status ON observation (status);
CREATE INDEX idx_observation_subject_reference ON observation (subject_reference);
CREATE INDEX idx_observation_value_canonical ON observation (value_canonical);
CREATE INDEX idx_observation_value_concept_code ON observation (value_concept_code);
CREATE INDEX idx_observation_value_date_datetime ON observation (value_date_datetime);
CREATE INDEX idx_observation_value_date_period ON observation (value_date_period_start, value_date_period_end);
CREATE INDEX idx_observation_value_markdown_name ON observation (value_markdown_name);
CREATE INDEX idx_observation_value_quantity_value ON observation (value_quantity_value);
CREATE INDEX idx_observation_value_reference_reference ON observation (value_reference_reference);

-- Create GIN index for JSONB content
CREATE INDEX idx_observation_content ON observation USING GIN (content);

-- Create observation history table
CREATE TABLE observation_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    _lastUpdated DATE,
    identifier_system TEXT[],
    identifier_value TEXT[],
    code_system TEXT,
    code_code TEXT,
    date_datetime TIMESTAMPTZ,
    date_period_start TIMESTAMPTZ,
    date_period_end TIMESTAMPTZ,
    encounter_reference TEXT DEFAULT '{}',
    based_on_reference TEXT[] DEFAULT '{}',
    category_system TEXT[],
    category_code TEXT[],
    combo_code_system TEXT,
    combo_code_code TEXT,
    combo_data_absent_reason_system TEXT,
    combo_data_absent_reason_code TEXT,
    combo_value_concept_system TEXT,
    combo_value_concept_code TEXT,
    combo_value_quantity_value NUMERIC(20, 6),
    combo_value_quantity_unit TEXT,
    combo_value_quantity_system TEXT,
    component_code_system TEXT[],
    component_code_code TEXT[],
    component_data_absent_reason_system TEXT[],
    component_data_absent_reason_code TEXT[],
    component_value_canonical TEXT,
    component_value_concept_system TEXT[],
    component_value_concept_code TEXT[],
    component_value_quantity_value NUMERIC(20, 6),
    component_value_quantity_unit TEXT,
    component_value_quantity_system TEXT,
    component_value_reference_reference TEXT[] DEFAULT '{}',
    data_absent_reason_system TEXT,
    data_absent_reason_code TEXT,
    derived_from_reference TEXT[] DEFAULT '{}',
    device_reference TEXT DEFAULT '{}',
    focus_reference TEXT[] DEFAULT '{}',
    has_member_reference TEXT[] DEFAULT '{}',
    method_system TEXT,
    method_code TEXT,
    part_of_reference TEXT[] DEFAULT '{}',
    performer_reference TEXT[] DEFAULT '{}',
    specimen_reference TEXT DEFAULT '{}',
    status TEXT,
    subject_reference TEXT DEFAULT '{}',
    value_canonical TEXT,
    value_concept_system TEXT,
    value_concept_code TEXT,
    value_date_datetime TIMESTAMPTZ,
    value_date_period_start TIMESTAMPTZ,
    value_date_period_end TIMESTAMPTZ,
    value_markdown_name TEXT,
    value_quantity_value NUMERIC(20, 6),
    value_quantity_unit TEXT,
    value_quantity_system TEXT,
    value_reference_reference TEXT DEFAULT '{}',

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
    _lastUpdated DATE,
    active BOOLEAN,
    address_name TEXT[],
    address_city_name TEXT[],
    address_country_name TEXT[],
    address_postalcode_name TEXT[],
    address_state_name TEXT[],
    address_use_system TEXT[],
    address_use_code TEXT[],
    birthdate DATE,
    deceased_system TEXT,
    deceased_code TEXT,
    email_system TEXT[],
    email_code TEXT[],
    family_name TEXT[],
    gender TEXT,
    general_practitioner_reference TEXT[] DEFAULT '{}',
    given_name TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    language_system TEXT[],
    language_code TEXT[],
    link_reference TEXT[] DEFAULT '{}',
    name_name TEXT[],
    organization_reference TEXT DEFAULT '{}',
    phone_system TEXT[],
    phone_code TEXT[],
    phonetic_name TEXT[],
    telecom_system TEXT[],
    telecom_code TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[]
);

-- Create indexes for current table
CREATE INDEX idx_patient_last_updated ON patient (last_updated);
CREATE INDEX idx_patient__lastUpdated ON patient (_lastUpdated);
CREATE INDEX idx_patient_active ON patient (active);
CREATE INDEX idx_patient_address_name ON patient USING GIN (address_name);
CREATE INDEX idx_patient_address_city_name ON patient USING GIN (address_city_name);
CREATE INDEX idx_patient_address_country_name ON patient USING GIN (address_country_name);
CREATE INDEX idx_patient_address_postalcode_name ON patient USING GIN (address_postalcode_name);
CREATE INDEX idx_patient_address_state_name ON patient USING GIN (address_state_name);
CREATE INDEX idx_patient_address_use_code ON patient USING GIN (address_use_code);
CREATE INDEX idx_patient_birthdate ON patient (birthdate);
CREATE INDEX idx_patient_deceased_code ON patient (deceased_code);
CREATE INDEX idx_patient_email_code ON patient USING GIN (email_code);
CREATE INDEX idx_patient_family_name ON patient USING GIN (family_name);
CREATE INDEX idx_patient_gender ON patient (gender);
CREATE INDEX idx_patient_general_practitioner_reference ON patient USING GIN (general_practitioner_reference);
CREATE INDEX idx_patient_given_name ON patient USING GIN (given_name);
CREATE INDEX idx_patient_identifier_value ON patient USING GIN (identifier_value);
CREATE INDEX idx_patient_language_code ON patient USING GIN (language_code);
CREATE INDEX idx_patient_link_reference ON patient USING GIN (link_reference);
CREATE INDEX idx_patient_name_name ON patient USING GIN (name_name);
CREATE INDEX idx_patient_organization_reference ON patient (organization_reference);
CREATE INDEX idx_patient_phone_code ON patient USING GIN (phone_code);
CREATE INDEX idx_patient_phonetic_name ON patient USING GIN (phonetic_name);
CREATE INDEX idx_patient_telecom_code ON patient USING GIN (telecom_code);
CREATE INDEX idx_patient_prefix ON patient USING GIN (prefix);
CREATE INDEX idx_patient_suffix ON patient USING GIN (suffix);
CREATE INDEX idx_patient_name_text ON patient USING GIN (name_text);

-- Create GIN index for JSONB content
CREATE INDEX idx_patient_content ON patient USING GIN (content);

-- Create patient history table
CREATE TABLE patient_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    _lastUpdated DATE,
    active BOOLEAN,
    address_name TEXT[],
    address_city_name TEXT[],
    address_country_name TEXT[],
    address_postalcode_name TEXT[],
    address_state_name TEXT[],
    address_use_system TEXT[],
    address_use_code TEXT[],
    birthdate DATE,
    deceased_system TEXT,
    deceased_code TEXT,
    email_system TEXT[],
    email_code TEXT[],
    family_name TEXT[],
    gender TEXT,
    general_practitioner_reference TEXT[] DEFAULT '{}',
    given_name TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    language_system TEXT[],
    language_code TEXT[],
    link_reference TEXT[] DEFAULT '{}',
    name_name TEXT[],
    organization_reference TEXT DEFAULT '{}',
    phone_system TEXT[],
    phone_code TEXT[],
    phonetic_name TEXT[],
    telecom_system TEXT[],
    telecom_code TEXT[],
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
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
    _lastUpdated DATE,
    address_name TEXT[],
    address_city_name TEXT[],
    address_country_name TEXT[],
    address_postalcode_name TEXT[],
    address_state_name TEXT[],
    address_use_system TEXT[],
    address_use_code TEXT[],
    email_system TEXT[],
    email_code TEXT[],
    family_name TEXT[],
    gender TEXT,
    given_name TEXT[],
    phone_system TEXT[],
    phone_code TEXT[],
    phonetic_name TEXT[],
    telecom_system TEXT[],
    telecom_code TEXT[],
    active BOOLEAN,
    communication_system TEXT[],
    communication_code TEXT[],
    deceased_system TEXT,
    deceased_code TEXT,
    identifier_system TEXT[],
    identifier_value TEXT[],
    name_name TEXT[],
    qualification_period DATE,
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    telecom_value TEXT[]
);

-- Create indexes for current table
CREATE INDEX idx_practitioner_last_updated ON practitioner (last_updated);
CREATE INDEX idx_practitioner__lastUpdated ON practitioner (_lastUpdated);
CREATE INDEX idx_practitioner_address_name ON practitioner USING GIN (address_name);
CREATE INDEX idx_practitioner_address_city_name ON practitioner USING GIN (address_city_name);
CREATE INDEX idx_practitioner_address_country_name ON practitioner USING GIN (address_country_name);
CREATE INDEX idx_practitioner_address_postalcode_name ON practitioner USING GIN (address_postalcode_name);
CREATE INDEX idx_practitioner_address_state_name ON practitioner USING GIN (address_state_name);
CREATE INDEX idx_practitioner_address_use_code ON practitioner USING GIN (address_use_code);
CREATE INDEX idx_practitioner_email_code ON practitioner USING GIN (email_code);
CREATE INDEX idx_practitioner_family_name ON practitioner USING GIN (family_name);
CREATE INDEX idx_practitioner_gender ON practitioner (gender);
CREATE INDEX idx_practitioner_given_name ON practitioner USING GIN (given_name);
CREATE INDEX idx_practitioner_phone_code ON practitioner USING GIN (phone_code);
CREATE INDEX idx_practitioner_phonetic_name ON practitioner USING GIN (phonetic_name);
CREATE INDEX idx_practitioner_telecom_code ON practitioner USING GIN (telecom_code);
CREATE INDEX idx_practitioner_active ON practitioner (active);
CREATE INDEX idx_practitioner_communication_code ON practitioner USING GIN (communication_code);
CREATE INDEX idx_practitioner_deceased_code ON practitioner (deceased_code);
CREATE INDEX idx_practitioner_identifier_value ON practitioner USING GIN (identifier_value);
CREATE INDEX idx_practitioner_name_name ON practitioner USING GIN (name_name);
CREATE INDEX idx_practitioner_qualification_period ON practitioner (qualification_period);
CREATE INDEX idx_practitioner_prefix ON practitioner USING GIN (prefix);
CREATE INDEX idx_practitioner_suffix ON practitioner USING GIN (suffix);
CREATE INDEX idx_practitioner_name_text ON practitioner USING GIN (name_text);
CREATE INDEX idx_practitioner_telecom_value ON practitioner USING GIN (telecom_value);

-- Create GIN index for JSONB content
CREATE INDEX idx_practitioner_content ON practitioner USING GIN (content);

-- Create practitioner history table
CREATE TABLE practitioner_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    _lastUpdated DATE,
    address_name TEXT[],
    address_city_name TEXT[],
    address_country_name TEXT[],
    address_postalcode_name TEXT[],
    address_state_name TEXT[],
    address_use_system TEXT[],
    address_use_code TEXT[],
    email_system TEXT[],
    email_code TEXT[],
    family_name TEXT[],
    gender TEXT,
    given_name TEXT[],
    phone_system TEXT[],
    phone_code TEXT[],
    phonetic_name TEXT[],
    telecom_system TEXT[],
    telecom_code TEXT[],
    active BOOLEAN,
    communication_system TEXT[],
    communication_code TEXT[],
    deceased_system TEXT,
    deceased_code TEXT,
    identifier_system TEXT[],
    identifier_value TEXT[],
    name_name TEXT[],
    qualification_period DATE,
    prefix TEXT[],
    suffix TEXT[],
    name_text TEXT[],
    telecom_value TEXT[],

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_practitioner_history_id ON practitioner_history (id);
CREATE INDEX idx_practitioner_history_timestamp ON practitioner_history (history_timestamp);
CREATE INDEX idx_practitioner_history_last_updated ON practitioner_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_practitioner_history_content ON practitioner_history USING GIN (content);

