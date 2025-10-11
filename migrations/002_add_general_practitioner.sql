-- Add general_practitioner_reference column to patient table
-- This supports searching for patients by their general practitioner

ALTER TABLE patient
ADD COLUMN general_practitioner_reference TEXT[] DEFAULT '{}';

-- Add index for efficient searching
CREATE INDEX idx_patient_general_practitioner ON patient USING GIN (general_practitioner_reference);

-- Add same column to patient_history table for consistency
ALTER TABLE patient_history
ADD COLUMN general_practitioner_reference TEXT[] DEFAULT '{}';
