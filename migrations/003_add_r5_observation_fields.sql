-- Add FHIR R5 specific fields to observation table

-- Add R5 fields to current table
ALTER TABLE observation
ADD COLUMN triggered_by_observation TEXT[],
ADD COLUMN triggered_by_type TEXT[],
ADD COLUMN focus_reference TEXT[],
ADD COLUMN body_structure_reference TEXT;

-- Add R5 fields to history table
ALTER TABLE observation_history
ADD COLUMN triggered_by_observation TEXT[],
ADD COLUMN triggered_by_type TEXT[],
ADD COLUMN focus_reference TEXT[],
ADD COLUMN body_structure_reference TEXT;

-- Create indexes for new R5 fields
CREATE INDEX idx_observation_triggered_by ON observation USING GIN (triggered_by_observation);
CREATE INDEX idx_observation_focus ON observation USING GIN (focus_reference);
CREATE INDEX idx_observation_body_structure ON observation (body_structure_reference);

-- Create comment to document R5 upgrade
COMMENT ON TABLE observation IS 'FHIR R5 (5.0.0) compliant Observation resource table';
COMMENT ON COLUMN observation.triggered_by_observation IS 'R5: References to observations that triggered this observation';
COMMENT ON COLUMN observation.triggered_by_type IS 'R5: Trigger type (reflex | repeat | re-run)';
COMMENT ON COLUMN observation.focus_reference IS 'R5: What the observation is about when not about the subject';
COMMENT ON COLUMN observation.body_structure_reference IS 'R5: Observed body structure';
