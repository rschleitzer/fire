-- Backfill general_practitioner_reference from existing patient JSON content
-- Extract generalPractitioner.reference values from the content JSONB column

UPDATE patient
SET general_practitioner_reference = (
    SELECT COALESCE(
        ARRAY_AGG(elem->>'reference'),
        ARRAY[]::TEXT[]
    )
    FROM jsonb_array_elements(
        COALESCE(content->'generalPractitioner', '[]'::jsonb)
    ) AS elem
    WHERE elem->>'reference' IS NOT NULL
)
WHERE id = patient.id;

-- Do the same for patient_history
UPDATE patient_history
SET general_practitioner_reference = (
    SELECT COALESCE(
        ARRAY_AGG(elem->>'reference'),
        ARRAY[]::TEXT[]
    )
    FROM jsonb_array_elements(
        COALESCE(content->'generalPractitioner', '[]'::jsonb)
    ) AS elem
    WHERE elem->>'reference' IS NOT NULL
)
WHERE id = patient_history.id AND version_id = patient_history.version_id;
