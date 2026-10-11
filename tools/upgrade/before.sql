-- What the database of tools/upgrade-test.sh lacks before its steps: the
-- column and the table the first step makes, and the key the second drops.
DROP INDEX IF EXISTS ix_Patient_gender;
ALTER TABLE "Patient" DROP COLUMN "gender";
ALTER TABLE "Patient".resource_history DROP COLUMN "gender";
DROP TABLE "Patient".search_family;
ALTER TABLE "Binary".resource_data
  ADD CONSTRAINT resource_data_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES "Binary"(id) ON DELETE CASCADE;
-- rows of the table the second step makes anew, which it must not keep
INSERT INTO "Practitioner".search_qualification_period (id, resource_id, value_low, value_high)
  SELECT 99, id, '1900-01-01', '1900-01-02' FROM "Practitioner";
