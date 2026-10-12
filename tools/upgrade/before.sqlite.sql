-- What the SQLite database of tools/upgrade-test.sh lacks before its steps:
-- the column and the table the first step makes. (The key the second step
-- drops is PostgreSQL's alone: on SQLite a Binary has no data table.)
DROP INDEX IF EXISTS ix_Patient_gender;
ALTER TABLE Patient DROP COLUMN gender;
ALTER TABLE Patient_resource_history DROP COLUMN gender;
DROP TABLE Patient_search_family;
-- rows of the table the second step makes anew, which it must not keep
INSERT INTO Practitioner_search_qualification_period (id, resource_id, value_low, value_high)
  SELECT 99, id, '1900-01-01 00:00:00', '1900-01-02 00:00:00' FROM Practitioner;
