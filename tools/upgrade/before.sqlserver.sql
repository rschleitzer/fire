-- What the SQL Server database of tools/upgrade-test.sh lacks before its
-- steps: the column and the table the first step makes. (The key the
-- second step drops is PostgreSQL's alone: here a Binary has no data table.)
DROP INDEX ix_Patient_gender ON [Patient];
ALTER TABLE [Patient] DROP COLUMN gender;
ALTER TABLE [Patient].resource_history DROP COLUMN gender;
DROP TABLE [Patient].search_family;
-- rows of the table the second step makes anew, which it must not keep
INSERT INTO [Practitioner].search_qualification_period (id, resource_id, value_low, value_high)
  SELECT 99, id, '1900-01-01', '1900-01-02' FROM [Practitioner];
