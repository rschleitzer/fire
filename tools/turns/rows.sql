-- Rows for the uri searches of Observation (tools/measure.sh, linked).
INSERT INTO "Observation".search_value_canonical (id, resource_id, value) VALUES
  (0, 'o1', 'http://example.org/fhir/ValueSet/vital-signs'),
  (0, 'o2', 'http://example.org/fhir/ValueSet/vital-signs/heart'),
  (1, 'o2', 'urn:oid:1.2.3.4'),
  (0, 'o3', 'http://example.org/fhir/ValueSet'),
  (0, 'o4', 'http://example.org/fhir_ValueSet/x'),
  (0, 'o5', 'HTTP://EXAMPLE.ORG/fhir/ValueSet/vital-signs');
INSERT INTO "Observation".search_component_value_canonical (id, resource_id, value) VALUES
  (0, 'o5', 'http://example.org/c/1'), (1, 'o5', 'http://example.org/c/2');
