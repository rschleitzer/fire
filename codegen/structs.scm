(define (struct)
  (let* ((resource-name (name-of (current-node)))
         (table-name (downcase-string (name-of (current-node))))
         (searches (select-children "searches" (current-node)))
         (has-date-search (not (node-list-empty? (node-list-filter
           (lambda (s) (string=? "date" (% "type" s)))
           (children searches))))))
    (file ($ "src/models/" table-name ".rs")
            ($"use chrono::{DateTime,"(if has-date-search " NaiveDate," "")" Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::FromRow;

use super::traits::VersionedResource;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct "resource-name" {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<""Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is, returned without deserialization
}

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct "resource-name"History {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<""Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is

    // History metadata
    pub history_operation: String,
    pub history_timestamp: DateTime<""Utc>,
}

impl VersionedResource for "resource-name" {
    type History = "resource-name"History;

    const RESOURCE_TYPE: &""'static str = \""resource-name"\";
    const TABLE_NAME: &""'static str = \""table-name"\";
    const HISTORY_TABLE_NAME: &""'static str = \""table-name"_history\";

    fn get_id(&""self) -> &""String {
        &""self.id
    }

    fn get_version_id(&""self) -> i32 {
        self.version_id
    }

    fn get_last_updated(&""self) -> &""DateTime<""Utc> {
        &""self.last_updated
    }

    fn get_content(&""self) -> &""Value {
        &""self.content
    }
}

/// Inject id and meta fields into a FHIR "resource-name" resource
/// This is used at storage time to ensure all stored resources have complete id/meta
pub fn inject_id_meta(content: &""Value, id: &""str, version_id: i32, last_updated: &""DateTime<""Utc>) -> Value {
    if let Some(content_obj) = content.as_object() {
        // Create new object with fields in FHIR-standard order
        let mut resource = serde_json::Map::new();

        // 1. resourceType (if present in content)
        if let Some(resource_type) = content_obj.get(\"resourceType\") {
            resource.insert(\"resourceType\".to_string(), resource_type.clone());
        }

        // 2. id (always inject)
        resource.insert(\"id\".to_string(), serde_json::json!(id));

        // 3. meta (always inject)
        resource.insert(\"meta\".to_string(), serde_json::json!({
            \"versionId\": version_id.to_string(),
            \"lastUpdated\": last_updated.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        }));

        // 4. All other fields from content (skip resourceType, id, meta if client sent them)
        for (key, value) in content_obj {
            if key != \"resourceType\" && key != \"id\" && key != \"meta\" {
                resource.insert(key.clone(), value.clone());
            }
        }

        Value::Object(resource)
    } else {
        // Fallback if content is not an object - just return as-is
        content.clone()
    }
}
"(generate-extractors resource-name table-name searches)))))

; Generate the extractor function and SearchParams struct
(define (generate-extractors resource-name table-name searches)
  ($"/// Extract search parameters from FHIR "resource-name" JSON
pub fn extract_"table-name"_search_params(content: &""Value) -> "resource-name"SearchParams {
    let mut params = "resource-name"SearchParams::default();
"(generate-extractor-body searches)"
    params
}

#[derive(Debug, Default)]
pub struct "resource-name"SearchParams {
"(generate-search-params-fields searches)"}
"))

; Get a unique key identifying what FHIR element this search extracts from
; Multiple searches that extract from the same element should have the same key
(define (search-extraction-key search)
  (let ((search-name (% "name" search))
        (search-type (% "type" search)))
    (cond
      ; All HumanName searches extract from "name" element
      ((search-is-humanname? search) "name")
      ; All ContactPoint searches extract from "telecom" element
      ((search-is-contactpoint? search) "telecom")
      ; All Identifier searches extract from "identifier" element
      ((search-is-identifier? search) "identifier")
      ; For other searches, use the search name itself as the key
      (else search-name))))

; Filter searches to keep only first occurrence of each extraction key
(define (deduplicate-searches search-list)
  (let loop ((remaining search-list)
             (seen-keys '())
             (result '()))
    (if (null? remaining)
        (reverse result)
        (let* ((search (car remaining))
               (key (search-extraction-key search)))
          (if (member key seen-keys)
              ; Skip this search, already seen
              (loop (cdr remaining) seen-keys result)
              ; Keep this search, add key to seen
              (loop (cdr remaining) (cons key seen-keys) (cons search result)))))))

; Generate the body of the extractor function
(define (generate-extractor-body searches)
  (if (node-list-empty? searches)
      ""
      (let ((search-list (node-list->list (select-elements (children searches) "search"))))
        (apply $ (map generate-extractor-for-search (deduplicate-searches search-list))))))

; Generate extractor code for a single search parameter
(define (generate-extractor-for-search search)
  (let* ((search-name (% "name" search))
         (search-type (% "type" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (is-humanname (search-is-humanname? search))
         (is-contactpoint (search-is-contactpoint? search)))
    (case search-type
      (("string")
        (if is-humanname
            (generate-humanname-extractor search-name col-name)
            ""))  ; Other string searches not yet implemented
      (("token")
        (if (search-is-simple-code? search)
            (generate-simple-token-extractor search-name col-name search)
            (if (search-is-identifier? search)
                (generate-identifier-extractor search-name col-name)
                (if is-contactpoint
                    (generate-contactpoint-extractor search-name col-name)
                    ""))))  ; Other token searches not yet implemented
      (("date")
        (generate-date-extractor search-name col-name search))
      (("reference")
        (generate-reference-extractor search-name col-name search))
      (else ""))))

; Generate HumanName extractor (name field)
(define (generate-humanname-extractor search-name col-name)
  ($"
    // Extract names
    if let Some(names) = content.get(\"name\").and_then(|n| n.as_array()) {
        for name in names {
            // Extract family
            if let Some(family) = name.get(\"family\").and_then(|f| f.as_str()) {
                params.family_name.push(family.to_string());
            }

            // Extract given names
            if let Some(given) = name.get(\"given\").and_then(|g| g.as_array()) {
                for g in given {
                    if let Some(given_str) = g.as_str() {
                        params.given_name.push(given_str.to_string());
                    }
                }
            }

            // Extract prefix (Dr., Mr., Ms., etc.)
            if let Some(prefix) = name.get(\"prefix\").and_then(|p| p.as_array()) {
                for p in prefix {
                    if let Some(prefix_str) = p.as_str() {
                        params.prefix.push(prefix_str.to_string());
                    }
                }
            }

            // Extract suffix (Jr., Sr., III, etc.)
            if let Some(suffix) = name.get(\"suffix\").and_then(|s| s.as_array()) {
                for s in suffix {
                    if let Some(suffix_str) = s.as_str() {
                        params.suffix.push(suffix_str.to_string());
                    }
                }
            }

            // Extract text (full name as text)
            if let Some(text) = name.get(\"text\").and_then(|t| t.as_str()) {
                params.name_text.push(text.to_string());
            }
        }
    }
"))

; Generate simple token extractor (boolean or code fields)
(define (generate-simple-token-extractor search-name col-name search)
  (let* ((property (search-property search))
         (property-type (if property (% "type" property) "code"))
         (fhir-field (or (search-fhir-field-name search) (camel-case search-name))))
    (case property-type
      (("boolean")
        ($"
    // Extract "search-name"
    if let Some("col-name") = content.get(\""fhir-field"\").and_then(|a| a.as_bool()) {
        params."col-name" = Some("col-name");
    }
"))
      (else ; code
        ($"
    // Extract "search-name"
    if let Some("col-name") = content.get(\""fhir-field"\").and_then(|g| g.as_str()) {
        params."col-name" = Some("col-name".to_string());
    }
")))))

; Generate identifier extractor
(define (generate-identifier-extractor search-name col-name)
  ($"
    // Extract identifiers
    if let Some(identifiers) = content.get(\"identifier\").and_then(|i| i.as_array()) {
        for identifier in identifiers {
            if let Some(system) = identifier.get(\"system\").and_then(|s| s.as_str()) {
                params.identifier_system.push(system.to_string());
            }
            if let Some(value) = identifier.get(\"value\").and_then(|v| v.as_str()) {
                params.identifier_value.push(value.to_string());
            }
        }
    }
"))

; Generate ContactPoint extractor (telecom field)
(define (generate-contactpoint-extractor search-name col-name)
  ($"
    // Extract telecom values (email, phone, etc.)
    if let Some(telecoms) = content.get(\"telecom\").and_then(|t| t.as_array()) {
        for telecom in telecoms {
            if let Some(value) = telecom.get(\"value\").and_then(|v| v.as_str()) {
                params.telecom_value.push(value.to_string());
            }
        }
    }
"))

; Generate date extractor
(define (generate-date-extractor search-name col-name search)
  (let ((fhir-field (or (search-fhir-field-name search) (camel-case search-name))))
    ($"
    // Extract "search-name"
    if let Some("col-name") = content.get(\""fhir-field"\").and_then(|b| b.as_str()) {
        if let Ok(date) = NaiveDate::parse_from_str("col-name", \"%Y-%m-%d\") {
            params."col-name" = Some(date);
        }
    }
")))

; Generate reference extractor
(define (generate-reference-extractor search-name col-name search)
  (let ((fhir-field (or (search-fhir-field-name search) (camel-case search-name)))
        (is-collection (search-is-collection? search)))
    (if is-collection
        ($"
    // Extract "search-name" references
    if let Some(refs) = content.get(\""fhir-field"\").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get(\"reference\").and_then(|r| r.as_str()) {
                params."col-name"_reference.push(reference.to_string());
            }
        }
    }
")
        ($"
    // Extract "search-name" reference
    if let Some(reference) = content
        .get(\""fhir-field"\")
        .and_then(|s| s.get(\"reference\"))
        .and_then(|r| r.as_str())
    {
        params."col-name"_reference = Some(reference.to_string());
    }
"))))

; Generate fields for the SearchParams struct
(define (generate-search-params-fields searches)
  (if (node-list-empty? searches)
      ""
      (let ((search-list (node-list->list (select-elements (children searches) "search"))))
        (apply $ (map generate-search-params-field (deduplicate-searches search-list))))))

; Generate a single field for the SearchParams struct
(define (generate-search-params-field search)
  (let* ((search-name (% "name" search))
         (search-type (% "type" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (is-collection (search-is-collection? search))
         (is-humanname (search-is-humanname? search)))
    (case search-type
      (("string")
        (if is-humanname
            ($"    pub family_name: Vec<""String>,
    pub given_name: Vec<""String>,
    pub prefix: Vec<""String>,
    pub suffix: Vec<""String>,
    pub name_text: Vec<""String>,
")
            ""))  ; Other string searches
      (("token")
        (if (search-is-simple-code? search)
            (let* ((property (search-property search))
                   (property-type (if property (% "type" property) "code")))
              (if (string=? property-type "boolean")
                  ($"    pub "col-name": Option<""bool>,
")
                  ($"    pub "col-name": Option<""String>,
")))
            (if (search-is-identifier? search)
                ($"    pub identifier_system: Vec<""String>,
    pub identifier_value: Vec<""String>,
")
                (if (search-is-contactpoint? search)
                    ($"    pub telecom_value: Vec<""String>,
")
                    ""))))
      (("date")
        ($"    pub "col-name": Option<""NaiveDate>,
"))
      (("reference")
        (if is-collection
            ($"    pub "col-name"_reference: Vec<""String>,
")
            ($"    pub "col-name"_reference: Option<""String>,
")))
      (else ""))))

; Convert hyphen-separated to camelCase for FHIR field names
(define (camel-case str)
  (let ((parts (split-string str #\-)))
    (if (null? parts)
        ""
        (apply $ (cons (car parts)
                      (map first-letter-upcase (cdr parts)))))))

; Helper to split a string by a delimiter
(define (split-string str delim)
  (let loop ((chars (string->list str))
             (current '())
             (result '()))
    (cond
      ((null? chars)
       (reverse (cons (list->string (reverse current)) result)))
      ((char=? (car chars) delim)
       (loop (cdr chars) '() (cons (list->string (reverse current)) result)))
      (else
       (loop (cdr chars) (cons (car chars) current) result)))))
