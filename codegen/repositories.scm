; Repository generator for Fire FHIR Server
; Generates CRUD and search methods for each resource

; Get the database column name for a search parameter (for ORDER BY clauses)
(define (search-param-to-column-name search)
  (let* ((search-name (% "name" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (search-type (% "type" search))
         (is-collection (search-is-collection? search)))
    (case search-type
      (("string" "uri")
       ; String/URI searches use _name suffix for trgm index
       ($ col-name "_name"))
      (("token")
       ; Token searches - check if simple code or system/code
       (if (or (search-is-simple-code? search) (search-is-identifier? search))
           (if (search-is-identifier? search)
               ($ col-name "_value")  ; identifier uses _value
               col-name)              ; simple code uses param name
           ($ col-name "_code")))     ; complex token uses _code
      (("reference")
       ; Reference searches use _reference suffix
       ($ col-name "_reference"))
      (("date")
       ; Date searches - check for datetime vs date
       (let ((property (search-property search)))
         (if (and property (property-has-variants? property))
             (let* ((variants (property-variants property))
                    (variant-list (node-list->list variants))
                    (variant-types (map (lambda (v) (% "type" v)) variant-list))
                    (has-datetime (member "dateTime" variant-types)))
               (if has-datetime
                   ($ col-name "_datetime")
                   ($ col-name "_date")))
             col-name)))
      (("quantity")
       ; Quantity searches use param name directly
       col-name)
      (("number")
       ; Number searches use param name directly
       col-name)
      (else col-name))))

; Generate repository for current resource (called from rules.scm for each resource)
(define (repository)
  (let ((resource (current-node)))
    (generate-repository-for-resource resource)))

; Generate a complete repository file for a resource
(define (generate-repository-for-resource resource)
  (let* ((resource-name (name-of resource))
         (resource-lower (downcase-string resource-name))
         (table-name resource-lower)
         (struct-name ($ resource-name "Repository"))
         (searches-node (select-children "searches" resource))
         ; Get search parameters and deduplicate same as structs.scm
         (search-list (if (not (node-list-empty? searches-node))
                          (node-list->list (select-elements (children searches-node) "search"))
                          '()))
         (unique-searches (deduplicate-searches search-list)))

    (file ($ "src/repository/" resource-lower ".rs")
      ($"use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::"resource-lower"::{extract_"resource-lower"_search_params, "resource-name", "resource-name"History};
"(generate-cross-resource-imports resource-name)"use crate::search::{SearchParam, SearchQuery, SortDirection};
use crate::services::validate_"resource-lower";

pub struct "struct-name" {
    pool: PgPool,
}

impl "struct-name" {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

"(generate-create-method resource-name resource-lower unique-searches)"
"(generate-read-method resource-name resource-lower)"
"(generate-upsert-method resource-name resource-lower unique-searches)"
"(generate-update-method resource-name resource-lower unique-searches)"
"(generate-delete-method resource-name resource-lower unique-searches)"
"(generate-history-method resource-name resource-lower)"
"(generate-read-version-method resource-name resource-lower)"
"(generate-rollback-method resource-name resource-lower unique-searches)"
"(generate-type-history-method resource-name resource-lower)"
"(generate-purge-method resource-name resource-lower)"
"(generate-search-and-helpers resource-name resource-lower)"
}
"))))

; Note: search-extraction-key and deduplicate-searches are defined in structs.scm

; Generate cross-resource imports (for helper methods)
(define (generate-cross-resource-imports resource-name)
  (case resource-name
    (("Patient") "use crate::models::observation::Observation;
")
    (("Observation") "use crate::models::patient::Patient;
")
    (("Practitioner") "use crate::models::patient::Patient;
")
    (else "")))

; Helper: Get column names for INSERT statement (without id, version_id, last_updated, content)
; searches is a Scheme list (not node-list)
(define (get-insert-columns searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (let ((col-name (camel-to-snake (string-replace (% "name" search) "-" "_"))))
                        (get-insert-columns-for-search search col-name)))
                    searches))))

(define (get-insert-columns-for-search search col-name)
  (let ((search-name (% "name" search))
        (search-type (% "type" search))
        (is-collection (search-is-collection? search)))
    ; Skip special FHIR parameters that start with underscore
    (if (and (> (string-length search-name) 0)
             (char=? (string-ref search-name 0) #\_))
        ""
        (case search-type
      (("composite" "special") "")
      (("token")
       (cond
         ((search-is-simple-code? search) ($",
                "col-name""))
         ((search-is-identifier? search) ($",
                identifier_system, identifier_value"))
         ((search-is-contactpoint? search)
          ; ContactPoint: only telecom_value
          ($",
                telecom_value"))
         ((search-has-codeableconcept-variant? search)
          ($",
                "col-name"_code"))
         ((search-is-codeableconcept? search)
          (if is-collection
              ($",
                "col-name"_system, "col-name"_code")
              ($",
                "col-name"_system, "col-name"_code")))
         (else "")))
      (("string")
       (cond
         ; HumanName search: generates component fields
         ((search-is-humanname? search)
          ",
                family_name, given_name, prefix, suffix, name_text")
         ; All other string searches: skip (not in SearchParams)
         (else "")))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             ($ (if (member "dateTime" variant-types)
                    ($",
                "col-name"_datetime")
                    "")
                (if (member "element" variant-types)
                    ($",
                "col-name"_period_start, "col-name"_period_end")
                    "")))
           ($",
                "col-name"")))
      (("reference")
       ($",
                "col-name"_reference"))
      (("quantity")
       ($",
                "col-name"_value, "col-name"_unit, "col-name"_system"))
      (else "")))))

; Helper: Get placeholders for INSERT VALUES ($5, $6, ...)
; searches is a Scheme list
(define (get-insert-placeholders searches)
  (let ((count (count-insert-params searches 0)))
    (let loop ((i 5) (result ""))
      (if (> i (+ 4 count))
          result
          (loop (+ i 1) ($ result ", $" (number->string i)))))))

; Helper: Count number of parameters for a Scheme list
(define (count-insert-params searches current)
  (if (null? searches)
      current
      (count-insert-params
        (cdr searches)
        (+ current (count-params-for-search (car searches))))))

(define (count-params-for-search search)
  (let ((search-name (% "name" search))
        (search-type (% "type" search)))
    ; Skip special FHIR parameters that start with underscore
    (if (and (> (string-length search-name) 0)
             (char=? (string-ref search-name 0) #\_))
        0
        (case search-type
      (("composite" "special") 0)
      (("token")
       (cond
         ((search-is-simple-code? search) 1)
         ((search-is-identifier? search) 2)
         ((search-is-contactpoint? search) 1)  ; only telecom_value
         ((search-has-codeableconcept-variant? search) 1)
         ((search-is-codeableconcept? search) 2)  ; system and code
         (else 0)))
      (("string")
       (if (search-is-humanname? search)
           5  ; family_name, given_name, prefix, suffix, name_text
           0  ; other string searches not in SearchParams
       ))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             (+ (if (member "dateTime" variant-types) 1 0)
                (if (member "element" variant-types) 2 0)))
           1))
      (("reference") 1)
      (("quantity") 3)
      (else 0)))))

; Helper: Get single parameter binding for a search
(define (get-param-binding-for-search search params-var)
  (let* ((search-name (% "name" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (search-type (% "type" search))
         (is-collection (search-is-collection? search))
         (property (search-property search)))
    ; Skip special FHIR parameters that start with underscore
    (if (and (> (string-length search-name) 0)
             (char=? (string-ref search-name 0) #\_))
        ""
        (case search-type
      (("composite" "special") "")
      (("token")
       (cond
         ((search-is-simple-code? search)
          (let ((prop-type (if property (% "type" property) "code")))
            (if (string=? prop-type "boolean")
                ($",
            "params-var"."col-name"")
                ($",
            "params-var"."col-name""))))
         ((search-is-identifier? search)
          ($",
            "params-var"
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".identifier_system[..])),
            "params-var"
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".identifier_value[..]))"))
         ((search-is-contactpoint? search)
          ; ContactPoint: only telecom_value
          ($",
            "params-var"
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".telecom_value[..]))"))
         ((search-has-codeableconcept-variant? search)
          ($",
            "params-var"."col-name"_code"))
         ((search-is-codeableconcept? search)
          (if is-collection
              ($",
            "params-var"
                ."col-name"_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var"."col-name"_system[..])),
            "params-var"
                ."col-name"_code
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var"."col-name"_code[..]))")
              ($",
            "params-var"."col-name"_system,
            "params-var"."col-name"_code")))
         (else "")))
      (("string")
       (cond
         ; HumanName: use component fields
         ((search-is-humanname? search)
          ($",
            "params-var"
                .family_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".family_name[..])),
            "params-var"
                .given_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".given_name[..])),
            "params-var"
                .prefix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".prefix[..])),
            "params-var"
                .suffix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".suffix[..])),
            "params-var"
                .name_text
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".name_text[..]))"))
         ; All other string searches: skip (not in SearchParams)
         (else "")))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             ($ (if (member "dateTime" variant-types)
                    ($",
            "params-var"."col-name"_datetime")
                    "")
                (if (member "element" variant-types)
                    ($",
            "params-var"."col-name"_period_start,
            "params-var"."col-name"_period_end")
                    "")))
           ($",
            "params-var"."col-name"")))
      (("reference")
       (if is-collection
           ($",
            "params-var"
                ."col-name"_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var"."col-name"_reference[..]))")
           ($",
            "params-var"."col-name"_reference")))
      (("quantity")
       ($",
            "params-var"."col-name"_value,
            "params-var"."col-name"_unit,
            "params-var"."col-name"_system"))
      (else "")))))

; Helper: Get parameter bindings for INSERT/UPDATE (calls get-param-binding-for-search above)
; searches is a Scheme list
(define (get-param-bindings searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (get-param-binding-for-search search "params"))
                    searches))))

; Helper: Get UPDATE SET assignments with proper parameter numbering
; searches is a Scheme list
(define (get-update-assignments searches)
  (let ((result (get-update-assignments-with-counter searches 5)))
    (car result)))  ; Return just the SQL string, discard final counter

; Returns (sql-string . next-param-number)
(define (get-update-assignments-with-counter searches param-num)
  (if (null? searches)
      (cons "" param-num)
      (let* ((search (car searches))
             (rest (cdr searches))
             (assignment-result (get-update-assignment-for-search-with-counter search param-num))
             (assignment-sql (car assignment-result))
             (next-param (cdr assignment-result))
             (rest-result (get-update-assignments-with-counter rest next-param))
             (rest-sql (car rest-result))
             (final-param (cdr rest-result)))
        (cons ($ assignment-sql rest-sql) final-param))))

; Returns (sql-string . next-param-number)
(define (get-update-assignment-for-search-with-counter search param-num)
  (let* ((search-name (% "name" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (search-type (% "type" search)))
    ; Skip special FHIR parameters that start with underscore
    (if (and (> (string-length search-name) 0)
             (char=? (string-ref search-name 0) #\_))
        (cons "" param-num)
        (case search-type
      (("composite" "special") (cons "" param-num))
      (("token")
       (cond
         ((search-is-simple-code? search)
          (cons ($",
                    "col-name" = $"(number->string param-num)"")
                (+ param-num 1)))
         ((search-is-identifier? search)
          (cons ($",
                    identifier_system = $"(number->string param-num)",
                    identifier_value = $"(number->string (+ param-num 1))"")
                (+ param-num 2)))
         ((search-is-contactpoint? search)
          (cons ($",
                    telecom_value = $"(number->string param-num)"")
                (+ param-num 1)))
         ((search-has-codeableconcept-variant? search)
          (cons ($",
                    "col-name"_code = $"(number->string param-num)"")
                (+ param-num 1)))
         ((search-is-codeableconcept? search)
          (cons ($",
                    "col-name"_system = $"(number->string param-num)",
                    "col-name"_code = $"(number->string (+ param-num 1))"")
                (+ param-num 2)))
         (else (cons "" param-num))))
      (("string")
       (cond
         ((search-is-humanname? search)
          (cons ($",
                    family_name = $"(number->string param-num)",
                    given_name = $"(number->string (+ param-num 1))",
                    prefix = $"(number->string (+ param-num 2))",
                    suffix = $"(number->string (+ param-num 3))",
                    name_text = $"(number->string (+ param-num 4))"")
                (+ param-num 5)))
         (else (cons "" param-num))))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             (let ((has-datetime (member "dateTime" variant-types))
                   (has-period (member "element" variant-types)))
               (cond
                 ((and has-datetime has-period)
                  (cons ($",
                    "col-name"_datetime = $"(number->string param-num)",
                    "col-name"_period_start = $"(number->string (+ param-num 1))",
                    "col-name"_period_end = $"(number->string (+ param-num 2))"")
                        (+ param-num 3)))
                 (has-datetime
                  (cons ($",
                    "col-name"_datetime = $"(number->string param-num)"")
                        (+ param-num 1)))
                 (has-period
                  (cons ($",
                    "col-name"_period_start = $"(number->string param-num)",
                    "col-name"_period_end = $"(number->string (+ param-num 1))"")
                        (+ param-num 2)))
                 (else (cons "" param-num)))))
           (cons ($",
                    "col-name" = $"(number->string param-num)"")
                 (+ param-num 1))))
      (("reference")
       (cons ($",
                    "col-name"_reference = $"(number->string param-num)"")
             (+ param-num 1)))
      (("quantity")
       (cons ($",
                    "col-name"_value = $"(number->string param-num)",
                    "col-name"_unit = $"(number->string (+ param-num 1))",
                    "col-name"_system = $"(number->string (+ param-num 2))"")
             (+ param-num 3)))
      (else (cons "" param-num))))))

; Helpers for history table (reuse existing functions)
(define (get-history-insert-columns searches)
  (get-insert-columns searches))

(define (get-history-insert-placeholders searches)
  (get-insert-placeholders searches))

(define (get-history-param-bindings searches params-var)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (get-param-binding-for-search search params-var))
                    searches))))

; Generate CREATE method
(define (generate-create-method resource-name resource-lower searches)
  (let ((columns (get-insert-columns searches))
        (placeholders (get-insert-placeholders searches))
        (param-bindings (get-param-bindings searches)))
    ($"    /// Create a new "resource-lower" resource (version 1)
    pub async fn create(&""self, mut content: Value) -> Result<"resource-name"> {
        tracing::debug!(\"Creating new "resource-lower" resource\");

        // Validate resource
        validate_"resource-lower"(&""content)?;

        let id = Uuid::new_v4().to_string();
        let version_id = 1;
        let last_updated = Utc::now();

        tracing::info!("resource-lower"_id = %id, \"Creating "resource-lower"\");

        // Inject id and meta fields into content before storing
        content = crate::models::"resource-lower"::inject_id_meta(&""content, &""id, version_id, &""last_updated);

        // Extract search parameters from complete content
        let params = extract_"resource-lower"_search_params(&""content);

        let mut tx = self.pool.begin().await?;

        // Insert into current table
        sqlx::query!(
            r#\"
            INSERT INTO "resource-lower" (
                id, version_id, last_updated, content"columns"
            )
            VALUES ($1, $2, $3, $4"placeholders")
            \"#,
            id,
            version_id,
            last_updated,
            content"param-bindings"
        )
        .execute(&""mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created "resource-lower"
        self.read(&""id).await
    }

")))

; Generate READ method
(define (generate-read-method resource-name resource-lower)
  ($"    /// Read current version of a "resource-lower" (returns raw JSON)
    pub async fn read(&""self, id: &""str) -> Result<"resource-name"> {
        let "resource-lower" = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1
            \"#,
            id
        )
        .fetch_optional(&""self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok("resource-lower")
    }

"))

; Generate UPSERT method
(define (generate-upsert-method resource-name resource-lower searches)
  (let ((columns (get-insert-columns searches))
        (placeholders (get-insert-placeholders searches))
        (param-bindings (get-param-bindings searches))
        (update-assignments (get-update-assignments searches))
        (history-columns (get-history-insert-columns searches))
        (history-placeholders (get-history-insert-placeholders searches))
        (history-param-bindings (get-history-param-bindings searches "old_params")))
    ($"    /// Upsert a "resource-lower" resource with specific ID (FHIR-compliant PUT)
    /// Creates with client-specified ID if doesn't exist, updates if exists
    pub async fn upsert(&""self, id: &""str, mut content: Value) -> Result<"resource-name"> {
        // Validate resource
        validate_"resource-lower"(&""content)?;

        let mut tx = self.pool.begin().await?;

        // Check if resource exists
        let existing = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1
            FOR UPDATE
            \"#,
            id
        )
        .fetch_optional(&""mut *tx)
        .await?;

        if let Some(old_"resource-lower") = existing {
            // Resource exists - perform update
            let new_version_id = old_"resource-lower".version_id + 1;
            let last_updated = Utc::now();

            tracing::info!("resource-lower"_id = %id, old_version = old_"resource-lower".version_id, new_version = new_version_id, \"Updating existing "resource-lower"\");

            // Clean up any orphaned history records
            let deleted_rows = sqlx::query!(
                r#\"
                DELETE FROM "resource-lower"_history
                WHERE id = $1 AND version_id >= $2
                \"#,
                id,
                old_"resource-lower".version_id
            )
            .execute(&""mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::warn!("resource-lower"_id = %id, deleted_versions = deleted_rows.rows_affected(), \"Cleaned up orphaned history records before update\");
            }

            // Inject id and meta into content before storing
            content = crate::models::"resource-lower"::inject_id_meta(&""content, id, new_version_id, &""last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_"resource-lower"_search_params(&""old_"resource-lower".content);

            // Insert OLD version into history before updating
            sqlx::query!(
                r#\"
                INSERT INTO "resource-lower"_history (
                    id, version_id, last_updated, content"history-columns",
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4"history-placeholders", $"(number->string (+ 5 (count-insert-params searches 0)))", $"(number->string (+ 6 (count-insert-params searches 0)))")
                \"#,
                old_"resource-lower".id,
                old_"resource-lower".version_id,
                old_"resource-lower".last_updated,
                &""old_"resource-lower".content"history-param-bindings",
                if old_"resource-lower".version_id == 1 {
                    \"CREATE\"
                } else {
                    \"UPDATE\"
                },
                Utc::now(),
            )
            .execute(&""mut *tx)
            .await?;

            // Extract search parameters for new content
            let params = extract_"resource-lower"_search_params(&""content);

            // Update current table with new version
            sqlx::query!(
                r#\"
                UPDATE "resource-lower"
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4"update-assignments"
                WHERE id = $1
                \"#,
                id,
                new_version_id,
                last_updated,
                content"param-bindings"
            )
            .execute(&""mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return updated "resource-lower"
            self.read(id).await
        } else {
            // Resource doesn't exist - perform create with specified ID
            let version_id = 1;
            let last_updated = Utc::now();

            tracing::info!("resource-lower"_id = %id, \"Creating "resource-lower" with client-specified ID\");

            // Clean up any orphaned history records for this ID
            let deleted_rows = sqlx::query!(
                r#\"
                DELETE FROM "resource-lower"_history
                WHERE id = $1
                \"#,
                id
            )
            .execute(&""mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::info!("resource-lower"_id = %id, orphaned_history_records = deleted_rows.rows_affected(), \"Cleaned up orphaned history records before creating new resource\");
            }

            // Inject id and meta into content before storing
            content = crate::models::"resource-lower"::inject_id_meta(&""content, id, version_id, &""last_updated);

            // Extract search parameters
            let params = extract_"resource-lower"_search_params(&""content);

            // Insert into current table
            sqlx::query!(
                r#\"
                INSERT INTO "resource-lower" (
                    id, version_id, last_updated, content"columns"
                )
                VALUES ($1, $2, $3, $4"placeholders")
                \"#,
                id,
                version_id,
                last_updated,
                content"param-bindings"
            )
            .execute(&""mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return created "resource-lower"
            self.read(id).await
        }
    }

")))

; Generate UPDATE method (similar to upsert but requires resource to exist)
(define (generate-update-method resource-name resource-lower searches)
  (let ((update-assignments (get-update-assignments searches))
        (param-bindings (get-param-bindings searches))
        (history-columns (get-history-insert-columns searches))
        (history-placeholders (get-history-insert-placeholders searches))
        (history-param-bindings (get-history-param-bindings searches "old_params")))
    ($"    /// Update an existing "resource-lower" resource
    pub async fn update(&""self, id: &""str, mut content: Value) -> Result<"resource-name"> {
        // Validate resource
        validate_"resource-lower"(&""content)?;

        let mut tx = self.pool.begin().await?;

        // Get existing resource
        let old_"resource-lower" = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1
            FOR UPDATE
            \"#,
            id
        )
        .fetch_optional(&""mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = old_"resource-lower".version_id + 1;
        let last_updated = Utc::now();

        tracing::info!("resource-lower"_id = %id, old_version = old_"resource-lower".version_id, new_version = new_version_id, \"Updating "resource-lower"\");

        // Clean up any orphaned history records
        let deleted_rows = sqlx::query!(
            r#\"
            DELETE FROM "resource-lower"_history
            WHERE id = $1 AND version_id >= $2
            \"#,
            id,
            old_"resource-lower".version_id
        )
        .execute(&""mut *tx)
        .await?;

        if deleted_rows.rows_affected() > 0 {
            tracing::warn!("resource-lower"_id = %id, deleted_versions = deleted_rows.rows_affected(), \"Cleaned up orphaned history records before update\");
        }

        // Inject id and meta into content
        content = crate::models::"resource-lower"::inject_id_meta(&""content, id, new_version_id, &""last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_"resource-lower"_search_params(&""old_"resource-lower".content);

        // Insert OLD version into history
        sqlx::query!(
            r#\"
            INSERT INTO "resource-lower"_history (
                id, version_id, last_updated, content"history-columns",
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4"history-placeholders", $"(number->string (+ 5 (count-insert-params searches 0)))", $"(number->string (+ 6 (count-insert-params searches 0)))")
            \"#,
            old_"resource-lower".id,
            old_"resource-lower".version_id,
            old_"resource-lower".last_updated,
            &""old_"resource-lower".content"history-param-bindings",
            if old_"resource-lower".version_id == 1 {
                \"CREATE\"
            } else {
                \"UPDATE\"
            },
            Utc::now(),
        )
        .execute(&""mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_"resource-lower"_search_params(&""content);

        // Update current table
        sqlx::query!(
            r#\"
            UPDATE "resource-lower"
            SET
                version_id = $2,
                last_updated = $3,
                content = $4"update-assignments"
            WHERE id = $1
            \"#,
            id,
            new_version_id,
            last_updated,
            content"param-bindings"
        )
        .execute(&""mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated "resource-lower"
        self.read(id).await
    }

")))

; Generate DELETE method
(define (generate-delete-method resource-name resource-lower searches)
  (let ((history-columns (get-history-insert-columns searches))
        (history-placeholders (get-history-insert-placeholders searches))
        (history-param-bindings (get-history-param-bindings searches "params")))
    ($"    /// Delete a "resource-lower" resource
    pub async fn delete(&""self, id: &""str) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current "resource-lower" (to store in history)
        let "resource-lower" = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1
            FOR UPDATE
            \"#,
            id
        )
        .fetch_optional(&""mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = "resource-lower".version_id + 1;
        let last_updated = Utc::now();

        // Extract search params from content for history
        let params = extract_"resource-lower"_search_params(&"resource-lower".content);

        // Delete from current table
        sqlx::query!(
            r#\"
            DELETE FROM "resource-lower"
            WHERE id = $1
            \"#,
            id,
        )
        .execute(&""mut *tx)
        .await?;

        // Insert delete record into history
        sqlx::query!(
            r#\"
            INSERT INTO "resource-lower"_history (
                id, version_id, last_updated, content"history-columns",
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4"history-placeholders", $"(number->string (+ 5 (count-insert-params searches 0)))", $"(number->string (+ 6 (count-insert-params searches 0)))")
            \"#,
            id,
            new_version_id,
            last_updated,
            &"resource-lower".content"history-param-bindings",
            \"DELETE\",
            Utc::now(),
        )
        .execute(&""mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }

")))

; Generate HISTORY method
(define (generate-history-method resource-name resource-lower)
  ($"    /// Get all versions of a "resource-lower" from history
    pub async fn history(&""self, id: &""str, count: Option<""i64>) -> Result<""Vec<"resource-name"History>> {
        let limit = count.unwrap_or(50);

        // Get current version
        let current = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1
            \"#,
            id
        )
        .fetch_optional(&""self.pool)
        .await?;

        // Get historical versions
        let mut history = sqlx::query_as!(
            "resource-name"History,
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\",
                history_operation, history_timestamp
            FROM "resource-lower"_history
            WHERE id = $1
            ORDER BY version_id DESC
            LIMIT $2
            \"#,
            id,
            limit
        )
        .fetch_all(&""self.pool)
        .await?;

        // Prepend current version if it exists
        if let Some(curr) = current {
            history.insert(0, "resource-name"History {
                id: curr.id,
                version_id: curr.version_id,
                last_updated: curr.last_updated,
                content: curr.content,
                history_operation: \"UPDATE\".to_string(),
                history_timestamp: curr.last_updated,
            });
        }

        // Apply limit
        history.truncate(limit as usize);

        Ok(history)
    }

"))

; Generate READ_VERSION method
(define (generate-read-version-method resource-name resource-lower)
  ($"    /// Read a specific version of a "resource-lower" from history
    pub async fn read_version(&""self, id: &""str, version_id: i32) -> Result<"resource-name"History> {
        // Try history table first
        if let Some(history) = sqlx::query_as!(
            "resource-name"History,
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\",
                history_operation, history_timestamp
            FROM "resource-lower"_history
            WHERE id = $1 AND version_id = $2
            \"#,
            id,
            version_id
        )
        .fetch_optional(&""self.pool)
        .await? {
            return Ok(history);
        }

        // If not in history, check if it's the current version
        if let Some(current) = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            WHERE id = $1 AND version_id = $2
            \"#,
            id,
            version_id
        )
        .fetch_optional(&""self.pool)
        .await? {
            // Convert current version to history format
            return Ok("resource-name"History {
                id: current.id,
                version_id: current.version_id,
                last_updated: current.last_updated,
                content: current.content,
                history_operation: \"UPDATE\".to_string(),
                history_timestamp: current.last_updated,
            });
        }

        Err(FhirError::NotFound)
    }
"))

; Generate ROLLBACK method
(define (generate-rollback-method resource-name resource-lower searches)
  (let ((update-assignments (get-update-assignments searches))
        (param-bindings (get-param-bindings searches)))
    ($"    /// Rollback a "resource-lower" to a specific version (deletes all versions >= rollback_to_version)
    pub async fn rollback(&""self, id: &""str, rollback_to_version: i32) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get all version IDs for this "resource-lower" from history
        let version_ids: Vec<""i32> = sqlx::query_scalar!(
            r#\"
            SELECT version_id
            FROM "resource-lower"_history
            WHERE id = $1
            ORDER BY version_id ASC
            \"#,
            id
        )
        .fetch_all(&""mut *tx)
        .await?;

        // Check if current resource exists and add its version
        let current_version = sqlx::query_scalar!(
            r#\"
            SELECT version_id
            FROM "resource-lower"
            WHERE id = $1
            \"#,
            id
        )
        .fetch_optional(&""mut *tx)
        .await?;

        let mut all_versions = version_ids;
        if let Some(cv) = current_version {
            if !all_versions.contains(&""cv) {
                all_versions.push(cv);
            }
        }
        all_versions.sort();

        // Find versions to delete (>= rollback_to_version)
        let versions_to_delete: Vec<""i32> = all_versions
            .iter()
            .filter(|&""&""v| v >= rollback_to_version)
            .copied()
            .collect();

        // Find highest version that remains (<"" rollback_to_version)
        let new_current_version = all_versions
            .iter()
            .filter(|&""&""v| v <"" rollback_to_version)
            .max()
            .copied();

        if versions_to_delete.is_empty() {
            return Err(FhirError::BadRequest(format!(
                \"No versions to rollback for "resource-lower" {}\",
                id
            )));
        }

        tracing::info!(
            "resource-lower"_id = %id,
            rollback_to = rollback_to_version,
            deleting_versions = ?versions_to_delete,
            new_current = ?new_current_version,
            \"Rolling back "resource-lower"\"
        );

        // Delete versions from history
        for version in &""versions_to_delete {
            sqlx::query!(
                r#\"
                DELETE FROM "resource-lower"_history
                WHERE id = $1 AND version_id = $2
                \"#,
                id,
                version
            )
            .execute(&""mut *tx)
            .await?;
        }

        // Update or delete current resource
        if let Some(new_version) = new_current_version {
            // Restore the previous version as current
            let restored = self.read_version(id, new_version).await?;

            let params = extract_"resource-lower"_search_params(&""restored.content);

            sqlx::query!(
                r#\"
                UPDATE "resource-lower"
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4"update-assignments"
                WHERE id = $1
                \"#,
                id,
                new_version,
                restored.last_updated,
                &""restored.content"param-bindings"
            )
            .execute(&""mut *tx)
            .await?;
        } else {
            // No versions remain, delete current resource
            sqlx::query!(
                r#\"
                DELETE FROM "resource-lower"
                WHERE id = $1
                \"#,
                id
            )
            .execute(&""mut *tx)
            .await?;
        }

        tx.commit().await?;

        Ok(())
    }

")))

; Generate TYPE_HISTORY method
(define (generate-type-history-method resource-name resource-lower)
  ($"    /// Get type-level history - all versions of all "resource-lower"s
    pub async fn type_history(&""self, count: Option<""i64>) -> Result<""Vec<"resource-name"History>> {
        // Query all history records with optional limit
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                "resource-name"History,
                r#\"
                SELECT
                    id, version_id, last_updated,
                    content as \"content: Value\",
                    history_operation, history_timestamp
                FROM "resource-lower"_history
                ORDER BY history_timestamp DESC
                LIMIT $1
                \"#,
                limit
            )
            .fetch_all(&""self.pool)
            .await?
        } else {
            sqlx::query_as!(
                "resource-name"History,
                r#\"
                SELECT
                    id, version_id, last_updated,
                    content as \"content: Value\",
                    history_operation, history_timestamp
                FROM "resource-lower"_history
                ORDER BY history_timestamp DESC
                \"#
            )
            .fetch_all(&""self.pool)
            .await?
        };

        // Also get all current versions
        let current_resources = sqlx::query_as!(
            "resource-name",
            r#\"
            SELECT
                id, version_id, last_updated,
                content as \"content: Value\"
            FROM "resource-lower"
            \"#
        )
        .fetch_all(&""self.pool)
        .await?;

        // Add current versions to history if not already present and not at limit
        for current in current_resources {
            // Check if we're at the limit
            let at_limit = count.map(|c| history.len() >= c as usize).unwrap_or(false);

            if at_limit {
                break;
            }

            let current_version_exists = history
                .iter()
                .any(|h| h.id == current.id &""&"" h.version_id == current.version_id);

            if !current_version_exists {
                let current_as_history = "resource-name"History {
                    id: current.id,
                    version_id: current.version_id,
                    last_updated: current.last_updated,
                    content: current.content,
                    history_operation: if current.version_id == 1 {
                        \"CREATE\".to_string()
                    } else {
                        \"UPDATE\".to_string()
                    },
                    history_timestamp: current.last_updated,
                };
                history.push(current_as_history);
            }
        }

        // Sort by timestamp descending
        history.sort_by(|a, b| b.history_timestamp.cmp(&""a.history_timestamp));

        // Apply final truncation if needed (after merging and sorting)
        if let Some(limit) = count {
            history.truncate(limit as usize);
        }

        Ok(history)
    }

"))

; Generate PURGE method
(define (generate-purge-method resource-name resource-lower)
  ($"    /// Purge all "resource-lower" history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) "resource-lower" records
    pub async fn purge(&""self) -> Result<()> {
        // Delete all history records for test isolation
        sqlx::query!(\"DELETE FROM "resource-lower"_history\")
            .execute(&""self.pool)
            .await?;

        Ok(())
    }
"))

; Replace the search generator functions in repositories.scm
; This generates search() using the new SearchParam architecture

; Generate SEARCH method and helpers (using new SearchParam architecture)
(define (generate-search-and-helpers resource-name resource-lower)
  (let* ((resource (current-node))
         (searches-node (select-children "searches" resource))
         (search-list (if (not (node-list-empty? searches-node))
                          (node-list->list (select-elements (children searches-node) "search"))
                          '()))
         (unique-searches (deduplicate-searches search-list))
         (all-searches search-list))  ; Keep all searches for generating match arms
    ($ ($"
    /// Search for "resource-lower"s based on FHIR search parameters
    pub async fn search(
        &""self,
        params: &""HashMap<""String, String>,
        include_total: bool,
    ) -> Result<""(Vec<"resource-name">, Option<""i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#\"SELECT "resource-lower".id, "resource-lower".version_id, "resource-lower".last_updated, "resource-lower".content
               FROM "resource-lower" WHERE 1=1\"#,
        );

        let mut bind_values: Vec<""String> = Vec::new();

        // Group parameters by name for OR logic
        let mut param_groups: HashMap<""String, Vec<""&""SearchParam>> = HashMap::new();
        for param in &""query.params {
            param_groups.entry(param.name.clone()).or_insert_with(Vec::new).push(param);
        }

        // Build WHERE clause from grouped search parameters
        for (param_name, param_list) in &""param_groups {
            // Start OR group if multiple values
            let needs_or_group = param_list.len() > 1;

            // Track SQL length before processing to detect if condition was added
            let sql_before_len = sql.len();

            // Tentatively add AND - we'll revert if no condition generated
            sql.push_str(\" AND \");

            if needs_or_group {
                sql.push_str(\"(\");
            }

            // Process each param in the group
            for (param_idx, param) in param_list.iter().enumerate() {
                // Add OR between params in same group
                if needs_or_group &""&"" param_idx > 0 {
                    sql.push_str(\" OR \");
                }

                // Generate condition based on parameter name
                match param.name.as_str() {
")
       (generate-search-param-matches-or resource-name resource-lower all-searches)
      ($"                    \"_has\" => {
                        // Reverse chaining: _has:ResourceType:ref_field:param=value
                        // modifier contains: ResourceType:ref_field:param
                        if let Some(modifier_str) = &""param.modifier {
                            let parts: Vec<&""str> = modifier_str.split(':').collect();
                            if parts.len() == 3 {
                                let target_resource_type = parts[0];
                                let target_table = target_resource_type.to_lowercase();
                                // Convert hyphens to underscores for SQL column names
                                let ref_field = parts[1].replace('-', \"_\");
                                let filter_param = parts[2];  // e.g., \"code\"

                                // Generate reverse chain EXISTS query
                                // This finds resources of the current type that are referenced by target_resource_type
                                // Example: Find Patients that have Observations where subject points to the Patient AND code=X
                                let bind_idx = bind_values.len() + 1;

                                // Determine if the reference field is a collection (array) based on common FHIR patterns
                                // Collections use = ANY(), scalars use simple =
                                let is_collection = matches!(ref_field.as_str(),
                                    \"general_practitioner\" | \"performer\" | \"participant\" |
                                    \"basedOn\" | \"partOf\" | \"reasonReference\" | \"insurance\" |
                                    \"supportingInfo\" | \"diagnosis\" | \"procedure\" | \"account\"
                                );

                                // Build filter condition based on parameter type
                                // For now, handle common cases: token (code, category)
                                if filter_param == \"code\" || filter_param == \"category\" {
                                    // Token search on target resource (scalar columns, not arrays)
                                    let col_name = if filter_param == \"code\" { \"code_code\" } else { \"category_code\" };

                                    if is_collection {
                                        // Use = ANY() for array reference columns
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE CONCAT('"resource-name"/', "resource-lower".id) = ANY({}.{}_reference) AND {}.{} = ${})\",
                                            target_table, target_table, ref_field, target_table, col_name, bind_idx
                                        ));
                                    } else {
                                        // Use simple = for scalar reference columns
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.{}_reference = CONCAT('"resource-name"/', "resource-lower".id) AND {}.{} = ${})\",
                                            target_table, target_table, ref_field, target_table, col_name, bind_idx
                                        ));
                                    }
                                    bind_values.push(param.value.clone());
                                } else if filter_param == \"family\" {
                                    // String search on target resource
                                    if is_collection {
                                        // Use = ANY() for array reference columns
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE CONCAT('"resource-name"/', "resource-lower".id) = ANY({}.{}_reference) AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, ref_field, target_table, bind_idx
                                        ));
                                    } else {
                                        // Use simple = for scalar reference columns
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.{}_reference = CONCAT('"resource-name"/', "resource-lower".id) AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, ref_field, target_table, bind_idx
                                        ));
                                    }
                                    bind_values.push(format!(\"%{}%\", param.value));
                                } else {
                                    tracing::warn!(\"Reverse chaining filter parameter '{}' not yet implemented\", filter_param);
                                }
                            }
                        }
                    }
                    param_name if param_name.contains('.') => {
                        // Check for chaining: base_param.chained_field
                        if let Some((base_param, chained_field)) = param_name.split_once('.') {
                            // Try to determine target resource type from base param
                            // For now, support common patterns
                            let (target_table, ref_col) = match base_param {
                                \"general-practitioner\" => (\"practitioner\", \"general_practitioner\"),
                                \"subject\" => {
                                    // Subject could reference different types, default to patient
                                    // TODO: handle multiple target types
                                    (\"patient\", \"subject\")
                                }
                                _ => {
                                    tracing::warn!(\"Unsupported chaining on parameter: {}\", base_param);
                                    continue;
                                }
                            };

                            // Handle chained field types
                            if chained_field == \"family\" {
                                let bind_idx = bind_values.len() + 1;
                                // Handle both collection and non-collection references
                                // For collections, unnest the array first
                                sql.push_str(&""format!(
                                    \"EXISTS (SELECT 1 FROM {} JOIN unnest("resource-lower".{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                    target_table, ref_col, target_table, target_table, bind_idx
                                ));
                                bind_values.push(format!(\"%{}%\", param.value));
                            }
                            else if chained_field == \"identifier\" {
                                if param.value.contains('|') {
                                    let parts: Vec<&""str> = param.value.split('|').collect();
                                    if parts.len() == 2 {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} JOIN unnest("resource-lower".{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                            target_table, ref_col, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                        ));
                                        bind_values.push(parts[0].to_string());
                                        bind_values.push(parts[1].to_string());
                                    }
                                } else {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&""format!(
                                        \"EXISTS (SELECT 1 FROM {} JOIN unnest("resource-lower".{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                        target_table, ref_col, target_table, target_table, bind_idx
                                    ));
                                    bind_values.push(param.value.clone());
                                }
                            }
                            else {
                                tracing::warn!(\"Chained field '{}' not yet supported\", chained_field);
                            }
                        }
                    }
                    _ => {
                        // Unknown parameter - ignore per FHIR spec
                        tracing::warn!(\"Unknown search parameter for "resource-name": {}\", param.name);
                    }
                }
            }

            // Close OR group if needed
            if needs_or_group {
                sql.push_str(\")\");
            }

            // Check if any condition was actually added
            // If sql length increased only by \" AND \" or \" AND ()\", revert it
            let added_text_len = sql.len() - sql_before_len;
            let expected_empty_len = if needs_or_group { 7 } else { 5 }; // \" AND ()\" or \" AND \"

            if added_text_len == expected_empty_len {
                // No actual condition was added, revert the AND
                sql.truncate(sql_before_len);
            }
        }

        // Add sorting
        if !query.sort.is_empty() {
            sql.push_str(\" ORDER BY \");
            for (i, sort) in query.sort.iter().enumerate() {
                if i > 0 {
                    sql.push_str(\", \");
                }
                // Map FHIR search parameter name to database column name
                let column_name = Self::map_sort_field_to_column(&""sort.field);
                sql.push_str(&""column_name);
                match sort.direction {
                    SortDirection::Ascending => sql.push_str(\" ASC\"),
                    SortDirection::Descending => sql.push_str(\" DESC\"),
                }
            }
        }

        // Add pagination
        sql.push_str(&""format!(\" LIMIT {} OFFSET {}\", query.limit, query.offset));

        // Execute query
        let mut query_builder = sqlx::query_as::<""_, "resource-name">(&""sql);
        for value in &""bind_values {
            query_builder = query_builder.bind(value);
        }

        let resources = query_builder
            .fetch_all(&""self.pool)
            .await?;

        // Get total count if requested
        let total = if include_total {
            let count_sql = sql.replace(
                &""format!(\"SELECT "resource-lower".id, "resource-lower".version_id, "resource-lower".last_updated, "resource-lower".content\\n               FROM "resource-lower"\"),
                \"SELECT COUNT(*) FROM "resource-lower"\"
            ).split(\" ORDER BY\").next().unwrap().to_string();

            let mut count_query = sqlx::query_scalar::<""_, i64>(&""count_sql);
            for value in &""bind_values {
                count_query = count_query.bind(value);
            }
            Some(count_query.fetch_one(&""self.pool).await?)
        } else {
            None
        };

        Ok((resources, total))
    }

    /// Map FHIR search parameter name to database column name for sorting
    fn map_sort_field_to_column(field: &""str) -> String {
        match field {
"(generate-sort-field-mappings unique-searches resource-lower)"            _ => field.to_string(),
        }
    }
")
       (generate-helper-methods resource-name resource-lower))))

; Generate sort field to column name mappings
(define (generate-sort-field-mappings searches resource-lower)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (let* ((search-name (% "name" search))
                             (col-name (search-param-to-column-name search)))
                        ; Skip special parameters like _lastUpdated
                        (if (and (> (string-length search-name) 0)
                                 (char=? (string-ref search-name 0) #\_))
                            ""
                            ($"            \""search-name"\" => \""col-name"\".to_string(),
"))))
                    searches))))

; Generate match arms for each search parameter
(define (generate-search-param-matches resource-name resource-lower searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (generate-search-param-match resource-name resource-lower search))
                    searches))))

; Generate match arms for OR logic (no AND prefix in conditions)
(define (generate-search-param-matches-or resource-name resource-lower searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (generate-search-param-match-or resource-name resource-lower search))
                    searches))))

; Generate a single search parameter match arm
(define (generate-search-param-match resource-name resource-lower search)
  (let* ((search-name (% "name" search))
         (search-type (% "type" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (extraction-key (search-extraction-key search))
         (is-collection (search-is-collection? search)))
    ; Skip special parameters like _lastUpdated
    (if (and (> (string-length search-name) 0)
             (string=? (substring search-name 0 1) "_"))
        ""
        (case search-type
          (("string") (generate-string-param-match search-name extraction-key resource-lower search))
          (("token") (generate-token-param-match search-name col-name search resource-lower is-collection))
          (("date") (generate-date-param-match search-name col-name resource-lower))
          (("reference") (generate-reference-param-match search-name col-name resource-lower is-collection search))
          (("composite") "") ; Skip composite for now
          (("special") "")
          (else "")))))

; Generate string parameter match (HumanName searches - field-specific or all fields)
(define (generate-string-param-match search-name extraction-key resource-lower search)
  (if (string=? extraction-key "name")
      ; HumanName search - determine which field(s) to search
      (let ((field-type (search-humanname-field search)))
        (if field-type
            (generate-humanname-search search-name field-type resource-lower)
            ""))
      ; Other string searches - skip for now
      ""))

; Generate HumanName field-specific search
(define (generate-humanname-search search-name field-type resource-lower)
  (cond
    ; Search ALL HumanName fields
    ((string=? field-type "all")
     ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref().unwrap_or(\"contains\");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        \"contains\" => {
                            sql.push_str(&""format!(
                                \" AND (EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn ILIKE ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".given_name) AS gn WHERE gn ILIKE ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".prefix) AS p WHERE p ILIKE ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".suffix) AS s WHERE s ILIKE ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".name_text) AS nt WHERE nt ILIKE ${}))\",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(format!(\"%{}%\", param.value));
                        }
                        \"exact\" => {
                            sql.push_str(&""format!(
                                \" AND (EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn = ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".given_name) AS gn WHERE gn = ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".prefix) AS p WHERE p = ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".suffix) AS s WHERE s = ${}) \\
                                 OR EXISTS (SELECT 1 FROM unnest("resource-lower".name_text) AS nt WHERE nt = ${}))\",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        \"missing\" => {
                            sql.push_str(\" AND ("resource-lower".family_name IS NULL OR array_length("resource-lower".family_name, 1) IS NULL)\");
                        }
                        \"not\" => {
                            sql.push_str(&""format!(
                                \" AND NOT (EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn ILIKE ${}))\",
                                bind_idx
                            ));
                            bind_values.push(format!(\"%{}%\", param.value));
                        }
                        _ => {}
                    }
                }
"))
    ; Search specific HumanName field (family, given, prefix, suffix, text)
    (else
     (let ((col-name ($ field-type "_name")))  ; e.g., "family" -> "family_name"
       ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref().unwrap_or(\"contains\");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        \"contains\" => {
                            sql.push_str(&""format!(
                                \" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v ILIKE ${})\",
                                bind_idx
                            ));
                            bind_values.push(format!(\"%{}%\", param.value));
                        }
                        \"exact\" => {
                            sql.push_str(&""format!(
                                \" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v = ${})\",
                                bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        \"missing\" => {
                            sql.push_str(\" AND ("resource-lower"."col-name" IS NULL OR array_length("resource-lower"."col-name", 1) IS NULL)\");
                        }
                        \"not\" => {
                            sql.push_str(&""format!(
                                \" AND NOT (EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v ILIKE ${}))\",
                                bind_idx
                            ));
                            bind_values.push(format!(\"%{}%\", param.value));
                        }
                        _ => {}
                    }
                }
")))))

; Generate token parameter match
(define (generate-token-param-match search-name col-name search resource-lower is-collection)
  (cond
    ; Simple code (boolean, gender, etc.) - with modifier support
    ((search-is-simple-code? search)
     (let* ((property (search-property search))
            (property-type (if property (% "type" property) "code"))
            (cast-suffix (if (string=? property-type "boolean") "::boolean" "")))
       ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            sql.push_str(&""format!(\" AND "resource-lower"."col-name" = ${}"cast-suffix"\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        Some(\"missing\") => {
                            // :missing modifier
                            let is_missing = param.value == \"true\";
                            if is_missing {
                                sql.push_str(\" AND "resource-lower"."col-name" IS NULL\");
                            } else {
                                sql.push_str(\" AND "resource-lower"."col-name" IS NOT NULL\");
                            }
                        }
                        Some(\"not\") => {
                            // :not modifier
                            sql.push_str(&""format!(\" AND "resource-lower"."col-name" != ${}"cast-suffix"\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        _ => {
                            // Unknown modifier - ignore
                            tracing::warn!(\"Unknown modifier for token search: {:?}\", modifier);
                        }
                    }
                }
")))
    ; Identifier (system|value)
    ((search-is-identifier? search)
     ($"                \""search-name"\" => {
                    if param.value.contains('|') {
                        let parts: Vec<""&""str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(
                                \" AND EXISTS (SELECT 1 FROM unnest("resource-lower".identifier_system, "resource-lower".identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})\",
                                bind_idx, bind_idx + 1
                            ));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&""format!(\" AND EXISTS (SELECT 1 FROM unnest("resource-lower".identifier_value) AS iv WHERE iv = ${})\", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
"))
    ; ContactPoint (email, phone, telecom)
    ((search-is-contactpoint? search)
     ($"                \""search-name"\" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&""format!(\" AND EXISTS (SELECT 1 FROM unnest("resource-lower".telecom_value) AS tv WHERE tv = ${})\", bind_idx));
                    bind_values.push(param.value.clone());
                }
"))
    ; CodeableConcept
    ((or (search-is-codeableconcept? search) (search-has-codeableconcept-variant? search))
     (if is-collection
       ; Array columns - use unnest for array comparison
       ($"                \""search-name"\" => {
                    if param.value.contains('|') {
                        let parts: Vec<""&""str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(
                                \" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_system, "resource-lower"."col-name"_code) AS cc(sys, code) WHERE cc.sys = ${} AND cc.code = ${})\",
                                bind_idx, bind_idx + 1
                            ));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&""format!(\" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_code) AS c WHERE c = ${})\", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
")
       ; Non-array columns - use direct comparison
       ($"                \""search-name"\" => {
                    if param.value.contains('|') {
                        let parts: Vec<""&""str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(\" AND "resource-lower"."col-name"_system = ${} AND "resource-lower"."col-name"_code = ${}\", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&""format!(\" AND "resource-lower"."col-name"_code = ${}\", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
")))
    (else "")))

; Generate date parameter match
(define (generate-date-param-match search-name col-name resource-lower)
  ; Determine the SQL type cast based on column name
  ; date columns use DATE, datetime/instant use TIMESTAMPTZ
  (let* ((is-datetime-col (or (string-ci=? col-name "last_updated")
                               (string-ci=? col-name "_lastupdated")
                               (string-suffix? "_datetime" col-name)
                               (string-suffix? "_instant" col-name)
                               (string-suffix? "_period_start" col-name)
                               (string-suffix? "_period_end" col-name)))
         (cast-suffix (if is-datetime-col "::timestamptz" "::date")))
    ($"                \""search-name"\" => {
                    let bind_idx = bind_values.len() + 1;
                    let op = match param.prefix.as_deref() {
                        Some(\"eq\") | None => \"=\",
                        Some(\"ne\") => \"!=\",
                        Some(\"gt\") => \">\",
                        Some(\"lt\") => \"<\",
                        Some(\"ge\") => \">=\",
                        Some(\"le\") => \"<=\",
                        _ => \"=\",
                    };
                    sql.push_str(&""format!(\" AND "resource-lower"."col-name" {} ${}"cast-suffix"\", op, bind_idx));
                    bind_values.push(param.value.clone());
                }
")))

; Generate reference parameter match
(define (generate-reference-param-match search-name col-name resource-lower is-collection search)
  (let* ((targets (search-target-resources search))
         (has-targets (not (null? targets)))
         (first-target (if has-targets (car targets) "")))
    (if is-collection
        (if has-targets
            ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            let mut ref_value = param.value.clone();
                            if !ref_value.contains('/') {
                                ref_value = format!(\""first-target"/{}\", ref_value);
                            }
                            sql.push_str(&""format!(\" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_reference) AS ref WHERE ref = ${})\", bind_idx));
                            bind_values.push(ref_value);
                        }
                        Some(\"missing\") => {
                            // :missing modifier for array
                            let is_missing = param.value == \"true\";
                            if is_missing {
                                sql.push_str(\" AND ("resource-lower"."col-name"_reference IS NULL OR array_length("resource-lower"."col-name"_reference, 1) IS NULL)\");
                            } else {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NOT NULL AND array_length("resource-lower"."col-name"_reference, 1) > 0\");
                            }
                        }
                        _ => {
                            tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                        }
                    }
                }
")
            ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            sql.push_str(&""format!(\" AND EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_reference) AS ref WHERE ref = ${})\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        Some(\"missing\") => {
                            // :missing modifier for array
                            let is_missing = param.value == \"true\";
                            if is_missing {
                                sql.push_str(\" AND ("resource-lower"."col-name"_reference IS NULL OR array_length("resource-lower"."col-name"_reference, 1) IS NULL)\");
                            } else {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NOT NULL AND array_length("resource-lower"."col-name"_reference, 1) > 0\");
                            }
                        }
                        _ => {
                            tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                        }
                    }
                }
"))
        (if has-targets
            ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            let mut ref_value = param.value.clone();
                            if !ref_value.contains('/') {
                                ref_value = format!(\""first-target"/{}\", ref_value);
                            }
                            sql.push_str(&""format!(\" AND "resource-lower"."col-name"_reference = ${}\", bind_idx));
                            bind_values.push(ref_value);
                        }
                        Some(\"missing\") => {
                            // :missing modifier for non-array
                            let is_missing = param.value == \"true\";
                            if is_missing {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NULL\");
                            } else {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NOT NULL\");
                            }
                        }
                        _ => {
                            tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                        }
                    }
                }
")
            ($"                \""search-name"\" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            sql.push_str(&""format!(\" AND "resource-lower"."col-name"_reference = ${}\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        Some(\"missing\") => {
                            // :missing modifier for non-array
                            let is_missing = param.value == \"true\";
                            if is_missing {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NULL\");
                            } else {
                                sql.push_str(\" AND "resource-lower"."col-name"_reference IS NOT NULL\");
                            }
                        }
                        _ => {
                            tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                        }
                    }
                }
")))))

; Generate a single search parameter match arm for OR logic (no AND prefix)
(define (generate-search-param-match-or resource-name resource-lower search)
  (let* ((search-name (% "name" search))
         (search-type (% "type" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (extraction-key (search-extraction-key search))
         (is-collection (search-is-collection? search)))
    ; Skip special parameters like _lastUpdated
    (if (and (> (string-length search-name) 0)
             (string=? (substring search-name 0 1) "_"))
        ""
        (case search-type
          (("string") (generate-string-param-match-or search-name extraction-key resource-lower search))
          (("token") (generate-token-param-match-or search-name col-name search resource-lower is-collection))
          (("date") (generate-date-param-match-or search-name col-name resource-lower))
          (("reference") (generate-reference-param-match-or search-name col-name resource-lower is-collection search))
          (("composite") (generate-composite-param-match-or search-name resource-lower search))
          (("special") "")
          (else "")))))

; String parameter match for OR logic (no AND prefix)
(define (generate-string-param-match-or search-name extraction-key resource-lower search)
  (if (string=? extraction-key "name")
      (let ((field-type (search-humanname-field search)))
        (if field-type
            (generate-humanname-search-or search-name field-type resource-lower)
            ""))
      ""))

; HumanName search for OR logic  (no AND prefix)
(define (generate-humanname-search-or search-name field-type resource-lower)
  (cond
    ((string=? field-type "all")
     ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref().unwrap_or(\"contains\");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            \"contains\" => {
                                sql.push_str(&""format!(
                                    \"(EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn ILIKE ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".given_name) AS gn WHERE gn ILIKE ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".prefix) AS p WHERE p ILIKE ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".suffix) AS s WHERE s ILIKE ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".name_text) AS nt WHERE nt ILIKE ${}))\",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(format!(\"%{}%\", param.value));
                            }
                            \"exact\" => {
                                sql.push_str(&""format!(
                                    \"(EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn = ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".given_name) AS gn WHERE gn = ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".prefix) AS p WHERE p = ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".suffix) AS s WHERE s = ${}) \\
                                     OR EXISTS (SELECT 1 FROM unnest("resource-lower".name_text) AS nt WHERE nt = ${}))\",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            \"missing\" => {
                                sql.push_str(\"("resource-lower".family_name IS NULL OR array_length("resource-lower".family_name, 1) IS NULL)\");
                            }
                            \"not\" => {
                                sql.push_str(&""format!(
                                    \"NOT (EXISTS (SELECT 1 FROM unnest("resource-lower".family_name) AS fn WHERE fn ILIKE ${}))\",
                                    bind_idx
                                ));
                                bind_values.push(format!(\"%{}%\", param.value));
                            }
                            _ => {}
                        }
                    }
"))
    (else
     (let ((col-name ($ field-type "_name")))
       ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref().unwrap_or(\"contains\");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            \"contains\" => {
                                sql.push_str(&""format!(
                                    \"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v ILIKE ${})\",
                                    bind_idx
                                ));
                                bind_values.push(format!(\"%{}%\", param.value));
                            }
                            \"exact\" => {
                                sql.push_str(&""format!(
                                    \"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v = ${})\",
                                    bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            \"missing\" => {
                                sql.push_str(\"("resource-lower"."col-name" IS NULL OR array_length("resource-lower"."col-name", 1) IS NULL)\");
                            }
                            \"not\" => {
                                sql.push_str(&""format!(
                                    \"NOT (EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name") AS v WHERE v ILIKE ${}))\",
                                    bind_idx
                                ));
                                bind_values.push(format!(\"%{}%\", param.value));
                            }
                            _ => {}
                        }
                    }
")))))

; Token parameter match for OR logic (no AND prefix)
(define (generate-token-param-match-or search-name col-name search resource-lower is-collection)
  (cond
    ((search-is-simple-code? search)
     (let* ((property (search-property search))
            (property-type (if property (% "type" property) "code"))
            (cast-suffix (if (string=? property-type "boolean") "::boolean" "")))
       ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                sql.push_str(&""format!(\"("resource-lower"."col-name" = ${}"cast-suffix")\", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some(\"missing\") => {
                                let is_missing = param.value == \"true\";
                                if is_missing {
                                    sql.push_str(\"("resource-lower"."col-name" IS NULL)\");
                                } else {
                                    sql.push_str(\"("resource-lower"."col-name" IS NOT NULL)\");
                                }
                            }
                            Some(\"not\") => {
                                sql.push_str(&""format!(\"("resource-lower"."col-name" != ${}"cast-suffix")\", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            _ => {
                                tracing::warn!(\"Unknown modifier for token search: {:?}\", modifier);
                            }
                        }
                    }
")))
    ((search-is-identifier? search)
     ($"                    \""search-name"\" => {
                        if param.value.contains('|') {
                            let parts: Vec<""&""str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&""format!(
                                    \"EXISTS (SELECT 1 FROM unnest("resource-lower".identifier_system, "resource-lower".identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})\",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(\"EXISTS (SELECT 1 FROM unnest("resource-lower".identifier_value) AS iv WHERE iv = ${})\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
"))
    ((search-is-contactpoint? search)
     ($"                    \""search-name"\" => {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&""format!(\"EXISTS (SELECT 1 FROM unnest("resource-lower".telecom_value) AS tv WHERE tv = ${})\", bind_idx));
                        bind_values.push(param.value.clone());
                    }
"))
    ((or (search-is-codeableconcept? search) (search-has-codeableconcept-variant? search))
     (if is-collection
       ($"                    \""search-name"\" => {
                        if param.value.contains('|') {
                            let parts: Vec<""&""str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&""format!(
                                    \"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_system, "resource-lower"."col-name"_code) AS cc(sys, code) WHERE cc.sys = ${} AND cc.code = ${})\",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(\"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_code) AS c WHERE c = ${})\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
")
       ($"                    \""search-name"\" => {
                        if param.value.contains('|') {
                            let parts: Vec<""&""str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&""format!(\"("resource-lower"."col-name"_system = ${} AND "resource-lower"."col-name"_code = ${})\", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&""format!(\"("resource-lower"."col-name"_code = ${})\", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
")))
    (else "")))

; Date parameter match for OR logic (no AND prefix)
(define (generate-date-param-match-or search-name col-name resource-lower)
  (let* ((is-datetime-col (or (string-ci=? col-name "last_updated")
                               (string-ci=? col-name "_lastupdated")
                               (string-suffix? "_datetime" col-name)
                               (string-suffix? "_instant" col-name)
                               (string-suffix? "_period_start" col-name)
                               (string-suffix? "_period_end" col-name)))
         (cast-suffix (if is-datetime-col "::timestamptz" "::date")))
    ($"                    \""search-name"\" => {
                        // Basic validation of date format (YYYY-MM-DD or partial dates)
                        let date_value = &""param.value;
                        let is_valid_date = date_value.chars().all(|c| c.is_ascii_digit() || c == '-')
                            &""&"" !date_value.is_empty()
                            &""&"" date_value.len() <= 10;  // Max length for YYYY-MM-DD

                        if !is_valid_date {
                            // Invalid date format - skip this parameter to avoid database error
                            // (returning empty results is acceptable per FHIR spec)
                            tracing::warn!(\"Invalid date format for '"search-name"': '{}', skipping parameter\", date_value);
                            continue;
                        }

                        let bind_idx = bind_values.len() + 1;
                        let op = match param.prefix.as_deref() {
                            Some(\"eq\") | None => \"=\",
                            Some(\"ne\") => \"!=\",
                            Some(\"gt\") => \">\",
                            Some(\"lt\") => \"<\",
                            Some(\"ge\") => \">=\",
                            Some(\"le\") => \"<=\",
                            _ => \"=\",
                        };
                        sql.push_str(&""format!(\"("resource-lower"."col-name" {} ${}"cast-suffix")\", op, bind_idx));
                        bind_values.push(param.value.clone());
                    }
")))

; Reference parameter match for OR logic (no AND prefix)
(define (generate-reference-param-match-or search-name col-name resource-lower is-collection search)
  (let* ((targets (search-target-resources search))
         (has-targets (not (null? targets)))
         (first-target (if has-targets (car targets) "")))
    (if is-collection
        (if has-targets
            ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!(\""first-target"/{}\", ref_value);
                                }
                                sql.push_str(&""format!(\"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_reference) AS ref WHERE ref = ${})\", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some(\"missing\") => {
                                let is_missing = param.value == \"true\";
                                if is_missing {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NULL OR array_length("resource-lower"."col-name"_reference, 1) IS NULL)\");
                                } else {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NOT NULL AND array_length("resource-lower"."col-name"_reference, 1) > 0)\");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', \"_\");

                                            if final_param == \"family\" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))\",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!(\"%{}%\", param.value));
                                            } else {
                                                tracing::warn!(\"Multi-level chained parameter '{}' not yet implemented\", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == \"family\" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!(\"%{}%\", param.value));
                                    }
                                    else if chained_param == \"identifier\" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&""str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&""format!(
                                                \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!(\"Chained parameter '{}' not yet implemented\", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                            }
                        }
                    }
")
            ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                sql.push_str(&""format!(\"EXISTS (SELECT 1 FROM unnest("resource-lower"."col-name"_reference) AS ref WHERE ref = ${})\", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some(\"missing\") => {
                                let is_missing = param.value == \"true\";
                                if is_missing {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NULL OR array_length("resource-lower"."col-name"_reference, 1) IS NULL)\");
                                } else {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NOT NULL AND array_length("resource-lower"."col-name"_reference, 1) > 0)\");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', \"_\");

                                            if final_param == \"family\" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))\",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!(\"%{}%\", param.value));
                                            } else {
                                                tracing::warn!(\"Multi-level chained parameter '{}' not yet implemented\", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == \"family\" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!(\"%{}%\", param.value));
                                    }
                                    else if chained_param == \"identifier\" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&""str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&""format!(
                                                \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!(\"Chained parameter '{}' not yet implemented\", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                            }
                        }
                    }
"))
        (if has-targets
            ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!(\""first-target"/{}\", ref_value);
                                }
                                sql.push_str(&""format!(\"("resource-lower"."col-name"_reference = ${})\", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some(\"missing\") => {
                                let is_missing = param.value == \"true\";
                                if is_missing {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NULL)\");
                                } else {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NOT NULL)\");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', \"_\");

                                            if final_param == \"family\" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))\",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!(\"%{}%\", param.value));
                                            } else {
                                                tracing::warn!(\"Multi-level chained parameter '{}' not yet implemented\", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == \"family\" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!(\"%{}%\", param.value));
                                    }
                                    else if chained_param == \"identifier\" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&""str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&""format!(
                                                \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!(\"Chained parameter '{}' not yet implemented\", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                            }
                        }
                    }
")
            ($"                    \""search-name"\" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                sql.push_str(&""format!(\"("resource-lower"."col-name"_reference = ${})\", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some(\"missing\") => {
                                let is_missing = param.value == \"true\";
                                if is_missing {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NULL)\");
                                } else {
                                    sql.push_str(\"("resource-lower"."col-name"_reference IS NOT NULL)\");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', \"_\");

                                            if final_param == \"family\" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))\",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!(\"%{}%\", param.value));
                                            } else {
                                                tracing::warn!(\"Multi-level chained parameter '{}' not yet implemented\", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == \"family\" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!(\"%{}%\", param.value));
                                    }
                                    else if chained_param == \"identifier\" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&""str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&""format!(
                                                \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!(\"Chained parameter '{}' not yet implemented\", chained_param);
                                    }
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', \"_\");

                                            if final_param == \"family\" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))\",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!(\"%{}%\", param.value));
                                            } else {
                                                tracing::warn!(\"Multi-level chained parameter '{}' not yet implemented\", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == \"family\" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&""format!(
                                            \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))\",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!(\"%{}%\", param.value));
                                    }
                                    else if chained_param == \"identifier\" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&""str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&""format!(
                                                    \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))\",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&""format!(
                                                \"EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING("resource-lower"."col-name"_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))\",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!(\"Chained parameter '{}' not yet implemented\", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!(\"Unknown modifier for reference search: {:?}\", modifier);
                            }
                        }
                    }
")))))

; Composite parameter match for OR logic (no AND prefix)
(define (generate-composite-param-match-or search-name resource-lower search)
  ; Parse composite search components from XML
  (let* ((paths-node (select-children "paths" search))
         (first-paths (if (node-list-empty? paths-node) #f (node-list-first paths-node)))
         (path-nodes (if first-paths (select-children "path" first-paths) (empty-node-list)))
         (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
         (components-node (if first-path (select-children "components" first-path) (empty-node-list)))
         (first-components (if (node-list-empty? components-node) #f (node-list-first components-node)))
         (component-nodes (if first-components (select-children "component" first-components) (empty-node-list)))
         (component-list (if (node-list-empty? component-nodes) '() (node-list->list component-nodes)))
         ; Check if this is a component search (searches within observation.component array)
         (parts-node (if first-path (select-children "parts" first-path) (empty-node-list)))
         (first-parts (if (node-list-empty? parts-node) #f (node-list-first parts-node)))
         (part-nodes (if first-parts (select-children "part" first-parts) (empty-node-list)))
         (first-part (if (node-list-empty? part-nodes) #f (node-list-first part-nodes)))
         (is-component-search (and first-part
                                   (let ((ref (% "ref" first-part)))
                                     (string-suffix? ".component" ref)))))

    (if (< (length component-list) 2)
        ""  ; Composite must have at least 2 components
        (let* ((comp1 (car component-list))
               (comp2 (cadr component-list))
               (comp1-ref (% "ref" comp1))
               (comp2-ref (% "ref" comp2))
               ; Determine component types
               (comp2-casts (select-children "casts" comp2))
               (first-casts (if (node-list-empty? comp2-casts) #f (node-list-first comp2-casts)))
               (cast-nodes (if first-casts (select-children "cast" first-casts) (empty-node-list)))
               (first-cast (if (node-list-empty? cast-nodes) #f (node-list-first cast-nodes)))
               (value-type (if first-cast (% "to" first-cast) ""))
               ; Extract column names from refs (e.g., "observation.code" -> "code")
               (code-col (let ((parts (string-split comp1-ref ".")))
                          (if (null? parts) "" (car (reverse parts)))))
               (value-col (let ((parts (string-split comp2-ref ".")))
                           (if (null? parts) "" (car (reverse parts))))))

          (cond
            ; Component searches - query JSONB within observation.content->'component' array
            (is-component-search
             (cond
               ((string=? value-type "Quantity")
                (generate-component-code-value-quantity-match search-name resource-lower))
               ((string=? value-type "CodeableConcept")
                (generate-component-code-value-concept-match search-name resource-lower))
               (else "")))

            ; Regular composite searches using indexed columns
            ((string=? value-type "Quantity")
             (generate-code-value-quantity-match search-name code-col resource-lower))
            ((string=? value-type "CodeableConcept")
             (generate-code-value-concept-match search-name code-col value-col resource-lower))
            ((string=? value-type "string")
             (generate-code-value-string-match search-name code-col value-col resource-lower))
            ((string=? value-type "date")
             (generate-code-value-date-match search-name code-col value-col resource-lower))
            (else ""))))))

; Generate code-value-quantity composite search (uses indexed columns)
(define (generate-code-value-quantity-match search-name code-col resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_part)) = param.value.split_once('$') {
                            // Parse value with optional prefix
                            let (prefix_str, value_str) = if value_part.starts_with(\"gt\") {
                                (\">\", &value_part[2..])
                            } else if value_part.starts_with(\"ge\") {
                                (\">=\", &value_part[2..])
                            } else if value_part.starts_with(\"lt\") {
                                (\"<\", &value_part[2..])
                            } else if value_part.starts_with(\"le\") {
                                (\"<=\", &value_part[2..])
                            } else if value_part.starts_with(\"ne\") {
                                (\"!=\", &value_part[2..])
                            } else if value_part.starts_with(\"eq\") {
                                (\"=\", &value_part[2..])
                            } else {
                                (\"=\", value_part)
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"("resource-lower"."code-col"_system = ${} AND "resource-lower"."code-col"_code = ${} AND "resource-lower".value_quantity_value {} ${}::numeric)\",<![CDATA[
                                        bind_idx, bind_idx + 1, prefix_str, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"("resource-lower"."code-col"_code = ${} AND "resource-lower".value_quantity_value {} ${}::numeric)\",<![CDATA[
                                    bind_idx, prefix_str, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_str.to_string());
                            }
                        }
                    ]]>}
"))

; Generate code-value-concept composite search (uses indexed columns)
(define (generate-code-value-concept-match search-name code-col value-col resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$value_code or code$value_code
                        if let Some((code_part, value_code)) = param.value.split_once('$') {
                            // Parse value part for optional system|code
                            let value_code_only = if value_code.contains('|') {
                                value_code.split('|').nth(1).unwrap_or(value_code)
                            } else {
                                value_code
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"("resource-lower"."code-col"_system = ${} AND "resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col"_concept_code = ${})\",<![CDATA[
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_code_only.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"("resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col"_concept_code = ${})\",<![CDATA[
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_code_only.to_string());
                            }
                        }
                    ]]>}
"))

; Generate code-value-string composite search
(define (generate-code-value-string-match search-name code-col value-col resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_str)) = param.value.split_once('$') {
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"("resource-lower"."code-col"_system = ${} AND "resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col" ILIKE ${})\",<![CDATA[
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(format!(\"%{}%\", value_str));
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"("resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col" ILIKE ${})\",<![CDATA[
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(format!(\"%{}%\", value_str));
                            }
                        }
                    ]]>}
"))

; Generate code-value-date composite search
(define (generate-code-value-date-match search-name code-col value-col resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$date or code$date
                        if let Some((code_part, date_str)) = param.value.split_once('$') {
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"("resource-lower"."code-col"_system = ${} AND "resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col" = ${})\",<![CDATA[
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(date_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"("resource-lower"."code-col"_code = ${} AND "resource-lower"."value-col" = ${})\",<![CDATA[
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(date_str.to_string());
                            }
                        }
                    ]]>}
"))

; Generate component-code-value-quantity composite search (JSONB query)
(define (generate-component-code-value-quantity-match search-name resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_part)) = param.value.split_once('$') {
                            // Parse value with optional prefix
                            let (prefix_str, value_str) = if value_part.starts_with(\"gt\") {
                                (\">\", &value_part[2..])
                            } else if value_part.starts_with(\"ge\") {
                                (\">=\", &value_part[2..])
                            } else if value_part.starts_with(\"lt\") {
                                (\"<\", &value_part[2..])
                            } else if value_part.starts_with(\"le\") {
                                (\"<=\", &value_part[2..])
                            } else if value_part.starts_with(\"ne\") {
                                (\"!=\", &value_part[2..])
                            } else if value_part.starts_with(\"eq\") {
                                (\"=\", &value_part[2..])
                            } else {
                                (\"=\", value_part)
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"EXISTS (SELECT 1 FROM jsonb_array_elements("resource-lower".content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)\",<![CDATA[
                                        bind_idx, bind_idx + 1, prefix_str, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"EXISTS (SELECT 1 FROM jsonb_array_elements("resource-lower".content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)\",<![CDATA[
                                    bind_idx, prefix_str, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_str.to_string());
                            }
                        }
                    ]]>}
"))

; Generate component-code-value-concept composite search (JSONB query)
(define (generate-component-code-value-concept-match search-name resource-lower)
  ($"                    \""search-name"\" => {<![CDATA[
                        // Parse composite: system|code$value_code or code$value_code
                        if let Some((code_part, value_code)) = param.value.split_once('$') {
                            // Parse value part for optional system|code
                            let value_code_only = if value_code.contains('|') {
                                value_code.split('|').nth(1).unwrap_or(value_code)
                            } else {
                                value_code
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        ]]>\"EXISTS (SELECT 1 FROM jsonb_array_elements("resource-lower".content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})\",<![CDATA[
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_code_only.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    ]]>\"EXISTS (SELECT 1 FROM jsonb_array_elements("resource-lower".content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})\",<![CDATA[
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_code_only.to_string());
                            }
                        }
                    ]]>}
"))

; Generate helper methods (resource-specific)
(define (generate-helper-methods resource-name resource-lower)
  (case resource-name
    (("Patient") ($"
    /// Find observations by patient ID (for _revinclude support)
    pub async fn find_observations_by_patient(&""self, patient_id: &""str) -> Result<""Vec<""Observation>> {
        let patient_ref = format!(\"Patient/{}\", patient_id);
        let observations = sqlx::query_as!(
            Observation,
            r#\"SELECT id, version_id, last_updated, content as \"content: Value\" FROM observation WHERE subject_reference = $1\"#,
            patient_ref
        ).fetch_all(&""self.pool).await?;
        Ok(observations)
    }

    /// Find practitioners by IDs (for _include support)
    pub async fn find_practitioners_by_ids(&""self, practitioner_ids: &""[String]) -> Result<""Vec<""crate::models::practitioner::Practitioner>> {
        use crate::models::practitioner::Practitioner;
        if practitioner_ids.is_empty() { return Ok(Vec::new()); }
        let practitioners = sqlx::query_as!(
            Practitioner,
            r#\"SELECT id, version_id, last_updated, content as \"content: Value\" FROM practitioner WHERE id = ANY($1)\"#,
            practitioner_ids
        ).fetch_all(&""self.pool).await?;
        Ok(practitioners)
    }
"))
    (("Observation") ($"
    /// Read a patient by ID (for chaining support)
    pub async fn read_patient(&""self, id: &""str) -> Result<""Patient> {
        let patient = sqlx::query_as!(
            Patient,
            r#\"SELECT id, version_id, last_updated, content as \"content: Value\" FROM patient WHERE id = $1\"#,
            id
        ).fetch_optional(&""self.pool).await?.ok_or(FhirError::NotFound)?;
        Ok(patient)
    }
"))
    (("Practitioner") ($"
    /// Find patients by practitioner ID (for _revinclude support)
    pub async fn find_patients_by_practitioner(&""self, practitioner_id: &""str) -> Result<""Vec<""Patient>> {
        let practitioner_ref = format!(\"Practitioner/{}\", practitioner_id);
        let patients = sqlx::query_as!(
            Patient,
            r#\"SELECT id, version_id, last_updated, content as \"content: Value\" FROM patient WHERE general_practitioner_reference @> ARRAY[$1]::text[]\"#,
            practitioner_ref
        ).fetch_all(&""self.pool).await?;
        Ok(patients)
    }
"))
    (else "")))

