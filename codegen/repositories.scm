; Repository generator for Fire FHIR Server
; Generates CRUD and search methods for each resource

(define (repositories)
  (for-active-resources generate-repository))

; Generate a complete repository file for a resource
(define (generate-repository resource)
  (let* ((resource-name (name-of resource))
         (resource-lower (downcase-string resource-name))
         (table-name resource-lower)
         (struct-name ($ resource-name "Repository"))
         (searches-node (select-children "searches" resource))
         (searches (if (not (node-list-empty? searches-node))
                      (select-children "search" (node-list-first searches-node))
                      (empty-node-list)))
         ; Get unique search params (deduplicate like in structs.scm)
         (all-searches (node-list->list searches))
         (unique-searches (deduplicate-searches all-searches)))

    (file ($ "src/repository/" resource-lower ".rs")
      ($"use chrono::Utc;
use serde_json::Value;
use sqlx::{PgPool, Row};
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::"resource-lower"::{extract_"resource-lower"_search_params, "resource-name", "resource-name"History};
use crate::search::{SearchCondition, SearchQuery};
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
}
"))))

; Note: search-extraction-key and deduplicate-searches are defined in structs.scm

; Helper: Get column names for INSERT statement (without id, version_id, last_updated, content)
(define (get-insert-columns searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (let ((col-name (camel-to-snake (string-replace (% "name" search) "-" "_"))))
                        (get-insert-columns-for-search search col-name)))
                    searches))))

(define (get-insert-columns-for-search search col-name)
  (let ((search-type (% "type" search))
        (is-collection (search-is-collection? search)))
    (case search-type
      (("composite" "special") "")
      (("token")
       (cond
         ((search-is-simple-code? search) ($",
                "col-name""))
         ((search-is-contactpoint? search) ($",
                "col-name"_value"))
         ((search-is-identifier? search) ($",
                identifier_system, identifier_value"))
         ((or (search-is-codeableconcept? search) (search-has-codeableconcept-variant? search))
          (if (search-has-codeableconcept-variant? search)
              ($",
                "col-name"_code")
              ($",
                "col-name"_system, "col-name"_code")))
         (else "")))
      (("string")
       (if (search-is-humanname? search)
           ",
                family_name, given_name, prefix, suffix, name_text"
           ""))
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
      (else ""))))

; Helper: Get placeholders for INSERT VALUES ($5, $6, ...)
(define (get-insert-placeholders searches)
  (let ((count (count-insert-params searches 0)))
    (let loop ((i 5) (result ""))
      (if (> i (+ 4 count))
          result
          (loop (+ i 1) ($ result ", $" (number->string i)))))))

; Helper: Count number of parameters
(define (count-insert-params searches current)
  (if (null? searches)
      current
      (count-insert-params
        (cdr searches)
        (+ current (count-params-for-search (car searches))))))

(define (count-params-for-search search)
  (let ((search-type (% "type" search)))
    (case search-type
      (("composite" "special") 0)
      (("token")
       (cond
         ((search-is-simple-code? search) 1)
         ((search-is-contactpoint? search) 1)
         ((search-is-identifier? search) 2)
         ((search-has-codeableconcept-variant? search) 1)
         ((or (search-is-codeableconcept? search)) 2)
         (else 0)))
      (("string")
       (if (search-is-humanname? search) 5 0))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             (+ (if (member "dateTime" variant-types) 1 0)
                (if (member "element" variant-types) 2 0)))
           1))
      (("reference") 1)
      (("quantity") 3)
      (else 0))))

; Helper: Get single parameter binding for a search
(define (get-param-binding-for-search search params-var)
  (let* ((search-name (% "name" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (search-type (% "type" search))
         (is-collection (search-is-collection? search))
         (property (search-property search)))
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
         ((search-is-contactpoint? search)
          ($",
            "params-var"
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&"params-var".telecom_value[..]))"))
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
                .unwrap_or(Some(&"params-var"."col-name"_code[..]))"))
              ($",
            "params-var"."col-name"_system,
            "params-var"."col-name"_code")))
         (else "")))
      (("string")
       (if (search-is-humanname? search)
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
                .unwrap_or(Some(&"params-var".name_text[..]))")
           ""))
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
      (else ""))))

; Helper: Get parameter bindings for INSERT/UPDATE (calls get-param-binding-for-search above)
(define (get-param-bindings searches)
  (if (null? searches)
      ""
      (apply $ (map (lambda (search)
                      (get-param-binding-for-search search "params"))
                    searches))))

; Helper: Get UPDATE SET assignments
(define (get-update-assignments searches)
  (if (null? searches)
      ""
      (apply $ (map get-update-assignment-for-search searches))))

(define (get-update-assignment-for-search search)
  (let* ((search-name (% "name" search))
         (col-name (camel-to-snake (string-replace search-name "-" "_")))
         (search-type (% "type" search)))
    (case search-type
      (("composite" "special") "")
      (("token")
       (cond
         ((search-is-simple-code? search) ($",
                    "col-name" = $5"))
         ((search-is-contactpoint? search) ($",
                    "col-name"_value = $5"))
         ((search-is-identifier? search) ($",
                    identifier_system = $5,
                    identifier_value = $6"))
         ((search-has-codeableconcept-variant? search) ($",
                    "col-name"_code = $5"))
         ((search-is-codeableconcept? search) ($",
                    "col-name"_system = $5,
                    "col-name"_code = $6"))
         (else "")))
      (("string")
       (if (search-is-humanname? search)
           ($",
                    family_name = $5,
                    given_name = $6,
                    prefix = $7,
                    suffix = $8,
                    name_text = $9")
           ""))
      (("date")
       (if (search-has-variants? search)
           (let ((variant-types (search-variant-types search)))
             ($ (if (member "dateTime" variant-types)
                    ($",
                    "col-name"_datetime = $5")
                    "")
                (if (member "element" variant-types)
                    ($",
                    "col-name"_period_start = $5,
                    "col-name"_period_end = $6")
                    "")))
           ($",
                    "col-name" = $5")))
      (("reference") ($",
                    "col-name"_reference = $5"))
      (("quantity") ($",
                    "col-name"_value = $5,
                    "col-name"_unit = $6,
                    "col-name"_system = $7"))
      (else ""))))

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
                VALUES ($1, $2, $3, $4"history-placeholders", $"(+ 5 (length (node-list->list searches)))", $"(+ 6 (length (node-list->list searches)))")
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
            VALUES ($1, $2, $3, $4"history-placeholders", $"(+ 5 (length (node-list->list searches)))", $"(+ 6 (length (node-list->list searches)))")
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
            VALUES ($1, $2, $3, $4"history-placeholders", $"(+ 5 (length (node-list->list searches)))", $"(+ 6 (length (node-list->list searches)))")
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

        let history = sqlx::query_as!(
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

        Ok(history)
    }

"))

; Generate READ_VERSION method
(define (generate-read-version-method resource-name resource-lower)
  ($"    /// Read a specific version of a "resource-lower" from history
    pub async fn read_version(&""self, id: &""str, version_id: i32) -> Result<"resource-name"History> {
        let history = sqlx::query_as!(
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
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(history)
    }
"))
