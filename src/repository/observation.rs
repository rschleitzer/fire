use chrono::Utc;
use serde_json::Value;
use sqlx::{PgPool, Row};
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{
    extract_observation_search_params, Observation, ObservationHistory,
};
use crate::models::patient::Patient;
use crate::search::{SearchCondition, SearchQuery};
use crate::services::validate_observation;

// Helper function to capitalize first letter
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Recursively build JOINs and WHERE conditions for a chained search
/// Returns (joins, where_clause, bind_count_increment, bind_values)
fn build_chain_joins(
    chain: &[crate::search::ChainLink],
    _source_table: &str,
    source_alias: &str,
    reference_field: &str,
    join_counter: &mut usize,
    bind_counter: &mut usize,
    search_value: &str,
) -> Result<(Vec<String>, Option<String>, Vec<String>)> {
    if chain.is_empty() {
        return Ok((Vec::new(), None, Vec::new()));
    }

    let current_param = &chain[0].param;
    let remaining_chain = &chain[1..];

    // Determine if this is a reference parameter (leads to another resource)
    // or a search parameter (final value to search for)
    let is_reference = !remaining_chain.is_empty();

    if is_reference {
        // This is a reference to another resource - need to follow it
        // Determine target table based on reference parameter name
        let target_table = match current_param.as_str() {
            "general-practitioner" => "practitioner",
            "subject" => "patient", // Could be other types, but for Observation it's usually Patient
            "patient" => "patient",
            _ => {
                return Err(FhirError::InvalidSearchParameter(format!(
                    "Unsupported reference parameter in chain: {}",
                    current_param
                )))
            }
        };

        let alias = format!("chain_{}", *join_counter);
        *join_counter += 1;

        // Get the reference column for THIS join from the source table
        // The reference_field parameter should already contain the fully qualified column name
        // (e.g., "chain_0.general_practitioner_reference" or "observation.patient_reference")
        let reference_type = capitalize_first(target_table);
        let join_sql = format!(
            "INNER JOIN {} AS {} ON {}.id::text IN (SELECT substring(ref from '{}/'||'(.*)') FROM unnest(ARRAY[{}]) AS refs(ref) WHERE ref LIKE '{}/%')",
            target_table,
            alias,
            alias,
            reference_type,
            reference_field,
            reference_type
        );

        // Determine the reference field for the next level (if any)
        // Only get the reference column if the next parameter is also a reference
        // (i.e., if there are more than 1 elements remaining in the chain)
        let next_reference_field = if remaining_chain.len() > 1 {
            format!("{}.{}", alias, get_reference_column(target_table, &remaining_chain[0].param)?)
        } else {
            // Next param is the final search parameter, no reference field needed
            String::new()
        };

        // Recurse to build the rest of the chain
        let (mut sub_joins, sub_where, sub_binds) = build_chain_joins(
            remaining_chain,
            target_table,
            &alias,
            &next_reference_field,
            join_counter,
            bind_counter,
            search_value,
        )?;

        // Prepend our JOIN to the sub-joins
        let mut all_joins = vec![join_sql];
        all_joins.append(&mut sub_joins);

        Ok((all_joins, sub_where, sub_binds))
    } else {
        // This is the final search parameter - generate WHERE clause
        *bind_counter += 1;
        let bind_num = *bind_counter;

        let (where_clause, bind_value) = match current_param.as_str() {
            "family" => (
                format!(
                    "EXISTS (SELECT 1 FROM unnest({}.family_name) AS fn WHERE fn ILIKE ${})",
                    source_alias, bind_num
                ),
                format!("{}%", search_value),
            ),
            "given" => (
                format!(
                    "EXISTS (SELECT 1 FROM unnest({}.given_name) AS gn WHERE gn ILIKE ${})",
                    source_alias, bind_num
                ),
                format!("{}%", search_value),
            ),
            "identifier" => (
                format!(
                    "EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${})",
                    source_alias, bind_num
                ),
                search_value.to_string(),
            ),
            "name" => (
                format!(
                    "(EXISTS (SELECT 1 FROM unnest({}.family_name) AS fn WHERE fn ILIKE ${}) OR EXISTS (SELECT 1 FROM unnest({}.given_name) AS gn WHERE gn ILIKE ${}))",
                    source_alias, bind_num, source_alias, bind_num
                ),
                format!("{}%", search_value),
            ),
            "telecom" | "email" => (
                format!(
                    "EXISTS (SELECT 1 FROM unnest({}.telecom_value) AS tv WHERE tv ILIKE ${})",
                    source_alias, bind_num
                ),
                format!("%{}%", search_value),
            ),
            _ => {
                *bind_counter -= 1; // Undo the increment
                return Err(FhirError::InvalidSearchParameter(format!(
                    "Unsupported search parameter in chain: {}",
                    current_param
                )));
            }
        };

        Ok((Vec::new(), Some(where_clause), vec![bind_value]))
    }
}

/// Get the reference column name for a given table and reference parameter
fn get_reference_column(table: &str, param: &str) -> Result<String> {
    match (table, param) {
        ("patient", "general-practitioner") => Ok("general_practitioner_reference".to_string()),
        ("observation", "subject") => Ok("subject_reference".to_string()),
        ("observation", "patient") => Ok("patient_reference".to_string()),
        _ => Err(FhirError::InvalidSearchParameter(format!(
            "Unsupported reference parameter '{}' for table '{}'",
            param, table
        ))),
    }
}

pub struct ObservationRepository {
    pool: PgPool,
}

impl ObservationRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// Create a new observation resource (version 1)
    pub async fn create(&self, mut content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let id = Uuid::new_v4().to_string();
        let version_id = 1;
        let last_updated = Utc::now();

        // Inject id and meta fields into content before storing
        content = crate::models::observation::inject_id_meta(&content, &id, version_id, &last_updated);

        // Extract search parameters from complete content
        let params = extract_observation_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Convert f64 to BigDecimal for database
        let value_qty_decimal = params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v)
                .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
        });

        // Insert into current table (simplified - not using query_as! due to complexity)
        sqlx::query!(
            r#"
            INSERT INTO observation (
                id, version_id, last_updated, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                date_datetime, date_period_start, date_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26)
            "#,
            id,
            version_id,
            last_updated,
            content,
            params.status,
            params.category_system.is_empty().then_some(None).unwrap_or(Some(&params.category_system[..])),
            params.category_code.is_empty().then_some(None).unwrap_or(Some(&params.category_code[..])),
            params.code_system,
            params.code_code,
            params.subject_reference,
            params.patient_reference,
            params.encounter_reference,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.issued,
            value_qty_decimal,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&params.value_codeable_concept_code[..])),
            params.value_string,
            params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&params.performer_reference[..])),
            params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_observation[..])),
            params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_type[..])),
            params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&params.focus_reference[..])),
            params.body_structure_reference,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created observation
        self.read(&id).await
    }

    /// Upsert an observation resource with specific ID (FHIR-compliant PUT)
    /// Creates with client-specified ID if doesn't exist, updates if exists
    pub async fn upsert(&self, id: &str, mut content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let mut tx = self.pool.begin().await?;

        // Check if resource exists
        let existing = sqlx::query_as!(
            Observation,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM observation
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?;

        if let Some(old_observation) = existing {
            // Resource exists - perform update
            let new_version_id = old_observation.version_id + 1;
            let last_updated = Utc::now();

            tracing::info!(observation_id = %id, old_version = old_observation.version_id, new_version = new_version_id, "Updating existing observation");

            // Clean up any orphaned history records that might conflict (from previous incomplete operations)
            // This handles the case where a previous operation failed mid-transaction
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM observation_history
                WHERE id = $1 AND version_id >= $2
                "#,
                id,
                old_observation.version_id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::warn!(observation_id = %id, deleted_versions = deleted_rows.rows_affected(), "Cleaned up orphaned history records before update");
            }

            // Inject id and meta into content before storing
            content = crate::models::observation::inject_id_meta(&content, id, new_version_id, &last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_observation_search_params(&old_observation.content);

            // Convert f64 to BigDecimal for old params
            let old_value_qty_decimal = old_params.value_quantity_value.map(|v| {
                sqlx::types::BigDecimal::try_from(v)
                    .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
            });

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO observation_history (
                    id, version_id, last_updated, content,
                    status, category_system, category_code,
                    code_system, code_code,
                    subject_reference, patient_reference, encounter_reference,
                    date_datetime, date_period_start, date_period_end,
                    issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                    value_codeable_concept_code, value_string, performer_reference,
                    triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28)
                "#,
                old_observation.id,
                old_observation.version_id,
                old_observation.last_updated,
                &old_observation.content,
                old_params.status,
                old_params.category_system.is_empty().then_some(None).unwrap_or(Some(&old_params.category_system[..])),
                old_params.category_code.is_empty().then_some(None).unwrap_or(Some(&old_params.category_code[..])),
                old_params.code_system,
                old_params.code_code,
                old_params.subject_reference,
                old_params.patient_reference,
                old_params.encounter_reference,
                old_params.date_datetime,
                old_params.date_period_start,
                old_params.date_period_end,
                old_params.issued,
                old_value_qty_decimal,
                old_params.value_quantity_unit,
                old_params.value_quantity_system,
                old_params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&old_params.value_codeable_concept_code[..])),
                old_params.value_string,
                old_params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&old_params.performer_reference[..])),
                old_params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&old_params.triggered_by_observation[..])),
                old_params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&old_params.triggered_by_type[..])),
                old_params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&old_params.focus_reference[..])),
                old_params.body_structure_reference,
                if old_observation.version_id == 1 { "CREATE" } else { "UPDATE" },
                Utc::now(),
            )
            .execute(&mut *tx)
            .await?;

            // Extract search parameters for new content
            let params = extract_observation_search_params(&content);

            // Convert f64 to BigDecimal
            let value_qty_decimal = params.value_quantity_value.map(|v| {
                sqlx::types::BigDecimal::try_from(v)
                    .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
            });

            // Update current table with new version
            sqlx::query!(
                r#"
                UPDATE observation
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    status = $5,
                    category_system = $6,
                    category_code = $7,
                    code_system = $8,
                    code_code = $9,
                    subject_reference = $10,
                    patient_reference = $11,
                    encounter_reference = $12,
                    date_datetime = $13,
                    date_period_start = $14,
                    date_period_end = $15,
                    issued = $16,
                    value_quantity_value = $17,
                    value_quantity_unit = $18,
                    value_quantity_system = $19,
                    value_codeable_concept_code = $20,
                    value_string = $21,
                    performer_reference = $22,
                    triggered_by_observation = $23,
                    triggered_by_type = $24,
                    focus_reference = $25,
                    body_structure_reference = $26
                WHERE id = $1
                "#,
                id,
                new_version_id,
                last_updated,
                content,
                params.status,
                params.category_system.is_empty().then_some(None).unwrap_or(Some(&params.category_system[..])),
                params.category_code.is_empty().then_some(None).unwrap_or(Some(&params.category_code[..])),
                params.code_system,
                params.code_code,
                params.subject_reference,
                params.patient_reference,
                params.encounter_reference,
                params.date_datetime,
                params.date_period_start,
                params.date_period_end,
                params.issued,
                value_qty_decimal,
                params.value_quantity_unit,
                params.value_quantity_system,
                params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&params.value_codeable_concept_code[..])),
                params.value_string,
                params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&params.performer_reference[..])),
                params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_observation[..])),
                params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_type[..])),
                params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&params.focus_reference[..])),
                params.body_structure_reference,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return updated observation
            self.read(id).await
        } else {
            // Resource doesn't exist - perform create with specified ID
            let version_id = 1;
            let last_updated = Utc::now();

            tracing::info!(observation_id = %id, "Creating observation with client-specified ID");

            // Clean up any orphaned history records for this ID (from previous delete)
            // This handles the case where a resource was deleted but history records remain
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM observation_history
                WHERE id = $1
                "#,
                id
            )
            .execute(&mut *tx)
            .await?;

            tracing::info!(observation_id = %id, orphaned_history_records = deleted_rows.rows_affected(), "Cleaned up orphaned history records before creating new resource");

            // Inject id and meta into content before storing
            content = crate::models::observation::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_observation_search_params(&content);

            // Convert f64 to BigDecimal
            let value_qty_decimal = params.value_quantity_value.map(|v| {
                sqlx::types::BigDecimal::try_from(v)
                    .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
            });

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO observation (
                    id, version_id, last_updated, content,
                    status, category_system, category_code,
                    code_system, code_code,
                    subject_reference, patient_reference, encounter_reference,
                    date_datetime, date_period_start, date_period_end,
                    issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                    value_codeable_concept_code, value_string, performer_reference,
                    triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26)
                "#,
                id,
                version_id,
                last_updated,
                content,
                params.status,
                params.category_system.is_empty().then_some(None).unwrap_or(Some(&params.category_system[..])),
                params.category_code.is_empty().then_some(None).unwrap_or(Some(&params.category_code[..])),
                params.code_system,
                params.code_code,
                params.subject_reference,
                params.patient_reference,
                params.encounter_reference,
                params.date_datetime,
                params.date_period_start,
                params.date_period_end,
                params.issued,
                value_qty_decimal,
                params.value_quantity_unit,
                params.value_quantity_system,
                params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&params.value_codeable_concept_code[..])),
                params.value_string,
                params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&params.performer_reference[..])),
                params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_observation[..])),
                params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_type[..])),
                params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&params.focus_reference[..])),
                params.body_structure_reference,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return the created observation
            self.read(id).await
        }
    }

    /// Read current version of an observation (returns raw JSON)
    pub async fn read(&self, id: &str) -> Result<Observation> {
        let observation = sqlx::query_as!(
            Observation,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM observation
            WHERE id = $1
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(observation)
    }

    /// Delete an observation (hard delete)
    pub async fn delete(&self, id: &str) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current observation (to store in history)
        let observation = self.read(id).await?;
        let new_version_id = observation.version_id + 1;
        let last_updated = Utc::now();

        // Extract search parameters from content for history
        let params = extract_observation_search_params(&observation.content);

        // Convert f64 to BigDecimal
        let value_qty_decimal = params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v)
                .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
        });

        // Delete from current table
        sqlx::query!(
            r#"
            DELETE FROM observation
            WHERE id = $1
            "#,
            id,
        )
        .execute(&mut *tx)
        .await?;

        // Insert delete record into history
        sqlx::query!(
            r#"
            INSERT INTO observation_history (
                id, version_id, last_updated, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                date_datetime, date_period_start, date_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28)
            "#,
            id,
            new_version_id,
            last_updated,
            &observation.content,
            params.status,
            params.category_system.is_empty().then_some(None).unwrap_or(Some(&params.category_system[..])),
            params.category_code.is_empty().then_some(None).unwrap_or(Some(&params.category_code[..])),
            params.code_system,
            params.code_code,
            params.subject_reference,
            params.patient_reference,
            params.encounter_reference,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.issued,
            value_qty_decimal,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&params.value_codeable_concept_code[..])),
            params.value_string,
            params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&params.performer_reference[..])),
            params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_observation[..])),
            params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_type[..])),
            params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&params.focus_reference[..])),
            params.body_structure_reference,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }

    /// Update an observation resource (increment version)
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get current version with lock (need full data to store in history)
        let old_observation = self.read(id).await?;

        let new_version_id = old_observation.version_id + 1;
        let last_updated = Utc::now();

        // Inject id and meta into new content before storing
        content = crate::models::observation::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search parameters from OLD content for history
        let old_params = extract_observation_search_params(&old_observation.content);

        // Convert f64 to BigDecimal for old params
        let old_value_qty_decimal = old_params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v)
                .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
        });

        // Insert OLD version into history before updating
        sqlx::query!(
            r#"
            INSERT INTO observation_history (
                id, version_id, last_updated, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                date_datetime, date_period_start, date_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28)
            "#,
            old_observation.id,
            old_observation.version_id,
            old_observation.last_updated,
            &old_observation.content,
            old_params.status,
            old_params.category_system.is_empty().then_some(None).unwrap_or(Some(&old_params.category_system[..])),
            old_params.category_code.is_empty().then_some(None).unwrap_or(Some(&old_params.category_code[..])),
            old_params.code_system,
            old_params.code_code,
            old_params.subject_reference,
            old_params.patient_reference,
            old_params.encounter_reference,
            old_params.date_datetime,
            old_params.date_period_start,
            old_params.date_period_end,
            old_params.issued,
            old_value_qty_decimal,
            old_params.value_quantity_unit,
            old_params.value_quantity_system,
            old_params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&old_params.value_codeable_concept_code[..])),
            old_params.value_string,
            old_params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&old_params.performer_reference[..])),
            old_params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&old_params.triggered_by_observation[..])),
            old_params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&old_params.triggered_by_type[..])),
            old_params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&old_params.focus_reference[..])),
            old_params.body_structure_reference,
            if old_observation.version_id == 1 { "CREATE" } else { "UPDATE" },
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_observation_search_params(&content);

        // Convert f64 to BigDecimal
        let value_qty_decimal = params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v)
                .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
        });

        // Update current table with new version
        sqlx::query!(
            r#"
            UPDATE observation
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                status = $5,
                category_system = $6,
                category_code = $7,
                code_system = $8,
                code_code = $9,
                subject_reference = $10,
                patient_reference = $11,
                encounter_reference = $12,
                date_datetime = $13,
                date_period_start = $14,
                date_period_end = $15,
                issued = $16,
                value_quantity_value = $17,
                value_quantity_unit = $18,
                value_quantity_system = $19,
                value_codeable_concept_code = $20,
                value_string = $21,
                performer_reference = $22,
                triggered_by_observation = $23,
                triggered_by_type = $24,
                focus_reference = $25,
                body_structure_reference = $26
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params.status,
            params
                .category_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.category_system[..])),
            params
                .category_code
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.category_code[..])),
            params.code_system,
            params.code_code,
            params.subject_reference,
            params.patient_reference,
            params.encounter_reference,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.issued,
            value_qty_decimal,
            params.value_quantity_unit,
            params.value_quantity_system,
            params
                .value_codeable_concept_code
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.value_codeable_concept_code[..])),
            params.value_string,
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params
                .triggered_by_observation
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.triggered_by_observation[..])),
            params
                .triggered_by_type
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.triggered_by_type[..])),
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params.body_structure_reference,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated observation
        self.read(id).await
    }

    /// Get all versions of an observation from history (includes current version if exists)
    pub async fn history(&self, id: &str) -> Result<Vec<ObservationHistory>> {
        let mut history = sqlx::query_as!(
            ObservationHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                history_operation, history_timestamp
            FROM observation_history
            WHERE id = $1
            ORDER BY version_id DESC
            "#,
            id
        )
        .fetch_all(&self.pool)
        .await?;

        // Try to get current version and add it to history if not already present
        if let Ok(current) = self.read(id).await {
            // Check if current version already exists in history
            let current_version_exists = history.iter().any(|h| h.version_id == current.version_id);

            if !current_version_exists {
                // Convert current Observation to ObservationHistory format
                let current_as_history = ObservationHistory {
                    id: current.id,
                    version_id: current.version_id,
                    last_updated: current.last_updated,
                    content: current.content,
                    history_operation: if current.version_id == 1 {
                        "CREATE".to_string()
                    } else {
                        "UPDATE".to_string()
                    },
                    history_timestamp: current.last_updated,
                };
                history.insert(0, current_as_history);
            }
        }

        if history.is_empty() {
            return Err(FhirError::NotFound);
        }

        Ok(history)
    }

    /// Read a specific version from history (checks current version first)
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<ObservationHistory> {
        // Check if requested version is the current version
        if let Ok(current) = self.read(id).await {
            if current.version_id == version_id {
                // Convert to ObservationHistory format
                return Ok(ObservationHistory {
                    id: current.id,
                    version_id: current.version_id,
                    last_updated: current.last_updated,
                    content: current.content,
                    history_operation: if current.version_id == 1 {
                        "CREATE".to_string()
                    } else {
                        "UPDATE".to_string()
                    },
                    history_timestamp: current.last_updated,
                });
            }
        }

        // Not current version, check history table
        let observation = sqlx::query_as!(
            ObservationHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                history_operation, history_timestamp
            FROM observation_history
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(observation)
    }

    /// Search observations with optional total count
    pub async fn search(
        &self,
        params: &HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Observation>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        // Track JOINs for chained searches
        let mut joins = Vec::new();
        let mut join_counter = 0;

        let mut sql = String::from(
            r#"SELECT observation.id, observation.version_id, observation.last_updated, observation.content
               FROM observation WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();
        let mut bind_count = 0;

        for condition in &query.conditions {
            match condition {
                SearchCondition::ObservationStatus(status) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND observation.status = ${}", bind_count));
                    bind_values.push(status.clone());
                }
                SearchCondition::ObservationCode { system, code } => {
                    if let Some(sys) = system {
                        bind_count += 2;
                        sql.push_str(&format!(
                            " AND observation.code_system = ${} AND observation.code_code = ${}",
                            bind_count - 1,
                            bind_count
                        ));
                        bind_values.push(sys.clone());
                        bind_values.push(code.clone());
                    } else {
                        bind_count += 1;
                        sql.push_str(&format!(" AND observation.code_code = ${}", bind_count));
                        bind_values.push(code.clone());
                    }
                }
                SearchCondition::ObservationCategory(category) => {
                    bind_count += 1;
                    sql.push_str(&format!(
                        " AND ${} = ANY(observation.category_code)",
                        bind_count
                    ));
                    bind_values.push(category.clone());
                }
                SearchCondition::ObservationPatient(patient_id) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND observation.patient_reference = ${}", bind_count));
                    bind_values.push(format!("Patient/{}", patient_id));
                }
                SearchCondition::ObservationSubject(subject_ref) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND observation.subject_reference = ${}", bind_count));
                    bind_values.push(subject_ref.clone());
                }
                SearchCondition::ObservationDate(comparison) => {
                    bind_count += 1;
                    let op = match comparison.prefix {
                        crate::search::DatePrefix::Eq => "=",
                        crate::search::DatePrefix::Ne => "!=",
                        crate::search::DatePrefix::Gt => ">",
                        crate::search::DatePrefix::Lt => "<",
                        crate::search::DatePrefix::Ge => ">=",
                        crate::search::DatePrefix::Le => "<=",
                    };
                    sql.push_str(&format!(
                        " AND observation.date_datetime {} ${}::timestamptz",
                        op, bind_count
                    ));
                    // Convert NaiveDate to string format (YYYY-MM-DD) - PostgreSQL will handle the comparison
                    bind_values.push(comparison.value.format("%Y-%m-%d").to_string());
                }
                SearchCondition::ObservationCodeValueQuantity(composite) => {
                    // Composite search: code AND value-quantity
                    let op = match composite.value_prefix {
                        crate::search::QuantityPrefix::Eq => "=",
                        crate::search::QuantityPrefix::Ne => "!=",
                        crate::search::QuantityPrefix::Gt => ">",
                        crate::search::QuantityPrefix::Lt => "<",
                        crate::search::QuantityPrefix::Ge => ">=",
                        crate::search::QuantityPrefix::Le => "<=",
                    };
                    if let Some(sys) = &composite.code_system {
                        bind_count += 3; // system, code, value
                        sql.push_str(&format!(
                            " AND observation.code_system = ${} AND observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric",
                            bind_count - 2, bind_count - 1, op, bind_count
                        ));
                        bind_values.push(sys.clone());
                        bind_values.push(composite.code.clone());
                        bind_values.push(composite.value.to_string());
                    } else {
                        bind_count += 2; // code, value
                        sql.push_str(&format!(
                            " AND observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric",
                            bind_count - 1, op, bind_count
                        ));
                        bind_values.push(composite.code.clone());
                        bind_values.push(composite.value.to_string());
                    }
                }
                SearchCondition::ObservationCodeValueConcept(composite) => {
                    // Composite search: code AND value-codeable-concept
                    if let Some(sys) = &composite.code_system {
                        bind_count += 3; // code_system, code, value_code
                        sql.push_str(&format!(
                            " AND observation.code_system = ${} AND observation.code_code = ${} AND ${} = ANY(observation.value_codeable_concept_code)",
                            bind_count - 2, bind_count - 1, bind_count
                        ));
                        bind_values.push(sys.clone());
                        bind_values.push(composite.code.clone());
                        bind_values.push(composite.value_code.clone());
                    } else {
                        bind_count += 2; // code, value_code
                        sql.push_str(&format!(
                            " AND observation.code_code = ${} AND ${} = ANY(observation.value_codeable_concept_code)",
                            bind_count - 1, bind_count
                        ));
                        bind_values.push(composite.code.clone());
                        bind_values.push(composite.value_code.clone());
                    }
                }
                SearchCondition::ObservationComponentCodeValueQuantity(composite) => {
                    // Component composite search: searches within component array in JSONB
                    let op = match composite.value_prefix {
                        crate::search::QuantityPrefix::Eq => "=",
                        crate::search::QuantityPrefix::Ne => "!=",
                        crate::search::QuantityPrefix::Gt => ">",
                        crate::search::QuantityPrefix::Lt => "<",
                        crate::search::QuantityPrefix::Ge => ">=",
                        crate::search::QuantityPrefix::Le => "<=",
                    };

                    if let Some(sys) = &composite.component_code_system {
                        bind_count += 3; // system, code, value
                        sql.push_str(&format!(
                            " AND EXISTS (
                                SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp
                                WHERE comp->'code'->'coding'->0->>'system' = ${}
                                AND comp->'code'->'coding'->0->>'code' = ${}
                                AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric
                            )",
                            bind_count - 2, bind_count - 1, op, bind_count
                        ));
                        bind_values.push(sys.clone());
                        bind_values.push(composite.component_code.clone());
                        bind_values.push(composite.value.to_string());
                    } else {
                        bind_count += 2; // code, value
                        sql.push_str(&format!(
                            " AND EXISTS (
                                SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp
                                WHERE comp->'code'->'coding'->0->>'code' = ${}
                                AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric
                            )",
                            bind_count - 1, op, bind_count
                        ));
                        bind_values.push(composite.component_code.clone());
                        bind_values.push(composite.value.to_string());
                    }
                }
                SearchCondition::ForwardChain(chain) => {
                    // Forward chaining for Observation: follow subject reference to Patient
                    // Supports arbitrary depth: subject:Patient.family=Brown OR subject:Patient.general-practitioner.family=Smith

                    // Determine target table and reference column for the first hop
                    let (target_table, reference_column) = match chain.reference_param.as_str() {
                        "subject" if chain.resource_type.as_deref() == Some("Patient") => {
                            ("patient", "observation.patient_reference")
                        }
                        "subject" => {
                            // Generic subject - could be various types, default to patient
                            ("patient", "observation.subject_reference")
                        }
                        "patient" => ("patient", "observation.patient_reference"),
                        _ => {
                            tracing::warn!(
                                "Unsupported chained reference in observation: {}",
                                chain.reference_param
                            );
                            continue;
                        }
                    };

                    // Create the first JOIN to the target resource
                    let alias = format!("chain_{}", join_counter);
                    join_counter += 1;

                    let reference_type = capitalize_first(target_table);
                    let join_sql = format!(
                        "INNER JOIN {} AS {} ON {}.id::text IN (SELECT substring(ref from '{}/'||'(.*)') FROM unnest(ARRAY[{}]) AS refs(ref) WHERE ref LIKE '{}/%')",
                        target_table,
                        alias,
                        alias,
                        reference_type,
                        reference_column,
                        reference_type
                    );
                    joins.push(join_sql);

                    // Now recursively build the rest of the chain starting from the target resource
                    // Need to determine the reference field for the next level if the chain continues
                    let next_reference_field = if chain.chain.len() > 1 {
                        // There's at least one more reference in the chain
                        // Get the reference column for the first element of the remaining chain
                        match get_reference_column(target_table, &chain.chain[0].param) {
                            Ok(col) => format!("{}.{}", alias, col),
                            Err(_) => String::new(), // Will be handled by recursive call
                        }
                    } else {
                        // Only one element left in chain - it's the final search parameter
                        String::new()
                    };

                    match build_chain_joins(
                        &chain.chain,
                        target_table,
                        &alias,
                        &next_reference_field,
                        &mut join_counter,
                        &mut bind_count,
                        &chain.search_value,
                    ) {
                        Ok((chain_joins, chain_where, mut chain_binds)) => {
                            joins.extend(chain_joins);
                            if let Some(where_clause) = chain_where {
                                sql.push_str(&format!(" AND {}", where_clause));
                            }
                            bind_values.append(&mut chain_binds);
                        }
                        Err(e) => {
                            tracing::error!("Error building chain joins: {}", e);
                            continue;
                        }
                    }
                }
                _ => {
                    // Other search conditions not yet mapped for observations
                    tracing::warn!("Unsupported search condition for observations: {:?}", condition);
                }
            }
        }

        // Add JOINs before WHERE clause
        if !joins.is_empty() {
            let where_pos = sql.find("WHERE").expect("WHERE clause not found");
            let before_where = &sql[..where_pos];
            let after_where = &sql[where_pos..];
            sql = format!("{} {} {}", before_where, joins.join(" "), after_where);
        }

        // Build ORDER BY clause
        if !query.sort.is_empty() {
            sql.push_str(" ORDER BY ");
            for (i, sort) in query.sort.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&sort.field);
                match sort.direction {
                    crate::search::SortDirection::Ascending => sql.push_str(" ASC"),
                    crate::search::SortDirection::Descending => sql.push_str(" DESC"),
                }
            }
        } else {
            // Default sort by last_updated DESC
            sql.push_str(" ORDER BY observation.last_updated DESC");
        }

        // Add LIMIT and OFFSET
        sql.push_str(&format!(" LIMIT {}", query.limit));
        sql.push_str(&format!(" OFFSET {}", query.offset));

        // Debug logging
        tracing::debug!("Generated Observation SQL: {}", sql);
        tracing::debug!("Bind values: {:?}", bind_values);

        // Build dynamic query
        let mut query_builder = sqlx::query(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let rows = query_builder.fetch_all(&self.pool).await?;

        let observations = rows
            .into_iter()
            .map(|row| Observation {
                id: row.get("id"),
                version_id: row.get("version_id"),
                last_updated: row.get("last_updated"),
                content: row.get("content"),
            })
            .collect();

        // Get total count if requested
        let total = if include_total {
            let count_sql = build_count_sql(&query);
            let mut count_query_builder = sqlx::query(&count_sql);
            for value in &bind_values {
                count_query_builder = count_query_builder.bind(value);
            }
            let count_row = count_query_builder.fetch_one(&self.pool).await?;
            Some(count_row.get::<i64, _>(0))
        } else {
            None
        };

        Ok((observations, total))
    }

    /// Read a patient by ID (for _include support)
    pub async fn read_patient(&self, id: &str) -> Result<Patient> {
        let patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM patient
            WHERE id = $1
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(patient)
    }

    /// Rollback an observation to a specific version (deletes all versions >= rollback_to_version)
    /// This is a destructive operation for dev/test purposes only
    pub async fn rollback(&self, id: &str, rollback_to_version: i32) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get all version IDs for this observation from history
        let version_ids: Vec<i32> = sqlx::query_scalar!(
            r#"
            SELECT version_id
            FROM observation_history
            WHERE id = $1
            ORDER BY version_id ASC
            "#,
            id
        )
        .fetch_all(&mut *tx)
        .await?;

        // Check if current resource exists and add its version
        let current_version = sqlx::query_scalar!(
            r#"
            SELECT version_id
            FROM observation
            WHERE id = $1
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?;

        let mut all_versions = version_ids;
        if let Some(cv) = current_version {
            if !all_versions.contains(&cv) {
                all_versions.push(cv);
            }
        }
        all_versions.sort();

        // Find versions to delete (>= rollback_to_version)
        let versions_to_delete: Vec<i32> = all_versions
            .iter()
            .filter(|&&v| v >= rollback_to_version)
            .copied()
            .collect();

        // Find highest version that remains (< rollback_to_version)
        let new_current_version = all_versions
            .iter()
            .filter(|&&v| v < rollback_to_version)
            .max()
            .copied();

        if versions_to_delete.is_empty() {
            return Err(FhirError::BadRequest(format!(
                "No versions to rollback for observation {}",
                id
            )));
        }

        tracing::info!(
            observation_id = %id,
            rollback_to = rollback_to_version,
            deleting_versions = ?versions_to_delete,
            new_current = ?new_current_version,
            "Rolling back observation"
        );

        // Delete versions from history
        for version in &versions_to_delete {
            sqlx::query!(
                r#"
                DELETE FROM observation_history
                WHERE id = $1 AND version_id = $2
                "#,
                id,
                version
            )
            .execute(&mut *tx)
            .await?;
        }

        // Update or delete current resource
        if let Some(new_version) = new_current_version {
            // Restore the previous version as current
            let restored = self.read_version(id, new_version).await?;

            let params = extract_observation_search_params(&restored.content);

            // Convert f64 to BigDecimal
            let value_qty_decimal = params.value_quantity_value.map(|v| {
                sqlx::types::BigDecimal::try_from(v)
                    .unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
            });

            sqlx::query!(
                r#"
                UPDATE observation
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    status = $5,
                    category_system = $6,
                    category_code = $7,
                    code_system = $8,
                    code_code = $9,
                    subject_reference = $10,
                    patient_reference = $11,
                    encounter_reference = $12,
                    date_datetime = $13,
                    date_period_start = $14,
                    date_period_end = $15,
                    issued = $16,
                    value_quantity_value = $17,
                    value_quantity_unit = $18,
                    value_quantity_system = $19,
                    value_codeable_concept_code = $20,
                    value_string = $21,
                    performer_reference = $22,
                    triggered_by_observation = $23,
                    triggered_by_type = $24,
                    focus_reference = $25,
                    body_structure_reference = $26
                WHERE id = $1
                "#,
                id,
                new_version,
                restored.last_updated,
                &restored.content,
                params.status,
                params.category_system.is_empty().then_some(None).unwrap_or(Some(&params.category_system[..])),
                params.category_code.is_empty().then_some(None).unwrap_or(Some(&params.category_code[..])),
                params.code_system,
                params.code_code,
                params.subject_reference,
                params.patient_reference,
                params.encounter_reference,
                params.date_datetime,
                params.date_period_start,
                params.date_period_end,
                params.issued,
                value_qty_decimal,
                params.value_quantity_unit,
                params.value_quantity_system,
                params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(&params.value_codeable_concept_code[..])),
                params.value_string,
                params.performer_reference.is_empty().then_some(None).unwrap_or(Some(&params.performer_reference[..])),
                params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_observation[..])),
                params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(&params.triggered_by_type[..])),
                params.focus_reference.is_empty().then_some(None).unwrap_or(Some(&params.focus_reference[..])),
                params.body_structure_reference,
            )
            .execute(&mut *tx)
            .await?;
        } else {
            // No versions remain, delete current resource
            sqlx::query!(
                r#"
                DELETE FROM observation
                WHERE id = $1
                "#,
                id
            )
            .execute(&mut *tx)
            .await?;
        }

        tx.commit().await?;

        Ok(())
    }

    /// Purge all observation history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) observation records
    pub async fn purge(&self) -> Result<()> {
        // Delete all history records for test isolation
        // This ensures no constraint violations on repeated test runs
        sqlx::query!("DELETE FROM observation_history")
            .execute(&self.pool)
            .await?;

        Ok(())
    }
}

fn build_count_sql(query: &SearchQuery) -> String {
    let mut sql = String::from("SELECT COUNT(*) FROM observation WHERE 1=1");
    let mut bind_count = 0;

    // Track JOINs for chained searches (same as main search)
    let mut joins = Vec::new();
    let mut join_counter = 0;

    for condition in &query.conditions {
        match condition {
            SearchCondition::ObservationStatus(_status) => {
                bind_count += 1;
                sql.push_str(&format!(" AND observation.status = ${}", bind_count));
            }
            SearchCondition::ObservationCode { system, code: _ } => {
                if system.is_some() {
                    bind_count += 2;
                    sql.push_str(&format!(
                        " AND observation.code_system = ${} AND observation.code_code = ${}",
                        bind_count - 1,
                        bind_count
                    ));
                } else {
                    bind_count += 1;
                    sql.push_str(&format!(" AND observation.code_code = ${}", bind_count));
                }
            }
            SearchCondition::ObservationCategory(_category) => {
                bind_count += 1;
                sql.push_str(&format!(
                    " AND ${} = ANY(observation.category_code)",
                    bind_count
                ));
            }
            SearchCondition::ObservationPatient(_patient_id) => {
                bind_count += 1;
                sql.push_str(&format!(" AND observation.patient_reference = ${}", bind_count));
            }
            SearchCondition::ObservationSubject(_subject_ref) => {
                bind_count += 1;
                sql.push_str(&format!(" AND observation.subject_reference = ${}", bind_count));
            }
            SearchCondition::ObservationDate(comparison) => {
                bind_count += 1;
                let op = match comparison.prefix {
                    crate::search::DatePrefix::Eq => "=",
                    crate::search::DatePrefix::Ne => "!=",
                    crate::search::DatePrefix::Gt => ">",
                    crate::search::DatePrefix::Lt => "<",
                    crate::search::DatePrefix::Ge => ">=",
                    crate::search::DatePrefix::Le => "<=",
                };
                sql.push_str(&format!(
                    " AND observation.date_datetime {} ${}::timestamptz",
                    op, bind_count
                ));
            }
            SearchCondition::ObservationCodeValueQuantity(composite) => {
                // Composite search in count query
                let op = match composite.value_prefix {
                    crate::search::QuantityPrefix::Eq => "=",
                    crate::search::QuantityPrefix::Ne => "!=",
                    crate::search::QuantityPrefix::Gt => ">",
                    crate::search::QuantityPrefix::Lt => "<",
                    crate::search::QuantityPrefix::Ge => ">=",
                    crate::search::QuantityPrefix::Le => "<=",
                };
                if composite.code_system.is_some() {
                    bind_count += 3;
                    sql.push_str(&format!(
                        " AND observation.code_system = ${} AND observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric",
                        bind_count - 2, bind_count - 1, op, bind_count
                    ));
                } else {
                    bind_count += 2;
                    sql.push_str(&format!(
                        " AND observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric",
                        bind_count - 1, op, bind_count
                    ));
                }
            }
            SearchCondition::ObservationCodeValueConcept(composite) => {
                // Composite search in count query
                if composite.code_system.is_some() {
                    bind_count += 3;
                    sql.push_str(&format!(
                        " AND observation.code_system = ${} AND observation.code_code = ${} AND ${} = ANY(observation.value_codeable_concept_code)",
                        bind_count - 2, bind_count - 1, bind_count
                    ));
                } else {
                    bind_count += 2;
                    sql.push_str(&format!(
                        " AND observation.code_code = ${} AND ${} = ANY(observation.value_codeable_concept_code)",
                        bind_count - 1, bind_count
                    ));
                }
            }
            SearchCondition::ObservationComponentCodeValueQuantity(composite) => {
                // Component composite search in count query
                let op = match composite.value_prefix {
                    crate::search::QuantityPrefix::Eq => "=",
                    crate::search::QuantityPrefix::Ne => "!=",
                    crate::search::QuantityPrefix::Gt => ">",
                    crate::search::QuantityPrefix::Lt => "<",
                    crate::search::QuantityPrefix::Ge => ">=",
                    crate::search::QuantityPrefix::Le => "<=",
                };

                if composite.component_code_system.is_some() {
                    bind_count += 3;
                    sql.push_str(&format!(
                        " AND EXISTS (
                            SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp
                            WHERE comp->'code'->'coding'->0->>'system' = ${}
                            AND comp->'code'->'coding'->0->>'code' = ${}
                            AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric
                        )",
                        bind_count - 2, bind_count - 1, op, bind_count
                    ));
                } else {
                    bind_count += 2;
                    sql.push_str(&format!(
                        " AND EXISTS (
                            SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp
                            WHERE comp->'code'->'coding'->0->>'code' = ${}
                            AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric
                        )",
                        bind_count - 1, op, bind_count
                    ));
                }
            }
            SearchCondition::ForwardChain(chain) => {
                // Forward chaining in count query - use same logic as main search

                // Determine target table and reference column for the first hop
                let (target_table, reference_column) = match chain.reference_param.as_str() {
                    "subject" if chain.resource_type.as_deref() == Some("Patient") => {
                        ("patient", "observation.patient_reference")
                    }
                    "subject" => {
                        // Generic subject - could be various types, default to patient
                        ("patient", "observation.subject_reference")
                    }
                    "patient" => ("patient", "observation.patient_reference"),
                    _ => {
                        tracing::warn!(
                            "Unsupported chained reference in observation count: {}",
                            chain.reference_param
                        );
                        continue;
                    }
                };

                // Create the first JOIN to the target resource
                let alias = format!("chain_{}", join_counter);
                join_counter += 1;

                let reference_type = capitalize_first(target_table);
                let join_sql = format!(
                    "INNER JOIN {} AS {} ON {}.id::text IN (SELECT substring(ref from '{}/'||'(.*)') FROM unnest(ARRAY[{}]) AS refs(ref) WHERE ref LIKE '{}/%')",
                    target_table,
                    alias,
                    alias,
                    reference_type,
                    reference_column,
                    reference_type
                );
                joins.push(join_sql);

                // Now recursively build the rest of the chain starting from the target resource
                // Need to determine the reference field for the next level if the chain continues
                let next_reference_field = if chain.chain.len() > 1 {
                    // There's at least one more reference in the chain
                    // Get the reference column for the first element of the remaining chain
                    match get_reference_column(target_table, &chain.chain[0].param) {
                        Ok(col) => format!("{}.{}", alias, col),
                        Err(_) => String::new(), // Will be handled by recursive call
                    }
                } else {
                    // Only one element left in chain - it's the final search parameter
                    String::new()
                };

                match build_chain_joins(
                    &chain.chain,
                    target_table,
                    &alias,
                    &next_reference_field,
                    &mut join_counter,
                    &mut bind_count,
                    &chain.search_value,
                ) {
                    Ok((chain_joins, chain_where, _chain_binds)) => {
                        joins.extend(chain_joins);
                        if let Some(where_clause) = chain_where {
                            sql.push_str(&format!(" AND {}", where_clause));
                        }
                        // Note: bind_values are not used in count_sql since we use the same bind_values from main search
                    }
                    Err(e) => {
                        tracing::error!("Error building chain joins in count: {}", e);
                        continue;
                    }
                }
            }
            _ => {
                // Other search conditions not yet mapped for observations
                tracing::debug!("Unsupported search condition in observation count: {:?}", condition);
            }
        }
    }

    // Add JOINs before WHERE clause (same as main search)
    if !joins.is_empty() {
        // Insert joins after FROM clause
        let where_pos = sql
            .find("WHERE")
            .expect("WHERE clause not found in observation count SQL");
        let before_where = &sql[..where_pos];
        let after_where = &sql[where_pos..];
        sql = format!("{} {} {}", before_where, joins.join(" "), after_where);
    }

    sql
}
