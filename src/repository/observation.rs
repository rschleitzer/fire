use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{extract_observation_search_params, Observation, ObservationHistory};
use crate::models::patient::Patient;
use crate::search::{SearchQuery, SortDirection};
use crate::services::validate_observation;

pub struct ObservationRepository {
    pool: PgPool,
}

impl ObservationRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// Create a new observation resource (version 1)
    pub async fn create(&self, mut content: Value) -> Result<Observation> {
        tracing::debug!("Creating new observation resource");

        // Validate resource
        validate_observation(&content)?;

        let id = Uuid::new_v4().to_string();
        let version_id = 1;
        let last_updated = Utc::now();

        tracing::info!(observation_id = %id, "Creating observation");

        // Inject id and meta fields into content before storing
        content = crate::models::observation::inject_id_meta(&content, &id, version_id, &last_updated);

        // Extract search parameters from complete content
        let params = extract_observation_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Insert into current table
        sqlx::query!(
            r#"
            INSERT INTO observation (
                id, version_id, last_updated, content,
                identifier_system, identifier_value,
                code_system, code_code,
                date_datetime,
                date_period_start, date_period_end,
                encounter_reference,
                based_on_reference,
                category_system, category_code,
                combo_code_system, combo_code_code,
                combo_data_absent_reason_system, combo_data_absent_reason_code,
                combo_value_concept_code,
                combo_value_quantity_value, combo_value_quantity_unit, combo_value_quantity_system,
                component_value_quantity_value, component_value_quantity_unit, component_value_quantity_system,
                component_value_reference_reference,
                data_absent_reason_system, data_absent_reason_code,
                derived_from_reference,
                device_reference,
                focus_reference,
                has_member_reference,
                method_system, method_code,
                part_of_reference,
                performer_reference,
                specimen_reference,
                status,
                subject_reference,
                value_concept_code,
                value_date_datetime,
                value_date_period_start, value_date_period_end,
                value_quantity_value, value_quantity_unit, value_quantity_system,
                value_reference_reference
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48)
            "#,
            id,
            version_id,
            last_updated,
            content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created observation
        self.read(&id).await
    }


    /// Read current version of a observation (returns raw JSON)
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


    /// Upsert a observation resource with specific ID (FHIR-compliant PUT)
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

            // Clean up any orphaned history records
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

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO observation_history (
                    id, version_id, last_updated, content,
                identifier_system, identifier_value,
                code_system, code_code,
                date_datetime,
                date_period_start, date_period_end,
                encounter_reference,
                based_on_reference,
                category_system, category_code,
                combo_code_system, combo_code_code,
                combo_data_absent_reason_system, combo_data_absent_reason_code,
                combo_value_concept_code,
                combo_value_quantity_value, combo_value_quantity_unit, combo_value_quantity_system,
                component_value_quantity_value, component_value_quantity_unit, component_value_quantity_system,
                component_value_reference_reference,
                data_absent_reason_system, data_absent_reason_code,
                derived_from_reference,
                device_reference,
                focus_reference,
                has_member_reference,
                method_system, method_code,
                part_of_reference,
                performer_reference,
                specimen_reference,
                status,
                subject_reference,
                value_concept_code,
                value_date_datetime,
                value_date_period_start, value_date_period_end,
                value_quantity_value, value_quantity_unit, value_quantity_system,
                value_reference_reference,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50)
                "#,
                old_observation.id,
                old_observation.version_id,
                old_observation.last_updated,
                &old_observation.content,
            old_params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.identifier_system[..])),
            old_params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.identifier_value[..])),
            old_params.code_system,
            old_params.code_code,
            old_params.date_datetime,
            old_params.date_period_start,
            old_params.date_period_end,
            old_params.encounter_reference,
            old_params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.based_on_reference[..])),
            old_params
                .category_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.category_system[..])),
            old_params
                .category_code
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.category_code[..])),
            old_params.combo_code_system,
            old_params.combo_code_code,
            old_params.combo_data_absent_reason_system,
            old_params.combo_data_absent_reason_code,
            old_params.combo_value_concept_code,
            old_params.combo_value_quantity_value,
            old_params.combo_value_quantity_unit,
            old_params.combo_value_quantity_system,
            old_params.component_value_quantity_value,
            old_params.component_value_quantity_unit,
            old_params.component_value_quantity_system,
            old_params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.component_value_reference_reference[..])),
            old_params.data_absent_reason_system,
            old_params.data_absent_reason_code,
            old_params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.derived_from_reference[..])),
            old_params.device_reference,
            old_params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.focus_reference[..])),
            old_params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.has_member_reference[..])),
            old_params.method_system,
            old_params.method_code,
            old_params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.part_of_reference[..])),
            old_params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.performer_reference[..])),
            old_params.specimen_reference,
            old_params.status,
            old_params.subject_reference,
            old_params.value_concept_code,
            old_params.value_date_datetime,
            old_params.value_date_period_start,
            old_params.value_date_period_end,
            old_params.value_quantity_value,
            old_params.value_quantity_unit,
            old_params.value_quantity_system,
            old_params.value_reference_reference,
                if old_observation.version_id == 1 {
                    "CREATE"
                } else {
                    "UPDATE"
                },
                Utc::now(),
            )
            .execute(&mut *tx)
            .await?;

            // Extract search parameters for new content
            let params = extract_observation_search_params(&content);

            // Update current table with new version
            sqlx::query!(
                r#"
                UPDATE observation
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    identifier_system = $5,
                    identifier_value = $6,
                    code_system = $7,
                    code_code = $8,
                    date_datetime = $9,
                    date_period_start = $10,
                    date_period_end = $11,
                    encounter_reference = $12,
                    based_on_reference = $13,
                    category_system = $14,
                    category_code = $15,
                    combo_code_system = $16,
                    combo_code_code = $17,
                    combo_data_absent_reason_system = $18,
                    combo_data_absent_reason_code = $19,
                    combo_value_concept_code = $20,
                    combo_value_quantity_value = $21,
                    combo_value_quantity_unit = $22,
                    combo_value_quantity_system = $23,
                    component_value_quantity_value = $24,
                    component_value_quantity_unit = $25,
                    component_value_quantity_system = $26,
                    component_value_reference_reference = $27,
                    data_absent_reason_system = $28,
                    data_absent_reason_code = $29,
                    derived_from_reference = $30,
                    device_reference = $31,
                    focus_reference = $32,
                    has_member_reference = $33,
                    method_system = $34,
                    method_code = $35,
                    part_of_reference = $36,
                    performer_reference = $37,
                    specimen_reference = $38,
                    status = $39,
                    subject_reference = $40,
                    value_concept_code = $41,
                    value_date_datetime = $42,
                    value_date_period_start = $43,
                    value_date_period_end = $44,
                    value_quantity_value = $45,
                    value_quantity_unit = $46,
                    value_quantity_system = $47,
                    value_reference_reference = $48
                WHERE id = $1
                "#,
                id,
                new_version_id,
                last_updated,
                content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference
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

            // Clean up any orphaned history records for this ID
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM observation_history
                WHERE id = $1
                "#,
                id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::info!(observation_id = %id, orphaned_history_records = deleted_rows.rows_affected(), "Cleaned up orphaned history records before creating new resource");
            }

            // Inject id and meta into content before storing
            content = crate::models::observation::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_observation_search_params(&content);

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO observation (
                    id, version_id, last_updated, content,
                identifier_system, identifier_value,
                code_system, code_code,
                date_datetime,
                date_period_start, date_period_end,
                encounter_reference,
                based_on_reference,
                category_system, category_code,
                combo_code_system, combo_code_code,
                combo_data_absent_reason_system, combo_data_absent_reason_code,
                combo_value_concept_code,
                combo_value_quantity_value, combo_value_quantity_unit, combo_value_quantity_system,
                component_value_quantity_value, component_value_quantity_unit, component_value_quantity_system,
                component_value_reference_reference,
                data_absent_reason_system, data_absent_reason_code,
                derived_from_reference,
                device_reference,
                focus_reference,
                has_member_reference,
                method_system, method_code,
                part_of_reference,
                performer_reference,
                specimen_reference,
                status,
                subject_reference,
                value_concept_code,
                value_date_datetime,
                value_date_period_start, value_date_period_end,
                value_quantity_value, value_quantity_unit, value_quantity_system,
                value_reference_reference
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48)
                "#,
                id,
                version_id,
                last_updated,
                content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return created observation
            self.read(id).await
        }
    }


    /// Update an existing observation resource
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get existing resource
        let old_observation = sqlx::query_as!(
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
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = old_observation.version_id + 1;
        let last_updated = Utc::now();

        tracing::info!(observation_id = %id, old_version = old_observation.version_id, new_version = new_version_id, "Updating observation");

        // Clean up any orphaned history records
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

        // Inject id and meta into content
        content = crate::models::observation::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_observation_search_params(&old_observation.content);

        // Insert OLD version into history
        sqlx::query!(
            r#"
            INSERT INTO observation_history (
                id, version_id, last_updated, content,
                identifier_system, identifier_value,
                code_system, code_code,
                date_datetime,
                date_period_start, date_period_end,
                encounter_reference,
                based_on_reference,
                category_system, category_code,
                combo_code_system, combo_code_code,
                combo_data_absent_reason_system, combo_data_absent_reason_code,
                combo_value_concept_code,
                combo_value_quantity_value, combo_value_quantity_unit, combo_value_quantity_system,
                component_value_quantity_value, component_value_quantity_unit, component_value_quantity_system,
                component_value_reference_reference,
                data_absent_reason_system, data_absent_reason_code,
                derived_from_reference,
                device_reference,
                focus_reference,
                has_member_reference,
                method_system, method_code,
                part_of_reference,
                performer_reference,
                specimen_reference,
                status,
                subject_reference,
                value_concept_code,
                value_date_datetime,
                value_date_period_start, value_date_period_end,
                value_quantity_value, value_quantity_unit, value_quantity_system,
                value_reference_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50)
            "#,
            old_observation.id,
            old_observation.version_id,
            old_observation.last_updated,
            &old_observation.content,
            old_params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.identifier_system[..])),
            old_params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.identifier_value[..])),
            old_params.code_system,
            old_params.code_code,
            old_params.date_datetime,
            old_params.date_period_start,
            old_params.date_period_end,
            old_params.encounter_reference,
            old_params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.based_on_reference[..])),
            old_params
                .category_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.category_system[..])),
            old_params
                .category_code
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.category_code[..])),
            old_params.combo_code_system,
            old_params.combo_code_code,
            old_params.combo_data_absent_reason_system,
            old_params.combo_data_absent_reason_code,
            old_params.combo_value_concept_code,
            old_params.combo_value_quantity_value,
            old_params.combo_value_quantity_unit,
            old_params.combo_value_quantity_system,
            old_params.component_value_quantity_value,
            old_params.component_value_quantity_unit,
            old_params.component_value_quantity_system,
            old_params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.component_value_reference_reference[..])),
            old_params.data_absent_reason_system,
            old_params.data_absent_reason_code,
            old_params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.derived_from_reference[..])),
            old_params.device_reference,
            old_params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.focus_reference[..])),
            old_params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.has_member_reference[..])),
            old_params.method_system,
            old_params.method_code,
            old_params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.part_of_reference[..])),
            old_params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.performer_reference[..])),
            old_params.specimen_reference,
            old_params.status,
            old_params.subject_reference,
            old_params.value_concept_code,
            old_params.value_date_datetime,
            old_params.value_date_period_start,
            old_params.value_date_period_end,
            old_params.value_quantity_value,
            old_params.value_quantity_unit,
            old_params.value_quantity_system,
            old_params.value_reference_reference,
            if old_observation.version_id == 1 {
                "CREATE"
            } else {
                "UPDATE"
            },
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_observation_search_params(&content);

        // Update current table
        sqlx::query!(
            r#"
            UPDATE observation
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                    identifier_system = $5,
                    identifier_value = $6,
                    code_system = $7,
                    code_code = $8,
                    date_datetime = $9,
                    date_period_start = $10,
                    date_period_end = $11,
                    encounter_reference = $12,
                    based_on_reference = $13,
                    category_system = $14,
                    category_code = $15,
                    combo_code_system = $16,
                    combo_code_code = $17,
                    combo_data_absent_reason_system = $18,
                    combo_data_absent_reason_code = $19,
                    combo_value_concept_code = $20,
                    combo_value_quantity_value = $21,
                    combo_value_quantity_unit = $22,
                    combo_value_quantity_system = $23,
                    component_value_quantity_value = $24,
                    component_value_quantity_unit = $25,
                    component_value_quantity_system = $26,
                    component_value_reference_reference = $27,
                    data_absent_reason_system = $28,
                    data_absent_reason_code = $29,
                    derived_from_reference = $30,
                    device_reference = $31,
                    focus_reference = $32,
                    has_member_reference = $33,
                    method_system = $34,
                    method_code = $35,
                    part_of_reference = $36,
                    performer_reference = $37,
                    specimen_reference = $38,
                    status = $39,
                    subject_reference = $40,
                    value_concept_code = $41,
                    value_date_datetime = $42,
                    value_date_period_start = $43,
                    value_date_period_end = $44,
                    value_quantity_value = $45,
                    value_quantity_unit = $46,
                    value_quantity_system = $47,
                    value_reference_reference = $48
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated observation
        self.read(id).await
    }


    /// Delete a observation resource
    pub async fn delete(&self, id: &str) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current observation (to store in history)
        let observation = sqlx::query_as!(
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
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = observation.version_id + 1;
        let last_updated = Utc::now();

        // Extract search params from content for history
        let params = extract_observation_search_params(&observation.content);

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
                identifier_system, identifier_value,
                code_system, code_code,
                date_datetime,
                date_period_start, date_period_end,
                encounter_reference,
                based_on_reference,
                category_system, category_code,
                combo_code_system, combo_code_code,
                combo_data_absent_reason_system, combo_data_absent_reason_code,
                combo_value_concept_code,
                combo_value_quantity_value, combo_value_quantity_unit, combo_value_quantity_system,
                component_value_quantity_value, component_value_quantity_unit, component_value_quantity_system,
                component_value_reference_reference,
                data_absent_reason_system, data_absent_reason_code,
                derived_from_reference,
                device_reference,
                focus_reference,
                has_member_reference,
                method_system, method_code,
                part_of_reference,
                performer_reference,
                specimen_reference,
                status,
                subject_reference,
                value_concept_code,
                value_date_datetime,
                value_date_period_start, value_date_period_end,
                value_quantity_value, value_quantity_unit, value_quantity_system,
                value_reference_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50)
            "#,
            id,
            new_version_id,
            last_updated,
            &observation.content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }


    /// Get all versions of a observation from history
    pub async fn history(&self, id: &str, count: Option<i64>) -> Result<Vec<ObservationHistory>> {
        let limit = count.unwrap_or(50);

        let history = sqlx::query_as!(
            ObservationHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                history_operation, history_timestamp
            FROM observation_history
            WHERE id = $1
            ORDER BY version_id DESC
            LIMIT $2
            "#,
            id,
            limit
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(history)
    }


    /// Read a specific version of a observation from history
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<ObservationHistory> {
        let history = sqlx::query_as!(
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

        Ok(history)
    }

    /// Rollback a observation to a specific version (deletes all versions >= rollback_to_version)
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

            sqlx::query!(
                r#"
                UPDATE observation
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    identifier_system = $5,
                    identifier_value = $6,
                    code_system = $7,
                    code_code = $8,
                    date_datetime = $9,
                    date_period_start = $10,
                    date_period_end = $11,
                    encounter_reference = $12,
                    based_on_reference = $13,
                    category_system = $14,
                    category_code = $15,
                    combo_code_system = $16,
                    combo_code_code = $17,
                    combo_data_absent_reason_system = $18,
                    combo_data_absent_reason_code = $19,
                    combo_value_concept_code = $20,
                    combo_value_quantity_value = $21,
                    combo_value_quantity_unit = $22,
                    combo_value_quantity_system = $23,
                    component_value_quantity_value = $24,
                    component_value_quantity_unit = $25,
                    component_value_quantity_system = $26,
                    component_value_reference_reference = $27,
                    data_absent_reason_system = $28,
                    data_absent_reason_code = $29,
                    derived_from_reference = $30,
                    device_reference = $31,
                    focus_reference = $32,
                    has_member_reference = $33,
                    method_system = $34,
                    method_code = $35,
                    part_of_reference = $36,
                    performer_reference = $37,
                    specimen_reference = $38,
                    status = $39,
                    subject_reference = $40,
                    value_concept_code = $41,
                    value_date_datetime = $42,
                    value_date_period_start = $43,
                    value_date_period_end = $44,
                    value_quantity_value = $45,
                    value_quantity_unit = $46,
                    value_quantity_system = $47,
                    value_reference_reference = $48
                WHERE id = $1
                "#,
                id,
                new_version,
                restored.last_updated,
                &restored.content,
            params
                .identifier_system
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_system[..])),
            params
                .identifier_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.identifier_value[..])),
            params.code_system,
            params.code_code,
            params.date_datetime,
            params.date_period_start,
            params.date_period_end,
            params.encounter_reference,
            params
                .based_on_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.based_on_reference[..])),
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
            params.combo_code_system,
            params.combo_code_code,
            params.combo_data_absent_reason_system,
            params.combo_data_absent_reason_code,
            params.combo_value_concept_code,
            params.combo_value_quantity_value,
            params.combo_value_quantity_unit,
            params.combo_value_quantity_system,
            params.component_value_quantity_value,
            params.component_value_quantity_unit,
            params.component_value_quantity_system,
            params
                .component_value_reference_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.component_value_reference_reference[..])),
            params.data_absent_reason_system,
            params.data_absent_reason_code,
            params
                .derived_from_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.derived_from_reference[..])),
            params.device_reference,
            params
                .focus_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.focus_reference[..])),
            params
                .has_member_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.has_member_reference[..])),
            params.method_system,
            params.method_code,
            params
                .part_of_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.part_of_reference[..])),
            params
                .performer_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.performer_reference[..])),
            params.specimen_reference,
            params.status,
            params.subject_reference,
            params.value_concept_code,
            params.value_date_datetime,
            params.value_date_period_start,
            params.value_date_period_end,
            params.value_quantity_value,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_reference_reference
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


    /// Get type-level history - all versions of all observations
    pub async fn type_history(&self, count: Option<i64>) -> Result<Vec<ObservationHistory>> {
        // Query all history records with optional limit
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                ObservationHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM observation_history
                ORDER BY history_timestamp DESC
                LIMIT $1
                "#,
                limit
            )
            .fetch_all(&self.pool)
            .await?
        } else {
            sqlx::query_as!(
                ObservationHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM observation_history
                ORDER BY history_timestamp DESC
                "#
            )
            .fetch_all(&self.pool)
            .await?
        };

        // Also get all current versions
        let current_resources = sqlx::query_as!(
            Observation,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM observation
            "#
        )
        .fetch_all(&self.pool)
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
                .any(|h| h.id == current.id && h.version_id == current.version_id);

            if !current_version_exists {
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
                history.push(current_as_history);
            }
        }

        // Sort by timestamp descending
        history.sort_by(|a, b| b.history_timestamp.cmp(&a.history_timestamp));

        // Apply final truncation if needed (after merging and sorting)
        if let Some(limit) = count {
            history.truncate(limit as usize);
        }

        Ok(history)
    }


    /// Purge all observation history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) observation records
    pub async fn purge(&self) -> Result<()> {
        // Delete all history records for test isolation
        sqlx::query!("DELETE FROM observation_history")
            .execute(&self.pool)
            .await?;

        Ok(())
    }


    /// Search for observations based on FHIR search parameters
    pub async fn search(
        &self,
        params: &HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Observation>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT observation.id, observation.version_id, observation.last_updated, observation.content
               FROM observation WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();

        // Build WHERE clause from search parameters
        for param in &query.params {
            match param.name.as_str() {
                "identifier" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(observation.identifier_system, observation.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})",
                                bind_idx, bind_idx + 1
                            ));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.identifier_value) AS iv WHERE iv = ${})", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "code" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.code_system = ${} AND observation.code_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.code_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "date" => {
                    let bind_idx = bind_values.len() + 1;
                    let op = match param.prefix.as_deref() {
                        Some("eq") | None => "=",
                        Some("ne") => "!=",
                        Some("gt") => ">",
                        Some("lt") => "<",
                        Some("ge") => ">=",
                        Some("le") => "<=",
                        _ => "=",
                    };
                    sql.push_str(&format!(" AND observation.date {} ${}::date", op, bind_idx));
                    bind_values.push(param.value.clone());
                }
                "encounter" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.encounter_reference = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "based-on" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.based_on_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "category" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.category_system = ${} AND observation.category_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.category_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "combo-code" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.combo_code_system = ${} AND observation.combo_code_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.combo_code_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "combo-data-absent-reason" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.combo_data_absent_reason_system = ${} AND observation.combo_data_absent_reason_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.combo_data_absent_reason_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "combo-value-concept" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.combo_value_concept_system = ${} AND observation.combo_value_concept_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.combo_value_concept_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "component-value-reference" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.component_value_reference_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "data-absent-reason" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.data_absent_reason_system = ${} AND observation.data_absent_reason_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.data_absent_reason_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "derived-from" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.derived_from_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "device" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.device_reference = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "focus" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.focus_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "has-member" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.has_member_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "method" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.method_system = ${} AND observation.method_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.method_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "part-of" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.part_of_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "performer" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(observation.performer_reference) AS ref WHERE ref = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "specimen" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.specimen_reference = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "status" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.status = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "subject" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.subject_reference = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "value-concept" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(" AND observation.value_concept_system = ${} AND observation.value_concept_code = ${}", bind_idx, bind_idx + 1));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND observation.value_concept_code = ${}", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "value-date" => {
                    let bind_idx = bind_values.len() + 1;
                    let op = match param.prefix.as_deref() {
                        Some("eq") | None => "=",
                        Some("ne") => "!=",
                        Some("gt") => ">",
                        Some("lt") => "<",
                        Some("ge") => ">=",
                        Some("le") => "<=",
                        _ => "=",
                    };
                    sql.push_str(&format!(" AND observation.value_date {} ${}::date", op, bind_idx));
                    bind_values.push(param.value.clone());
                }
                "value-reference" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND observation.value_reference_reference = ${}", bind_idx));
                    bind_values.push(param.value.clone());
                }
                _ => {
                    // Unknown parameter - ignore per FHIR spec
                    tracing::warn!("Unknown search parameter for Observation: {}", param.name);
                }
            }
        }

        // Add sorting
        if !query.sort.is_empty() {
            sql.push_str(" ORDER BY ");
            for (i, sort) in query.sort.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&sort.field);
                match sort.direction {
                    SortDirection::Ascending => sql.push_str(" ASC"),
                    SortDirection::Descending => sql.push_str(" DESC"),
                }
            }
        }

        // Add pagination
        sql.push_str(&format!(" LIMIT {} OFFSET {}", query.limit, query.offset));

        // Execute query
        let mut query_builder = sqlx::query_as::<_, Observation>(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let resources = query_builder
            .fetch_all(&self.pool)
            .await?;

        // Get total count if requested
        let total = if include_total {
            let count_sql = sql.replace(
                &format!("SELECT observation.id, observation.version_id, observation.last_updated, observation.content\n               FROM observation"),
                "SELECT COUNT(*) FROM observation"
            ).split(" ORDER BY").next().unwrap().to_string();

            let mut count_query = sqlx::query_scalar::<_, i64>(&count_sql);
            for value in &bind_values {
                count_query = count_query.bind(value);
            }
            Some(count_query.fetch_one(&self.pool).await?)
        } else {
            None
        };

        Ok((resources, total))
    }

    /// Read a patient by ID (for chaining support)
    pub async fn read_patient(&self, id: &str) -> Result<Patient> {
        let patient = sqlx::query_as!(
            Patient,
            r#"SELECT id, version_id, last_updated, content as "content: Value" FROM patient WHERE id = $1"#,
            id
        ).fetch_optional(&self.pool).await?.ok_or(FhirError::NotFound)?;
        Ok(patient)
    }

}
