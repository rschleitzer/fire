use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{extract_observation_search_params, Observation, ObservationHistory};
use crate::models::patient::Patient;
use crate::search::{SearchParam, SearchQuery, SortDirection};
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
                patient_reference,
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
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49)
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
            params.patient_reference,
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
                patient_reference,
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
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50, $51)
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
            old_params.patient_reference,
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
                    patient_reference = $7,
                    code_system = $8,
                    code_code = $9,
                    date_datetime = $10,
                    date_period_start = $11,
                    date_period_end = $12,
                    encounter_reference = $13,
                    based_on_reference = $14,
                    category_system = $15,
                    category_code = $16,
                    combo_code_system = $17,
                    combo_code_code = $18,
                    combo_data_absent_reason_system = $19,
                    combo_data_absent_reason_code = $20,
                    combo_value_concept_code = $21,
                    combo_value_quantity_value = $22,
                    combo_value_quantity_unit = $23,
                    combo_value_quantity_system = $24,
                    component_value_quantity_value = $25,
                    component_value_quantity_unit = $26,
                    component_value_quantity_system = $27,
                    component_value_reference_reference = $28,
                    data_absent_reason_system = $29,
                    data_absent_reason_code = $30,
                    derived_from_reference = $31,
                    device_reference = $32,
                    focus_reference = $33,
                    has_member_reference = $34,
                    method_system = $35,
                    method_code = $36,
                    part_of_reference = $37,
                    performer_reference = $38,
                    specimen_reference = $39,
                    status = $40,
                    subject_reference = $41,
                    value_concept_code = $42,
                    value_date_datetime = $43,
                    value_date_period_start = $44,
                    value_date_period_end = $45,
                    value_quantity_value = $46,
                    value_quantity_unit = $47,
                    value_quantity_system = $48,
                    value_reference_reference = $49
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
            params.patient_reference,
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
                patient_reference,
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
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49)
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
            params.patient_reference,
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
                patient_reference,
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
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50, $51)
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
            old_params.patient_reference,
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
                    patient_reference = $7,
                    code_system = $8,
                    code_code = $9,
                    date_datetime = $10,
                    date_period_start = $11,
                    date_period_end = $12,
                    encounter_reference = $13,
                    based_on_reference = $14,
                    category_system = $15,
                    category_code = $16,
                    combo_code_system = $17,
                    combo_code_code = $18,
                    combo_data_absent_reason_system = $19,
                    combo_data_absent_reason_code = $20,
                    combo_value_concept_code = $21,
                    combo_value_quantity_value = $22,
                    combo_value_quantity_unit = $23,
                    combo_value_quantity_system = $24,
                    component_value_quantity_value = $25,
                    component_value_quantity_unit = $26,
                    component_value_quantity_system = $27,
                    component_value_reference_reference = $28,
                    data_absent_reason_system = $29,
                    data_absent_reason_code = $30,
                    derived_from_reference = $31,
                    device_reference = $32,
                    focus_reference = $33,
                    has_member_reference = $34,
                    method_system = $35,
                    method_code = $36,
                    part_of_reference = $37,
                    performer_reference = $38,
                    specimen_reference = $39,
                    status = $40,
                    subject_reference = $41,
                    value_concept_code = $42,
                    value_date_datetime = $43,
                    value_date_period_start = $44,
                    value_date_period_end = $45,
                    value_quantity_value = $46,
                    value_quantity_unit = $47,
                    value_quantity_system = $48,
                    value_reference_reference = $49
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
            params.patient_reference,
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
                patient_reference,
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
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $50, $51)
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
            params.patient_reference,
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

        // Get current version
        let current = sqlx::query_as!(
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
        .await?;

        // Get historical versions
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
            LIMIT $2
            "#,
            id,
            limit
        )
        .fetch_all(&self.pool)
        .await?;

        // Prepend current version if it exists
        if let Some(curr) = current {
            history.insert(0, ObservationHistory {
                id: curr.id,
                version_id: curr.version_id,
                last_updated: curr.last_updated,
                content: curr.content,
                history_operation: "UPDATE".to_string(),
                history_timestamp: curr.last_updated,
            });
        }

        // Apply limit
        history.truncate(limit as usize);

        Ok(history)
    }


    /// Read a specific version of a observation from history
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<ObservationHistory> {
        // Try history table first
        if let Some(history) = sqlx::query_as!(
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
        .await? {
            return Ok(history);
        }

        // If not in history, check if it's the current version
        if let Some(current) = sqlx::query_as!(
            Observation,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM observation
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await? {
            // Convert current version to history format
            return Ok(ObservationHistory {
                id: current.id,
                version_id: current.version_id,
                last_updated: current.last_updated,
                content: current.content,
                history_operation: "UPDATE".to_string(),
                history_timestamp: current.last_updated,
            });
        }

        Err(FhirError::NotFound)
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
                    patient_reference = $7,
                    code_system = $8,
                    code_code = $9,
                    date_datetime = $10,
                    date_period_start = $11,
                    date_period_end = $12,
                    encounter_reference = $13,
                    based_on_reference = $14,
                    category_system = $15,
                    category_code = $16,
                    combo_code_system = $17,
                    combo_code_code = $18,
                    combo_data_absent_reason_system = $19,
                    combo_data_absent_reason_code = $20,
                    combo_value_concept_code = $21,
                    combo_value_quantity_value = $22,
                    combo_value_quantity_unit = $23,
                    combo_value_quantity_system = $24,
                    component_value_quantity_value = $25,
                    component_value_quantity_unit = $26,
                    component_value_quantity_system = $27,
                    component_value_reference_reference = $28,
                    data_absent_reason_system = $29,
                    data_absent_reason_code = $30,
                    derived_from_reference = $31,
                    device_reference = $32,
                    focus_reference = $33,
                    has_member_reference = $34,
                    method_system = $35,
                    method_code = $36,
                    part_of_reference = $37,
                    performer_reference = $38,
                    specimen_reference = $39,
                    status = $40,
                    subject_reference = $41,
                    value_concept_code = $42,
                    value_date_datetime = $43,
                    value_date_period_start = $44,
                    value_date_period_end = $45,
                    value_quantity_value = $46,
                    value_quantity_unit = $47,
                    value_quantity_system = $48,
                    value_reference_reference = $49
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
            params.patient_reference,
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

        // Group parameters by name for OR logic
        let mut param_groups: HashMap<String, Vec<&SearchParam>> = HashMap::new();
        for param in &query.params {
            param_groups.entry(param.name.clone()).or_insert_with(Vec::new).push(param);
        }

        // Build WHERE clause from grouped search parameters
        for (param_name, param_list) in &param_groups {
            // Start OR group if multiple values
            let needs_or_group = param_list.len() > 1;

            // Track SQL length before processing to detect if condition was added
            let sql_before_len = sql.len();

            // Tentatively add AND - we'll revert if no condition generated
            sql.push_str(" AND ");

            if needs_or_group {
                sql.push_str("(");
            }

            // Process each param in the group
            for (param_idx, param) in param_list.iter().enumerate() {
                // Add OR between params in same group
                if needs_or_group && param_idx > 0 {
                    sql.push_str(" OR ");
                }

                // Generate condition based on parameter name
                match param.name.as_str() {
                    "identifier" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(observation.identifier_system, observation.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.identifier_value) AS iv WHERE iv = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "patient" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Patient/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.patient_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.patient_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.patient_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.patient_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.patient_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.patient_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.patient_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "code" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.code_system = ${} AND observation.code_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.code_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "date" => {
                        // Basic validation of date format (YYYY-MM-DD or partial dates)
                        let date_value = &param.value;
                        let is_valid_date = date_value.chars().all(|c| c.is_ascii_digit() || c == '-')
                            && !date_value.is_empty()
                            && date_value.len() <= 10;  // Max length for YYYY-MM-DD

                        if !is_valid_date {
                            // Invalid date format - skip this parameter to avoid database error
                            // (returning empty results is acceptable per FHIR spec)
                            tracing::warn!("Invalid date format for 'date': '{}', skipping parameter", date_value);
                            continue;
                        }

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
                        sql.push_str(&format!("(observation.date_datetime {} ${}::timestamptz)", op, bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    "encounter" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Encounter/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.encounter_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.encounter_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.encounter_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.encounter_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.encounter_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.encounter_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.encounter_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "based-on" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("DeviceRequest/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.based_on_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.based_on_reference IS NULL OR array_length(observation.based_on_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.based_on_reference IS NOT NULL AND array_length(observation.based_on_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.based_on_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.based_on_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.based_on_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.based_on_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "category" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(observation.category_system, observation.category_code) AS cc(sys, code) WHERE cc.sys = ${} AND cc.code = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.category_code) AS c WHERE c = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "combo-code" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.combo_code_system = ${} AND observation.combo_code_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.combo_code_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "combo-data-absent-reason" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.combo_data_absent_reason_system = ${} AND observation.combo_data_absent_reason_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.combo_data_absent_reason_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "combo-value-concept" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.combo_value_concept_system = ${} AND observation.combo_value_concept_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.combo_value_concept_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "component-value-reference" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("MolecularSequence/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.component_value_reference_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.component_value_reference_reference IS NULL OR array_length(observation.component_value_reference_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.component_value_reference_reference IS NOT NULL AND array_length(observation.component_value_reference_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.component_value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.component_value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.component_value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.component_value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "data-absent-reason" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.data_absent_reason_system = ${} AND observation.data_absent_reason_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.data_absent_reason_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "derived-from" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("ImagingStudy/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.derived_from_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.derived_from_reference IS NULL OR array_length(observation.derived_from_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.derived_from_reference IS NOT NULL AND array_length(observation.derived_from_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.derived_from_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.derived_from_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.derived_from_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.derived_from_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "device" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Device/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.device_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.device_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.device_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.device_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.device_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.device_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.device_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "focus" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Account/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.focus_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.focus_reference IS NULL OR array_length(observation.focus_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.focus_reference IS NOT NULL AND array_length(observation.focus_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.focus_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.focus_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.focus_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.focus_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "has-member" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Observation/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.has_member_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.has_member_reference IS NULL OR array_length(observation.has_member_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.has_member_reference IS NOT NULL AND array_length(observation.has_member_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.has_member_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.has_member_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.has_member_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.has_member_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "method" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.method_system = ${} AND observation.method_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.method_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "part-of" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("ImagingStudy/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.part_of_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.part_of_reference IS NULL OR array_length(observation.part_of_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.part_of_reference IS NOT NULL AND array_length(observation.part_of_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.part_of_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.part_of_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.part_of_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.part_of_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "performer" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Organization/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(observation.performer_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.performer_reference IS NULL OR array_length(observation.performer_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(observation.performer_reference IS NOT NULL AND array_length(observation.performer_reference, 1) > 0)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.performer_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.performer_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.performer_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.performer_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "specimen" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Group/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.specimen_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.specimen_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.specimen_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.specimen_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.specimen_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.specimen_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.specimen_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "status" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                sql.push_str(&format!("(observation.status = ${})", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.status IS NULL)");
                                } else {
                                    sql.push_str("(observation.status IS NOT NULL)");
                                }
                            }
                            Some("not") => {
                                sql.push_str(&format!("(observation.status != ${})", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for token search: {:?}", modifier);
                            }
                        }
                    }
                    "subject" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Device/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.subject_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.subject_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.subject_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.subject_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.subject_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.subject_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.subject_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "value-concept" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!("(observation.value_concept_system = ${} AND observation.value_concept_code = ${})", bind_idx, bind_idx + 1));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("(observation.value_concept_code = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "value-date" => {
                        // Basic validation of date format (YYYY-MM-DD or partial dates)
                        let date_value = &param.value;
                        let is_valid_date = date_value.chars().all(|c| c.is_ascii_digit() || c == '-')
                            && !date_value.is_empty()
                            && date_value.len() <= 10;  // Max length for YYYY-MM-DD

                        if !is_valid_date {
                            // Invalid date format - skip this parameter to avoid database error
                            // (returning empty results is acceptable per FHIR spec)
                            tracing::warn!("Invalid date format for 'value-date': '{}', skipping parameter", date_value);
                            continue;
                        }

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
                        sql.push_str(&format!("(observation.value_date_datetime {} ${}::timestamptz)", op, bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    "value-reference" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("MolecularSequence/{}", ref_value);
                                }
                                sql.push_str(&format!("(observation.value_reference_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(observation.value_reference_reference IS NULL)");
                                } else {
                                    sql.push_str("(observation.value_reference_reference IS NOT NULL)");
                                }
                            }
                            Some(modifier_str) if modifier_str.contains('.') => {
                                // Forward chaining: ResourceType.parameter
                                if let Some((resource_type, chained_param)) = modifier_str.split_once('.') {
                                    let target_table = resource_type.to_lowercase();

                                    // Check for multi-level chaining (e.g., Patient.general-practitioner.family)
                                    if chained_param.contains('.') {
                                        if let Some((intermediate_field, final_param)) = chained_param.split_once('.') {
                                            let intermediate_col = intermediate_field.replace('-', "_");

                                            if final_param == "family" {
                                                let bind_idx = bind_values.len() + 1;
                                                // Build nested EXISTS for two-level chain (scalar reference version)
                                                // Extract ID from scalar reference like Patient/123
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.{}_reference) AS inter_ref WHERE EXISTS (SELECT 1 FROM practitioner WHERE practitioner.id = SUBSTRING(inter_ref FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))))",
                                                    target_table, target_table, target_table, intermediate_col, bind_idx
                                                ));
                                                bind_values.push(format!("%{}%", param.value));
                                            } else {
                                                tracing::warn!("Multi-level chained parameter '{}' not yet implemented", final_param);
                                            }
                                        }
                                    }
                                    // Handle single-level chaining
                                    else if chained_param == "family" {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, target_table, bind_idx
                                        ));
                                        bind_values.push(format!("%{}%", param.value));
                                    }
                                    else if chained_param == "identifier" {
                                        if param.value.contains('|') {
                                            let parts: Vec<&str> = param.value.split('|').collect();
                                            if parts.len() == 2 {
                                                let bind_idx = bind_values.len() + 1;
                                                sql.push_str(&format!(
                                                    "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                                    target_table, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                                ));
                                                bind_values.push(parts[0].to_string());
                                                bind_values.push(parts[1].to_string());
                                            }
                                        } else {
                                            let bind_idx = bind_values.len() + 1;
                                            sql.push_str(&format!(
                                                "EXISTS (SELECT 1 FROM {} WHERE {}.id = SUBSTRING(observation.value_reference_reference FROM '[^/]+$') AND EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                                target_table, target_table, target_table, bind_idx
                                            ));
                                            bind_values.push(param.value.clone());
                                        }
                                    }
                                    else {
                                        tracing::warn!("Chained parameter '{}' not yet implemented", chained_param);
                                    }
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "code-value-concept" => {
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
                                        "(observation.code_system = ${} AND observation.code_code = ${} AND observation.value_concept_code = ${})",
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_code_only.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "(observation.code_code = ${} AND observation.value_concept_code = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_code_only.to_string());
                            }
                        }
                    }
                    "code-value-quantity" => {
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_part)) = param.value.split_once('$') {
                            // Parse value with optional prefix
                            let (prefix_str, value_str) = if value_part.starts_with("gt") {
                                (">", &value_part[2..])
                            } else if value_part.starts_with("ge") {
                                (">=", &value_part[2..])
                            } else if value_part.starts_with("lt") {
                                ("<", &value_part[2..])
                            } else if value_part.starts_with("le") {
                                ("<=", &value_part[2..])
                            } else if value_part.starts_with("ne") {
                                ("!=", &value_part[2..])
                            } else if value_part.starts_with("eq") {
                                ("=", &value_part[2..])
                            } else {
                                ("=", value_part)
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        "(observation.code_system = ${} AND observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric)",
                                        bind_idx, bind_idx + 1, prefix_str, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "(observation.code_code = ${} AND observation.value_quantity_value {} ${}::numeric)",
                                    bind_idx, prefix_str, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_str.to_string());
                            }
                        }
                    }
                    "code-value-string" => {
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_str)) = param.value.split_once('$') {
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        "(observation.code_system = ${} AND observation.code_code = ${} AND observation.value ILIKE ${})",
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(format!("%{}%", value_str));
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "(observation.code_code = ${} AND observation.value ILIKE ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(format!("%{}%", value_str));
                            }
                        }
                    }
                    "combo-code-value-concept" => {
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
                                        "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})",
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_code_only.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_code_only.to_string());
                            }
                        }
                    }
                    "combo-code-value-quantity" => {
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_part)) = param.value.split_once('$') {
                            // Parse value with optional prefix
                            let (prefix_str, value_str) = if value_part.starts_with("gt") {
                                (">", &value_part[2..])
                            } else if value_part.starts_with("ge") {
                                (">=", &value_part[2..])
                            } else if value_part.starts_with("lt") {
                                ("<", &value_part[2..])
                            } else if value_part.starts_with("le") {
                                ("<=", &value_part[2..])
                            } else if value_part.starts_with("ne") {
                                ("!=", &value_part[2..])
                            } else if value_part.starts_with("eq") {
                                ("=", &value_part[2..])
                            } else {
                                ("=", value_part)
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)",
                                        bind_idx, bind_idx + 1, prefix_str, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)",
                                    bind_idx, prefix_str, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_str.to_string());
                            }
                        }
                    }
                    "component-code-value-concept" => {
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
                                        "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})",
                                        bind_idx, bind_idx + 1, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_code_only.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND comp->'valueCodeableConcept'->'coding'->0->>'code' = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_code_only.to_string());
                            }
                        }
                    }
                    "component-code-value-quantity" => {
                        // Parse composite: system|code$value or code$value
                        if let Some((code_part, value_part)) = param.value.split_once('$') {
                            // Parse value with optional prefix
                            let (prefix_str, value_str) = if value_part.starts_with("gt") {
                                (">", &value_part[2..])
                            } else if value_part.starts_with("ge") {
                                (">=", &value_part[2..])
                            } else if value_part.starts_with("lt") {
                                ("<", &value_part[2..])
                            } else if value_part.starts_with("le") {
                                ("<=", &value_part[2..])
                            } else if value_part.starts_with("ne") {
                                ("!=", &value_part[2..])
                            } else if value_part.starts_with("eq") {
                                ("=", &value_part[2..])
                            } else {
                                ("=", value_part)
                            };

                            // Parse code part for optional system|code
                            if code_part.contains('|') {
                                let parts: Vec<&str> = code_part.split('|').collect();
                                if parts.len() == 2 {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'system' = ${} AND comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)",
                                        bind_idx, bind_idx + 1, prefix_str, bind_idx + 2
                                    ));
                                    bind_values.push(parts[0].to_string());
                                    bind_values.push(parts[1].to_string());
                                    bind_values.push(value_str.to_string());
                                }
                            } else {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM jsonb_array_elements(observation.content->'component') AS comp WHERE comp->'code'->'coding'->0->>'code' = ${} AND (comp->'valueQuantity'->>'value')::numeric {} ${}::numeric)",
                                    bind_idx, prefix_str, bind_idx + 1
                                ));
                                bind_values.push(code_part.to_string());
                                bind_values.push(value_str.to_string());
                            }
                        }
                    }
                    "_has" => {
                        // Reverse chaining: _has:ResourceType:ref_field:param=value
                        // modifier contains: ResourceType:ref_field:param
                        if let Some(modifier_str) = &param.modifier {
                            let parts: Vec<&str> = modifier_str.split(':').collect();
                            if parts.len() == 3 {
                                let target_resource_type = parts[0];
                                let target_table = target_resource_type.to_lowercase();
                                // Convert hyphens to underscores for SQL column names
                                let ref_field = parts[1].replace('-', "_");
                                let filter_param = parts[2];  // e.g., "code"

                                // Generate reverse chain EXISTS query
                                // This finds resources of the current type that are referenced by target_resource_type
                                // Example: Find Patients that have Observations where subject points to the Patient AND code=X
                                let bind_idx = bind_values.len() + 1;

                                // Determine if the reference field is a collection (array) based on common FHIR patterns
                                // Collections use = ANY(), scalars use simple =
                                let is_collection = matches!(ref_field.as_str(),
                                    "general_practitioner" | "performer" | "participant" |
                                    "basedOn" | "partOf" | "reasonReference" | "insurance" |
                                    "supportingInfo" | "diagnosis" | "procedure" | "account"
                                );

                                // Build filter condition based on parameter type
                                // For now, handle common cases: token (code, category)
                                if filter_param == "code" || filter_param == "category" {
                                    // Token search on target resource (scalar columns, not arrays)
                                    let col_name = if filter_param == "code" { "code_code" } else { "category_code" };

                                    if is_collection {
                                        // Use = ANY() for array reference columns
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE CONCAT('Observation/', observation.id) = ANY({}.{}_reference) AND {}.{} = ${})",
                                            target_table, target_table, ref_field, target_table, col_name, bind_idx
                                        ));
                                    } else {
                                        // Use simple = for scalar reference columns
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.{}_reference = CONCAT('Observation/', observation.id) AND {}.{} = ${})",
                                            target_table, target_table, ref_field, target_table, col_name, bind_idx
                                        ));
                                    }
                                    bind_values.push(param.value.clone());
                                } else if filter_param == "family" {
                                    // String search on target resource
                                    if is_collection {
                                        // Use = ANY() for array reference columns
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE CONCAT('Observation/', observation.id) = ANY({}.{}_reference) AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, ref_field, target_table, bind_idx
                                        ));
                                    } else {
                                        // Use simple = for scalar reference columns
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} WHERE {}.{}_reference = CONCAT('Observation/', observation.id) AND EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                            target_table, target_table, ref_field, target_table, bind_idx
                                        ));
                                    }
                                    bind_values.push(format!("%{}%", param.value));
                                } else {
                                    tracing::warn!("Reverse chaining filter parameter '{}' not yet implemented", filter_param);
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
                                "general-practitioner" => ("practitioner", "general_practitioner"),
                                "subject" => {
                                    // Subject could reference different types, default to patient
                                    // TODO: handle multiple target types
                                    ("patient", "subject")
                                }
                                _ => {
                                    tracing::warn!("Unsupported chaining on parameter: {}", base_param);
                                    continue;
                                }
                            };

                            // Handle chained field types
                            if chained_field == "family" {
                                let bind_idx = bind_values.len() + 1;
                                // Handle both collection and non-collection references
                                // For collections, unnest the array first
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM {} JOIN unnest(observation.{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.family_name) AS v WHERE v ILIKE ${}))",
                                    target_table, ref_col, target_table, target_table, bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            else if chained_field == "given" {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM {} JOIN unnest(observation.{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.given_name) AS v WHERE v ILIKE ${}))",
                                    target_table, ref_col, target_table, target_table, bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            else if chained_field == "identifier" {
                                if param.value.contains('|') {
                                    let parts: Vec<&str> = param.value.split('|').collect();
                                    if parts.len() == 2 {
                                        let bind_idx = bind_values.len() + 1;
                                        sql.push_str(&format!(
                                            "EXISTS (SELECT 1 FROM {} JOIN unnest(observation.{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.identifier_system, {}.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${}))",
                                            target_table, ref_col, target_table, target_table, target_table, bind_idx, bind_idx + 1
                                        ));
                                        bind_values.push(parts[0].to_string());
                                        bind_values.push(parts[1].to_string());
                                    }
                                } else {
                                    let bind_idx = bind_values.len() + 1;
                                    sql.push_str(&format!(
                                        "EXISTS (SELECT 1 FROM {} JOIN unnest(observation.{}_reference) AS ref_val ON {}.id = SUBSTRING(ref_val FROM '[^/]+$') WHERE EXISTS (SELECT 1 FROM unnest({}.identifier_value) AS iv WHERE iv = ${}))",
                                        target_table, ref_col, target_table, target_table, bind_idx
                                    ));
                                    bind_values.push(param.value.clone());
                                }
                            }
                            else {
                                tracing::warn!("Chained field '{}' not yet supported", chained_field);
                            }
                        }
                    }
                    _ => {
                        // Unknown parameter - ignore per FHIR spec
                        tracing::warn!("Unknown search parameter for Observation: {}", param.name);
                    }
                }
            }

            // Close OR group if needed
            if needs_or_group {
                sql.push_str(")");
            }

            // Check if any condition was actually added
            // If sql length increased only by " AND " or " AND ()", revert it
            let added_text_len = sql.len() - sql_before_len;
            let expected_empty_len = if needs_or_group { 7 } else { 5 }; // " AND ()" or " AND "

            if added_text_len == expected_empty_len {
                // No actual condition was added, revert the AND
                sql.truncate(sql_before_len);
            }
        }

        // Add sorting
        if !query.sort.is_empty() {
            sql.push_str(" ORDER BY ");
            for (i, sort) in query.sort.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                // Map FHIR search parameter name to database column name
                let column_name = Self::map_sort_field_to_column(&sort.field);
                sql.push_str(&column_name);
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

    /// Map FHIR search parameter name to database column name for sorting
    fn map_sort_field_to_column(field: &str) -> String {
        match field {
            "identifier" => "identifier_value".to_string(),
            "patient" => "patient_reference".to_string(),
            "code" => "code_code".to_string(),
            "date" => "date_datetime".to_string(),
            "encounter" => "encounter_reference".to_string(),
            "based-on" => "based_on_reference".to_string(),
            "category" => "category_code".to_string(),
            "combo-code" => "combo_code_code".to_string(),
            "combo-data-absent-reason" => "combo_data_absent_reason_code".to_string(),
            "combo-value-concept" => "combo_value_concept_code".to_string(),
            "combo-value-quantity" => "combo_value_quantity".to_string(),
            "component-code" => "component_code_code".to_string(),
            "component-data-absent-reason" => "component_data_absent_reason_code".to_string(),
            "component-value-canonical" => "component_value_canonical_name".to_string(),
            "component-value-concept" => "component_value_concept_code".to_string(),
            "component-value-quantity" => "component_value_quantity".to_string(),
            "component-value-reference" => "component_value_reference_reference".to_string(),
            "data-absent-reason" => "data_absent_reason_code".to_string(),
            "derived-from" => "derived_from_reference".to_string(),
            "device" => "device_reference".to_string(),
            "focus" => "focus_reference".to_string(),
            "has-member" => "has_member_reference".to_string(),
            "method" => "method_code".to_string(),
            "part-of" => "part_of_reference".to_string(),
            "performer" => "performer_reference".to_string(),
            "specimen" => "specimen_reference".to_string(),
            "status" => "status".to_string(),
            "subject" => "subject_reference".to_string(),
            "value-canonical" => "value_canonical_name".to_string(),
            "value-concept" => "value_concept_code".to_string(),
            "value-date" => "value_date_datetime".to_string(),
            "value-markdown" => "value_markdown_name".to_string(),
            "value-quantity" => "value_quantity".to_string(),
            "value-reference" => "value_reference_reference".to_string(),
            "code-value-concept" => "code_value_concept".to_string(),
            "code-value-date" => "code_value_date".to_string(),
            "code-value-quantity" => "code_value_quantity".to_string(),
            "code-value-string" => "code_value_string".to_string(),
            "combo-code-value-concept" => "combo_code_value_concept".to_string(),
            "combo-code-value-quantity" => "combo_code_value_quantity".to_string(),
            "component-code-value-concept" => "component_code_value_concept".to_string(),
            "component-code-value-quantity" => "component_code_value_quantity".to_string(),
            _ => field.to_string(),
        }
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
