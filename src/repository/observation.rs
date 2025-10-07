use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{extract_observation_search_params, Observation, ObservationHistory};
use crate::services::validate_observation;

pub struct ObservationRepository {
    pool: PgPool,
}

impl ObservationRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// Create a new observation resource (version 1)
    pub async fn create(&self, content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let id = Uuid::new_v4();
        let version_id = 1;
        let last_updated = Utc::now();

        // Extract search parameters
        let params = extract_observation_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Convert f64 to BigDecimal for database
        let value_qty_decimal = params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v).unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
        });

        // Insert into current table (simplified - not using query_as! due to complexity)
        sqlx::query!(
            r#"
            INSERT INTO observation (
                id, version_id, last_updated, deleted, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23)
            "#,
            id,
            version_id,
            last_updated,
            false,
            content,
            params.status,
            params.category_system.is_empty().then_some(None).unwrap_or(Some(params.category_system.clone())),
            params.category_code.is_empty().then_some(None).unwrap_or(Some(params.category_code.clone())),
            params.code_system,
            params.code_code,
            params.subject_reference,
            params.patient_reference,
            params.encounter_reference,
            params.effective_datetime,
            params.effective_period_start,
            params.effective_period_end,
            params.issued,
            value_qty_decimal,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(params.value_codeable_concept_code.clone())),
            params.value_string,
            params.performer_reference.is_empty().then_some(None).unwrap_or(Some(params.performer_reference)),
        )
        .execute(&mut *tx)
        .await?;

        // Insert into history table
        sqlx::query!(
            r#"
            INSERT INTO observation_history (
                id, version_id, last_updated, deleted, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25)
            "#,
            id,
            version_id,
            last_updated,
            false,
            &content,
            params.status,
            params.category_system.is_empty().then_some(None).unwrap_or(Some(params.category_system)),
            params.category_code.is_empty().then_some(None).unwrap_or(Some(params.category_code)),
            params.code_system,
            params.code_code,
            params.subject_reference,
            params.patient_reference,
            params.encounter_reference,
            params.effective_datetime,
            params.effective_period_start,
            params.effective_period_end,
            params.issued,
            value_qty_decimal,
            params.value_quantity_unit,
            params.value_quantity_system,
            params.value_codeable_concept_code.is_empty().then_some(None).unwrap_or(Some(params.value_codeable_concept_code)),
            params.value_string,
            params.performer_reference.is_empty().then_some(None).unwrap_or(Some(params.performer_reference)),
            "CREATE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created observation
        self.read(&id).await
    }

    /// Read current version of an observation
    pub async fn read(&self, id: &Uuid) -> Result<Observation> {
        let row = sqlx::query!(
            r#"
            SELECT
                id, version_id, last_updated, deleted,
                content as "content: Value",
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference
            FROM observation
            WHERE id = $1 AND deleted = FALSE
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(Observation {
            id: row.id,
            version_id: row.version_id,
            last_updated: row.last_updated,
            deleted: row.deleted,
            content: row.content,
            status: row.status,
            category_system: row.category_system,
            category_code: row.category_code,
            code_system: row.code_system,
            code_code: row.code_code,
            subject_reference: row.subject_reference,
            patient_reference: row.patient_reference,
            encounter_reference: row.encounter_reference,
            effective_datetime: row.effective_datetime,
            effective_period_start: row.effective_period_start,
            effective_period_end: row.effective_period_end,
            issued: row.issued,
            value_quantity_value: row.value_quantity_value,
            value_quantity_unit: row.value_quantity_unit,
            value_quantity_system: row.value_quantity_system,
            value_codeable_concept_code: row.value_codeable_concept_code,
            value_string: row.value_string,
            performer_reference: row.performer_reference,
        })
    }

    /// Delete an observation (soft delete)
    pub async fn delete(&self, id: &Uuid) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current observation
        let observation = self.read(id).await?;
        let new_version_id = observation.version_id + 1;
        let last_updated = Utc::now();

        // Mark as deleted in current table
        sqlx::query!(
            r#"
            UPDATE observation
            SET deleted = TRUE, version_id = $2, last_updated = $3
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
        )
        .execute(&mut *tx)
        .await?;

        // Insert delete record into history
        sqlx::query!(
            r#"
            INSERT INTO observation_history (
                id, version_id, last_updated, deleted, content,
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, TRUE, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24)
            "#,
            id,
            new_version_id,
            last_updated,
            &observation.content,
            observation.status,
            &observation.category_system,
            &observation.category_code,
            observation.code_system,
            observation.code_code,
            observation.subject_reference,
            observation.patient_reference,
            observation.encounter_reference,
            observation.effective_datetime,
            observation.effective_period_start,
            observation.effective_period_end,
            observation.issued,
            observation.value_quantity_value,
            observation.value_quantity_unit,
            observation.value_quantity_system,
            &observation.value_codeable_concept_code,
            observation.value_string,
            &observation.performer_reference,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }
}
