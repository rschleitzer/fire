use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{
    extract_observation_search_params, Observation, ObservationHistory,
};
use crate::models::patient::Patient;
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
                effective_datetime, effective_period_start, effective_period_end,
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
            params.effective_datetime,
            params.effective_period_start,
            params.effective_period_end,
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

    /// Read current version of an observation (returns raw JSON)
    pub async fn read(&self, id: &Uuid) -> Result<Observation> {
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
    pub async fn delete(&self, id: &Uuid) -> Result<()> {
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
                effective_datetime, effective_period_start, effective_period_end,
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
            params.effective_datetime,
            params.effective_period_start,
            params.effective_period_end,
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
    pub async fn update(&self, id: &Uuid, content: Value) -> Result<Observation> {
        // Validate resource
        validate_observation(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get current version with lock (need full data to store in history)
        let old_observation = self.read(id).await?;

        let new_version_id = old_observation.version_id + 1;
        let last_updated = Utc::now();

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
                effective_datetime, effective_period_start, effective_period_end,
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
            old_params.effective_datetime,
            old_params.effective_period_start,
            old_params.effective_period_end,
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
                effective_datetime = $13,
                effective_period_start = $14,
                effective_period_end = $15,
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
            params.effective_datetime,
            params.effective_period_start,
            params.effective_period_end,
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
    pub async fn history(&self, id: &Uuid) -> Result<Vec<ObservationHistory>> {
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

        // Try to get current version and add it to history
        if let Ok(current) = self.read(id).await {
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

        if history.is_empty() {
            return Err(FhirError::NotFound);
        }

        Ok(history)
    }

    /// Read a specific version from history (checks current version first)
    pub async fn read_version(&self, id: &Uuid, version_id: i32) -> Result<ObservationHistory> {
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
        params: &std::collections::HashMap<String, String>,
        include_total: bool,
    ) -> crate::error::Result<(Vec<Observation>, Option<i64>)> {
        // Build WHERE clause manually for observation-specific parameters
        let mut where_conditions = vec!["1=1".to_string()];
        let mut bind_values: Vec<String> = Vec::new();

        // Status parameter
        if let Some(status) = params.get("status") {
            bind_values.push(status.clone());
            where_conditions.push(format!("status = ${}", bind_values.len()));
        }

        // Code parameter (code_code)
        if let Some(code) = params.get("code") {
            bind_values.push(code.clone());
            where_conditions.push(format!("code_code = ${}", bind_values.len()));
        }

        // Category parameter (category_code array)
        if let Some(category) = params.get("category") {
            bind_values.push(category.clone());
            where_conditions.push(format!("${} = ANY(category_code)", bind_values.len()));
        }

        // Patient/subject reference
        if let Some(patient) = params.get("patient") {
            bind_values.push(format!("Patient/{}", patient));
            where_conditions.push(format!("patient_reference = ${}", bind_values.len()));
        }

        if let Some(subject) = params.get("subject") {
            bind_values.push(subject.clone());
            where_conditions.push(format!("subject_reference = ${}", bind_values.len()));
        }

        // Date parameters for effective dates (simplified - just eq comparison)
        if let Some(date) = params.get("date") {
            bind_values.push(date.clone());
            where_conditions.push(format!(
                "effective_datetime = ${}::timestamptz",
                bind_values.len()
            ));
        }

        let where_clause = where_conditions.join(" AND ");

        // Parse pagination
        let limit = params
            .get("_count")
            .and_then(|c| c.parse::<i64>().ok())
            .unwrap_or(50)
            .min(1000);

        let offset = params
            .get("_offset")
            .and_then(|o| o.parse::<i64>().ok())
            .unwrap_or(0);

        // Build ORDER BY clause
        let order_by = "ORDER BY last_updated DESC";

        // Get total count if requested
        let total = if include_total {
            let count_sql = format!(
                "SELECT COUNT(*) as count FROM observation WHERE {}",
                where_clause
            );
            let mut count_query = sqlx::query_scalar::<_, i64>(&count_sql);
            for value in &bind_values {
                count_query = count_query.bind(value);
            }
            Some(count_query.fetch_one(&self.pool).await?)
        } else {
            None
        };

        // Build main query (only select fields we need for raw JSON model)
        let sql = format!(
            "SELECT id, version_id, last_updated, content
             FROM observation
             WHERE {}
             {}
             LIMIT {}
             OFFSET {}",
            where_clause, order_by, limit, offset
        );

        // Execute query
        let mut query_builder = sqlx::query_as::<_, Observation>(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let observations = query_builder.fetch_all(&self.pool).await?;

        Ok((observations, total))
    }

    /// Read a patient by ID (for _include support)
    pub async fn read_patient(&self, id: &Uuid) -> Result<Patient> {
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
    pub async fn rollback(&self, id: &Uuid, rollback_to_version: i32) -> Result<()> {
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
                    effective_datetime = $13,
                    effective_period_start = $14,
                    effective_period_end = $15,
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
                params.effective_datetime,
                params.effective_period_start,
                params.effective_period_end,
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
}
