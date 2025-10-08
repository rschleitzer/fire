use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::{extract_observation_search_params, Observation, ObservationHistory};
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
            sqlx::types::BigDecimal::try_from(v).unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
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
            params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(params.triggered_by_observation.clone())),
            params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(params.triggered_by_type.clone())),
            params.focus_reference.is_empty().then_some(None).unwrap_or(Some(params.focus_reference.clone())),
            params.body_structure_reference,
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
                id, version_id, last_updated,
                content as "content: Value",
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference
            FROM observation
            WHERE id = $1
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
            triggered_by_observation: row.triggered_by_observation,
            triggered_by_type: row.triggered_by_type,
            focus_reference: row.focus_reference,
            body_structure_reference: row.body_structure_reference,
        })
    }

    /// Delete an observation (hard delete)
    pub async fn delete(&self, id: &Uuid) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current observation (to store in history)
        let observation = self.read(id).await?;
        let new_version_id = observation.version_id + 1;
        let last_updated = Utc::now();

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
            &observation.triggered_by_observation,
            &observation.triggered_by_type,
            &observation.focus_reference,
            observation.body_structure_reference,
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
            old_observation.status,
            &old_observation.category_system,
            &old_observation.category_code,
            old_observation.code_system,
            old_observation.code_code,
            old_observation.subject_reference,
            old_observation.patient_reference,
            old_observation.encounter_reference,
            old_observation.effective_datetime,
            old_observation.effective_period_start,
            old_observation.effective_period_end,
            old_observation.issued,
            old_observation.value_quantity_value,
            old_observation.value_quantity_unit,
            old_observation.value_quantity_system,
            &old_observation.value_codeable_concept_code,
            old_observation.value_string,
            &old_observation.performer_reference,
            &old_observation.triggered_by_observation,
            &old_observation.triggered_by_type,
            &old_observation.focus_reference,
            old_observation.body_structure_reference,
            "UPDATE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_observation_search_params(&content);

        // Convert f64 to BigDecimal
        let value_qty_decimal = params.value_quantity_value.map(|v| {
            sqlx::types::BigDecimal::try_from(v).unwrap_or_else(|_| sqlx::types::BigDecimal::from(0))
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
            params.performer_reference.is_empty().then_some(None).unwrap_or(Some(params.performer_reference.clone())),
            params.triggered_by_observation.is_empty().then_some(None).unwrap_or(Some(params.triggered_by_observation.clone())),
            params.triggered_by_type.is_empty().then_some(None).unwrap_or(Some(params.triggered_by_type.clone())),
            params.focus_reference.is_empty().then_some(None).unwrap_or(Some(params.focus_reference.clone())),
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
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference,
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
                status: current.status,
                category_system: current.category_system,
                category_code: current.category_code,
                code_system: current.code_system,
                code_code: current.code_code,
                subject_reference: current.subject_reference,
                patient_reference: current.patient_reference,
                encounter_reference: current.encounter_reference,
                effective_datetime: current.effective_datetime,
                effective_period_start: current.effective_period_start,
                effective_period_end: current.effective_period_end,
                issued: current.issued,
                value_quantity_value: current.value_quantity_value,
                value_quantity_unit: current.value_quantity_unit,
                value_quantity_system: current.value_quantity_system,
                value_codeable_concept_code: current.value_codeable_concept_code,
                value_string: current.value_string,
                performer_reference: current.performer_reference,
                triggered_by_observation: current.triggered_by_observation,
                triggered_by_type: current.triggered_by_type,
                focus_reference: current.focus_reference,
                body_structure_reference: current.body_structure_reference,
                history_operation: if current.version_id == 1 { "CREATE".to_string() } else { "UPDATE".to_string() },
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
                    status: current.status,
                    category_system: current.category_system,
                    category_code: current.category_code,
                    code_system: current.code_system,
                    code_code: current.code_code,
                    subject_reference: current.subject_reference,
                    patient_reference: current.patient_reference,
                    encounter_reference: current.encounter_reference,
                    effective_datetime: current.effective_datetime,
                    effective_period_start: current.effective_period_start,
                    effective_period_end: current.effective_period_end,
                    issued: current.issued,
                    value_quantity_value: current.value_quantity_value,
                    value_quantity_unit: current.value_quantity_unit,
                    value_quantity_system: current.value_quantity_system,
                    value_codeable_concept_code: current.value_codeable_concept_code,
                    value_string: current.value_string,
                    performer_reference: current.performer_reference,
                    triggered_by_observation: current.triggered_by_observation,
                    triggered_by_type: current.triggered_by_type,
                    focus_reference: current.focus_reference,
                    body_structure_reference: current.body_structure_reference,
                    history_operation: if current.version_id == 1 { "CREATE".to_string() } else { "UPDATE".to_string() },
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
                status, category_system, category_code,
                code_system, code_code,
                subject_reference, patient_reference, encounter_reference,
                effective_datetime, effective_period_start, effective_period_end,
                issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                value_codeable_concept_code, value_string, performer_reference,
                triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference,
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
    pub async fn search(&self, params: &std::collections::HashMap<String, String>, include_total: bool) -> crate::error::Result<(Vec<Observation>, Option<i64>)> {
        use crate::search::SearchQuery;

        let query = SearchQuery::from_params(params)?;

        // Build WHERE clause
        let mut where_conditions = vec!["1=1".to_string()];
        let mut bind_values: Vec<String> = Vec::new();

        // Status parameter
        if let Some(status) = query.string_params.get("status") {
            bind_values.push(status.value.clone());
            where_conditions.push(format!("status = ${}", bind_values.len()));
        }

        // Code parameter (code_code)
        if let Some(code) = query.string_params.get("code") {
            bind_values.push(code.value.clone());
            where_conditions.push(format!("code_code = ${}", bind_values.len()));
        }

        // Category parameter (category_code array)
        if let Some(category) = query.string_params.get("category") {
            bind_values.push(category.value.clone());
            where_conditions.push(format!("${} = ANY(category_code)", bind_values.len()));
        }

        // Patient/subject reference
        if let Some(patient) = query.string_params.get("patient") {
            bind_values.push(format!("Patient/{}", patient.value));
            where_conditions.push(format!("patient_reference = ${}", bind_values.len()));
        }

        if let Some(subject) = query.string_params.get("subject") {
            bind_values.push(subject.value.clone());
            where_conditions.push(format!("subject_reference = ${}", bind_values.len()));
        }

        // Date parameters for effective dates
        if let Some(date_param) = query.date_params.get("date") {
            let column = "effective_datetime";
            match date_param.prefix.as_str() {
                "eq" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} = ${}::timestamptz", column, bind_values.len()));
                }
                "ne" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} != ${}::timestamptz", column, bind_values.len()));
                }
                "gt" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} > ${}::timestamptz", column, bind_values.len()));
                }
                "lt" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} < ${}::timestamptz", column, bind_values.len()));
                }
                "ge" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} >= ${}::timestamptz", column, bind_values.len()));
                }
                "le" => {
                    bind_values.push(date_param.value.clone());
                    where_conditions.push(format!("{} <= ${}::timestamptz", column, bind_values.len()));
                }
                _ => {}
            }
        }

        let where_clause = where_conditions.join(" AND ");

        // Build ORDER BY clause
        let order_by = if query.sort.is_empty() {
            "ORDER BY last_updated DESC".to_string()
        } else {
            let mut order_parts = Vec::new();
            for sort in &query.sort {
                let direction = match sort.direction {
                    crate::search::SortDirection::Ascending => "ASC",
                    crate::search::SortDirection::Descending => "DESC",
                };
                let column = match sort.field.as_str() {
                    "status" => "status",
                    "code" => "code_code",
                    "date" => "effective_datetime",
                    "patient" => "patient_reference",
                    "subject" => "subject_reference",
                    "_lastUpdated" => "last_updated",
                    _ => continue,
                };
                order_parts.push(format!("{} {}", column, direction));
            }
            if order_parts.is_empty() {
                "ORDER BY last_updated DESC".to_string()
            } else {
                format!("ORDER BY {}", order_parts.join(", "))
            }
        };

        // Get total count if requested
        let total = if include_total {
            let count_sql = format!("SELECT COUNT(*) as count FROM observation WHERE {}", where_clause);
            let mut count_query = sqlx::query_scalar::<_, i64>(&count_sql);
            for value in &bind_values {
                count_query = count_query.bind(value);
            }
            Some(count_query.fetch_one(&self.pool).await?)
        } else {
            None
        };

        // Build main query
        let sql = format!(
            "SELECT id, version_id, last_updated, content as \"content: Value\",
                    status, category_system, category_code, code_system, code_code,
                    subject_reference, patient_reference, encounter_reference,
                    effective_datetime, effective_period_start, effective_period_end,
                    issued, value_quantity_value, value_quantity_unit, value_quantity_system,
                    value_codeable_concept_code, value_string, performer_reference,
                    triggered_by_observation, triggered_by_type, focus_reference, body_structure_reference
             FROM observation
             WHERE {}
             {}
             LIMIT {}
             OFFSET {}",
            where_clause,
            order_by,
            query.count.unwrap_or(50).min(1000),
            query.offset.unwrap_or(0)
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
        let row = sqlx::query!(
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name, given_name,
                identifier_system, identifier_value,
                birthdate, gender, active
            FROM patient
            WHERE id = $1
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(Patient {
            id: row.id,
            version_id: row.version_id,
            last_updated: row.last_updated,
            content: row.content,
            family_name: row.family_name,
            given_name: row.given_name,
            identifier_system: row.identifier_system,
            identifier_value: row.identifier_value,
            birthdate: row.birthdate,
            gender: row.gender,
            active: row.active,
        })
    }
}
