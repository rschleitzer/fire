use chrono::Utc;
use serde_json::Value;
use sqlx::{PgPool, Row};
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::observation::Observation;
use crate::models::patient::{extract_patient_search_params, Patient, PatientHistory};
use crate::search::{SearchCondition, SearchQuery};
use crate::services::validate_patient;

pub struct PatientRepository {
    pool: PgPool,
}

impl PatientRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// Create a new patient resource (version 1)
    pub async fn create(&self, content: Value) -> Result<Patient> {
        tracing::debug!("Creating new patient resource");

        // Validate resource
        validate_patient(&content)?;

        let id = Uuid::new_v4();
        let version_id = 1;
        let last_updated = Utc::now();

        tracing::info!(patient_id = %id, "Creating patient");

        // Extract search parameters
        let params = extract_patient_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Insert into current table
        let patient = sqlx::query_as!(
            Patient,
            r#"
            INSERT INTO patient (
                id, version_id, last_updated, content,
                family_name, given_name, identifier_system, identifier_value,
                birthdate, gender, active
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
            RETURNING
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active
            "#,
            id,
            version_id,
            last_updated,
            content,
            params.family_name.is_empty().then_some(None).unwrap_or(Some(params.family_name)),
            params.given_name.is_empty().then_some(None).unwrap_or(Some(params.given_name)),
            params.identifier_system.is_empty().then_some(None).unwrap_or(Some(params.identifier_system)),
            params.identifier_value.is_empty().then_some(None).unwrap_or(Some(params.identifier_value)),
            params.birthdate,
            params.gender,
            params.active,
        )
        .fetch_one(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(patient)
    }

    /// Read current version of a patient
    pub async fn read(&self, id: &Uuid) -> Result<Patient> {
        let patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active
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

    /// Update a patient resource (increment version)
    pub async fn update(&self, id: &Uuid, content: Value) -> Result<Patient> {
        // Validate resource
        validate_patient(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get current version with lock (need full data to store in history)
        let old_patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active
            FROM patient
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = old_patient.version_id + 1;
        let last_updated = Utc::now();

        // Insert OLD version into history before updating
        sqlx::query!(
            r#"
            INSERT INTO patient_history (
                id, version_id, last_updated, content,
                family_name, given_name, identifier_system, identifier_value,
                birthdate, gender, active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
            "#,
            old_patient.id,
            old_patient.version_id,
            old_patient.last_updated,
            &old_patient.content,
            &old_patient.family_name,
            &old_patient.given_name,
            &old_patient.identifier_system,
            &old_patient.identifier_value,
            old_patient.birthdate,
            &old_patient.gender,
            old_patient.active,
            "UPDATE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_patient_search_params(&content);

        // Update current table with new version
        let patient = sqlx::query_as!(
            Patient,
            r#"
            UPDATE patient
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                family_name = $5,
                given_name = $6,
                identifier_system = $7,
                identifier_value = $8,
                birthdate = $9,
                gender = $10,
                active = $11
            WHERE id = $1
            RETURNING
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params.family_name.is_empty().then_some(None).unwrap_or(Some(params.family_name)),
            params.given_name.is_empty().then_some(None).unwrap_or(Some(params.given_name)),
            params.identifier_system.is_empty().then_some(None).unwrap_or(Some(params.identifier_system)),
            params.identifier_value.is_empty().then_some(None).unwrap_or(Some(params.identifier_value)),
            params.birthdate,
            params.gender,
            params.active,
        )
        .fetch_one(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(patient)
    }

    /// Delete a patient resource (hard delete)
    pub async fn delete(&self, id: &Uuid) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current patient (to store in history)
        let patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active
            FROM patient
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = patient.version_id + 1;
        let last_updated = Utc::now();

        // Delete from current table
        sqlx::query!(
            r#"
            DELETE FROM patient
            WHERE id = $1
            "#,
            id,
        )
        .execute(&mut *tx)
        .await?;

        // Insert delete record into history
        sqlx::query!(
            r#"
            INSERT INTO patient_history (
                id, version_id, last_updated, content,
                family_name, given_name, identifier_system, identifier_value,
                birthdate, gender, active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
            "#,
            id,
            new_version_id,
            last_updated,
            &patient.content,
            &patient.family_name,
            &patient.given_name,
            &patient.identifier_system,
            &patient.identifier_value,
            patient.birthdate,
            &patient.gender,
            patient.active,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }

    /// Get all versions of a patient from history (includes current version if exists)
    pub async fn history(&self, id: &Uuid) -> Result<Vec<PatientHistory>> {
        let mut history = sqlx::query_as!(
            PatientHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active,
                history_operation, history_timestamp
            FROM patient_history
            WHERE id = $1
            ORDER BY version_id DESC
            "#,
            id
        )
        .fetch_all(&self.pool)
        .await?;

        // Try to get current version and add it to history
        if let Ok(current) = self.read(id).await {
            // Convert current Patient to PatientHistory format
            // Current version isn't in history yet (only gets added on update/delete)
            let current_as_history = PatientHistory {
                id: current.id,
                version_id: current.version_id,
                last_updated: current.last_updated,
                content: current.content,
                family_name: current.family_name,
                given_name: current.given_name,
                identifier_system: current.identifier_system,
                identifier_value: current.identifier_value,
                birthdate: current.birthdate,
                gender: current.gender,
                active: current.active,
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
    pub async fn read_version(&self, id: &Uuid, version_id: i32) -> Result<PatientHistory> {
        // Check if requested version is the current version
        if let Ok(current) = self.read(id).await {
            if current.version_id == version_id {
                // Convert to PatientHistory format
                return Ok(PatientHistory {
                    id: current.id,
                    version_id: current.version_id,
                    last_updated: current.last_updated,
                    content: current.content,
                    family_name: current.family_name,
                    given_name: current.given_name,
                    identifier_system: current.identifier_system,
                    identifier_value: current.identifier_value,
                    birthdate: current.birthdate,
                    gender: current.gender,
                    active: current.active,
                    history_operation: if current.version_id == 1 { "CREATE".to_string() } else { "UPDATE".to_string() },
                    history_timestamp: current.last_updated,
                });
            }
        }

        // Not current version, check history table
        let patient = sqlx::query_as!(
            PatientHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                family_name as "family_name?",
                given_name as "given_name?",
                identifier_system as "identifier_system?",
                identifier_value as "identifier_value?",
                birthdate, gender, active,
                history_operation, history_timestamp
            FROM patient_history
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(patient)
    }

    /// Search for patients based on parameters
    pub async fn search(&self, params: &HashMap<String, String>, include_total: bool) -> Result<(Vec<Patient>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT id, version_id, last_updated, content,
               family_name, given_name, identifier_system, identifier_value,
               birthdate, gender, active
               FROM patient WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();
        let mut bind_count = 0;

        for condition in &query.conditions {
            match condition {
                SearchCondition::FamilyName(search) => {
                    match search.modifier {
                        crate::search::StringModifier::Contains => {
                            bind_count += 1;
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${})", bind_count));
                            bind_values.push(format!("%{}%", search.value));
                        }
                        crate::search::StringModifier::Exact => {
                            bind_count += 1;
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${})", bind_count));
                            bind_values.push(search.value.clone());
                        }
                        crate::search::StringModifier::Missing => {
                            sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL)");
                        }
                    }
                }
                SearchCondition::GivenName(search) => {
                    match search.modifier {
                        crate::search::StringModifier::Contains => {
                            bind_count += 1;
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${})", bind_count));
                            bind_values.push(format!("%{}%", search.value));
                        }
                        crate::search::StringModifier::Exact => {
                            bind_count += 1;
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${})", bind_count));
                            bind_values.push(search.value.clone());
                        }
                        crate::search::StringModifier::Missing => {
                            sql.push_str(" AND (given_name IS NULL OR array_length(given_name, 1) IS NULL)");
                        }
                    }
                }
                SearchCondition::Identifier(value) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(identifier_value) AS iv WHERE iv = ${})", bind_count));
                    bind_values.push(value.clone());
                }
                SearchCondition::Birthdate(comparison) => {
                    bind_count += 1;
                    let op = match comparison.prefix {
                        crate::search::DatePrefix::Eq => "=",
                        crate::search::DatePrefix::Ne => "!=",
                        crate::search::DatePrefix::Gt => ">",
                        crate::search::DatePrefix::Lt => "<",
                        crate::search::DatePrefix::Ge => ">=",
                        crate::search::DatePrefix::Le => "<=",
                    };
                    sql.push_str(&format!(" AND birthdate {} ${}", op, bind_count));
                    bind_values.push(comparison.value.to_string());
                }
                SearchCondition::Gender(gender) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND gender = ${}", bind_count));
                    bind_values.push(gender.clone());
                }
                SearchCondition::Active(active) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND active = ${}", bind_count));
                    bind_values.push(active.to_string());
                }
            }
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
        }

        bind_count += 1;
        sql.push_str(&format!(" LIMIT ${}", bind_count));
        bind_values.push(query.limit.to_string());
        bind_count += 1;
        sql.push_str(&format!(" OFFSET ${}", bind_count));
        bind_values.push(query.offset.to_string());

        // Build dynamic query
        let mut query_builder = sqlx::query(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let rows = query_builder.fetch_all(&self.pool).await?;

        let patients = rows
            .into_iter()
            .map(|row| Patient {
                id: row.get("id"),
                version_id: row.get("version_id"),
                last_updated: row.get("last_updated"),
                content: row.get("content"),
                family_name: row.get("family_name"),
                given_name: row.get("given_name"),
                identifier_system: row.get("identifier_system"),
                identifier_value: row.get("identifier_value"),
                birthdate: row.get("birthdate"),
                gender: row.get("gender"),
                active: row.get("active"),
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

        Ok((patients, total))
    }
}

fn build_count_sql(query: &SearchQuery) -> String {
    let mut sql = String::from("SELECT COUNT(*) FROM patient WHERE 1=1");
    let mut bind_count = 0;

    for condition in &query.conditions {
        match condition {
            SearchCondition::FamilyName(search) => {
                bind_count += 1;
                match search.modifier {
                    crate::search::StringModifier::Contains => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${})", bind_count));
                    }
                    crate::search::StringModifier::Exact => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${})", bind_count));
                    }
                    crate::search::StringModifier::Missing => {
                        sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL)");
                        bind_count -= 1;
                    }
                }
            }
            SearchCondition::GivenName(search) => {
                bind_count += 1;
                match search.modifier {
                    crate::search::StringModifier::Contains => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${})", bind_count));
                    }
                    crate::search::StringModifier::Exact => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${})", bind_count));
                    }
                    crate::search::StringModifier::Missing => {
                        sql.push_str(" AND (given_name IS NULL OR array_length(given_name, 1) IS NULL)");
                        bind_count -= 1;
                    }
                }
            }
            SearchCondition::Identifier(value) => {
                bind_count += 1;
                sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(identifier_value) AS iv WHERE iv = ${})", bind_count));
                let _ = value; // Silence unused warning
            }
            SearchCondition::Birthdate(comparison) => {
                bind_count += 1;
                let op = match comparison.prefix {
                    crate::search::DatePrefix::Eq => "=",
                    crate::search::DatePrefix::Ne => "!=",
                    crate::search::DatePrefix::Gt => ">",
                    crate::search::DatePrefix::Lt => "<",
                    crate::search::DatePrefix::Ge => ">=",
                    crate::search::DatePrefix::Le => "<=",
                };
                sql.push_str(&format!(" AND birthdate {} ${}", op, bind_count));
            }
            SearchCondition::Gender(_gender) => {
                bind_count += 1;
                sql.push_str(&format!(" AND gender = ${}", bind_count));
            }
            SearchCondition::Active(_active) => {
                bind_count += 1;
                sql.push_str(&format!(" AND active = ${}", bind_count));
            }
        }
    }

    sql
}

impl PatientRepository {
    /// Find observations that reference a specific patient (for _revinclude support)
    pub async fn find_observations_by_patient(&self, patient_id: &Uuid) -> Result<Vec<Observation>> {
        let patient_ref = format!("Patient/{}", patient_id);

        let observations = sqlx::query_as!(
            Observation,
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
            WHERE patient_reference = $1
            "#,
            patient_ref
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(observations)
    }
}
