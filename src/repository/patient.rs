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
    pub async fn create(&self, mut content: Value) -> Result<Patient> {
        tracing::debug!("Creating new patient resource");

        // Validate resource
        validate_patient(&content)?;

        let id = Uuid::new_v4().to_string();
        let version_id = 1;
        let last_updated = Utc::now();

        tracing::info!(patient_id = %id, "Creating patient");

        // Inject id and meta fields into content before storing
        content = crate::models::patient::inject_id_meta(&content, &id, version_id, &last_updated);

        // Extract search parameters from complete content
        let params = extract_patient_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Insert into current table
        sqlx::query!(
            r#"
            INSERT INTO patient (
                id, version_id, last_updated, content,
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                birthdate, gender, active
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            "#,
            id,
            version_id,
            last_updated,
            content,
            params
                .family_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.family_name[..])),
            params
                .given_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.given_name[..])),
            params
                .prefix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.prefix[..])),
            params
                .suffix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.suffix[..])),
            params
                .name_text
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.name_text[..])),
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
            params.birthdate,
            params.gender,
            params.active,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created patient
        self.read(&id).await
    }

    /// Read current version of a patient (returns raw JSON)
    pub async fn read(&self, id: &str) -> Result<Patient> {
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

    /// Upsert a patient resource with specific ID (FHIR-compliant PUT)
    /// Creates with client-specified ID if doesn't exist, updates if exists
    pub async fn upsert(&self, id: &str, mut content: Value) -> Result<Patient> {
        // Validate resource
        validate_patient(&content)?;

        let mut tx = self.pool.begin().await?;

        // Check if resource exists
        let existing = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM patient
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?;

        if let Some(old_patient) = existing {
            // Resource exists - perform update
            let new_version_id = old_patient.version_id + 1;
            let last_updated = Utc::now();

            // Inject id and meta into content before storing
            content = crate::models::patient::inject_id_meta(&content, id, new_version_id, &last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_patient_search_params(&old_patient.content);

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO patient_history (
                    id, version_id, last_updated, content,
                    family_name, given_name, prefix, suffix, name_text,
                    identifier_system, identifier_value,
                    birthdate, gender, active,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
                "#,
                old_patient.id,
                old_patient.version_id,
                old_patient.last_updated,
                &old_patient.content,
                old_params
                    .family_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.family_name[..])),
                old_params
                    .given_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.given_name[..])),
                old_params
                    .prefix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.prefix[..])),
                old_params
                    .suffix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.suffix[..])),
                old_params
                    .name_text
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.name_text[..])),
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
                old_params.birthdate,
                old_params.gender,
                old_params.active,
                if old_patient.version_id == 1 {
                    "CREATE"
                } else {
                    "UPDATE"
                },
                Utc::now(),
            )
            .execute(&mut *tx)
            .await?;

            // Extract search parameters for new content
            let params = extract_patient_search_params(&content);

            // Update current table with new version
            sqlx::query!(
                r#"
                UPDATE patient
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    family_name = $5,
                    given_name = $6,
                    prefix = $7,
                    suffix = $8,
                    name_text = $9,
                    identifier_system = $10,
                    identifier_value = $11,
                    birthdate = $12,
                    gender = $13,
                    active = $14
                WHERE id = $1
                "#,
                id,
                new_version_id,
                last_updated,
                content,
                params
                    .family_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.family_name[..])),
                params
                    .given_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.given_name[..])),
                params
                    .prefix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.prefix[..])),
                params
                    .suffix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.suffix[..])),
                params
                    .name_text
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.name_text[..])),
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
                params.birthdate,
                params.gender,
                params.active,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return updated patient
            self.read(id).await
        } else {
            // Resource doesn't exist - perform create with specified ID
            let version_id = 1;
            let last_updated = Utc::now();

            tracing::info!(patient_id = %id, "Creating patient with client-specified ID");

            // Inject id and meta into content before storing
            content = crate::models::patient::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_patient_search_params(&content);

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO patient (
                    id, version_id, last_updated, content,
                    family_name, given_name, prefix, suffix, name_text,
                    identifier_system, identifier_value,
                    birthdate, gender, active
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
                "#,
                id,
                version_id,
                last_updated,
                content,
                params
                    .family_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.family_name[..])),
                params
                    .given_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.given_name[..])),
                params
                    .prefix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.prefix[..])),
                params
                    .suffix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.suffix[..])),
                params
                    .name_text
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.name_text[..])),
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
                params.birthdate,
                params.gender,
                params.active,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return the created patient
            self.read(id).await
        }
    }

    /// Update a patient resource (increment version)
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Patient> {
        // Validate resource
        validate_patient(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get current version with lock (only need content to store in history)
        let old_patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
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

        // Inject id and meta into new content before storing
        content = crate::models::patient::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_patient_search_params(&old_patient.content);

        // Insert OLD version into history before updating
        sqlx::query!(
            r#"
            INSERT INTO patient_history (
                id, version_id, last_updated, content,
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                birthdate, gender, active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
            "#,
            old_patient.id,
            old_patient.version_id,
            old_patient.last_updated,
            &old_patient.content,
            old_params
                .family_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.family_name[..])),
            old_params
                .given_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.given_name[..])),
            old_params
                .prefix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.prefix[..])),
            old_params
                .suffix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.suffix[..])),
            old_params
                .name_text
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.name_text[..])),
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
            old_params.birthdate,
            old_params.gender,
            old_params.active,
            if old_patient.version_id == 1 {
                "CREATE"
            } else {
                "UPDATE"
            },
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_patient_search_params(&content);

        // Update current table with new version
        sqlx::query!(
            r#"
            UPDATE patient
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                family_name = $5,
                given_name = $6,
                prefix = $7,
                suffix = $8,
                name_text = $9,
                identifier_system = $10,
                identifier_value = $11,
                birthdate = $12,
                gender = $13,
                active = $14
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params
                .family_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.family_name[..])),
            params
                .given_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.given_name[..])),
            params
                .prefix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.prefix[..])),
            params
                .suffix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.suffix[..])),
            params
                .name_text
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.name_text[..])),
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
            params.birthdate,
            params.gender,
            params.active,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated patient
        self.read(id).await
    }

    /// Delete a patient resource (hard delete)
    pub async fn delete(&self, id: &str) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current patient (to store in history)
        let patient = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
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

        // Extract search params from content for history
        let params = extract_patient_search_params(&patient.content);

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
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                birthdate, gender, active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
            "#,
            id,
            new_version_id,
            last_updated,
            &patient.content,
            params
                .family_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.family_name[..])),
            params
                .given_name
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.given_name[..])),
            params
                .prefix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.prefix[..])),
            params
                .suffix
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.suffix[..])),
            params
                .name_text
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.name_text[..])),
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
            params.birthdate,
            params.gender,
            params.active,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }

    /// Get all versions of a patient from history (includes current version if exists)
    pub async fn history(&self, id: &str, count: Option<i64>) -> Result<Vec<PatientHistory>> {
        // Use LIMIT if count is provided, otherwise fetch all
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                PatientHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM patient_history
                WHERE id = $1
                ORDER BY version_id DESC
                LIMIT $2
                "#,
                id,
                limit
            )
            .fetch_all(&self.pool)
            .await?
        } else {
            sqlx::query_as!(
                PatientHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM patient_history
                WHERE id = $1
                ORDER BY version_id DESC
                "#,
                id
            )
            .fetch_all(&self.pool)
            .await?
        };

        // Try to get current version and add it to history if not already present
        // Only add if we haven't reached the count limit
        if let Ok(current) = self.read(id).await {
            // Check if current version already exists in history
            let current_version_exists = history.iter().any(|h| h.version_id == current.version_id);

            if !current_version_exists {
                // Check if we're at the limit
                let at_limit = count.map(|c| history.len() >= c as usize).unwrap_or(false);

                if !at_limit {
                    // Convert current Patient to PatientHistory format
                    let current_as_history = PatientHistory {
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
        }

        if history.is_empty() {
            return Err(FhirError::NotFound);
        }

        Ok(history)
    }

    /// Read a specific version from history (checks current version first)
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<PatientHistory> {
        // Check if requested version is the current version
        if let Ok(current) = self.read(id).await {
            if current.version_id == version_id {
                // Convert to PatientHistory format (much simpler now!)
                return Ok(PatientHistory {
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
        let patient = sqlx::query_as!(
            PatientHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
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
    pub async fn search(
        &self,
        params: &HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Patient>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT id, version_id, last_updated, content
               FROM patient WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();
        let mut bind_count = 0;

        for condition in &query.conditions {
            match condition {
                SearchCondition::Name(search) => {
                    // Search all HumanName string fields: family, given, prefix, suffix, text
                    match search.modifier {
                        crate::search::StringModifier::Contains => {
                            bind_count += 1;
                            let param_num = bind_count;
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt ILIKE ${0}))",
                                param_num
                            ));
                            bind_values.push(format!("%{}%", search.value));
                        }
                        crate::search::StringModifier::Exact => {
                            bind_count += 1;
                            let param_num = bind_count;
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt = ${0}))",
                                param_num
                            ));
                            bind_values.push(search.value.clone());
                        }
                        crate::search::StringModifier::Missing => {
                            sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL) \
                                         AND (given_name IS NULL OR array_length(given_name, 1) IS NULL) \
                                         AND (prefix IS NULL OR array_length(prefix, 1) IS NULL) \
                                         AND (suffix IS NULL OR array_length(suffix, 1) IS NULL) \
                                         AND (name_text IS NULL OR array_length(name_text, 1) IS NULL)");
                        }
                    }
                }
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
                SearchCondition::FamilyNameOr(searches) => {
                    // Multiple family names with OR logic: (fn ILIKE value1 OR fn ILIKE value2 OR ...)
                    let mut or_conditions = Vec::new();
                    for search in searches {
                        bind_count += 1;
                        match search.modifier {
                            crate::search::StringModifier::Contains => {
                                or_conditions.push(format!("fn ILIKE ${}", bind_count));
                                bind_values.push(format!("%{}%", search.value));
                            }
                            crate::search::StringModifier::Exact => {
                                or_conditions.push(format!("fn = ${}", bind_count));
                                bind_values.push(search.value.clone());
                            }
                            crate::search::StringModifier::Missing => {
                                // Missing doesn't make sense with multiple values, skip
                                bind_count -= 1;
                            }
                        }
                    }
                    if !or_conditions.is_empty() {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE {})", or_conditions.join(" OR ")));
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
                            sql.push_str(
                                " AND (given_name IS NULL OR array_length(given_name, 1) IS NULL)",
                            );
                        }
                    }
                }
                SearchCondition::GivenNameOr(searches) => {
                    let mut or_conditions = Vec::new();
                    for search in searches {
                        bind_count += 1;
                        match search.modifier {
                            crate::search::StringModifier::Contains => {
                                or_conditions.push(format!("gn ILIKE ${}", bind_count));
                                bind_values.push(format!("%{}%", search.value));
                            }
                            crate::search::StringModifier::Exact => {
                                or_conditions.push(format!("gn = ${}", bind_count));
                                bind_values.push(search.value.clone());
                            }
                            crate::search::StringModifier::Missing => {
                                bind_count -= 1;
                            }
                        }
                    }
                    if !or_conditions.is_empty() {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE {})", or_conditions.join(" OR ")));
                    }
                }
                SearchCondition::Identifier(value) => {
                    bind_count += 1;
                    sql.push_str(&format!(
                        " AND EXISTS (SELECT 1 FROM unnest(identifier_value) AS iv WHERE iv = ${})",
                        bind_count
                    ));
                    bind_values.push(value.clone());
                }
                SearchCondition::IdentifierSystemValue { system, value } => {
                    bind_count += 2;
                    sql.push_str(&format!(
                        " AND EXISTS (
                            SELECT 1 FROM unnest(identifier_system, identifier_value) AS ident(sys, val)
                            WHERE ident.sys = ${} AND ident.val = ${}
                        )",
                        bind_count - 1, bind_count
                    ));
                    bind_values.push(system.clone());
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
                    // Cast the string parameter to DATE in SQL
                    sql.push_str(&format!(" AND birthdate {} ${}::date", op, bind_count));
                    // Format as YYYY-MM-DD which PostgreSQL expects
                    bind_values.push(comparison.value.format("%Y-%m-%d").to_string());
                }
                SearchCondition::Gender(gender) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND gender = ${}", bind_count));
                    bind_values.push(gender.clone());
                }
                SearchCondition::GenderOr(genders) => {
                    // Multiple genders with OR logic: (gender = value1 OR gender = value2 OR ...)
                    let mut or_conditions = Vec::new();
                    for gender in genders {
                        bind_count += 1;
                        or_conditions.push(format!("gender = ${}", bind_count));
                        bind_values.push(gender.clone());
                    }
                    if !or_conditions.is_empty() {
                        sql.push_str(&format!(" AND ({})", or_conditions.join(" OR ")));
                    }
                }
                SearchCondition::Active(active) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND active = ${}", bind_count));
                    bind_values.push(active.to_string());
                }
                SearchCondition::ActiveMissing(is_missing) => {
                    if *is_missing {
                        // Field IS NULL
                        sql.push_str(" AND active IS NULL");
                    } else {
                        // Field IS NOT NULL
                        sql.push_str(" AND active IS NOT NULL");
                    }
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

        // Add LIMIT and OFFSET directly (not as bind parameters since they're integers we control)
        sql.push_str(&format!(" LIMIT {}", query.limit));
        sql.push_str(&format!(" OFFSET {}", query.offset));

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
            SearchCondition::Name(search) => {
                bind_count += 1;
                let param_num = bind_count;
                match search.modifier {
                    crate::search::StringModifier::Contains => {
                        sql.push_str(&format!(
                            " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p ILIKE ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s ILIKE ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt ILIKE ${0}))",
                            param_num
                        ));
                    }
                    crate::search::StringModifier::Exact => {
                        sql.push_str(&format!(
                            " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p = ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s = ${0}) \
                             OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt = ${0}))",
                            param_num
                        ));
                    }
                    crate::search::StringModifier::Missing => {
                        sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL) \
                                     AND (given_name IS NULL OR array_length(given_name, 1) IS NULL) \
                                     AND (prefix IS NULL OR array_length(prefix, 1) IS NULL) \
                                     AND (suffix IS NULL OR array_length(suffix, 1) IS NULL) \
                                     AND (name_text IS NULL OR array_length(name_text, 1) IS NULL)");
                        bind_count -= 1;
                    }
                }
            }
            SearchCondition::FamilyName(search) => {
                bind_count += 1;
                match search.modifier {
                    crate::search::StringModifier::Contains => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${})", bind_count));
                    }
                    crate::search::StringModifier::Exact => {
                        sql.push_str(&format!(
                            " AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${})",
                            bind_count
                        ));
                    }
                    crate::search::StringModifier::Missing => {
                        sql.push_str(
                            " AND (family_name IS NULL OR array_length(family_name, 1) IS NULL)",
                        );
                        bind_count -= 1;
                    }
                }
            }
            SearchCondition::FamilyNameOr(searches) => {
                let mut or_conditions = Vec::new();
                for search in searches {
                    bind_count += 1;
                    match search.modifier {
                        crate::search::StringModifier::Contains => {
                            or_conditions.push(format!("fn ILIKE ${}", bind_count));
                        }
                        crate::search::StringModifier::Exact => {
                            or_conditions.push(format!("fn = ${}", bind_count));
                        }
                        crate::search::StringModifier::Missing => {
                            bind_count -= 1;
                        }
                    }
                }
                if !or_conditions.is_empty() {
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE {})", or_conditions.join(" OR ")));
                }
            }
            SearchCondition::GivenName(search) => {
                bind_count += 1;
                match search.modifier {
                    crate::search::StringModifier::Contains => {
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${})", bind_count));
                    }
                    crate::search::StringModifier::Exact => {
                        sql.push_str(&format!(
                            " AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${})",
                            bind_count
                        ));
                    }
                    crate::search::StringModifier::Missing => {
                        sql.push_str(
                            " AND (given_name IS NULL OR array_length(given_name, 1) IS NULL)",
                        );
                        bind_count -= 1;
                    }
                }
            }
            SearchCondition::GivenNameOr(searches) => {
                let mut or_conditions = Vec::new();
                for search in searches {
                    bind_count += 1;
                    match search.modifier {
                        crate::search::StringModifier::Contains => {
                            or_conditions.push(format!("gn ILIKE ${}", bind_count));
                        }
                        crate::search::StringModifier::Exact => {
                            or_conditions.push(format!("gn = ${}", bind_count));
                        }
                        crate::search::StringModifier::Missing => {
                            bind_count -= 1;
                        }
                    }
                }
                if !or_conditions.is_empty() {
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE {})", or_conditions.join(" OR ")));
                }
            }
            SearchCondition::Identifier(value) => {
                bind_count += 1;
                sql.push_str(&format!(
                    " AND EXISTS (SELECT 1 FROM unnest(identifier_value) AS iv WHERE iv = ${})",
                    bind_count
                ));
                let _ = value; // Silence unused warning
            }
            SearchCondition::IdentifierSystemValue { system: _, value: _ } => {
                bind_count += 2;
                sql.push_str(&format!(
                    " AND EXISTS (
                        SELECT 1 FROM unnest(identifier_system, identifier_value) AS ident(sys, val)
                        WHERE ident.sys = ${} AND ident.val = ${}
                    )",
                    bind_count - 1, bind_count
                ));
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
                sql.push_str(&format!(" AND birthdate {} ${}::date", op, bind_count));
            }
            SearchCondition::Gender(_gender) => {
                bind_count += 1;
                sql.push_str(&format!(" AND gender = ${}", bind_count));
            }
            SearchCondition::GenderOr(genders) => {
                let mut or_conditions = Vec::new();
                for _gender in genders {
                    bind_count += 1;
                    or_conditions.push(format!("gender = ${}", bind_count));
                }
                if !or_conditions.is_empty() {
                    sql.push_str(&format!(" AND ({})", or_conditions.join(" OR ")));
                }
            }
            SearchCondition::Active(_active) => {
                bind_count += 1;
                sql.push_str(&format!(" AND active = ${}", bind_count));
            }
            SearchCondition::ActiveMissing(is_missing) => {
                if *is_missing {
                    sql.push_str(" AND active IS NULL");
                } else {
                    sql.push_str(" AND active IS NOT NULL");
                }
            }
        }
    }

    sql
}

impl PatientRepository {
    /// Find observations that reference a specific patient (for _revinclude support)
    pub async fn find_observations_by_patient(
        &self,
        patient_id: &str,
    ) -> Result<Vec<Observation>> {
        let patient_ref = format!("Patient/{}", patient_id);

        let observations = sqlx::query_as!(
            Observation,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM observation
            WHERE patient_reference = $1
            "#,
            patient_ref
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(observations)
    }

    /// Rollback a patient to a specific version (deletes all versions >= rollback_to_version)
    /// This is a destructive operation for dev/test purposes only
    pub async fn rollback(&self, id: &str, rollback_to_version: i32) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get all version IDs for this patient from history
        let version_ids: Vec<i32> = sqlx::query_scalar!(
            r#"
            SELECT version_id
            FROM patient_history
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
            FROM patient
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
                "No versions to rollback for patient {}",
                id
            )));
        }

        tracing::info!(
            patient_id = %id,
            rollback_to = rollback_to_version,
            deleting_versions = ?versions_to_delete,
            new_current = ?new_current_version,
            "Rolling back patient"
        );

        // Delete versions from history
        for version in &versions_to_delete {
            sqlx::query!(
                r#"
                DELETE FROM patient_history
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

            let params = extract_patient_search_params(&restored.content);

            sqlx::query!(
                r#"
                UPDATE patient
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    family_name = $5,
                    given_name = $6,
                    prefix = $7,
                    suffix = $8,
                    name_text = $9,
                    identifier_system = $10,
                    identifier_value = $11,
                    birthdate = $12,
                    gender = $13,
                    active = $14
                WHERE id = $1
                "#,
                id,
                new_version,
                restored.last_updated,
                &restored.content,
                params
                    .family_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.family_name[..])),
                params
                    .given_name
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.given_name[..])),
                params
                    .prefix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.prefix[..])),
                params
                    .suffix
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.suffix[..])),
                params
                    .name_text
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.name_text[..])),
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
                params.birthdate,
                params.gender,
                params.active,
            )
            .execute(&mut *tx)
            .await?;
        } else {
            // No versions remain, delete current resource
            sqlx::query!(
                r#"
                DELETE FROM patient
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

    /// Get type-level history - all versions of all patients
    pub async fn type_history(&self, count: Option<i64>) -> Result<Vec<PatientHistory>> {
        // Query all history records with optional limit
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                PatientHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM patient_history
                ORDER BY history_timestamp DESC
                LIMIT $1
                "#,
                limit
            )
            .fetch_all(&self.pool)
            .await?
        } else {
            sqlx::query_as!(
                PatientHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM patient_history
                ORDER BY history_timestamp DESC
                "#
            )
            .fetch_all(&self.pool)
            .await?
        };

        // Also get all current versions
        let current_patients = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM patient
            "#
        )
        .fetch_all(&self.pool)
        .await?;

        // Add current versions to history if not already present and not at limit
        for current in current_patients {
            // Check if we're at the limit
            let at_limit = count.map(|c| history.len() >= c as usize).unwrap_or(false);

            if at_limit {
                break;
            }

            let current_version_exists = history
                .iter()
                .any(|h| h.id == current.id && h.version_id == current.version_id);

            if !current_version_exists {
                let current_as_history = PatientHistory {
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
}
