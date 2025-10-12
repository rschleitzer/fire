use chrono::Utc;
use serde_json::Value;
use sqlx::{PgPool, Row};
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::practitioner::{extract_practitioner_search_params, Practitioner, PractitionerHistory};

pub struct PractitionerRepository {
    pool: PgPool,
}

impl PractitionerRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// Create a new practitioner resource (version 1)
    pub async fn create(&self, mut content: Value) -> Result<Practitioner> {
        tracing::debug!("Creating new practitioner resource");

        let id = Uuid::new_v4().to_string();
        let version_id = 1;
        let last_updated = Utc::now();

        tracing::info!(practitioner_id = %id, "Creating practitioner");

        // Inject id and meta fields into content before storing
        content = crate::models::practitioner::inject_id_meta(&content, &id, version_id, &last_updated);

        // Extract search parameters from complete content
        let params = extract_practitioner_search_params(&content);

        let mut tx = self.pool.begin().await?;

        // Insert into current table
        sqlx::query!(
            r#"
            INSERT INTO practitioner (
                id, version_id, last_updated, content,
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                telecom_value,
                active
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
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
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
            params.active,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return the created practitioner
        self.read(&id).await
    }

    /// Read current version of a practitioner (returns raw JSON)
    pub async fn read(&self, id: &str) -> Result<Practitioner> {
        let practitioner = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
            WHERE id = $1
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(practitioner)
    }

    /// Upsert a practitioner resource with specific ID (FHIR-compliant PUT)
    /// Creates with client-specified ID if doesn't exist, updates if exists
    pub async fn upsert(&self, id: &str, mut content: Value) -> Result<Practitioner> {
        let mut tx = self.pool.begin().await?;

        // Check if resource exists
        let existing = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?;

        if let Some(old_practitioner) = existing {
            // Resource exists - perform update
            let new_version_id = old_practitioner.version_id + 1;
            let last_updated = Utc::now();

            // Inject id and meta into content before storing
            content = crate::models::practitioner::inject_id_meta(&content, id, new_version_id, &last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_practitioner_search_params(&old_practitioner.content);

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO practitioner_history (
                    id, version_id, last_updated, content,
                    family_name, given_name, prefix, suffix, name_text,
                    identifier_system, identifier_value,
                    telecom_value,
                    active,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
                "#,
                old_practitioner.id,
                old_practitioner.version_id,
                old_practitioner.last_updated,
                &old_practitioner.content,
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
                old_params
                    .telecom_value
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&old_params.telecom_value[..])),
                old_params.active,
                if old_practitioner.version_id == 1 {
                    "CREATE"
                } else {
                    "UPDATE"
                },
                Utc::now(),
            )
            .execute(&mut *tx)
            .await?;

            // Extract search parameters for new content
            let params = extract_practitioner_search_params(&content);

            // Update current table with new version
            sqlx::query!(
                r#"
                UPDATE practitioner
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
                    telecom_value = $12,
                    active = $13
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
                params
                    .telecom_value
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.telecom_value[..])),
                params.active,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return updated practitioner
            self.read(id).await
        } else {
            // Resource doesn't exist - perform create with specified ID
            let version_id = 1;
            let last_updated = Utc::now();

            tracing::info!(practitioner_id = %id, "Creating practitioner with client-specified ID");

            // Inject id and meta into content before storing
            content = crate::models::practitioner::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_practitioner_search_params(&content);

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO practitioner (
                    id, version_id, last_updated, content,
                    family_name, given_name, prefix, suffix, name_text,
                    identifier_system, identifier_value,
                    telecom_value,
                    active
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
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
                params
                    .telecom_value
                    .is_empty()
                    .then_some(None)
                    .unwrap_or(Some(&params.telecom_value[..])),
                params.active,
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return the created practitioner
            self.read(id).await
        }
    }

    /// Update a practitioner resource (increment version)
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Practitioner> {
        let mut tx = self.pool.begin().await?;

        // Get current version with lock (only need content to store in history)
        let old_practitioner = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = old_practitioner.version_id + 1;
        let last_updated = Utc::now();

        // Inject id and meta into new content before storing
        content = crate::models::practitioner::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_practitioner_search_params(&old_practitioner.content);

        // Insert OLD version into history before updating
        sqlx::query!(
            r#"
            INSERT INTO practitioner_history (
                id, version_id, last_updated, content,
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                telecom_value,
                active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            "#,
            old_practitioner.id,
            old_practitioner.version_id,
            old_practitioner.last_updated,
            &old_practitioner.content,
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
            old_params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.telecom_value[..])),
            old_params.active,
            if old_practitioner.version_id == 1 {
                "CREATE"
            } else {
                "UPDATE"
            },
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        // Extract search parameters for new content
        let params = extract_practitioner_search_params(&content);

        // Update current table with new version
        sqlx::query!(
            r#"
            UPDATE practitioner
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
                telecom_value = $12,
                active = $13
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
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
            params.active,
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated practitioner
        self.read(id).await
    }

    /// Delete a practitioner resource (hard delete)
    pub async fn delete(&self, id: &str) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get current practitioner (to store in history)
        let practitioner = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
            WHERE id = $1
            FOR UPDATE
            "#,
            id
        )
        .fetch_optional(&mut *tx)
        .await?
        .ok_or(FhirError::NotFound)?;

        let new_version_id = practitioner.version_id + 1;
        let last_updated = Utc::now();

        // Extract search params from content for history
        let params = extract_practitioner_search_params(&practitioner.content);

        // Delete from current table
        sqlx::query!(
            r#"
            DELETE FROM practitioner
            WHERE id = $1
            "#,
            id,
        )
        .execute(&mut *tx)
        .await?;

        // Insert delete record into history
        sqlx::query!(
            r#"
            INSERT INTO practitioner_history (
                id, version_id, last_updated, content,
                family_name, given_name, prefix, suffix, name_text,
                identifier_system, identifier_value,
                telecom_value,
                active,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            "#,
            id,
            new_version_id,
            last_updated,
            &practitioner.content,
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
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
            params.active,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }

    /// Get all versions of a practitioner from history (includes current version if exists)
    pub async fn history(&self, id: &str, count: Option<i64>) -> Result<Vec<PractitionerHistory>> {
        // Use LIMIT if count is provided, otherwise fetch all
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                PractitionerHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM practitioner_history
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
                PractitionerHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM practitioner_history
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
                    // Convert current Practitioner to PractitionerHistory format
                    let current_as_history = PractitionerHistory {
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
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<PractitionerHistory> {
        // Check if requested version is the current version
        if let Ok(current) = self.read(id).await {
            if current.version_id == version_id {
                // Convert to PractitionerHistory format
                return Ok(PractitionerHistory {
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
        let practitioner = sqlx::query_as!(
            PractitionerHistory,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value",
                history_operation, history_timestamp
            FROM practitioner_history
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await?
        .ok_or(FhirError::NotFound)?;

        Ok(practitioner)
    }

    /// Search for practitioners with pagination and filtering
    pub async fn search(
        &self,
        params: &std::collections::HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Practitioner>, Option<i64>)> {
        use crate::search::{SearchCondition, SearchQuery};

        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT practitioner.id, practitioner.version_id, practitioner.last_updated, practitioner.content
               FROM practitioner WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();
        let mut bind_count = 0;

        for condition in &query.conditions {
            match condition {
                SearchCondition::ReverseChain(reverse) => {
                    // Reverse chaining: find practitioners that are referenced by patients
                    // Example: _has:Patient:general-practitioner:family=Brown
                    //   Find Practitioners who are GPs for patients with family name Brown

                    match reverse.target_resource_type.as_str() {
                        "Patient" => {
                            // Find practitioners that are general practitioners for patients matching the criteria
                            bind_count += 1;

                            match reverse.search_param.as_str() {
                                "family" => {
                                    sql.push_str(&format!(
                                        " AND EXISTS (
                                            SELECT 1 FROM patient
                                            WHERE 'Practitioner/' || practitioner.id = ANY(patient.general_practitioner_reference)
                                            AND EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${})
                                        )",
                                        bind_count
                                    ));
                                    bind_values.push(format!("{}%", reverse.search_value));
                                }
                                _ => {
                                    tracing::warn!("Unsupported reverse chain parameter for Patient: {}", reverse.search_param);
                                    bind_count -= 1;
                                }
                            }
                        }
                        _ => {
                            tracing::warn!("Unsupported reverse chain resource type for Practitioner: {}", reverse.target_resource_type);
                        }
                    }
                }
                // Other search conditions can be added here as needed
                _ => {
                    tracing::debug!("Search condition not yet implemented for Practitioner: {:?}", condition);
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
        } else {
            // Default sort
            sql.push_str(" ORDER BY last_updated DESC");
        }

        // Add LIMIT and OFFSET
        sql.push_str(&format!(" LIMIT {}", query.limit));
        sql.push_str(&format!(" OFFSET {}", query.offset));

        // Debug logging
        tracing::debug!("Generated SQL: {}", sql);
        tracing::debug!("Bind values: {:?}", bind_values);

        // Build dynamic query
        let mut query_builder = sqlx::query(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let rows = query_builder.fetch_all(&self.pool).await?;

        let practitioners = rows
            .into_iter()
            .map(|row| Practitioner {
                id: row.get("id"),
                version_id: row.get("version_id"),
                last_updated: row.get("last_updated"),
                content: row.get("content"),
            })
            .collect();

        // Get total count if requested
        let total = if include_total {
            let count_sql = build_practitioner_count_sql(&query);
            let mut count_query_builder = sqlx::query(&count_sql);
            for value in &bind_values {
                count_query_builder = count_query_builder.bind(value);
            }
            let count_row = count_query_builder.fetch_one(&self.pool).await?;
            Some(count_row.get::<i64, _>(0))
        } else {
            None
        };

        Ok((practitioners, total))
    }

    /// Find patients that reference a specific practitioner (for _revinclude support)
    pub async fn find_patients_by_practitioner(
        &self,
        practitioner_id: &str,
    ) -> Result<Vec<crate::models::patient::Patient>> {
        use crate::models::patient::Patient;

        let practitioner_ref = format!("Practitioner/{}", practitioner_id);

        // Query patients WHERE practitioner_ref is in the general_practitioner_reference array
        let patients = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM patient
            WHERE $1 = ANY(general_practitioner_reference)
            "#,
            practitioner_ref
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(patients)
    }

    /// Purge all practitioner history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) practitioner records
    pub async fn purge(&self) -> Result<()> {
        // Delete all history records for test isolation
        // This ensures no constraint violations on repeated test runs
        sqlx::query!("DELETE FROM practitioner_history")
            .execute(&self.pool)
            .await?;

        Ok(())
    }
}

fn build_practitioner_count_sql(query: &crate::search::SearchQuery) -> String {
    use crate::search::SearchCondition;

    let mut sql = String::from("SELECT COUNT(*) FROM practitioner WHERE 1=1");
    let mut bind_count = 0;

    for condition in &query.conditions {
        match condition {
            SearchCondition::ReverseChain(reverse) => {
                match reverse.target_resource_type.as_str() {
                    "Patient" => {
                        bind_count += 1;

                        match reverse.search_param.as_str() {
                            "family" => {
                                sql.push_str(&format!(
                                    " AND EXISTS (
                                        SELECT 1 FROM patient
                                        WHERE 'Practitioner/' || practitioner.id = ANY(patient.general_practitioner_reference)
                                        AND EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${})
                                    )",
                                    bind_count
                                ));
                            }
                            _ => {
                                tracing::warn!("Unsupported reverse chain parameter for Patient in count: {}", reverse.search_param);
                                bind_count -= 1;
                            }
                        }
                    }
                    _ => {
                        tracing::warn!("Unsupported reverse chain resource type for Practitioner in count: {}", reverse.target_resource_type);
                    }
                }
            }
            _ => {
                tracing::debug!("Search condition not yet implemented for Practitioner count: {:?}", condition);
            }
        }
    }

    sql
}
