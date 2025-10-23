use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::practitioner::{extract_practitioner_search_params, Practitioner, PractitionerHistory};
use crate::models::patient::Patient;
use crate::search::{SearchQuery, SortDirection};
use crate::services::validate_practitioner;

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

        // Validate resource
        validate_practitioner(&content)?;

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
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                active,
                identifier_system, identifier_value,
                qualification_period
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            "#,
            id,
            version_id,
            last_updated,
            content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period
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
        // Validate resource
        validate_practitioner(&content)?;

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

            tracing::info!(practitioner_id = %id, old_version = old_practitioner.version_id, new_version = new_version_id, "Updating existing practitioner");

            // Clean up any orphaned history records
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM practitioner_history
                WHERE id = $1 AND version_id >= $2
                "#,
                id,
                old_practitioner.version_id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::warn!(practitioner_id = %id, deleted_versions = deleted_rows.rows_affected(), "Cleaned up orphaned history records before update");
            }

            // Inject id and meta into content before storing
            content = crate::models::practitioner::inject_id_meta(&content, id, new_version_id, &last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_practitioner_search_params(&old_practitioner.content);

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO practitioner_history (
                    id, version_id, last_updated, content,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                active,
                identifier_system, identifier_value,
                qualification_period,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
                "#,
                old_practitioner.id,
                old_practitioner.version_id,
                old_practitioner.last_updated,
                &old_practitioner.content,
            old_params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.telecom_value[..])),
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
            old_params.gender,
            old_params.active,
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
            old_params.qualification_period,
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
                    telecom_value = $5,
                    family_name = $6,
                    given_name = $7,
                    prefix = $8,
                    suffix = $9,
                    name_text = $10,
                    gender = $11,
                    active = $12,
                    identifier_system = $13,
                    identifier_value = $14,
                    qualification_period = $15
                WHERE id = $1
                "#,
                id,
                new_version_id,
                last_updated,
                content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period
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

            // Clean up any orphaned history records for this ID
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM practitioner_history
                WHERE id = $1
                "#,
                id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::info!(practitioner_id = %id, orphaned_history_records = deleted_rows.rows_affected(), "Cleaned up orphaned history records before creating new resource");
            }

            // Inject id and meta into content before storing
            content = crate::models::practitioner::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_practitioner_search_params(&content);

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO practitioner (
                    id, version_id, last_updated, content,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                active,
                identifier_system, identifier_value,
                qualification_period
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
                "#,
                id,
                version_id,
                last_updated,
                content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return created practitioner
            self.read(id).await
        }
    }


    /// Update an existing practitioner resource
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Practitioner> {
        // Validate resource
        validate_practitioner(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get existing resource
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

        tracing::info!(practitioner_id = %id, old_version = old_practitioner.version_id, new_version = new_version_id, "Updating practitioner");

        // Clean up any orphaned history records
        let deleted_rows = sqlx::query!(
            r#"
            DELETE FROM practitioner_history
            WHERE id = $1 AND version_id >= $2
            "#,
            id,
            old_practitioner.version_id
        )
        .execute(&mut *tx)
        .await?;

        if deleted_rows.rows_affected() > 0 {
            tracing::warn!(practitioner_id = %id, deleted_versions = deleted_rows.rows_affected(), "Cleaned up orphaned history records before update");
        }

        // Inject id and meta into content
        content = crate::models::practitioner::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_practitioner_search_params(&old_practitioner.content);

        // Insert OLD version into history
        sqlx::query!(
            r#"
            INSERT INTO practitioner_history (
                id, version_id, last_updated, content,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                active,
                identifier_system, identifier_value,
                qualification_period,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
            "#,
            old_practitioner.id,
            old_practitioner.version_id,
            old_practitioner.last_updated,
            &old_practitioner.content,
            old_params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.telecom_value[..])),
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
            old_params.gender,
            old_params.active,
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
            old_params.qualification_period,
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

        // Update current table
        sqlx::query!(
            r#"
            UPDATE practitioner
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                    telecom_value = $5,
                    family_name = $6,
                    given_name = $7,
                    prefix = $8,
                    suffix = $9,
                    name_text = $10,
                    gender = $11,
                    active = $12,
                    identifier_system = $13,
                    identifier_value = $14,
                    qualification_period = $15
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated practitioner
        self.read(id).await
    }


    /// Delete a practitioner resource
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
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                active,
                identifier_system, identifier_value,
                qualification_period,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
            "#,
            id,
            new_version_id,
            last_updated,
            &practitioner.content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }


    /// Get all versions of a practitioner from history
    pub async fn history(&self, id: &str, count: Option<i64>) -> Result<Vec<PractitionerHistory>> {
        let limit = count.unwrap_or(50);

        // Get current version
        let current = sqlx::query_as!(
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
        .await?;

        // Get historical versions
        let mut history = sqlx::query_as!(
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
        .await?;

        // Prepend current version if it exists
        if let Some(curr) = current {
            history.insert(0, PractitionerHistory {
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


    /// Read a specific version of a practitioner from history
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<PractitionerHistory> {
        // Try history table first
        if let Some(history) = sqlx::query_as!(
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
        .await? {
            return Ok(history);
        }

        // If not in history, check if it's the current version
        if let Some(current) = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await? {
            // Convert current version to history format
            return Ok(PractitionerHistory {
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

    /// Rollback a practitioner to a specific version (deletes all versions >= rollback_to_version)
    pub async fn rollback(&self, id: &str, rollback_to_version: i32) -> Result<()> {
        let mut tx = self.pool.begin().await?;

        // Get all version IDs for this practitioner from history
        let version_ids: Vec<i32> = sqlx::query_scalar!(
            r#"
            SELECT version_id
            FROM practitioner_history
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
            FROM practitioner
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
                "No versions to rollback for practitioner {}",
                id
            )));
        }

        tracing::info!(
            practitioner_id = %id,
            rollback_to = rollback_to_version,
            deleting_versions = ?versions_to_delete,
            new_current = ?new_current_version,
            "Rolling back practitioner"
        );

        // Delete versions from history
        for version in &versions_to_delete {
            sqlx::query!(
                r#"
                DELETE FROM practitioner_history
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

            let params = extract_practitioner_search_params(&restored.content);

            sqlx::query!(
                r#"
                UPDATE practitioner
                SET
                    version_id = $2,
                    last_updated = $3,
                    content = $4,
                    telecom_value = $5,
                    family_name = $6,
                    given_name = $7,
                    prefix = $8,
                    suffix = $9,
                    name_text = $10,
                    gender = $11,
                    active = $12,
                    identifier_system = $13,
                    identifier_value = $14,
                    qualification_period = $15
                WHERE id = $1
                "#,
                id,
                new_version,
                restored.last_updated,
                &restored.content,
            params
                .telecom_value
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.telecom_value[..])),
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
            params.gender,
            params.active,
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
            params.qualification_period
            )
            .execute(&mut *tx)
            .await?;
        } else {
            // No versions remain, delete current resource
            sqlx::query!(
                r#"
                DELETE FROM practitioner
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


    /// Get type-level history - all versions of all practitioners
    pub async fn type_history(&self, count: Option<i64>) -> Result<Vec<PractitionerHistory>> {
        // Query all history records with optional limit
        let mut history = if let Some(limit) = count {
            sqlx::query_as!(
                PractitionerHistory,
                r#"
                SELECT
                    id, version_id, last_updated,
                    content as "content: Value",
                    history_operation, history_timestamp
                FROM practitioner_history
                ORDER BY history_timestamp DESC
                LIMIT $1
                "#,
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
                ORDER BY history_timestamp DESC
                "#
            )
            .fetch_all(&self.pool)
            .await?
        };

        // Also get all current versions
        let current_resources = sqlx::query_as!(
            Practitioner,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM practitioner
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


    /// Purge all practitioner history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) practitioner records
    pub async fn purge(&self) -> Result<()> {
        // Delete all history records for test isolation
        sqlx::query!("DELETE FROM practitioner_history")
            .execute(&self.pool)
            .await?;

        Ok(())
    }


    /// Search for practitioners based on FHIR search parameters
    pub async fn search(
        &self,
        params: &HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Practitioner>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT practitioner.id, practitioner.version_id, practitioner.last_updated, practitioner.content
               FROM practitioner WHERE 1=1"#,
        );

        let mut bind_values: Vec<String> = Vec::new();

        // Build WHERE clause from search parameters
        for param in &query.params {
            match param.name.as_str() {
                "email" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(practitioner.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "family" => {
                    let modifier = param.modifier.as_deref().unwrap_or("contains");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        "contains" => {
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${})",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        "exact" => {
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v = ${})",
                                bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        "missing" => {
                            sql.push_str(" AND (practitioner.family_name IS NULL OR array_length(practitioner.family_name, 1) IS NULL)");
                        }
                        "not" => {
                            sql.push_str(&format!(
                                " AND NOT (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS v WHERE v ILIKE ${}))",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        _ => {}
                    }
                }
                "gender" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            sql.push_str(&format!(" AND practitioner.gender = ${}", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        Some("missing") => {
                            // :missing modifier
                            let is_missing = param.value == "true";
                            if is_missing {
                                sql.push_str(" AND practitioner.gender IS NULL");
                            } else {
                                sql.push_str(" AND practitioner.gender IS NOT NULL");
                            }
                        }
                        Some("not") => {
                            // :not modifier
                            sql.push_str(&format!(" AND practitioner.gender != ${}", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        _ => {
                            // Unknown modifier - ignore
                            tracing::warn!("Unknown modifier for token search: {:?}", modifier);
                        }
                    }
                }
                "given" => {
                    let modifier = param.modifier.as_deref().unwrap_or("contains");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        "contains" => {
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS v WHERE v ILIKE ${})",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        "exact" => {
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS v WHERE v = ${})",
                                bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        "missing" => {
                            sql.push_str(" AND (practitioner.given_name IS NULL OR array_length(practitioner.given_name, 1) IS NULL)");
                        }
                        "not" => {
                            sql.push_str(&format!(
                                " AND NOT (EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS v WHERE v ILIKE ${}))",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        _ => {}
                    }
                }
                "phone" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(practitioner.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "phonetic" => {
                    let modifier = param.modifier.as_deref().unwrap_or("contains");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        "contains" => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS gn WHERE gn ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.prefix) AS p WHERE p ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.suffix) AS s WHERE s ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.name_text) AS nt WHERE nt ILIKE ${}))",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        "exact" => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS gn WHERE gn = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.prefix) AS p WHERE p = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.suffix) AS s WHERE s = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.name_text) AS nt WHERE nt = ${}))",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        "missing" => {
                            sql.push_str(" AND (practitioner.family_name IS NULL OR array_length(practitioner.family_name, 1) IS NULL)");
                        }
                        "not" => {
                            sql.push_str(&format!(
                                " AND NOT (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn ILIKE ${}))",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        _ => {}
                    }
                }
                "telecom" => {
                    let bind_idx = bind_values.len() + 1;
                    sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(practitioner.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                    bind_values.push(param.value.clone());
                }
                "active" => {
                    let modifier = param.modifier.as_deref();
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        None => {
                            // No modifier - exact match
                            sql.push_str(&format!(" AND practitioner.active = ${}::boolean", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        Some("missing") => {
                            // :missing modifier
                            let is_missing = param.value == "true";
                            if is_missing {
                                sql.push_str(" AND practitioner.active IS NULL");
                            } else {
                                sql.push_str(" AND practitioner.active IS NOT NULL");
                            }
                        }
                        Some("not") => {
                            // :not modifier
                            sql.push_str(&format!(" AND practitioner.active != ${}::boolean", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                        _ => {
                            // Unknown modifier - ignore
                            tracing::warn!("Unknown modifier for token search: {:?}", modifier);
                        }
                    }
                }
                "identifier" => {
                    if param.value.contains('|') {
                        let parts: Vec<&str> = param.value.split('|').collect();
                        if parts.len() == 2 {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!(
                                " AND EXISTS (SELECT 1 FROM unnest(practitioner.identifier_system, practitioner.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})",
                                bind_idx, bind_idx + 1
                            ));
                            bind_values.push(parts[0].to_string());
                            bind_values.push(parts[1].to_string());
                        }
                    } else {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(practitioner.identifier_value) AS iv WHERE iv = ${})", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                }
                "name" => {
                    let modifier = param.modifier.as_deref().unwrap_or("contains");
                    let bind_idx = bind_values.len() + 1;

                    match modifier {
                        "contains" => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS gn WHERE gn ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.prefix) AS p WHERE p ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.suffix) AS s WHERE s ILIKE ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.name_text) AS nt WHERE nt ILIKE ${}))",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        "exact" => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.given_name) AS gn WHERE gn = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.prefix) AS p WHERE p = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.suffix) AS s WHERE s = ${}) \
                                 OR EXISTS (SELECT 1 FROM unnest(practitioner.name_text) AS nt WHERE nt = ${}))",
                                bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                            ));
                            bind_values.push(param.value.clone());
                        }
                        "missing" => {
                            sql.push_str(" AND (practitioner.family_name IS NULL OR array_length(practitioner.family_name, 1) IS NULL)");
                        }
                        "not" => {
                            sql.push_str(&format!(
                                " AND NOT (EXISTS (SELECT 1 FROM unnest(practitioner.family_name) AS fn WHERE fn ILIKE ${}))",
                                bind_idx
                            ));
                            bind_values.push(format!("%{}%", param.value));
                        }
                        _ => {}
                    }
                }
                "qualification-period" => {
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
                    sql.push_str(&format!(" AND practitioner.qualification_period {} ${}::date", op, bind_idx));
                    bind_values.push(param.value.clone());
                }
                _ => {
                    // Unknown parameter - ignore per FHIR spec
                    tracing::warn!("Unknown search parameter for Practitioner: {}", param.name);
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
        let mut query_builder = sqlx::query_as::<_, Practitioner>(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let resources = query_builder
            .fetch_all(&self.pool)
            .await?;

        // Get total count if requested
        let total = if include_total {
            let count_sql = sql.replace(
                &format!("SELECT practitioner.id, practitioner.version_id, practitioner.last_updated, practitioner.content\n               FROM practitioner"),
                "SELECT COUNT(*) FROM practitioner"
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
            "address" => "address_name".to_string(),
            "address-city" => "address_city_name".to_string(),
            "address-country" => "address_country_name".to_string(),
            "address-postalcode" => "address_postalcode_name".to_string(),
            "address-state" => "address_state_name".to_string(),
            "address-use" => "address_use_code".to_string(),
            "email" => "email_code".to_string(),
            "family" => "family_name".to_string(),
            "gender" => "gender".to_string(),
            "active" => "active".to_string(),
            "communication" => "communication_code".to_string(),
            "deceased" => "deceased_code".to_string(),
            "identifier" => "identifier_value".to_string(),
            "qualification-period" => "qualification_period".to_string(),
            _ => field.to_string(),
        }
    }

    /// Find patients by practitioner ID (for _revinclude support)
    pub async fn find_patients_by_practitioner(&self, practitioner_id: &str) -> Result<Vec<Patient>> {
        let practitioner_ref = format!("Practitioner/{}", practitioner_id);
        let patients = sqlx::query_as!(
            Patient,
            r#"SELECT id, version_id, last_updated, content as "content: Value" FROM patient WHERE general_practitioner_reference @> ARRAY[$1]::text[]"#,
            practitioner_ref
        ).fetch_all(&self.pool).await?;
        Ok(patients)
    }

}
