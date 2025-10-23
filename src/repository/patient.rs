use chrono::Utc;
use serde_json::Value;
use sqlx::PgPool;
use std::collections::HashMap;
use uuid::Uuid;

use crate::error::{FhirError, Result};
use crate::models::patient::{extract_patient_search_params, Patient, PatientHistory};
use crate::models::observation::Observation;
use crate::search::{SearchParam, SearchQuery, SortDirection};
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
                active,
                birthdate,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                general_practitioner_reference,
                identifier_system, identifier_value,
                link_reference,
                organization_reference
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18)
            "#,
            id,
            version_id,
            last_updated,
            content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference
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

            tracing::info!(patient_id = %id, old_version = old_patient.version_id, new_version = new_version_id, "Updating existing patient");

            // Clean up any orphaned history records
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM patient_history
                WHERE id = $1 AND version_id >= $2
                "#,
                id,
                old_patient.version_id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::warn!(patient_id = %id, deleted_versions = deleted_rows.rows_affected(), "Cleaned up orphaned history records before update");
            }

            // Inject id and meta into content before storing
            content = crate::models::patient::inject_id_meta(&content, id, new_version_id, &last_updated);

            // Extract search params from OLD content for history
            let old_params = extract_patient_search_params(&old_patient.content);

            // Insert OLD version into history before updating
            sqlx::query!(
                r#"
                INSERT INTO patient_history (
                    id, version_id, last_updated, content,
                active,
                birthdate,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                general_practitioner_reference,
                identifier_system, identifier_value,
                link_reference,
                organization_reference,
                    history_operation, history_timestamp
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)
                "#,
                old_patient.id,
                old_patient.version_id,
                old_patient.last_updated,
                &old_patient.content,
            old_params.active,
            old_params.birthdate,
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
            old_params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.link_reference[..])),
            old_params.organization_reference,
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
                    active = $5,
                    birthdate = $6,
                    telecom_value = $7,
                    family_name = $8,
                    given_name = $9,
                    prefix = $10,
                    suffix = $11,
                    name_text = $12,
                    gender = $13,
                    general_practitioner_reference = $14,
                    identifier_system = $15,
                    identifier_value = $16,
                    link_reference = $17,
                    organization_reference = $18
                WHERE id = $1
                "#,
                id,
                new_version_id,
                last_updated,
                content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference
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

            // Clean up any orphaned history records for this ID
            let deleted_rows = sqlx::query!(
                r#"
                DELETE FROM patient_history
                WHERE id = $1
                "#,
                id
            )
            .execute(&mut *tx)
            .await?;

            if deleted_rows.rows_affected() > 0 {
                tracing::info!(patient_id = %id, orphaned_history_records = deleted_rows.rows_affected(), "Cleaned up orphaned history records before creating new resource");
            }

            // Inject id and meta into content before storing
            content = crate::models::patient::inject_id_meta(&content, id, version_id, &last_updated);

            // Extract search parameters
            let params = extract_patient_search_params(&content);

            // Insert into current table
            sqlx::query!(
                r#"
                INSERT INTO patient (
                    id, version_id, last_updated, content,
                active,
                birthdate,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                general_practitioner_reference,
                identifier_system, identifier_value,
                link_reference,
                organization_reference
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18)
                "#,
                id,
                version_id,
                last_updated,
                content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference
            )
            .execute(&mut *tx)
            .await?;

            tx.commit().await?;

            // Fetch and return created patient
            self.read(id).await
        }
    }


    /// Update an existing patient resource
    pub async fn update(&self, id: &str, mut content: Value) -> Result<Patient> {
        // Validate resource
        validate_patient(&content)?;

        let mut tx = self.pool.begin().await?;

        // Get existing resource
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

        tracing::info!(patient_id = %id, old_version = old_patient.version_id, new_version = new_version_id, "Updating patient");

        // Clean up any orphaned history records
        let deleted_rows = sqlx::query!(
            r#"
            DELETE FROM patient_history
            WHERE id = $1 AND version_id >= $2
            "#,
            id,
            old_patient.version_id
        )
        .execute(&mut *tx)
        .await?;

        if deleted_rows.rows_affected() > 0 {
            tracing::warn!(patient_id = %id, deleted_versions = deleted_rows.rows_affected(), "Cleaned up orphaned history records before update");
        }

        // Inject id and meta into content
        content = crate::models::patient::inject_id_meta(&content, id, new_version_id, &last_updated);

        // Extract search params from OLD content for history
        let old_params = extract_patient_search_params(&old_patient.content);

        // Insert OLD version into history
        sqlx::query!(
            r#"
            INSERT INTO patient_history (
                id, version_id, last_updated, content,
                active,
                birthdate,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                general_practitioner_reference,
                identifier_system, identifier_value,
                link_reference,
                organization_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)
            "#,
            old_patient.id,
            old_patient.version_id,
            old_patient.last_updated,
            &old_patient.content,
            old_params.active,
            old_params.birthdate,
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
            old_params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&old_params.link_reference[..])),
            old_params.organization_reference,
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

        // Update current table
        sqlx::query!(
            r#"
            UPDATE patient
            SET
                version_id = $2,
                last_updated = $3,
                content = $4,
                    active = $5,
                    birthdate = $6,
                    telecom_value = $7,
                    family_name = $8,
                    given_name = $9,
                    prefix = $10,
                    suffix = $11,
                    name_text = $12,
                    gender = $13,
                    general_practitioner_reference = $14,
                    identifier_system = $15,
                    identifier_value = $16,
                    link_reference = $17,
                    organization_reference = $18
            WHERE id = $1
            "#,
            id,
            new_version_id,
            last_updated,
            content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        // Fetch and return updated patient
        self.read(id).await
    }


    /// Delete a patient resource
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
                active,
                birthdate,
                telecom_value,
                family_name, given_name, prefix, suffix, name_text,
                gender,
                general_practitioner_reference,
                identifier_system, identifier_value,
                link_reference,
                organization_reference,
                history_operation, history_timestamp
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)
            "#,
            id,
            new_version_id,
            last_updated,
            &patient.content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference,
            "DELETE",
            Utc::now(),
        )
        .execute(&mut *tx)
        .await?;

        tx.commit().await?;

        Ok(())
    }


    /// Get all versions of a patient from history
    pub async fn history(&self, id: &str, count: Option<i64>) -> Result<Vec<PatientHistory>> {
        let limit = count.unwrap_or(50);

        // Get current version
        let current = sqlx::query_as!(
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
        .await?;

        // Get historical versions
        let mut history = sqlx::query_as!(
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
        .await?;

        // Prepend current version if it exists
        if let Some(curr) = current {
            history.insert(0, PatientHistory {
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


    /// Read a specific version of a patient from history
    pub async fn read_version(&self, id: &str, version_id: i32) -> Result<PatientHistory> {
        // Try history table first
        if let Some(history) = sqlx::query_as!(
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
        .await? {
            return Ok(history);
        }

        // If not in history, check if it's the current version
        if let Some(current) = sqlx::query_as!(
            Patient,
            r#"
            SELECT
                id, version_id, last_updated,
                content as "content: Value"
            FROM patient
            WHERE id = $1 AND version_id = $2
            "#,
            id,
            version_id
        )
        .fetch_optional(&self.pool)
        .await? {
            // Convert current version to history format
            return Ok(PatientHistory {
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

    /// Rollback a patient to a specific version (deletes all versions >= rollback_to_version)
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
                    active = $5,
                    birthdate = $6,
                    telecom_value = $7,
                    family_name = $8,
                    given_name = $9,
                    prefix = $10,
                    suffix = $11,
                    name_text = $12,
                    gender = $13,
                    general_practitioner_reference = $14,
                    identifier_system = $15,
                    identifier_value = $16,
                    link_reference = $17,
                    organization_reference = $18
                WHERE id = $1
                "#,
                id,
                new_version,
                restored.last_updated,
                &restored.content,
            params.active,
            params.birthdate,
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
            params
                .general_practitioner_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.general_practitioner_reference[..])),
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
                .link_reference
                .is_empty()
                .then_some(None)
                .unwrap_or(Some(&params.link_reference[..])),
            params.organization_reference
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
        let current_resources = sqlx::query_as!(
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


    /// Purge all patient history records - FOR TESTING ONLY
    /// This removes ALL history records to ensure test isolation
    /// Note: This does NOT delete current (non-deleted) patient records
    pub async fn purge(&self) -> Result<()> {
        // Delete all history records for test isolation
        sqlx::query!("DELETE FROM patient_history")
            .execute(&self.pool)
            .await?;

        Ok(())
    }


    /// Search for patients based on FHIR search parameters
    pub async fn search(
        &self,
        params: &HashMap<String, String>,
        include_total: bool,
    ) -> Result<(Vec<Patient>, Option<i64>)> {
        let query = SearchQuery::from_params(params)?;

        let mut sql = String::from(
            r#"SELECT patient.id, patient.version_id, patient.last_updated, patient.content
               FROM patient WHERE 1=1"#,
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
                    "active" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                sql.push_str(&format!("(patient.active = ${}::boolean)", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(patient.active IS NULL)");
                                } else {
                                    sql.push_str("(patient.active IS NOT NULL)");
                                }
                            }
                            Some("not") => {
                                sql.push_str(&format!("(patient.active != ${}::boolean)", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for token search: {:?}", modifier);
                            }
                        }
                    }
                    "birthdate" => {
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
                        sql.push_str(&format!("(patient.birthdate {} ${}::date)", op, bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    "email" => {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    "family" => {
                        let modifier = param.modifier.as_deref().unwrap_or("contains");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            "contains" => {
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(patient.family_name) AS v WHERE v ILIKE ${})",
                                    bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            "exact" => {
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(patient.family_name) AS v WHERE v = ${})",
                                    bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            "missing" => {
                                sql.push_str("(patient.family_name IS NULL OR array_length(patient.family_name, 1) IS NULL)");
                            }
                            "not" => {
                                sql.push_str(&format!(
                                    "NOT (EXISTS (SELECT 1 FROM unnest(patient.family_name) AS v WHERE v ILIKE ${}))",
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
                                sql.push_str(&format!("(patient.gender = ${})", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(patient.gender IS NULL)");
                                } else {
                                    sql.push_str("(patient.gender IS NOT NULL)");
                                }
                            }
                            Some("not") => {
                                sql.push_str(&format!("(patient.gender != ${})", bind_idx));
                                bind_values.push(param.value.clone());
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for token search: {:?}", modifier);
                            }
                        }
                    }
                    "general-practitioner" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Organization/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.general_practitioner_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(patient.general_practitioner_reference IS NULL OR array_length(patient.general_practitioner_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(patient.general_practitioner_reference IS NOT NULL AND array_length(patient.general_practitioner_reference, 1) > 0)");
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "given" => {
                        let modifier = param.modifier.as_deref().unwrap_or("contains");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            "contains" => {
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(patient.given_name) AS v WHERE v ILIKE ${})",
                                    bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            "exact" => {
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(patient.given_name) AS v WHERE v = ${})",
                                    bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            "missing" => {
                                sql.push_str("(patient.given_name IS NULL OR array_length(patient.given_name, 1) IS NULL)");
                            }
                            "not" => {
                                sql.push_str(&format!(
                                    "NOT (EXISTS (SELECT 1 FROM unnest(patient.given_name) AS v WHERE v ILIKE ${}))",
                                    bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            _ => {}
                        }
                    }
                    "identifier" => {
                        if param.value.contains('|') {
                            let parts: Vec<&str> = param.value.split('|').collect();
                            if parts.len() == 2 {
                                let bind_idx = bind_values.len() + 1;
                                sql.push_str(&format!(
                                    "EXISTS (SELECT 1 FROM unnest(patient.identifier_system, patient.identifier_value) AS ident(sys, val) WHERE ident.sys = ${} AND ident.val = ${})",
                                    bind_idx, bind_idx + 1
                                ));
                                bind_values.push(parts[0].to_string());
                                bind_values.push(parts[1].to_string());
                            }
                        } else {
                            let bind_idx = bind_values.len() + 1;
                            sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.identifier_value) AS iv WHERE iv = ${})", bind_idx));
                            bind_values.push(param.value.clone());
                        }
                    }
                    "link" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("RelatedPerson/{}", ref_value);
                                }
                                sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.link_reference) AS ref WHERE ref = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(patient.link_reference IS NULL OR array_length(patient.link_reference, 1) IS NULL)");
                                } else {
                                    sql.push_str("(patient.link_reference IS NOT NULL AND array_length(patient.link_reference, 1) > 0)");
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "name" => {
                        let modifier = param.modifier.as_deref().unwrap_or("contains");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            "contains" => {
                                sql.push_str(&format!(
                                    "(EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.given_name) AS gn WHERE gn ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.prefix) AS p WHERE p ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.suffix) AS s WHERE s ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.name_text) AS nt WHERE nt ILIKE ${}))",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            "exact" => {
                                sql.push_str(&format!(
                                    "(EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.given_name) AS gn WHERE gn = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.prefix) AS p WHERE p = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.suffix) AS s WHERE s = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.name_text) AS nt WHERE nt = ${}))",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            "missing" => {
                                sql.push_str("(patient.family_name IS NULL OR array_length(patient.family_name, 1) IS NULL)");
                            }
                            "not" => {
                                sql.push_str(&format!(
                                    "NOT (EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${}))",
                                    bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            _ => {}
                        }
                    }
                    "organization" => {
                        let modifier = param.modifier.as_deref();
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            None => {
                                let mut ref_value = param.value.clone();
                                if !ref_value.contains('/') {
                                    ref_value = format!("Organization/{}", ref_value);
                                }
                                sql.push_str(&format!("(patient.organization_reference = ${})", bind_idx));
                                bind_values.push(ref_value);
                            }
                            Some("missing") => {
                                let is_missing = param.value == "true";
                                if is_missing {
                                    sql.push_str("(patient.organization_reference IS NULL)");
                                } else {
                                    sql.push_str("(patient.organization_reference IS NOT NULL)");
                                }
                            }
                            _ => {
                                tracing::warn!("Unknown modifier for reference search: {:?}", modifier);
                            }
                        }
                    }
                    "phone" => {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    "phonetic" => {
                        let modifier = param.modifier.as_deref().unwrap_or("contains");
                        let bind_idx = bind_values.len() + 1;

                        match modifier {
                            "contains" => {
                                sql.push_str(&format!(
                                    "(EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.given_name) AS gn WHERE gn ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.prefix) AS p WHERE p ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.suffix) AS s WHERE s ILIKE ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.name_text) AS nt WHERE nt ILIKE ${}))",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            "exact" => {
                                sql.push_str(&format!(
                                    "(EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.given_name) AS gn WHERE gn = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.prefix) AS p WHERE p = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.suffix) AS s WHERE s = ${}) \
                                     OR EXISTS (SELECT 1 FROM unnest(patient.name_text) AS nt WHERE nt = ${}))",
                                    bind_idx, bind_idx, bind_idx, bind_idx, bind_idx
                                ));
                                bind_values.push(param.value.clone());
                            }
                            "missing" => {
                                sql.push_str("(patient.family_name IS NULL OR array_length(patient.family_name, 1) IS NULL)");
                            }
                            "not" => {
                                sql.push_str(&format!(
                                    "NOT (EXISTS (SELECT 1 FROM unnest(patient.family_name) AS fn WHERE fn ILIKE ${}))",
                                    bind_idx
                                ));
                                bind_values.push(format!("%{}%", param.value));
                            }
                            _ => {}
                        }
                    }
                    "telecom" => {
                        let bind_idx = bind_values.len() + 1;
                        sql.push_str(&format!("EXISTS (SELECT 1 FROM unnest(patient.telecom_value) AS tv WHERE tv = ${})", bind_idx));
                        bind_values.push(param.value.clone());
                    }
                    _ => {
                        // Unknown parameter - ignore per FHIR spec
                        tracing::warn!("Unknown search parameter for Patient: {}", param.name);
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
        let mut query_builder = sqlx::query_as::<_, Patient>(&sql);
        for value in &bind_values {
            query_builder = query_builder.bind(value);
        }

        let resources = query_builder
            .fetch_all(&self.pool)
            .await?;

        // Get total count if requested
        let total = if include_total {
            let count_sql = sql.replace(
                &format!("SELECT patient.id, patient.version_id, patient.last_updated, patient.content\n               FROM patient"),
                "SELECT COUNT(*) FROM patient"
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
            "active" => "active".to_string(),
            "address" => "address_name".to_string(),
            "address-city" => "address_city_name".to_string(),
            "address-country" => "address_country_name".to_string(),
            "address-postalcode" => "address_postalcode_name".to_string(),
            "address-state" => "address_state_name".to_string(),
            "address-use" => "address_use_code".to_string(),
            "birthdate" => "birthdate".to_string(),
            "deceased" => "deceased_code".to_string(),
            "email" => "email_code".to_string(),
            "family" => "family_name".to_string(),
            "gender" => "gender".to_string(),
            "general-practitioner" => "general_practitioner_reference".to_string(),
            "identifier" => "identifier_value".to_string(),
            "language" => "language_code".to_string(),
            "link" => "link_reference".to_string(),
            "organization" => "organization_reference".to_string(),
            _ => field.to_string(),
        }
    }

    /// Find observations by patient ID (for _revinclude support)
    pub async fn find_observations_by_patient(&self, patient_id: &str) -> Result<Vec<Observation>> {
        let patient_ref = format!("Patient/{}", patient_id);
        let observations = sqlx::query_as!(
            Observation,
            r#"SELECT id, version_id, last_updated, content as "content: Value" FROM observation WHERE subject_reference = $1"#,
            patient_ref
        ).fetch_all(&self.pool).await?;
        Ok(observations)
    }

    /// Find practitioners by IDs (for _include support)
    pub async fn find_practitioners_by_ids(&self, practitioner_ids: &[String]) -> Result<Vec<crate::models::practitioner::Practitioner>> {
        use crate::models::practitioner::Practitioner;
        if practitioner_ids.is_empty() { return Ok(Vec::new()); }
        let practitioners = sqlx::query_as!(
            Practitioner,
            r#"SELECT id, version_id, last_updated, content as "content: Value" FROM practitioner WHERE id = ANY($1)"#,
            practitioner_ids
        ).fetch_all(&self.pool).await?;
        Ok(practitioners)
    }

}
