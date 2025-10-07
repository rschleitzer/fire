use chrono::NaiveDate;
use std::collections::HashMap;

use crate::error::{FhirError, Result};

#[derive(Debug, Clone)]
pub struct SearchQuery {
    pub conditions: Vec<SearchCondition>,
    pub limit: i64,
    pub offset: i64,
}

#[derive(Debug, Clone)]
pub enum SearchCondition {
    FamilyName(String),
    GivenName(String),
    Identifier(String),
    Birthdate(DateComparison),
    Gender(String),
    Active(bool),
}

#[derive(Debug, Clone)]
pub struct DateComparison {
    pub prefix: DatePrefix,
    pub value: NaiveDate,
}

#[derive(Debug, Clone)]
pub enum DatePrefix {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

impl SearchQuery {
    pub fn from_params(params: &HashMap<String, String>) -> Result<Self> {
        let mut conditions = Vec::new();

        // Parse family name
        if let Some(family) = params.get("family") {
            conditions.push(SearchCondition::FamilyName(family.clone()));
        }

        // Parse given name
        if let Some(given) = params.get("given") {
            conditions.push(SearchCondition::GivenName(given.clone()));
        }

        // Parse name (searches both family and given)
        if let Some(name) = params.get("name") {
            conditions.push(SearchCondition::FamilyName(name.clone()));
            conditions.push(SearchCondition::GivenName(name.clone()));
        }

        // Parse identifier
        if let Some(identifier) = params.get("identifier") {
            conditions.push(SearchCondition::Identifier(identifier.clone()));
        }

        // Parse birthdate
        if let Some(birthdate) = params.get("birthdate") {
            let comparison = parse_date_param(birthdate)?;
            conditions.push(SearchCondition::Birthdate(comparison));
        }

        // Parse gender
        if let Some(gender) = params.get("gender") {
            conditions.push(SearchCondition::Gender(gender.clone()));
        }

        // Parse active
        if let Some(active) = params.get("active") {
            let active_bool = active.parse::<bool>().map_err(|_| {
                FhirError::InvalidSearchParameter(format!("Invalid active value: {}", active))
            })?;
            conditions.push(SearchCondition::Active(active_bool));
        }

        // Parse pagination
        let limit = params
            .get("_count")
            .and_then(|c| c.parse::<i64>().ok())
            .unwrap_or(50)
            .min(1000); // Max 1000 results per page

        let offset = params
            .get("_offset")
            .and_then(|o| o.parse::<i64>().ok())
            .unwrap_or(0);

        Ok(SearchQuery {
            conditions,
            limit,
            offset,
        })
    }

    pub fn build_sql(&self) -> (String, Vec<Box<dyn sqlx::Encode<'_, sqlx::Postgres> + Send + Sync>>) {
        let mut sql = String::from("SELECT id, version_id, last_updated, deleted, content, family_name, given_name, identifier_system, identifier_value, birthdate, gender, active FROM patient WHERE deleted = FALSE");
        let mut bind_count = 0;
        let params: Vec<Box<dyn sqlx::Encode<'_, sqlx::Postgres> + Send + Sync>> = Vec::new();

        for condition in &self.conditions {
            match condition {
                SearchCondition::FamilyName(_name) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND family_name && ARRAY[${}]::TEXT[]", bind_count));
                }
                SearchCondition::GivenName(_name) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND given_name && ARRAY[${}]::TEXT[]", bind_count));
                }
                SearchCondition::Identifier(_value) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND identifier_value && ARRAY[${}]::TEXT[]", bind_count));
                }
                SearchCondition::Birthdate(comparison) => {
                    bind_count += 1;
                    let op = match comparison.prefix {
                        DatePrefix::Eq => "=",
                        DatePrefix::Ne => "!=",
                        DatePrefix::Gt => ">",
                        DatePrefix::Lt => "<",
                        DatePrefix::Ge => ">=",
                        DatePrefix::Le => "<=",
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

        sql.push_str(" ORDER BY last_updated DESC");
        bind_count += 1;
        sql.push_str(&format!(" LIMIT ${}", bind_count));
        bind_count += 1;
        sql.push_str(&format!(" OFFSET ${}", bind_count));

        (sql, params)
    }
}

fn parse_date_param(value: &str) -> Result<DateComparison> {
    let (prefix, date_str) = if value.starts_with("eq") {
        (DatePrefix::Eq, &value[2..])
    } else if value.starts_with("ne") {
        (DatePrefix::Ne, &value[2..])
    } else if value.starts_with("gt") {
        (DatePrefix::Gt, &value[2..])
    } else if value.starts_with("ge") {
        (DatePrefix::Ge, &value[2..])
    } else if value.starts_with("lt") {
        (DatePrefix::Lt, &value[2..])
    } else if value.starts_with("le") {
        (DatePrefix::Le, &value[2..])
    } else {
        (DatePrefix::Eq, value)
    };

    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").map_err(|_| {
        FhirError::InvalidSearchParameter(format!("Invalid date format: {}", date_str))
    })?;

    Ok(DateComparison {
        prefix,
        value: date,
    })
}
