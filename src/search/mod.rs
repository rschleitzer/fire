use chrono::NaiveDate;
use std::collections::HashMap;

use crate::error::{FhirError, Result};

#[derive(Debug, Clone)]
pub struct SearchQuery {
    pub conditions: Vec<SearchCondition>,
    pub limit: i64,
    pub offset: i64,
    pub sort: Vec<SortParam>,
}

#[derive(Debug, Clone)]
pub struct SortParam {
    pub field: String,
    pub direction: SortDirection,
}

#[derive(Debug, Clone)]
pub enum SortDirection {
    Ascending,
    Descending,
}

#[derive(Debug, Clone)]
pub enum SearchCondition {
    // Patient search conditions
    FamilyName(StringSearch),
    GivenName(StringSearch),
    Name(StringSearch), // Searches both family and given with OR
    Identifier(String),
    IdentifierSystemValue { system: String, value: String }, // system|value search
    IdentifierMissing(bool), // true = IS NULL, false = IS NOT NULL
    Birthdate(DateComparison),
    Gender(String),
    GenderNot(String), // :not modifier - NOT equal
    Active(bool),
    ActiveNot(bool), // :not modifier - NOT equal
    ActiveMissing(bool), // true = IS NULL, false = IS NOT NULL
    GeneralPractitioner(String), // Reference search: Practitioner/id
    GeneralPractitionerMissing(bool), // true = no GP, false = has GP
    // Multiple values with OR logic (comma-separated in FHIR)
    FamilyNameOr(Vec<StringSearch>),
    GivenNameOr(Vec<StringSearch>),
    GenderOr(Vec<String>),
    // Observation search conditions
    ObservationStatus(String),
    ObservationCode { system: Option<String>, code: String },
    ObservationCategory(String),
    ObservationPatient(String), // patient parameter (just ID)
    ObservationSubject(String), // subject parameter (full reference)
    ObservationDate(DateComparison),
    // Forward chaining: reference.parameter (e.g., general-practitioner.family=Smith)
    ForwardChain(ChainedSearch),
    // Reverse chaining: _has:ResourceType:reference:parameter (e.g., _has:Observation:subject:code=8867-4)
    ReverseChain(ReverseChainedSearch),
}

#[derive(Debug, Clone)]
pub struct ChainedSearch {
    pub reference_param: String, // e.g., "general-practitioner", "subject"
    pub resource_type: Option<String>, // e.g., "Patient", "Practitioner" (from :ResourceType modifier)
    pub chain: Vec<ChainLink>, // The chain of parameters (e.g., ["family"], ["general-practitioner", "family"])
    pub search_value: String, // The final search value
    pub modifier: StringModifier, // The modifier for the final parameter
}

#[derive(Debug, Clone)]
pub struct ChainLink {
    pub param: String, // Parameter name (e.g., "family", "general-practitioner")
}

#[derive(Debug, Clone)]
pub struct ReverseChainedSearch {
    pub target_resource_type: String, // e.g., "Observation" (what we're looking for)
    pub reference_param: String, // e.g., "subject" (the reference field in target resource)
    pub search_param: String, // e.g., "code" (the search parameter on target resource)
    pub search_value: String, // The search value
}

#[derive(Debug, Clone)]
pub struct StringSearch {
    pub value: String,
    pub modifier: StringModifier,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringModifier {
    Contains, // Default - partial match (ILIKE)
    Exact,    // :exact - exact match
    Missing,  // :missing - check if field is null/empty
    Not,      // :not - NOT match (negation)
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

        // Parse family name with modifiers
        if let Some(family) = params.get("family") {
            // Check for comma-separated values (OR logic)
            if family.contains(',') {
                let values: Vec<StringSearch> = family
                    .split(',')
                    .map(|v| StringSearch {
                        value: v.trim().to_string(),
                        modifier: StringModifier::Contains,
                    })
                    .collect();
                conditions.push(SearchCondition::FamilyNameOr(values));
            } else {
                let search = parse_string_param("family", family, params)?;
                conditions.push(SearchCondition::FamilyName(search));
            }
        }

        // Parse family:exact
        if let Some(family) = params.get("family:exact") {
            conditions.push(SearchCondition::FamilyName(StringSearch {
                value: family.clone(),
                modifier: StringModifier::Exact,
            }));
        }

        // Parse family:contains (explicit)
        if let Some(family) = params.get("family:contains") {
            conditions.push(SearchCondition::FamilyName(StringSearch {
                value: family.clone(),
                modifier: StringModifier::Contains,
            }));
        }

        // Parse family:missing
        if let Some(missing) = params.get("family:missing") {
            if missing == "true" {
                conditions.push(SearchCondition::FamilyName(StringSearch {
                    value: String::new(),
                    modifier: StringModifier::Missing,
                }));
            } else if missing == "false" {
                // NOT missing - field must exist
                // This is handled as the inverse
            }
        }

        // Parse family:not
        if let Some(family) = params.get("family:not") {
            conditions.push(SearchCondition::FamilyName(StringSearch {
                value: family.clone(),
                modifier: StringModifier::Not,
            }));
        }

        // Parse given name with modifiers
        if let Some(given) = params.get("given") {
            // Check for comma-separated values (OR logic)
            if given.contains(',') {
                let values: Vec<StringSearch> = given
                    .split(',')
                    .map(|v| StringSearch {
                        value: v.trim().to_string(),
                        modifier: StringModifier::Contains,
                    })
                    .collect();
                conditions.push(SearchCondition::GivenNameOr(values));
            } else {
                let search = parse_string_param("given", given, params)?;
                conditions.push(SearchCondition::GivenName(search));
            }
        }

        // Parse given:exact
        if let Some(given) = params.get("given:exact") {
            conditions.push(SearchCondition::GivenName(StringSearch {
                value: given.clone(),
                modifier: StringModifier::Exact,
            }));
        }

        // Parse given:contains (explicit)
        if let Some(given) = params.get("given:contains") {
            conditions.push(SearchCondition::GivenName(StringSearch {
                value: given.clone(),
                modifier: StringModifier::Contains,
            }));
        }

        // Parse given:missing
        if let Some(missing) = params.get("given:missing") {
            if missing == "true" {
                conditions.push(SearchCondition::GivenName(StringSearch {
                    value: String::new(),
                    modifier: StringModifier::Missing,
                }));
            }
        }

        // Parse given:not
        if let Some(given) = params.get("given:not") {
            conditions.push(SearchCondition::GivenName(StringSearch {
                value: given.clone(),
                modifier: StringModifier::Not,
            }));
        }

        // Parse name (searches both family and given with OR)
        if let Some(name) = params.get("name") {
            let search = parse_string_param("name", name, params)?;
            conditions.push(SearchCondition::Name(search));
        }

        // Parse identifier (supports system|value format)
        if let Some(identifier) = params.get("identifier") {
            // Check if identifier contains system|value format
            if identifier.contains('|') {
                if let Some((system, value)) = identifier.split_once('|') {
                    conditions.push(SearchCondition::IdentifierSystemValue {
                        system: system.to_string(),
                        value: value.to_string(),
                    });
                } else {
                    // Malformed system|value, treat as simple identifier
                    conditions.push(SearchCondition::Identifier(identifier.clone()));
                }
            } else {
                // No pipe - simple value-only search
                conditions.push(SearchCondition::Identifier(identifier.clone()));
            }
        }

        // Parse identifier:missing
        if let Some(missing) = params.get("identifier:missing") {
            if missing == "true" {
                conditions.push(SearchCondition::IdentifierMissing(true));
            } else if missing == "false" {
                conditions.push(SearchCondition::IdentifierMissing(false));
            }
        }

        // Parse identifier_system and identifier_value (both must be present)
        if let (Some(system), Some(value)) = (params.get("identifier_system"), params.get("identifier_value")) {
            conditions.push(SearchCondition::IdentifierSystemValue {
                system: system.clone(),
                value: value.clone(),
            });
        }

        // Parse birthdate
        if let Some(birthdate) = params.get("birthdate") {
            let comparison = parse_date_param(birthdate)?;
            conditions.push(SearchCondition::Birthdate(comparison));
        }

        // Parse gender
        if let Some(gender) = params.get("gender") {
            // Check for comma-separated values (OR logic)
            if gender.contains(',') {
                let values: Vec<String> = gender
                    .split(',')
                    .map(|v| v.trim().to_string())
                    .collect();
                conditions.push(SearchCondition::GenderOr(values));
            } else {
                conditions.push(SearchCondition::Gender(gender.clone()));
            }
        }

        // Parse gender:not
        if let Some(gender) = params.get("gender:not") {
            conditions.push(SearchCondition::GenderNot(gender.clone()));
        }

        // Parse active
        if let Some(active) = params.get("active") {
            let active_bool = active.parse::<bool>().map_err(|_| {
                FhirError::InvalidSearchParameter(format!("Invalid active value: {}", active))
            })?;
            conditions.push(SearchCondition::Active(active_bool));
        }

        // Parse active:not
        if let Some(active) = params.get("active:not") {
            let active_bool = active.parse::<bool>().map_err(|_| {
                FhirError::InvalidSearchParameter(format!("Invalid active:not value: {}", active))
            })?;
            conditions.push(SearchCondition::ActiveNot(active_bool));
        }

        // Parse active:missing
        if let Some(missing) = params.get("active:missing") {
            if missing == "true" {
                conditions.push(SearchCondition::ActiveMissing(true));
            } else if missing == "false" {
                conditions.push(SearchCondition::ActiveMissing(false));
            }
        }

        // Parse general-practitioner (reference search)
        if let Some(gp) = params.get("general-practitioner") {
            conditions.push(SearchCondition::GeneralPractitioner(gp.clone()));
        }

        // Parse general-practitioner:missing
        if let Some(missing) = params.get("general-practitioner:missing") {
            if missing == "true" {
                conditions.push(SearchCondition::GeneralPractitionerMissing(true));
            } else if missing == "false" {
                conditions.push(SearchCondition::GeneralPractitionerMissing(false));
            }
        }

        // Observation-specific parameters

        // Parse status
        if let Some(status) = params.get("status") {
            conditions.push(SearchCondition::ObservationStatus(status.clone()));
        }

        // Parse code (supports system|value format)
        if let Some(code) = params.get("code") {
            if code.contains('|') {
                if let Some((system, value)) = code.split_once('|') {
                    conditions.push(SearchCondition::ObservationCode {
                        system: Some(system.to_string()),
                        code: value.to_string(),
                    });
                }
            } else {
                conditions.push(SearchCondition::ObservationCode {
                    system: None,
                    code: code.clone(),
                });
            }
        }

        // Parse category
        if let Some(category) = params.get("category") {
            conditions.push(SearchCondition::ObservationCategory(category.clone()));
        }

        // Parse patient parameter
        if let Some(patient) = params.get("patient") {
            conditions.push(SearchCondition::ObservationPatient(patient.clone()));
        }

        // Parse subject parameter
        if let Some(subject) = params.get("subject") {
            conditions.push(SearchCondition::ObservationSubject(subject.clone()));
        }

        // Parse date parameter (for Observation.effectiveDateTime)
        if let Some(date) = params.get("date") {
            let comparison = parse_date_param(date)?;
            conditions.push(SearchCondition::ObservationDate(comparison));
        }

        // Parse chained parameters (forward chaining)
        // Look for parameters containing "." (e.g., "general-practitioner.family")
        for (key, value) in params.iter() {
            if key.contains('.') && !key.starts_with('_') {
                let chained = parse_chained_param(key, value)?;
                conditions.push(SearchCondition::ForwardChain(chained));
            }
        }

        // Parse reverse chaining (_has parameter)
        // Format: _has:ResourceType:reference:parameter=value
        // Example: _has:Observation:subject:code=8867-4
        if let Some(has_value) = params.iter().find(|(k, _)| k.starts_with("_has:")).map(|(k, v)| (k.clone(), v.clone())) {
            let reverse_chain = parse_reverse_chain(&has_value.0, &has_value.1)?;
            conditions.push(SearchCondition::ReverseChain(reverse_chain));
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

        // Parse sort
        let sort = if let Some(sort_param) = params.get("_sort") {
            parse_sort_param(sort_param)?
        } else {
            // Default sort by last_updated descending
            vec![SortParam {
                field: "last_updated".to_string(),
                direction: SortDirection::Descending,
            }]
        };

        Ok(SearchQuery {
            conditions,
            limit,
            offset,
            sort,
        })
    }

    pub fn build_sql(
        &self,
    ) -> (
        String,
        Vec<Box<dyn sqlx::Encode<'_, sqlx::Postgres> + Send + Sync>>,
    ) {
        let mut sql = String::from("SELECT id, version_id, last_updated, deleted, content, family_name, given_name, prefix, suffix, name_text, identifier_system, identifier_value, birthdate, gender, active FROM patient WHERE deleted = FALSE");
        let mut bind_count = 0;
        let params: Vec<Box<dyn sqlx::Encode<'_, sqlx::Postgres> + Send + Sync>> = Vec::new();

        for condition in &self.conditions {
            match condition {
                SearchCondition::Name(search) => {
                    bind_count += 1;
                    let param_num = bind_count;
                    match search.modifier {
                        StringModifier::Contains => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt ILIKE ${0}))",
                                param_num
                            ));
                        }
                        StringModifier::Exact => {
                            sql.push_str(&format!(
                                " AND (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s = ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt = ${0}))",
                                param_num
                            ));
                        }
                        StringModifier::Missing => {
                            sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL) \
                                         AND (given_name IS NULL OR array_length(given_name, 1) IS NULL) \
                                         AND (prefix IS NULL OR array_length(prefix, 1) IS NULL) \
                                         AND (suffix IS NULL OR array_length(suffix, 1) IS NULL) \
                                         AND (name_text IS NULL OR array_length(name_text, 1) IS NULL)");
                            bind_count -= 1; // No bind parameter needed
                        }
                        StringModifier::Not => {
                            sql.push_str(&format!(
                                " AND NOT (EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(prefix) AS p WHERE p ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(suffix) AS s WHERE s ILIKE ${0}) \
                                 OR EXISTS (SELECT 1 FROM unnest(name_text) AS nt WHERE nt ILIKE ${0}))",
                                param_num
                            ));
                        }
                    }
                }
                SearchCondition::FamilyName(search) => {
                    bind_count += 1;
                    match search.modifier {
                        StringModifier::Contains => {
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${})", bind_count));
                        }
                        StringModifier::Exact => {
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn = ${})", bind_count));
                        }
                        StringModifier::Missing => {
                            sql.push_str(" AND (family_name IS NULL OR array_length(family_name, 1) IS NULL)");
                            bind_count -= 1; // No bind parameter needed
                        }
                        StringModifier::Not => {
                            sql.push_str(&format!(" AND NOT EXISTS (SELECT 1 FROM unnest(family_name) AS fn WHERE fn ILIKE ${})", bind_count));
                        }
                    }
                }
                SearchCondition::FamilyNameOr(searches) => {
                    // OR logic handled by patient repository, not this method
                    bind_count += searches.len();
                }
                SearchCondition::GivenName(search) => {
                    bind_count += 1;
                    match search.modifier {
                        StringModifier::Contains => {
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${})", bind_count));
                        }
                        StringModifier::Exact => {
                            sql.push_str(&format!(" AND EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn = ${})", bind_count));
                        }
                        StringModifier::Missing => {
                            sql.push_str(
                                " AND (given_name IS NULL OR array_length(given_name, 1) IS NULL)",
                            );
                            bind_count -= 1; // No bind parameter needed
                        }
                        StringModifier::Not => {
                            sql.push_str(&format!(" AND NOT EXISTS (SELECT 1 FROM unnest(given_name) AS gn WHERE gn ILIKE ${})", bind_count));
                        }
                    }
                }
                SearchCondition::GivenNameOr(searches) => {
                    // OR logic handled by patient repository, not this method
                    bind_count += searches.len();
                }
                SearchCondition::Identifier(_value) => {
                    bind_count += 1;
                    sql.push_str(&format!(
                        " AND EXISTS (SELECT 1 FROM unnest(identifier_value) AS iv WHERE iv = ${})",
                        bind_count
                    ));
                }
                SearchCondition::IdentifierSystemValue { system: _system, value: _value } => {
                    // Match both system AND value in arrays at the same index
                    bind_count += 2; // Need two parameters
                    sql.push_str(&format!(
                        " AND EXISTS (
                            SELECT 1 FROM unnest(identifier_system, identifier_value) AS ident(sys, val)
                            WHERE ident.sys = ${} AND ident.val = ${}
                        )",
                        bind_count - 1, bind_count
                    ));
                }
                SearchCondition::IdentifierMissing(is_missing) => {
                    if *is_missing {
                        // Field IS NULL or empty array
                        sql.push_str(" AND (identifier_value IS NULL OR array_length(identifier_value, 1) IS NULL)");
                    } else {
                        // Field IS NOT NULL and has values
                        sql.push_str(" AND (identifier_value IS NOT NULL AND array_length(identifier_value, 1) > 0)");
                    }
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
                SearchCondition::GenderNot(_gender) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND (gender IS NULL OR gender != ${})", bind_count));
                }
                SearchCondition::GenderOr(genders) => {
                    // OR logic handled by patient repository, not this method
                    bind_count += genders.len();
                }
                SearchCondition::Active(_active) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND active = ${}", bind_count));
                }
                SearchCondition::ActiveNot(_active) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND (active IS NULL OR active != ${})", bind_count));
                }
                SearchCondition::ActiveMissing(is_missing) => {
                    if *is_missing {
                        sql.push_str(" AND active IS NULL");
                    } else {
                        sql.push_str(" AND active IS NOT NULL");
                    }
                }
                SearchCondition::GeneralPractitioner(_reference) => {
                    bind_count += 1;
                    sql.push_str(&format!(
                        " AND EXISTS (SELECT 1 FROM unnest(general_practitioner_reference) AS gp WHERE gp = ${})",
                        bind_count
                    ));
                }
                SearchCondition::GeneralPractitionerMissing(is_missing) => {
                    if *is_missing {
                        // Field IS NULL or empty array
                        sql.push_str(" AND (general_practitioner_reference IS NULL OR array_length(general_practitioner_reference, 1) IS NULL)");
                    } else {
                        // Field IS NOT NULL and has values
                        sql.push_str(" AND (general_practitioner_reference IS NOT NULL AND array_length(general_practitioner_reference, 1) > 0)");
                    }
                }
                SearchCondition::ObservationStatus(_status) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND status = ${}", bind_count));
                }
                SearchCondition::ObservationCode { system, code: _ } => {
                    if system.is_some() {
                        bind_count += 2;
                        sql.push_str(&format!(
                            " AND code_system = ${} AND code_code = ${}",
                            bind_count - 1,
                            bind_count
                        ));
                    } else {
                        bind_count += 1;
                        sql.push_str(&format!(" AND code_code = ${}", bind_count));
                    }
                }
                SearchCondition::ObservationCategory(_category) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND ${} = ANY(category_code)", bind_count));
                }
                SearchCondition::ObservationPatient(_patient_id) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND patient_reference = ${}", bind_count));
                }
                SearchCondition::ObservationSubject(_subject_ref) => {
                    bind_count += 1;
                    sql.push_str(&format!(" AND subject_reference = ${}", bind_count));
                }
                SearchCondition::ObservationDate(comparison) => {
                    bind_count += 1;
                    let op = match comparison.prefix {
                        DatePrefix::Eq => "=",
                        DatePrefix::Ne => "!=",
                        DatePrefix::Gt => ">",
                        DatePrefix::Lt => "<",
                        DatePrefix::Ge => ">=",
                        DatePrefix::Le => "<=",
                    };
                    sql.push_str(&format!(" AND effective_datetime {} ${}::timestamptz", op, bind_count));
                }
                SearchCondition::ForwardChain(_chain) => {
                    // Chaining is handled in the repository layer with proper JOINs
                    // This build_sql method is not used when chaining is present
                }
                SearchCondition::ReverseChain(_reverse) => {
                    // Reverse chaining is handled in the repository layer with proper EXISTS clauses
                    // This build_sql method is not used when reverse chaining is present
                }
            }
        }

        // Build ORDER BY clause
        if !self.sort.is_empty() {
            sql.push_str(" ORDER BY ");
            for (i, sort) in self.sort.iter().enumerate() {
                if i > 0 {
                    sql.push_str(", ");
                }
                sql.push_str(&sort.field);
                match sort.direction {
                    SortDirection::Ascending => sql.push_str(" ASC"),
                    SortDirection::Descending => sql.push_str(" DESC"),
                }
            }
        }

        bind_count += 1;
        sql.push_str(&format!(" LIMIT ${}", bind_count));
        bind_count += 1;
        sql.push_str(&format!(" OFFSET ${}", bind_count));

        (sql, params)
    }
}

fn parse_string_param(
    _param_name: &str,
    value: &str,
    _params: &HashMap<String, String>,
) -> Result<StringSearch> {
    // Default to contains (partial match)
    Ok(StringSearch {
        value: value.to_string(),
        modifier: StringModifier::Contains,
    })
}

fn parse_sort_param(value: &str) -> Result<Vec<SortParam>> {
    let mut sorts = Vec::new();

    // _sort can be comma-separated list: _sort=birthdate,-name
    for field in value.split(',') {
        let field = field.trim();
        if field.is_empty() {
            continue;
        }

        let (direction, field_name) = if let Some(stripped) = field.strip_prefix('-') {
            (SortDirection::Descending, stripped)
        } else {
            (SortDirection::Ascending, field)
        };

        // Map FHIR search parameters to database columns
        let db_field = match field_name {
            "name" => "family_name", // Use family_name for name sort
            "family" => "family_name",
            "given" => "given_name",
            "birthdate" => "birthdate",
            "gender" => "gender",
            "active" => "active",
            "_lastUpdated" | "last_updated" => "last_updated",
            _ => {
                return Err(FhirError::InvalidSearchParameter(format!(
                    "Invalid sort field: {}",
                    field_name
                )))
            }
        };

        sorts.push(SortParam {
            field: db_field.to_string(),
            direction,
        });
    }

    if sorts.is_empty() {
        // Default to last_updated descending
        sorts.push(SortParam {
            field: "last_updated".to_string(),
            direction: SortDirection::Descending,
        });
    }

    Ok(sorts)
}

fn parse_date_param(value: &str) -> Result<DateComparison> {
    let (prefix, date_str) = if let Some(stripped) = value.strip_prefix("eq") {
        (DatePrefix::Eq, stripped)
    } else if let Some(stripped) = value.strip_prefix("ne") {
        (DatePrefix::Ne, stripped)
    } else if let Some(stripped) = value.strip_prefix("gt") {
        (DatePrefix::Gt, stripped)
    } else if let Some(stripped) = value.strip_prefix("ge") {
        (DatePrefix::Ge, stripped)
    } else if let Some(stripped) = value.strip_prefix("lt") {
        (DatePrefix::Lt, stripped)
    } else if let Some(stripped) = value.strip_prefix("le") {
        (DatePrefix::Le, stripped)
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

fn parse_chained_param(key: &str, value: &str) -> Result<ChainedSearch> {
    // Parse forward chaining parameters
    // Format: reference_param.param1.param2...=value
    // Example: "general-practitioner.family" = "Smith"
    // Example: "subject:Patient.family" = "Brown"
    // Example: "subject:Patient.general-practitioner.family" = "Smith" (multi-level)

    // Check for resource type modifier (e.g., "subject:Patient")
    let (base_param, resource_type) = if let Some(colon_pos) = key.find(':') {
        let dot_pos = key.find('.').unwrap_or(key.len());
        if colon_pos < dot_pos {
            // Resource type modifier before first dot
            let parts: Vec<&str> = key[..dot_pos].split(':').collect();
            if parts.len() == 2 {
                (parts[0].to_string(), Some(parts[1].to_string()))
            } else {
                (key[..dot_pos].to_string(), None)
            }
        } else {
            (key[..dot_pos].to_string(), None)
        }
    } else {
        let dot_pos = key.find('.').unwrap_or(key.len());
        (key[..dot_pos].to_string(), None)
    };

    // Parse the chain (everything after first dot)
    let chain_str = if let Some(dot_pos) = key.find('.') {
        &key[dot_pos + 1..]
    } else {
        return Err(FhirError::InvalidSearchParameter(format!(
            "Invalid chained parameter (no dot): {}",
            key
        )));
    };

    // Split chain by dots (e.g., "general-practitioner.family" or "family")
    let chain_parts: Vec<&str> = chain_str.split('.').collect();
    let mut chain_links = Vec::new();

    for part in chain_parts {
        chain_links.push(ChainLink {
            param: part.to_string(),
        });
    }

    Ok(ChainedSearch {
        reference_param: base_param,
        resource_type,
        chain: chain_links,
        search_value: value.to_string(),
        modifier: StringModifier::Contains, // Default to contains for now
    })
}

fn parse_reverse_chain(key: &str, value: &str) -> Result<ReverseChainedSearch> {
    // Parse reverse chaining (_has parameter)
    // Format: _has:ResourceType:reference:parameter=value
    // Example: "_has:Observation:subject:code" = "8867-4"

    // Remove "_has:" prefix
    let without_prefix = key.strip_prefix("_has:").ok_or_else(|| {
        FhirError::InvalidSearchParameter(format!("Invalid _has parameter: {}", key))
    })?;

    // Split by colons: ResourceType:reference:parameter
    let parts: Vec<&str> = without_prefix.split(':').collect();
    if parts.len() != 3 {
        return Err(FhirError::InvalidSearchParameter(format!(
            "Invalid _has format (expected ResourceType:reference:parameter): {}",
            key
        )));
    }

    Ok(ReverseChainedSearch {
        target_resource_type: parts[0].to_string(),
        reference_param: parts[1].to_string(),
        search_param: parts[2].to_string(),
        search_value: value.to_string(),
    })
}
