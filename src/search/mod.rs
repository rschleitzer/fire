use chrono::NaiveDate;
use std::collections::HashMap;

use crate::error::Result;

/// Generic search parameter structure (scalable to 140+ resources)
#[derive(Debug, Clone)]
pub struct SearchParam {
    pub name: String,
    pub value: String,
    pub modifier: Option<String>, // "exact", "contains", "missing", "not"
    pub prefix: Option<String>,   // "eq", "ne", "gt", "lt", "ge", "le", "sa", "eb", "ap"
}

#[derive(Debug, Clone)]
pub struct SearchQuery {
    pub params: Vec<SearchParam>,
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

impl SearchQuery {
    /// Parse FHIR search parameters from HTTP query string
    /// This is the universal parser for all resources - resource-specific logic in repositories
    pub fn from_params(params: &HashMap<String, String>) -> Result<Self> {
        let mut search_params = Vec::new();

        // Parse all non-special parameters
        for (key, value) in params.iter() {
            // Skip pagination and sort parameters - handled separately
            if key.starts_with('_') && !key.starts_with("_has:") {
                continue;
            }

            // Parse parameter name and modifier
            let (param_name, modifier) = if key.contains(':') {
                let parts: Vec<&str> = key.splitn(2, ':').collect();
                (parts[0].to_string(), Some(parts[1].to_string()))
            } else {
                (key.clone(), None)
            };

            // Split comma-separated values (FHIR OR semantics)
            // e.g., family=Smith,Johnson becomes two separate search params
            let values: Vec<&str> = value.split(',').collect();

            for val in values {
                let val = val.trim();
                if val.is_empty() {
                    continue;
                }

                // Extract prefix for date/quantity parameters
                let (prefix, clean_value) = extract_prefix(val);

                search_params.push(SearchParam {
                    name: param_name.clone(),
                    value: clean_value,
                    modifier: modifier.clone(),
                    prefix,
                });
            }
        }

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

        // Parse sort
        let sort = if let Some(sort_param) = params.get("_sort") {
            parse_sort_param(sort_param)?
        } else {
            vec![SortParam {
                field: "last_updated".to_string(),
                direction: SortDirection::Descending,
            }]
        };

        Ok(SearchQuery {
            params: search_params,
            limit,
            offset,
            sort,
        })
    }
}

/// Extract prefix from value (for date and quantity comparisons)
fn extract_prefix(value: &str) -> (Option<String>, String) {
    for prefix in &["eq", "ne", "gt", "ge", "lt", "le", "sa", "eb", "ap"] {
        if let Some(stripped) = value.strip_prefix(prefix) {
            return (Some(prefix.to_string()), stripped.to_string());
        }
    }
    (None, value.to_string())
}

fn parse_sort_param(value: &str) -> Result<Vec<SortParam>> {
    let mut sorts = Vec::new();

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

        sorts.push(SortParam {
            field: field_name.to_string(),
            direction,
        });
    }

    if sorts.is_empty() {
        sorts.push(SortParam {
            field: "last_updated".to_string(),
            direction: SortDirection::Descending,
        });
    }

    Ok(sorts)
}

// Keep old types for backward compatibility during migration
#[derive(Debug, Clone, PartialEq)]
pub enum StringModifier {
    Contains,
    Exact,
    Missing,
    Not,
}

#[derive(Debug, Clone)]
pub struct StringSearch {
    pub value: String,
    pub modifier: StringModifier,
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
