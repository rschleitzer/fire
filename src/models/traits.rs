use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::Value;

/// Trait for FHIR resources that support versioning
#[async_trait]
pub trait VersionedResource: Sized + Send + Sync {
    type History: Send + Sync;

    const RESOURCE_TYPE: &'static str;
    const TABLE_NAME: &'static str;
    const HISTORY_TABLE_NAME: &'static str;

    fn get_id(&self) -> &String;
    fn get_version_id(&self) -> i32;
    fn get_last_updated(&self) -> &DateTime<Utc>;
    fn get_content(&self) -> &Value;
}

/// Search parameters extracted from FHIR resources
#[derive(Debug, Clone)]
pub struct SearchParams {
    pub strings: Vec<(String, Vec<String>)>,
    pub tokens: Vec<(String, Vec<TokenValue>)>,
    pub dates: Vec<(String, DateTime<Utc>)>,
    pub references: Vec<(String, Vec<String>)>,
}

#[derive(Debug, Clone)]
pub struct TokenValue {
    pub system: Option<String>,
    pub value: String,
}

impl SearchParams {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            tokens: Vec::new(),
            dates: Vec::new(),
            references: Vec::new(),
        }
    }
}

impl Default for SearchParams {
    fn default() -> Self {
        Self::new()
    }
}
