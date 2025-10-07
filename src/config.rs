use std::env;

#[derive(Debug, Clone)]
pub struct Config {
    pub database_url: String,
    pub server_host: String,
    pub server_port: u16,
    pub db_max_connections: u32,
    pub db_min_connections: u32,
    pub db_connection_timeout_secs: u64,
    pub db_idle_timeout_secs: u64,
}

impl Config {
    pub fn from_env() -> Result<Self, String> {
        let database_url = env::var("DATABASE_URL")
            .unwrap_or_else(|_| "postgres://postgres:postgres@localhost/fhir".to_string());

        let server_host = env::var("SERVER_HOST")
            .unwrap_or_else(|_| "127.0.0.1".to_string());

        let server_port = env::var("SERVER_PORT")
            .unwrap_or_else(|_| "3000".to_string())
            .parse()
            .map_err(|e| format!("Invalid SERVER_PORT: {}", e))?;

        let db_max_connections = env::var("DB_MAX_CONNECTIONS")
            .unwrap_or_else(|_| "10".to_string())
            .parse()
            .map_err(|e| format!("Invalid DB_MAX_CONNECTIONS: {}", e))?;

        let db_min_connections = env::var("DB_MIN_CONNECTIONS")
            .unwrap_or_else(|_| "2".to_string())
            .parse()
            .map_err(|e| format!("Invalid DB_MIN_CONNECTIONS: {}", e))?;

        let db_connection_timeout_secs = env::var("DB_CONNECTION_TIMEOUT_SECS")
            .unwrap_or_else(|_| "30".to_string())
            .parse()
            .map_err(|e| format!("Invalid DB_CONNECTION_TIMEOUT_SECS: {}", e))?;

        let db_idle_timeout_secs = env::var("DB_IDLE_TIMEOUT_SECS")
            .unwrap_or_else(|_| "600".to_string())
            .parse()
            .map_err(|e| format!("Invalid DB_IDLE_TIMEOUT_SECS: {}", e))?;

        Ok(Config {
            database_url,
            server_host,
            server_port,
            db_max_connections,
            db_min_connections,
            db_connection_timeout_secs,
            db_idle_timeout_secs,
        })
    }

    pub fn server_addr(&self) -> String {
        format!("{}:{}", self.server_host, self.server_port)
    }
}
