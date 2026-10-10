; dbprovider.scm — generates IDatabaseProvider interface and per-DB implementations
; Called from (element resources ...) context.

; Generate IDatabaseProvider.cs interface
(define (dbp-interface)
    (file "IDatabaseProvider.cs" ($
"using System.Data.Common;

namespace " (project-name) ";

public interface IDatabaseProvider
{
    // Opens a connection to the configured database; the caller owns and disposes it.
    Task<""DbConnection> OpenConnectionAsync();
    // Opens a connection for the work at start-up - a versioned upgrade step or the one-time data
    // migration. Its commands have no time limit: a statement over a whole table takes as long as
    // the table is large, and the client default of 30 seconds would end the run on real data.
    Task<""DbConnection> OpenMaintenanceConnectionAsync();
    Task<""object?> ExecuteScalarAsync(string sql);
    Task ExecuteSqlAsync(string sql);
    Task EnsureDatabaseExists(string databaseName);
    string InitialSchema { get; }
    string CreateDbInfoTableSql { get; }
    string SelectVersionSql { get; }
    string SelectLegacyVersionSql { get; }
    string HasFireMetaSchemaSql { get; }
    string CreateIndexesSql { get; }
    string DropIndexesSql { get; }
    string? ReseedTruncateSql(string resourceType);
    string InsertVersionSql(string version);
" (dbp-application-migration-interface)
"    string UpdateVersionSql(string version);
    string UpgradeSql(string version);
}
")))

; Generate DatabaseProvider.cs for a specific DB
(define (dbp-implementation db)
    (file "DatabaseProvider.cs" ($
"using " (db-using db) ";

namespace " (project-name) ".Repositories." (db-namespace db) ";

public class DatabaseProvider : IDatabaseProvider
{
"
    (case db
        ((PostgreSql) ($
"    private readonly NpgsqlDataSource _dataSource;
    private readonly string _connectionString;

    public DatabaseProvider(NpgsqlDataSource dataSource, string connectionString)
    {
        _dataSource = dataSource;
        _connectionString = connectionString;
    }

    public async Task<""System.Data.Common.DbConnection> OpenConnectionAsync()
        => await _dataSource.OpenConnectionAsync();

    // Command Timeout is a setting of the connection in Npgsql and the default of every command
    // created on it, so the one connection of a DbTransactionScope covers all statements of the run.
    public async Task<""System.Data.Common.DbConnection> OpenMaintenanceConnectionAsync()
    {
        var builder = new NpgsqlConnectionStringBuilder(_connectionString) { CommandTimeout = 0 };
        var conn = new NpgsqlConnection(builder.ConnectionString);
        await conn.OpenAsync();
        return conn;
    }

    // Inside a DbTransactionScope (a versioned upgrade step, the one-time data migration or a
    // transaction bundle) both methods run on the connection and within the transaction of the
    // scope, so they see its uncommitted work and roll back with it.
    public async Task<""object?> ExecuteScalarAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? await _dataSource.OpenConnectionAsync() : null;
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        return await cmd.ExecuteScalarAsync();
    }

    public async Task ExecuteSqlAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? await _dataSource.OpenConnectionAsync() : null;
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        await cmd.ExecuteNonQueryAsync();
    }

    public async Task EnsureDatabaseExists(string databaseName)
    {
        var builder = new NpgsqlConnectionStringBuilder(_connectionString);
        // The data source connects to the database named in the connection string;
        // that is the one that must exist. The passed name is only a fallback.
        if (!string.IsNullOrWhiteSpace(builder.Database))
            databaseName = builder.Database!;
        builder.Database = \"postgres\";
        try
        {
            var ds = NpgsqlDataSource.Create(builder.ConnectionString);
            await using var conn = await ds.OpenConnectionAsync();
            await using var cmd = conn.CreateCommand();
            cmd.CommandText = $\"SELECT COUNT(*) FROM pg_database WHERE datname = '{databaseName}'\";
            var count = (long)(await cmd.ExecuteScalarAsync())!;
            if (count == 0)
            {
                Console.WriteLine($\"Creating database {databaseName}...\");
                cmd.CommandText = $\"CREATE DATABASE \\\"{databaseName}\\\"\";
                await cmd.ExecuteNonQueryAsync();
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($\"Warning: Could not check/create database: {ex.Message}\");
        }
    }
"       ))
        ((SqlServer) ($
"    private readonly string _connectionString;

    public DatabaseProvider(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task<""System.Data.Common.DbConnection> OpenConnectionAsync()
    {
        var conn = new SqlConnection(_connectionString);
        await conn.OpenAsync();
        return conn;
    }

    // Command Timeout in the connection string is the default of every command created on the
    // connection (Microsoft.Data.SqlClient), 0 waits without limit.
    public async Task<""System.Data.Common.DbConnection> OpenMaintenanceConnectionAsync()
    {
        var builder = new SqlConnectionStringBuilder(_connectionString) { CommandTimeout = 0 };
        var conn = new SqlConnection(builder.ConnectionString);
        await conn.OpenAsync();
        return conn;
    }

    // Inside a DbTransactionScope both methods run on its connection and within its transaction;
    // SqlClient rejects a command on that connection without the pending transaction.
    public async Task<""object?> ExecuteScalarAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? new SqlConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        return await cmd.ExecuteScalarAsync();
    }

    public async Task ExecuteSqlAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? new SqlConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        foreach (var stmt in sql.Split(';', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
        {
            if (string.IsNullOrWhiteSpace(stmt)) continue;
            try
            {
                await using var cmd = conn.CreateCommand();
                cmd.Transaction = scope?.Transaction;
                cmd.CommandText = stmt;
                await cmd.ExecuteNonQueryAsync();
            }
            catch (SqlException ex) when (ex.Number is 2714 or 1913 or 1779)
            {
                // 2714: object already exists, 1913: schema already exists, 1779: table already has PK
            }
        }
    }

    public async Task EnsureDatabaseExists(string databaseName)
    {
        var builder = new SqlConnectionStringBuilder(_connectionString);
        // The repositories connect to the database named in the connection string;
        // that is the one that must exist. The passed name is only a fallback.
        if (!string.IsNullOrWhiteSpace(builder.InitialCatalog))
            databaseName = builder.InitialCatalog;
        builder.InitialCatalog = \"master\";
        try
        {
            await using var conn = new SqlConnection(builder.ConnectionString);
            await conn.OpenAsync();
            await using var cmd = conn.CreateCommand();
            cmd.CommandText = $\"SELECT DB_ID('{databaseName}')\";
            if (await cmd.ExecuteScalarAsync() == DBNull.Value)
            {
                Console.WriteLine($\"Creating database {databaseName}...\");
                cmd.CommandText = $\"CREATE DATABASE \\\"{databaseName}\\\"\";
                await cmd.ExecuteNonQueryAsync();
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($\"Warning: Could not check/create database: {ex.Message}\");
        }
    }
"       ))
        ((Sqlite) ($
"    private readonly string _connectionString;

    public DatabaseProvider(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task<""System.Data.Common.DbConnection> OpenConnectionAsync()
    {
        var conn = new SqliteConnection(_connectionString);
        await conn.OpenAsync();
        return conn;
    }

    // SQLite runs in process, a statement is not cut off by a time limit; the timeout only bounds
    // the wait for a locked database. The ordinary connection serves.
    public Task<""System.Data.Common.DbConnection> OpenMaintenanceConnectionAsync()
        => OpenConnectionAsync();

    // Inside a DbTransactionScope both methods run on its connection and within its transaction;
    // Microsoft.Data.Sqlite rejects a command on that connection without the pending transaction,
    // and a second connection would wait for the write lock the scope holds.
    public async Task<""object?> ExecuteScalarAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? new SqliteConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        return await cmd.ExecuteScalarAsync();
    }

    public async Task ExecuteSqlAsync(string sql)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? new SqliteConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        await cmd.ExecuteNonQueryAsync();
    }

    public Task EnsureDatabaseExists(string databaseName) => Task.CompletedTask;
"       ))
    )
"
    public string InitialSchema => \"\"\"
" (schema-sql db) "\"\"\";

    public string CreateIndexesSql => \"\"\"
" (create-indexes-sql db) "\"\"\";

    public string DropIndexesSql => \"\"\"
" (drop-indexes-sql db) "\"\"\";

    public string? ReseedTruncateSql(string resourceType) => resourceType switch
    {
"
    (for-active-resources (lambda (resource) ($
"        \"" (name-of resource) "\" => \"\"\"
" (reseed-delete-sql-for-resource resource db) "\"\"\",
")))
"        _ => null
    };

    public string CreateDbInfoTableSql => \""
    (case db
        ((SqlServer) "IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'fire_meta') EXEC('CREATE SCHEMA [fire_meta]');IF NOT EXISTS (SELECT * FROM sys.tables t JOIN sys.schemas s ON t.schema_id = s.schema_id WHERE s.name = 'fire_meta' AND t.name = 'dbinfo') CREATE TABLE [fire_meta].dbinfo (name VARCHAR(127) NOT NULL PRIMARY KEY, value VARCHAR(127))")
        ((Sqlite) "CREATE TABLE IF NOT EXISTS fire_meta_dbinfo (name VARCHAR(127) NOT NULL PRIMARY KEY, value VARCHAR(127))")
        ((PostgreSql) "CREATE SCHEMA IF NOT EXISTS \\\"fire_meta\\\"; CREATE TABLE IF NOT EXISTS \\\"fire_meta\\\".dbinfo (name VARCHAR(127) NOT NULL PRIMARY KEY, value VARCHAR(127))"))
"\";

    public string SelectVersionSql => \""
    (case db
        ((SqlServer) "SELECT value FROM [fire_meta].dbinfo WHERE name = 'version'")
        ((Sqlite) "SELECT value FROM fire_meta_dbinfo WHERE name = 'version'")
        ((PostgreSql) "SELECT value FROM \\\"fire_meta\\\".dbinfo WHERE name = 'version'"))
"\";

    public string SelectLegacyVersionSql => \""
    (case db
        ((SqlServer) "SELECT value FROM [storage].dbinfo WHERE name = 'version'")
        ((Sqlite) "SELECT value FROM storage_dbinfo WHERE name = 'version'")
        ((PostgreSql) "SELECT value FROM storage.dbinfo WHERE name = 'version'"))
"\";

    public string HasFireMetaSchemaSql => \""
    (case db
        ((SqlServer) "SELECT COUNT(*) FROM sys.schemas WHERE name = 'fire_meta'")
        ((Sqlite) "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name LIKE 'fire_meta_%'")
        ((PostgreSql) "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'fire_meta'"))
"\";

    public string InsertVersionSql(string version) =>
        $\""
    (case db
        ((SqlServer) "INSERT INTO [fire_meta].dbinfo (name, value) VALUES ('version', '{version}')")
        ((Sqlite) "INSERT INTO fire_meta_dbinfo (name, value) VALUES ('version', '{version}')")
        ((PostgreSql) "INSERT INTO \\\"fire_meta\\\".dbinfo (name, value) VALUES ('version', '{version}')"))
"\";

" (dbp-application-migration-sql db)
"    public string UpdateVersionSql(string version) =>
        $\""
    (case db
        ((SqlServer) "UPDATE [fire_meta].dbinfo SET value = '{version}' WHERE name = 'version'")
        ((Sqlite) "UPDATE fire_meta_dbinfo SET value = '{version}' WHERE name = 'version'")
        ((PostgreSql) "UPDATE \\\"fire_meta\\\".dbinfo SET value = '{version}' WHERE name = 'version'"))
"\";

" (upgrade-sql-method db)
"}
")))

; Generate all database provider files
; Generate DbTransactionScope.cs: one connection and transaction shared by every repository
; operation of a transaction bundle. The repositories are singletons, so the scope travels with
; the async flow in an AsyncLocal instead of an instance field; open-conn and open-conn-tx in
; repositories.scm join it when it is set.
(define (dbp-transaction-scope)
    (file "DbTransactionScope.cs" ($
"using System.Data.Common;

namespace " (project-name) ".Repositories;

// One database transaction spanning several repository operations, used for FHIR transaction
// bundles. While a scope is entered, every repository method running in the same async flow uses
// its connection and transaction instead of opening its own, and skips its own commit and rollback;
// the owner of the scope commits once at the end or rolls everything back.
public sealed class DbTransactionScope : IAsyncDisposable
{
    private static readonly AsyncLocal<""DbTransactionScope?> CurrentScope = new();

    // The scope the current async flow has entered, or null outside of any scope.
    public static DbTransactionScope? Current => CurrentScope.Value;

    public DbConnection Connection { get; }
    public DbTransaction Transaction { get; }
    private bool _committed;

    private DbTransactionScope(DbConnection connection, DbTransaction transaction)
    {
        Connection = connection;
        Transaction = transaction;
    }

    public static Task<""DbTransactionScope> BeginAsync(IDatabaseProvider db)
        => BeginOnAsync(db.OpenConnectionAsync());

    // The scope of a versioned upgrade step or of the one-time data migration: the same, on a
    // connection whose commands have no time limit (IDatabaseProvider.OpenMaintenanceConnectionAsync).
    public static Task<""DbTransactionScope> BeginMaintenanceAsync(IDatabaseProvider db)
        => BeginOnAsync(db.OpenMaintenanceConnectionAsync());

    private static async Task<""DbTransactionScope> BeginOnAsync(Task<""DbConnection> opening)
    {
        var connection = await opening;
        try
        {
            var transaction = await connection.BeginTransactionAsync();
            return new DbTransactionScope(connection, transaction);
        }
        catch
        {
            await connection.DisposeAsync();
            throw;
        }
    }

    // Makes this scope current for the calling async flow until the returned handle is disposed.
    // Synchronous on purpose: an AsyncLocal set inside an async method does not flow back to its
    // caller, so entering has to happen in the method that runs the operations.
    public IDisposable Enter()
    {
        if (CurrentScope.Value != null)
            throw new InvalidOperationException(\"Nested database transaction scopes are not supported.\");
        CurrentScope.Value = this;
        return new Exit();
    }

    public async Task CommitAsync()
    {
        await Transaction.CommitAsync();
        _committed = true;
    }

    public async ValueTask DisposeAsync()
    {
        try
        {
            if (!_committed)
            {
                // A failed rollback must not hide the error that ended the scope; closing the
                // connection below discards the open transaction on the server anyway.
                try { await Transaction.RollbackAsync(); } catch { }
            }
        }
        finally
        {
            await Transaction.DisposeAsync();
            await Connection.DisposeAsync();
        }
    }

    private sealed class Exit : IDisposable
    {
        public void Dispose() => CurrentScope.Value = null;
    }
}
")))

(define (database-providers)
    (make directory path: "Repositories"
    (sosofo-append
        (dbp-interface)
        (dbp-transaction-scope)
        (apply sosofo-append
            (map (lambda (db)
                (make directory path: (db-folder db)
                    (dbp-implementation db)))
                target-databases)))))
