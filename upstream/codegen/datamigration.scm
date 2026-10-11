; =============================================================================
; C# DataMigration generator - generates {project-dir}/DataMigration.cs
; Embeds all migration SQL as C# string literals for startup execution
; =============================================================================

; Helper: JSONB extraction expression for an inline search parameter.
;
; The cast has to match the column, and the column type is (db-inline-type prop-type PostgreSql),
; which on PostgreSql is postgres-type - so the cast is derived from the same function instead of
; being enumerated here. Only the text-ish types need no cast, because ->> already yields text.
; Until 2026-09 this listed boolean and instant and let everything else fall through to text, so
; the copy of a resource with an inline INTEGER or DECIMAL search column (Medium.length, say) died
; with "column is of type integer but expression is of type text" and took the whole migration
; with it.
(define (dm-inline-extract-from search col)
    (let* ((prop (search-property search))
           (prop-type (if prop (% "type" prop) "code"))
           (json-field (search-fhir-field-name search))
           (extract ($"convert_from(" col ", 'UTF8')::jsonb->>'" json-field "'")))
      (case prop-type
        (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") extract)
        (else ($"(" extract ")::" (postgres-type prop-type))))))

; From a source version row during the copy.
(define (dm-inline-extract search alias)
    (dm-inline-extract-from search ($ alias ".resource")))

; From the target table's own content, for the rebuild after the transforms.
(define (dm-inline-extract-content search)
    (dm-inline-extract-from search "content"))

; Helper: inline JSONB extractions for SELECT list (leading comma+newline per expr)
(define (dm-inline-select-exprs inlines alias)
    (let loop ((items (node-list->list inlines)))
      (if (null? items) ""
          ($",
    " (dm-inline-extract (car items) alias)
             (loop (cdr items))))))

; Helper: inline column names for C# verbatim strings (""col"" format)
(define (dm-cs-inline-cols inlines)
    (let loop ((items (node-list->list inlines)))
      (if (null? items) ""
          ($", \"\"" (search-col (car items)) "\"\""
             (loop (cdr items))))))

; Helper: copy search tables for C# verbatim strings
(define (dm-cs-copy-search-tables schema source-schema searches insert-cols select-cols)
    (let loop ((items searches))
      (if (null? items) ""
          (let ((tn (search-table-name (car items))))
            ($"INSERT INTO \"\"" schema "\"\"." tn " (" insert-cols ")
SELECT " select-cols "
FROM \"\"" source-schema "\"\"." tn " s
JOIN \"\"" source-schema "\"\".resource r ON r.id = s.resource_id
WHERE s.version = r.current_version AND r.current_version > 0;
" (loop (cdr items)))))))

(define (dm-cs-phase1 rn schema source-schema inlines has-history)
    ($"INSERT INTO \"\"" rn "\"\" (id, " (if has-history "version_id, " "") "last_updated, content" (dm-cs-inline-cols inlines) ")
SELECT r.id, " (if has-history "r.current_version, " "") "rv.last_updated, rv.resource" (dm-inline-select-exprs inlines "rv") "
FROM \"\"" source-schema "\"\".resource r
JOIN \"\"" source-schema "\"\".resource_version rv
    ON rv.resource_id = r.id AND rv.version = r.current_version
WHERE r.current_version > 0;
"))

(define (dm-cs-phase2 schema source-schema inlines)
    ($"INSERT INTO \"\"" schema "\"\".resource_history (id, version_id, last_updated, content" (dm-cs-inline-cols inlines) ",
    history_operation, history_timestamp)
SELECT rv.resource_id, rv.version, rv.last_updated, rv.resource" (dm-inline-select-exprs inlines "rv") ",
    CASE WHEN rv.version = 1 THEN 'create' ELSE 'update' END,
    rv.last_updated
FROM \"\"" source-schema "\"\".resource_version rv
JOIN \"\"" source-schema "\"\".resource r ON r.id = rv.resource_id
WHERE rv.resource IS NOT NULL AND rv.version != r.current_version;
"))

(define (dm-cs-phase3 schema source-schema inlines)
    ($"INSERT INTO \"\"" schema "\"\".resource_history (id, version_id, last_updated, content" (dm-cs-inline-cols inlines) ",
    history_operation, history_timestamp)
SELECT rv.resource_id, rv.version + 1, r.deleted_at, rv.resource" (dm-inline-select-exprs inlines "rv") ",
    'delete', r.deleted_at
FROM \"\"" source-schema "\"\".resource r
JOIN \"\"" source-schema "\"\".resource_version rv
    ON rv.resource_id = r.id
    AND rv.version = (SELECT MAX(version) FROM \"\"" source-schema "\"\".resource_version WHERE resource_id = r.id)
WHERE r.current_version = 0 AND r.deleted_at IS NOT NULL;
"))

(define (dm-cs-phase4 schema source-schema resource)
    (let ((strings (node-list->list (string-table-searches resource)))
          (tokens (node-list->list (token-table-searches resource)))
          (dates (node-list->list (date-table-searches resource)))
          (refs (node-list->list (reference-table-searches resource))))
      (if (and (null? strings) (null? tokens) (null? dates) (null? refs))
          ""
          ($ (dm-cs-copy-search-tables schema source-schema strings
                "id, resource_id, value"
                "s.id, s.resource_id, s.value")
             (dm-cs-copy-search-tables schema source-schema tokens
                "id, resource_id, system, value, display"
                "s.id, s.resource_id, s.system, s.value, s.display")
             (dm-cs-copy-search-tables schema source-schema dates
                "id, resource_id, value_low, value_high"
                "s.id, s.resource_id, s.value_low, s.value_high")
             (dm-cs-copy-search-tables schema source-schema refs
                "id, resource_id, target_type, target_id, target_version"
                "s.id, s.resource_id, s.target_type, s.target_id, s.target_version")))))

(define (dm-cs-resource resource)
    (let* ((rn (name-of resource))
           (source-schema (downcase-string rn))
           (schema rn)
           (inlines (inline-searches resource))
           (has-history (history? resource)))
      ($"
            if (await SchemaExistsAsync(dataSource, \"" source-schema "\"))
            {
                Console.WriteLine(\"Data migration: " rn "...\");
                await ExecuteAsync(dataSource, @\""
        (dm-cs-phase1 rn schema source-schema inlines has-history)
        (if has-history
            ($ (dm-cs-phase2 schema source-schema inlines)
               (dm-cs-phase3 schema source-schema inlines))
            "")
        "\");
"
    (if (string=? "Binary" rn)
        "                await TryCopyTableAsync(dataSource, \"binary\", \"resource_data\",
                    @\"INSERT INTO \"\"Binary\"\".resource_data (resource_id, version_id, data)
SELECT rd.resource_id, rd.version, rd.data
FROM \"\"binary\"\".resource_data rd
JOIN \"\"binary\"\".resource r ON r.id = rd.resource_id
WHERE r.current_version > 0;\");
"
        "")
"            }
")))

(define (dm-cs-phase4-robust schema source-schema resource)
    (let ((strings (node-list->list (string-table-searches resource)))
          (tokens (node-list->list (token-table-searches resource)))
          (dates (node-list->list (date-table-searches resource)))
          (refs (node-list->list (reference-table-searches resource))))
      (if (and (null? strings) (null? tokens) (null? dates) (null? refs))
          ""
          ($ (dm-cs-robust-copy schema source-schema strings
                "id, resource_id, value"
                "s.id, s.resource_id, s.value")
             (dm-cs-robust-copy schema source-schema tokens
                "id, resource_id, system, value, display"
                "s.id, s.resource_id, s.system, s.value, s.display")
             (dm-cs-robust-copy schema source-schema dates
                "id, resource_id, value_low, value_high"
                "s.id, s.resource_id, s.value_low, s.value_high")
             (dm-cs-robust-copy schema source-schema refs
                "id, resource_id, target_type, target_id, target_version"
                "s.id, s.resource_id, s.target_type, s.target_id, s.target_version")))))

(define (dm-cs-robust-copy schema source-schema searches insert-cols select-cols)
    (let loop ((items searches))
      (if (null? items) ""
          (let ((tn (search-table-name (car items))))
            ($"                await TryCopyTableAsync(dataSource, \"" source-schema "\", \"" tn "\",
                    @\"INSERT INTO \"\"" schema "\"\"." tn " (" insert-cols ")
SELECT " select-cols "
FROM \"\"" source-schema "\"\"." tn " s
JOIN \"\"" source-schema "\"\".resource r ON r.id = s.resource_id
WHERE s.version = r.current_version AND r.current_version > 0;\");
" (loop (cdr items)))))))

; Data-only migrations declared as datamigration (class=...) children of an upgrade step
; also run in the one-time V1 data migration: Database.GetDatabaseVersion puts a legacy database
; straight at the current software version, so its versioned upgrade steps never execute. Here the
; data has just been copied over in its old encoding, so the same rewrite is due. Both paths are
; therefore mutually exclusive - no migration is ever applied twice. The run attribute selects the
; path: both (default), upgrade (only the versioned step) or v1 (only the V1 migration, for fixups
; that have no meaning for data that never was a V1 database).
;
; The legacy schemas are never dropped, so a migration can always read V1 source data; the run
; records its completion with a marker in fire_meta.dbinfo instead. Beyond the copied data they
; get the service provider - repositories are usable here, which is what a migration needs to
; rebuild search indexes for content it rewrote.
(define (runs-in-v1? action)
    (let ((run (% "run" action)))
        (or (not run) (string=? run "both") (string=? run "v1"))))

(define (runs-in-upgrade? action)
    (let ((run (% "run" action)))
        (or (not run) (string=? run "both") (string=? run "upgrade"))))

(define (datamigration-hook-calls)
    (for (upgrade-list) (lambda (upgrade)
        (for-children-of upgrade (lambda (action)
            (if (and (string=? (gi action) "datamigration") (runs-in-v1? action))
                ($"            await " (% "class" action) ".RunAsync(dataSource, services);
")
                ""))))))

; (legacy schema, target main table) pairs for the closing balance.
(define (dm-balance-pair-list)
    (for-active-resources (lambda (resource)
        (let ((rn (name-of resource)))
            ($"        (\"" (downcase-string rn) "\", \"" rn "\"),
")))))

; --- Rebuilding the search indexes after the copy -----------------------------------------
;
; The copy no longer carries the source search tables over. Copying them was wrong twice: a
; parameter whose type changed between the versions has different columns on either side and the
; INSERT simply fails (a reference search that became a string search, say), and any index copied
; before the content transforms is stale the moment a transform rewrites the content. Rebuilding
; from the migrated content answers both, and it is the same path the reseed endpoint uses.
;
; Two halves, because the two kinds of index live in different places: the table-based parameters
; are rebuilt by the repository's CreateSearchAsync, which re-extracts them from content, and the
; inline parameters are columns on the main table itself and are re-derived here with the same
; expression the copy uses.

; The parameters the repository can actually rebuild. It emits its CreateSearchAsync cases from
; the table searches of type string, token, date, reference, number, quantity and uri only (see
; repo-create-search), so anything else - a special parameter, say - would be answered with
; "Unknown search parameter" and take the migration down. The same filter has to be applied here.
(define (dm-reindex-searches resource)
    (node-list-filter
        (lambda (s) (let ((t (% "type" s)))
            (or (string=? "string" t) (string=? "token" t) (string=? "date" t)
                (string=? "reference" t) (string=? "number" t) (string=? "quantity" t)
                (string=? "uri" t))))
        (table-searches resource)))

; Drop and recreate, rather than emptying and refilling: DropSearchAsync is a DROP TABLE IF
; EXISTS and CreateSearchAsync a CREATE TABLE IF NOT EXISTS followed by the extraction, so the
; pair tolerates a table that does not exist yet. Not every search table is part of the initial
; schema, and emptying a missing one with a plain DELETE fails.
(define (dm-reindex-create-calls resource)
    (for (dm-reindex-searches resource) (lambda (s)
        ($"                await repo.DropSearchAsync(\"" (% "name" s) "\");
                await repo.CreateSearchAsync(\"" (% "name" s) "\");
"))))

(define (dm-reindex-inline-sql resource)
    (let ((inlines (node-list->list (inline-searches resource))))
      (if (null? inlines)
          ""
          ($"UPDATE \"\"" (name-of resource) "\"\" SET "
            (let loop ((items inlines) (first #t))
              (if (null? items) ""
                  ($ (if first "" ", ")
                     "\"\"" (search-col (car items)) "\"\" = " (dm-inline-extract-content (car items))
                     (loop (cdr items) #f))))
            ";"))))

(define (dm-reindex-resource resource)
    (let* ((rn (name-of resource))
           (tables (node-list->list (dm-reindex-searches resource)))
           (inline-sql (dm-reindex-inline-sql resource)))
      (if (and (null? tables) (string=? "" inline-sql))
          ""
          ($"            {
                Console.WriteLine(\"Data migration: reindex " rn "...\");
"
            (if (null? tables) ""
                ($"                var repo = services.GetRequiredService<""I" rn "Repository>();
" (dm-reindex-create-calls resource)))
            (if (string=? "" inline-sql) ""
                ($"                await ExecuteAsync(dataSource, @\"" inline-sql "\");
"))
            "            }
"))))

(define (datamigration-cs)
    (let ((first-schema (downcase-string (name-of (node-list-first (active-resources))))))
      (file "DataMigration.cs"
          ($"using Microsoft.Extensions.DependencyInjection;
using " (project-name) ".Repositories;

namespace " (project-name) ";

public static class DataMigration
{
    public static async Task RunIfNeededAsync(string dbType, string connectionString, IServiceProvider services)
    {
        if (dbType != \"PostgreSql\") return;

        await using var dataSource = Npgsql.NpgsqlDataSource.Create(connectionString);

        // Check if old-style lowercase schema exists (e.g. \"" first-schema "\".resource)
        bool hasOldSchema;
        await using (var conn = await dataSource.OpenConnectionAsync())
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = \"SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_schema = '" first-schema "' AND table_name = 'resource')\";
            hasOldSchema = (bool)(await cmd.ExecuteScalarAsync())!;
        }
        if (!hasOldSchema) return;

        // A completed migration is recorded by a marker in fire_meta.dbinfo. Until 2026-09 the
        // marker was the dropping of the old schemas - which meant the only way back to the
        // previous image was gone the moment the copy succeeded. The old schemas are kept now,
        // so the run needs a record of its own: without one it would copy again on every
        // server start.
        var migrationState = await ReadDbInfoAsync(dataSource, MigrationStateKey);
        if (migrationState == StateDone)
        {
            Console.WriteLine(\"Data migration: already completed (marker in fire_meta.dbinfo), skipped.\");
            return;
        }

        // Guard against data loss: target data that no completed run of this migration wrote is
        // never touched. Since the run is one transaction, a run of its own leaves either the done
        // marker or empty target tables. Data without the marker therefore comes from something
        // else - or from an image before the transaction that was interrupted mid-run; those wrote
        // the state running. What is correct then is a decision for a person, so startup ends with
        // a message instead of a delete.
        if (await TargetHasDataAsync(dataSource))
            throw new InvalidOperationException(migrationState == \"running\"
                ? \"Data migration: aborted - an earlier image was interrupted during the migration \"
                  + \"(state running in fire_meta.dbinfo) and left partly filled target tables. Empty \"
                  + \"the target tables and remove the marker; the next start then migrates from the \"
                  + \"kept legacy schemas.\"
                : \"Data migration: aborted - the target tables already contain data, but no marker \"
                  + \"in fire_meta.dbinfo records a migration run. This database is not migrated \"
                  + \"automatically; please clarify its state.\");

        Console.WriteLine(\"Data migration: starting...\");

        // The whole run - copy, balance, data migrations, search index rebuild and the done marker -
        // is one database transaction. Every statement joins the DbTransactionScope: the helpers
        // below, the provider, the repositories and the hand-written migrations. A run that fails
        // or is interrupted leaves nothing behind, the database is exactly as before, and the next
        // start begins anew. The legacy schemas are only ever read. The scope is a maintenance scope:
        // copying a large table in one statement takes longer than the 30 seconds a command gets by
        // default, and the run would end there on every start.
        await using var scope = await DbTransactionScope.BeginMaintenanceAsync(services.GetRequiredService<""IDatabaseProvider>());
        try
        {
          using (scope.Enter())
          {
" (for-active-resources dm-cs-resource)
"
            // Per-type balance of the copy itself, source against target. It runs before the
            // hooks and before any seed data is added, so the numbers answer exactly one
            // question: did every source row arrive? With no access to the database the startup
            // log is the only evidence of that.
            await ReportBalanceAsync(dataSource);

            // Data-only migrations declared as <""datamigration> upgrade actions: the copied data
            // still carries the old encoding, and the versioned upgrade steps are skipped for a
            // legacy database (it starts out at the current software version). The legacy schemas
            // are kept, so a migration can read V1 source data directly.
" (datamigration-hook-calls)
"
            // The search indexes are rebuilt from the migrated content, after every transform has
            // had its say. They are not copied over any more - see the note above the generator.
            Console.WriteLine(\"Data migration: rebuilding search indexes from the migrated content...\");
" (for-active-resources dm-reindex-resource)
"            Console.WriteLine(\"Data migration: search indexes rebuilt.\");

            // The old schemas are deliberately kept: they are the only way back to the previous
            // image, and nothing reads them from here on (the marker below stops the run).
            // Removing them is a later, explicit step.
            Console.WriteLine(\"Data migration: old schemas retained (way back to the previous version).\");

            // The marker comes last - it only counts once everything before it has succeeded.
            await ExecuteAsync(dataSource, MigrationDoneSql);

            // A PostgreSQL transaction that hit an error rejects every later statement, and a
            // COMMIT on it rolls back without an error. The probe turns that into an exception
            // instead of a run that looks committed but is not.
            await ExecuteAsync(dataSource, \"SELECT 1\");
          }
          await scope.CommitAsync();
          Console.WriteLine(\"Data migration: completed successfully.\");
        }
        catch (Exception ex)
        {
            Console.WriteLine($\"Data migration failed, everything rolled back: {ex.Message}\");
            throw;
        }
    }

    // Inside the run's DbTransactionScope a statement goes to the connection and transaction of the
    // scope; before it (the guards) it gets a connection of its own.
    private static async Task<""T> WithCommandAsync<""T>(Npgsql.NpgsqlDataSource dataSource, string sql, Func<""System.Data.Common.DbCommand, Task<""T>> run)
    {
        var scope = DbTransactionScope.Current;
        await using var ownConn = scope == null ? await dataSource.OpenConnectionAsync() : null;
        System.Data.Common.DbConnection conn = scope?.Connection ?? ownConn!;
        await using var cmd = conn.CreateCommand();
        cmd.Transaction = scope?.Transaction;
        cmd.CommandText = sql;
        return await run(cmd);
    }

    private static Task<""bool> SchemaExistsAsync(Npgsql.NpgsqlDataSource dataSource, string schema)
        => WithCommandAsync(dataSource,
            $\"SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_schema = '{schema}' AND table_name = 'resource')\",
            async cmd => (bool)(await cmd.ExecuteScalarAsync())!);

    private static Task<""bool> TableExistsAsync(Npgsql.NpgsqlDataSource dataSource, string schema, string table)
        => WithCommandAsync(dataSource,
            $\"SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_schema = '{schema}' AND table_name = '{table}')\",
            async cmd => (bool)(await cmd.ExecuteScalarAsync())!);

    private static async Task TryCopyTableAsync(Npgsql.NpgsqlDataSource dataSource, string schema, string table, string sql)
    {
        if (!await TableExistsAsync(dataSource, schema, table))
        {
            Console.WriteLine($\"  Skipping {schema}.{table} (not found in source)\");
            return;
        }
        await ExecuteAsync(dataSource, sql);
    }

    private static Task ExecuteAsync(Npgsql.NpgsqlDataSource dataSource, string sql)
        => WithCommandAsync(dataSource, sql, cmd => cmd.ExecuteNonQueryAsync());

    // State of the one-time legacy data migration, kept next to 'version' in fire_meta.dbinfo.
    // The SQL is spelled out here because the whole file assumes PostgreSql (see RunIfNeededAsync).
    private const string MigrationStateKey = \"v1migration\";
    private const string StateDone = \"done\";

    private static string SetStateSql(string state) =>
        $\"INSERT INTO \\\"fire_meta\\\".dbinfo (name, value) VALUES ('{MigrationStateKey}', '{state}') \"
        + \"ON CONFLICT (name) DO UPDATE SET value = EXCLUDED.value\";

    private static string MigrationDoneSql => SetStateSql(StateDone);

    // The first populated target table is enough; this only asks whether any data is present.
    private static async Task<""bool> TargetHasDataAsync(Npgsql.NpgsqlDataSource dataSource)
    {
        foreach (var (_, targetTable) in BalancePairs)
            if (await CountAsync(dataSource, $\"SELECT COUNT(*) FROM (SELECT 1 FROM \\\"{targetTable}\\\" LIMIT 1) t\") > 0)
                return true;
        return false;
    }

    // A missing table counts as no marker at all - the same tolerance as Database.ReadDbVersion.
    private static async Task<""string?> ReadDbInfoAsync(Npgsql.NpgsqlDataSource dataSource, string name)
    {
        try
        {
            await using var conn = await dataSource.OpenConnectionAsync();
            await using var cmd = conn.CreateCommand();
            cmd.CommandText = $\"SELECT value FROM \\\"fire_meta\\\".dbinfo WHERE name = '{name}'\";
            return await cmd.ExecuteScalarAsync() as string;
        }
        catch { return null; }
    }

    // Row count per type: source (current, undeleted version) against the target main table.
    // A difference is not necessarily an error - migrations deliberately discard resources, test
    // cases for instance - but it has to be visible in the log.
    private static async Task ReportBalanceAsync(Npgsql.NpgsqlDataSource dataSource)
    {
        Console.WriteLine(\"Data migration: balance source -> target\");
        var differing = 0;
        foreach (var (sourceSchema, targetTable) in BalancePairs)
        {
            if (!await SchemaExistsAsync(dataSource, sourceSchema)) continue;
            var source = await CountAsync(dataSource, $\"SELECT COUNT(*) FROM \\\"{sourceSchema}\\\".resource WHERE current_version > 0\");
            var target = await CountAsync(dataSource, $\"SELECT COUNT(*) FROM \\\"{targetTable}\\\"\");
            var mark = source == target ? \"\" : \"   DIFFERS\";
            if (source != target) differing++;
            Console.WriteLine($\"  {targetTable,-24} {source,8} -> {target,8}{mark}\");
        }
        Console.WriteLine(differing == 0
            ? \"Data migration: balance matches for every type.\"
            : $\"Data migration: {differing} type(s) with a differing row count - see the notes of the individual migrations above.\");
    }

    // Outside the transaction a failing count reads as -1. Inside it an error is not swallowed: it
    // has already aborted the transaction, and every later statement would fail anyway.
    private static async Task<""long> CountAsync(Npgsql.NpgsqlDataSource dataSource, string sql)
    {
        try
        {
            return await WithCommandAsync(dataSource, sql, async cmd => Convert.ToInt64(await cmd.ExecuteScalarAsync()));
        }
        catch when (DbTransactionScope.Current == null) { return -1; }
    }

    private static readonly (string SourceSchema, string TargetTable)[] BalancePairs =
    {
" (dm-balance-pair-list) "    };
}
"))))
