; dbupgrade.scm — model-driven database upgrade generation
; Called from (element resources ...) context.

; Get the upgrade children of upgrades
(define (upgrade-list)
    (let ((upgrades-node (select-children "upgrades" (parent (current-node)))))
        (if (node-list-empty? upgrades-node)
            (empty-node-list)
            (children (node-list-first upgrades-node)))))

(define (has-upgrades?)
    (not (node-list-empty? (upgrade-list))))

; Generate the version checks for Database.InitializeAsync.
; `services` is in scope at the call site so Upgrade can ImportSeedBundle for newresource actions.
(define (upgrade-version-checks)
    (if (not (has-upgrades?)) ""
        (for (upgrade-list) (lambda (upgrade) ($
"
        if (version <"" new Version(\""(% "to" upgrade)"\"))
            await Upgrade(\""(% "to" upgrade)"\", db, services);
")))))

; For each upgrade-version, emit a switch case running per-action runtime work:
;   - newresource: ImportSeedBundle for the new resource type.
;   - newsearch: CreateSearchAsync indexes the parameter from the stored
;     resources, so existing rows are found too. For a subtable search it
;     creates and fills the table, and UpgradeSql deliberately emits no DDL
;     for it - see upgrade-newsearch-sql. For an inline search (token-on-code,
;     instant, number-on-integer/decimal) UpgradeSql adds the column and the
;     call backfills it; without that backfill every row that predates the
;     upgrade would keep a NULL column and drop out of filter and sort.
;   - applybundle: apply a named transaction bundle from the seed directory.
;   - datamigration: call a hand-written migration class that rewrites existing
;     data (e.g. re-coding stored JSON). See (datamigration-hook-calls) in
;     datamigration.scm — the same classes also run in the one-time V1 data
;     migration, because a legacy database jumps straight to the current
;     software version and never executes these upgrade steps. The run attribute
;     (both | upgrade | v1) restricts a migration to one of the two paths.
;   - addhistory: no runtime callback needed.
(define (upgrade-seed-imports)
    (if (not (has-upgrades?)) ""
        (let ((cases (for (upgrade-list) (lambda (upgrade) ($
"                case \""(% "to" upgrade)"\":
" (for-children-of upgrade (lambda (action)
        (case (gi action)
            (("newresource") ($"                    await ImportSeedBundle(\""(name-of (ref action))"\", services);
"))
            (("newsearch")
                (let* ((search (ref action))
                       (resource (parent (parent search)))
                       (rn (name-of resource))
                       (sn (% "name" search)))
                    ($"                    await services.GetRequiredService<""I"rn"Repository>().CreateSearchAsync(\""sn"\");
")))
            (("applybundle") ($"                    await ApplyUpgradeBundle(\""(% "file" action)"\", services);
"))
            (("datamigration")
                (if (runs-in-upgrade? action)
                    ($"                    await " (% "class" action) ".RunAsync(db, services);
")
                    ""))
            (else ""))))
"                    break;
")))))
            ($
"        if (services != null)
        {
            switch (version)
            {
" cases
"            }
        }
"))))

; Generate SQL for a single newsearch action for a specific db type
(define (upgrade-newsearch-sql search db)
    (let* ((resource (parent (parent search)))
           (rn (name-of resource))
           (schema rn)
           (stype (% "type" search))
           (tn (search-table-name search))
           (tbl (migration-table schema tn db))
           (res-tbl (db-public-table rn db)))
        (cond
            ; Inline search (token on simple code, instant, or number on integer/decimal):
            ; add column to main (and history) table, not a subtable.
            ((search-is-inline? search)
             (let* ((prop (search-property search))
                    (prop-type (if prop (% "type" prop) "code"))
                    (col-type (db-inline-type prop-type db))
                    (col-raw (search-col search))
                    (col-name (db-quote-col col-raw db))
                    (has-hist (history? resource))
                    (hist-tbl (migration-table schema "resource_history" db)))
                 ($
                    "ALTER TABLE " res-tbl " " (db-add-column db) col-name " " col-type ";
"
                    (if has-hist
                        ($ "ALTER TABLE " hist-tbl " " (db-add-column db) col-name " " col-type ";
")
                        "")
                    (db-create-index db) "ix_" schema "_" col-raw " ON " res-tbl " (" col-name ");
")))
            ; Non-inline searches get their table from the CreateSearchAsync call in
            ; this upgrade's runtime step, which creates it AND fills it from the stored
            ; resources. Emitting the DDL here as well made the upgrade issue CREATE TABLE
            ; twice: silently ignored on PostgreSQL and SQLite (IF NOT EXISTS), a hard
            ; failure on SQL Server, where the upgrade aborted and the version stayed put.
            (else ""))))

; Generate SQL for a newresource action for a specific db type
(define (upgrade-newresource-sql resource db)
    (let* ((rn (name-of resource))
           (schema rn)
           (inlines (inline-searches resource))
           (has-history (history? resource)))
        ($
            (migration-header rn schema db)
            (migration-resource-table rn schema inlines has-history db)
            (if has-history (migration-history-table rn schema inlines db) "")
            (if (and (string=? "Binary" rn) (equal? db 'PostgreSql))
                (migration-binary-data-table rn schema db) "")
            (migration-string-tables rn schema resource db)
            (migration-token-tables rn schema resource db)
            (migration-date-tables rn schema resource db)
            (migration-instant-tables rn schema resource db)
            (migration-reference-tables rn schema resource db))))

; Generate SQL for an addhistory action: create resource_history table and indexes
; for a resource that previously had history="false" and is now being upgraded to
; have history enabled.
(define (upgrade-addhistory-sql resource db)
    (let* ((rn (name-of resource))
           (schema rn)
           (inlines (inline-searches resource)))
        (migration-history-table rn schema inlines db)))

; Generate SQL for a dropsearch action: drop the per-search subtable.
; The search element is no longer in the model at this point, so the action
; carries the resource and search name itself (instead of an IDREF). Only the
; subtable form is supported; inline searches (column on main table) are not
; covered here yet — extend if needed.
(define (upgrade-dropsearch-sql action db)
    (let* ((schema (% "resource" action))
           (sn (% "name" action))
           (tn ($ "search_" (string-replace (downcase-string sn) "-" "_")))
           (tbl (migration-table schema tn db)))
        ($ "DROP TABLE IF EXISTS " tbl ";
")))

; Generate SQL for a dropresource action: remove all storage for a resource
; that has been removed from the model. The resource element is no longer
; available, so the action carries just the resource name.
;   PostgreSQL: DROP TABLE ... CASCADE drops the resource table in public
;               (and any FK constraints referring to it); DROP SCHEMA ... CASCADE
;               drops every per-resource table (history, search_*) and index.
;   SqlServer:  drop the resource table in dbo (CASCADE-equivalent via DROP …
;               CONSTRAINT first is implicit via per-table drop), enumerate
;               sys.tables in the resource schema, drop each, then drop schema.
;   Sqlite:     manual cleanup required (no schemas, no dynamic SQL).
(define (upgrade-dropresource-sql action db)
    (let ((schema (% "resource" action)))
        (case db
            ((PostgreSql)
                ($ "DROP TABLE IF EXISTS \"" schema "\" CASCADE;
DROP SCHEMA IF EXISTS \"" schema "\" CASCADE;
"))
            ((SqlServer) ($
"DECLARE @sql NVARCHAR(MAX) = N'';
SELECT @sql = @sql + N'DROP TABLE [" schema "].[' + name + N'];' FROM sys.tables WHERE schema_id = SCHEMA_ID('" schema "');
EXEC sp_executesql @sql;
IF EXISTS (SELECT * FROM sys.schemas WHERE name = '" schema "') EXEC('DROP SCHEMA [" schema "]');
DROP TABLE IF EXISTS [" schema "];
"))
            ((Sqlite)
                ; SQLite has no schemas and no dynamic SQL execution, so we
                ; cannot enumerate-and-drop in a single SQL string. Test/dev
                ; databases are typically discarded between runs; if you need
                ; to scrub a long-lived SQLite DB, do it manually.
                ($ "-- dropresource " schema ": SQLite manual cleanup required
")))))

; Generate SQL for all actions in a single upgrade step, for a specific db type
(define (upgrade-step-sql upgrade db)
    (for-children-of upgrade (lambda (action)
        (case (gi action)
            (("newsearch") (upgrade-newsearch-sql (ref action) db))
            (("newresource") (upgrade-newresource-sql (ref action) db))
            (("addhistory") (upgrade-addhistory-sql (ref action) db))
            (("dropsearch") (upgrade-dropsearch-sql action db))
            (("dropresource") (upgrade-dropresource-sql action db))
            (("applybundle") "") ; data-only: applied at runtime via ApplyUpgradeBundle, no DDL
            (("datamigration") "") ; data-only: applied at runtime by the named class, no DDL
            (else "")))))

; Generate the UpgradeSql method for a DatabaseProvider (per DB)
(define (upgrade-sql-method db)
    (if (not (has-upgrades?))
"    public string UpgradeSql(string version) =>
        throw new InvalidDataException($\"Upgrade to version {version} is not supported.\");
"
        ($
"    public string UpgradeSql(string version) => version switch
    {
"   (for (upgrade-list) (lambda (upgrade)
        (let ((sql (upgrade-step-sql upgrade db)))
            (if (string=? "" sql)
                ; Data-only upgrade (e.g. applybundle): no DDL. Emit a plain empty
                ; string — an empty raw-string literal is not valid C#, and the
                ; runtime guards against executing blank SQL.
                ($"        \""(% "to" upgrade)"\" => \"\",
")
                ($
"        \""(% "to" upgrade)"\" =>
\"\"\"
" sql "\"\"\",
")))))
"        _ => throw new InvalidDataException($\"Upgrade to version {version} is not supported.\")
    };
")))
