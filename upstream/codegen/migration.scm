; Migration generator - generates sql/{db}/001_initial_schema.sql
; Called from (element resources ...) context
; Generates schema-per-resource with separate search index tables

; Check if a search parameter should be stored inline in the resource table
; Inline: (1) token type, simple code (code/boolean), NOT collection
;         (2) instant type property, NOT collection, NOT _lastUpdated (built-in column)
;         (3) number type over an integer/decimal property, NOT collection
(define (search-is-inline? search)
    (and (not (search-is-collection? search))
         (or (and (string=? "token" (% "type" search))
                  (search-is-simple-code? search))
             (search-is-inline-instant? search)
             (and (string=? "number" (% "type" search))
                  (search-is-inline-number? search)))))

; Check if a search param targets an instant-type property that should be inlined
; Excludes _lastUpdated which maps to the built-in last_updated column
(define (search-is-inline-instant? search)
    (and (not (string=? "_lastUpdated" (% "name" search)))
         (let ((property (search-property search)))
           (if property
               (let ((prop-type (% "type" property)))
                 (if prop-type
                     (string=? "instant" prop-type)
                     #f))
               #f))))

; Check if a number search targets a numeric scalar property suitable for inlining
(define (search-is-inline-number? search)
    (let ((property (search-property search)))
      (if property
          (case (% "type" property)
            (("integer" "positiveInt" "unsignedInt" "integer64" "decimal") #t)
            (else #f))
          #f)))

; Check if this is the _lastUpdated search (built-in last_updated column)
(define (search-is-last-updated? search)
    (string=? "_lastUpdated" (% "name" search)))

; Get the SQL table name for a search parameter
(define (search-table-name search)
    ($ "search_" (string-replace (downcase-string (% "name" search)) "-" "_")))

; Helper: collect inline searches for a resource
(define (inline-searches resource)
    (let ((searches (select-children "searches" resource)))
        (if (node-list-empty? searches)
            (empty-node-list)
            (node-list-filter search-is-inline? (children (node-list-first searches))))))

; Helper: collect non-inline, non-composite, non-special searches
(define (table-searches resource)
    (let ((searches (select-children "searches" resource)))
        (if (node-list-empty? searches)
            (empty-node-list)
            (node-list-filter
                (lambda (s) (and (not (search-is-inline? s))
                                 (not (search-is-last-updated? s))
                                 (not (string=? "composite" (% "type" s)))
                                 (not (string=? "special" (% "type" s)))))
                (children (node-list-first searches))))))

; Helper: collect searches by type
(define (string-table-searches resource)
    (node-list-filter (lambda (s) (string=? "string" (% "type" s))) (table-searches resource)))

(define (token-table-searches resource)
    (node-list-filter (lambda (s) (and (string=? "token" (% "type" s)) (not (search-is-inline? s)))) (table-searches resource)))

(define (date-table-searches resource)
    (node-list-filter (lambda (s) (and (string=? "date" (% "type" s)) (not (search-is-inline-instant? s)))) (table-searches resource)))

(define (instant-table-searches resource)
    (node-list-filter search-is-inline-instant? (table-searches resource)))

(define (reference-table-searches resource)
    (node-list-filter (lambda (s) (string=? "reference" (% "type" s))) (table-searches resource)))

(define (number-table-searches resource)
    (node-list-filter (lambda (s) (and (string=? "number" (% "type" s)) (not (search-is-inline? s)))) (table-searches resource)))

(define (quantity-table-searches resource)
    (node-list-filter (lambda (s) (string=? "quantity" (% "type" s))) (table-searches resource)))

; Table name with schema prefix: "patient.resource" (PG/MSSQL) or "patient_resource" (SQLite)
(define (migration-table schema table db)
    ($ (db-schema-prefix schema db) table))

; --- Schema SQL generator (used by Database.cs inline) ---

(define (schema-sql db)
    ($
        (if (equal? db 'Sqlite) "PRAGMA foreign_keys = ON;

" "")
        (for-active-resources (lambda (resource)
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
                    (migration-reference-tables rn schema resource db)
                    (migration-number-tables rn schema resource db)
                    (migration-quantity-tables rn schema resource db)
                ))))))

; --- Header ---

(define (migration-header rn schema db)
    ($"-- " (project-name) " FHIR Server - " rn " Schema
-- Resource table in public, history/search tables in per-resource schema

" (db-schema-create schema db)
"
"))

; --- Resource table ---

(define (migration-resource-table rn schema inlines has-history db)
    (let ((inline-list (node-list->list inlines))
          (tbl (db-public-table rn db)))
    ($"-- Main resource table
" (db-create-table db) tbl " (
    id " (db-id-type db) " PRIMARY KEY,"
    "
    version_id INTEGER NOT NULL DEFAULT 1,"
    "
    last_updated " (db-timestamp-type db) " NOT NULL DEFAULT " (db-now db) ",
    content " (db-content-type db) " NOT NULL"
    (if (null? inline-list) "" ($",
    -- Inline: non-collection, non-variant, non-object, non-date
" (let loop ((items inline-list) (first? #t))
    (if (null? items) ""
        (let* ((s (car items))
               (prop (search-property s))
               (prop-type (if prop (% "type" prop) "code"))
               (col-type (db-inline-type prop-type db))
               (col-name (db-quote-col (search-col s) db))
               (rest (cdr items)))
            ($ (if first? "" ",
")
               "    " col-name " " col-type
               (loop rest #f)))))))
"
);

" (db-create-index db) "ix_" schema "_last_updated ON " tbl " (last_updated);
" (let loop ((items inline-list))
    (if (null? items) ""
        (let* ((s (car items))
               (col (search-col s)))
            ($"" (db-create-index db) "ix_" schema "_" col " ON " tbl " (" (db-quote-col col db) ");
" (loop (cdr items))))))
"
")))

; --- History table ---

(define (migration-history-table rn schema inlines db)
    (let ((inline-list (node-list->list inlines))
          (tbl (migration-table schema "resource_history" db)))
    ($"-- History table
" (db-create-table db) tbl " (
    id " (db-id-type db) " NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated " (db-timestamp-type db) " NOT NULL,
    content " (db-content-type db) " NOT NULL,
" (let loop ((items inline-list))
    (if (null? items) ""
        (let* ((s (car items))
               (prop (search-property s))
               (prop-type (if prop (% "type" prop) "code"))
               (col-type (db-inline-type prop-type db))
               (col-name (db-quote-col (search-col s) db)))
            ($"    " col-name " " col-type ",
" (loop (cdr items))))))
"    history_operation VARCHAR(10) NOT NULL,
    history_timestamp " (db-timestamp-type db) " NOT NULL DEFAULT " (db-now db) ",
    PRIMARY KEY (id, version_id)
);

" (db-create-index db) "ix_" schema "_resource_history_id ON " tbl " (id);
" (db-create-index db) "ix_" schema "_resource_history_timestamp ON " tbl " (history_timestamp);
" (db-create-index db) "ix_" schema "_resource_history_last_updated ON " tbl " (last_updated);

")))

; --- Binary resource_data table (PostgreSql only) ---

(define (migration-binary-data-table rn schema db)
    (let ((tbl (migration-table schema "resource_data" db))
          (res-tbl (db-public-table rn db)))
    ($"-- Binary resource_data table (raw BYTEA storage)
" (db-create-table db) tbl " (
    resource_id VARCHAR(64) NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    version_id INTEGER NOT NULL,
    data BYTEA NOT NULL,
    PRIMARY KEY (resource_id, version_id)
);

")))

; --- String search tables ---

(define (migration-string-tables rn schema resource db)
    (let ((searches (node-list->list (string-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- String search tables (value TEXT)
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    value " (db-search-text-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value ON " tbl " (value);

" (loop (cdr items))))))))))

; --- Token search tables ---

(define (migration-token-tables rn schema resource db)
    (let ((searches (node-list->list (token-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Token search tables (system TEXT, value TEXT, display TEXT)
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((s (car items))
               (tn (search-table-name s))
               (sn (% "name" s))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    system " (db-search-text-type db) ",
    value " (db-search-text-type db) ",
    display " (db-search-display-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value ON " tbl " (value);
" (if (or (string=? "identifier" sn) (string=? "language" sn))
    ($"" (db-create-index db) "ix_" schema "_" tn "_system ON " tbl " (system);
")
    "")
"
" (loop (cdr items))))))))))

; --- Date search tables ---

(define (migration-date-tables rn schema resource db)
    (let ((searches (node-list->list (date-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Date search tables
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    value_low " (db-timestamp-type db) ",
    value_high " (db-timestamp-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value_low ON " tbl " (value_low);
" (db-create-index db) "ix_" schema "_" tn "_value_high ON " tbl " (value_high);

" (loop (cdr items))))))))))

; --- Instant search tables ---

(define (migration-instant-tables rn schema resource db)
    (let ((searches (node-list->list (instant-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Instant search tables
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    value " (db-timestamp-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value ON " tbl " (value);

" (loop (cdr items))))))))))

; --- Reference search tables ---

(define (migration-reference-tables rn schema resource db)
    (let ((searches (node-list->list (reference-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Reference search tables (target_type TEXT, target_id TEXT, target_version TEXT)
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    target_type " (db-search-text-type db) ",
    target_id " (db-search-text-type db) ",
    target_version " (db-search-display-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_reference ON " tbl " (target_id, target_type);

" (loop (cdr items))))))))))

; --- Number search tables ---

(define (migration-number-tables rn schema resource db)
    (let ((searches (node-list->list (number-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Number search tables (value DECIMAL)
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    value " (db-type "decimal" db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value ON " tbl " (value);

" (loop (cdr items))))))))))

; --- Quantity search tables ---

(define (migration-quantity-tables rn schema resource db)
    (let ((searches (node-list->list (quantity-table-searches resource))))
        (if (null? searches) ""
            ($"-- =============================================================================
-- Quantity search tables (value DECIMAL, system, code, unit)
-- =============================================================================

" (let loop ((items searches))
    (if (null? items) ""
        (let* ((tn (search-table-name (car items)))
               (tbl (migration-table schema tn db))
               (res-tbl (db-public-table rn db)))
            ($"" (db-create-table db) tbl " (
    id INT NOT NULL,
    resource_id " (db-id-type db) " NOT NULL REFERENCES " res-tbl "(id) ON DELETE CASCADE,
    value " (db-type "decimal" db) ",
    system " (db-search-text-type db) ",
    code " (db-search-text-type db) ",
    unit " (db-search-text-type db) ",
    PRIMARY KEY (id, resource_id)
);
" (db-create-index db) "ix_" schema "_" tn "_value ON " tbl " (value);

" (loop (cdr items))))))))))

; --- Index management SQL ---

; Helper to get DROP INDEX prefix with schema for a given db
(define (drop-index-prefix schema db)
    (case db
        ((PostgreSql) ($"DROP INDEX IF EXISTS \"" schema "\"."))
        ((SqlServer) ($"DROP INDEX IF EXISTS [" schema "]."))
        ((Sqlite) ($"DROP INDEX IF EXISTS " schema "_"))))

; Drop a public-table index
(define (drop-public-ix ix-name rn db)
    (case db
        ((PostgreSql Sqlite) ($"DROP INDEX IF EXISTS " ix-name ";
"))
        ((SqlServer) ($"DROP INDEX IF EXISTS " ix-name " ON " (db-public-table rn db) ";
"))))

; Drop a schema-table index
(define (drop-schema-ix ix-name schema table db)
    (case db
        ((PostgreSql) ($"DROP INDEX IF EXISTS \"" schema "\"." ix-name ";
"))
        ((Sqlite) ($"DROP INDEX IF EXISTS " ix-name ";
"))
        ((SqlServer) ($"DROP INDEX IF EXISTS " ix-name " ON [" schema "].[" table "];
"))))

; Generate CREATE INDEX SQL for all resources
(define (create-indexes-sql db)
    (let ((ci (db-create-index db)))
    (for-active-resources (lambda (resource)
        (let* ((rn (name-of resource))
               (schema rn)
               (pub (db-public-table rn db)))
        ($
            ci "ix_" schema "_last_updated ON " pub " (last_updated);
"           (for (inline-searches resource) (lambda (s)
                ($"" ci "ix_" schema "_" (search-col s) " ON " pub " (" (db-quote-col (search-col s) db) ");
")))
            (if (history? resource)
                (let ((htbl (migration-table schema "resource_history" db)))
                ($"" ci "ix_" schema "_resource_history_id ON " htbl " (id);
" ci "ix_" schema "_resource_history_timestamp ON " htbl " (history_timestamp);
" ci "ix_" schema "_resource_history_last_updated ON " htbl " (last_updated);
"))
                "")
            (for (string-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                ($"" ci "ix_" schema "_" tn "_value ON " (migration-table schema tn db) " (value);
"))))
            (for (token-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)) (sn (% "name" s)))
                ($"" ci "ix_" schema "_" tn "_value ON " (migration-table schema tn db) " (value);
" (if (or (string=? "identifier" sn) (string=? "language" sn))
    ($"" ci "ix_" schema "_" tn "_system ON " (migration-table schema tn db) " (system);
") "")))))
            (for (date-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                ($"" ci "ix_" schema "_" tn "_value_low ON " (migration-table schema tn db) " (value_low);
" ci "ix_" schema "_" tn "_value_high ON " (migration-table schema tn db) " (value_high);
"))))
            (for (instant-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                ($"" ci "ix_" schema "_" tn "_value ON " (migration-table schema tn db) " (value);
"))))
            (for (reference-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                ($"" ci "ix_" schema "_" tn "_reference ON " (migration-table schema tn db) " (target_id, target_type);
"))))
        ))))))

; Generate DROP INDEX SQL for all resources
(define (drop-indexes-sql db)
    (for-active-resources (lambda (resource)
        (let* ((rn (name-of resource))
               (schema rn))
        ($
            (drop-public-ix ($"ix_" schema "_last_updated") rn db)
            (for (inline-searches resource) (lambda (s)
                (drop-public-ix ($"ix_" schema "_" (search-col s)) rn db)))
            (if (history? resource)
                ($  (drop-schema-ix ($"ix_" schema "_resource_history_id") schema "resource_history" db)
                    (drop-schema-ix ($"ix_" schema "_resource_history_timestamp") schema "resource_history" db)
                    (drop-schema-ix ($"ix_" schema "_resource_history_last_updated") schema "resource_history" db))
                "")
            (for (string-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                (drop-schema-ix ($"ix_" schema "_" tn "_value") schema tn db))))
            (for (token-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)) (sn (% "name" s)))
                ($  (drop-schema-ix ($"ix_" schema "_" tn "_value") schema tn db)
                    (if (or (string=? "identifier" sn) (string=? "language" sn))
                        (drop-schema-ix ($"ix_" schema "_" tn "_system") schema tn db) "")))))
            (for (date-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                ($  (drop-schema-ix ($"ix_" schema "_" tn "_value_low") schema tn db)
                    (drop-schema-ix ($"ix_" schema "_" tn "_value_high") schema tn db)))))
            (for (instant-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                (drop-schema-ix ($"ix_" schema "_" tn "_value") schema tn db))))
            (for (reference-table-searches resource) (lambda (s)
                (let ((tn (search-table-name s)))
                (drop-schema-ix ($"ix_" schema "_" tn "_reference") schema tn db))))
        )))))


; --- Per-type reseed: DELETE every row of one resource type ---
; Children first (search tables, Binary resource_data, history), then the
; public table. Plain DELETE (not TRUNCATE) so it works on all three DBs and
; does not depend on FK/CASCADE state (SQLite enforces FKs only per-connection).
(define (reseed-delete-sql-for-resource resource db)
    (let* ((rn (name-of resource))
           (schema rn)
           (del (lambda (tbl) ($"DELETE FROM " tbl ";
")))
           (del-search (lambda (s) (del (migration-table schema (search-table-name s) db)))))
        ($
            (for (string-table-searches resource) del-search)
            (for (token-table-searches resource) del-search)
            (for (date-table-searches resource) del-search)
            (for (instant-table-searches resource) del-search)
            (for (reference-table-searches resource) del-search)
            (for (number-table-searches resource) del-search)
            (for (quantity-table-searches resource) del-search)
            (if (binary-data-table? rn db) (del (migration-table schema "resource_data" db)) "")
            (if (history? resource) (del (migration-table schema "resource_history" db)) "")
            (del (db-public-table rn db)))))


