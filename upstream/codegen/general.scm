(define (csharp-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") "string")
    (("boolean") "bool")
    (("integer" "positiveInt" "unsignedInt") "int")
    (("integer64") "long")
    (("decimal") "decimal")
    (("date" "dateTime" "instant") "DateTime")
    (("time") "TimeSpan")
    (("base64Binary") "byte[]")
    (else fhir-type)))

(define (postgres-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") "TEXT")
    (("boolean") "BOOLEAN")
    (("integer" "positiveInt" "unsignedInt") "INTEGER")
    (("integer64") "BIGINT")
    (("decimal") "DECIMAL")
    (("date") "DATE")
    (("dateTime" "instant") "TIMESTAMPTZ")
    (("time") "TIME")
    (("base64Binary") "BYTEA")
    (else "JSONB")))

; ===================================================================
; Multi-database helpers
; ===================================================================

(define (sqlserver-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") "NVARCHAR(MAX)")
    (("boolean") "BIT")
    (("integer" "positiveInt" "unsignedInt") "INT")
    (("integer64") "BIGINT")
    (("decimal") "DECIMAL(18,6)")
    (("date") "DATE")
    (("dateTime" "instant") "DATETIME2")
    (("time") "TIME")
    (("base64Binary") "VARBINARY(MAX)")
    (else "NVARCHAR(MAX)")))

(define (sqlite-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") "TEXT")
    (("boolean") "INTEGER")
    (("integer" "positiveInt" "unsignedInt") "INTEGER")
    (("integer64") "INTEGER")
    (("decimal") "REAL")
    (("date" "dateTime" "instant") "TEXT")
    (("time") "TEXT")
    (("base64Binary") "BLOB")
    (else "TEXT")))

(define (db-type fhir-type db)
  (case db
    ((PostgreSql) (postgres-type fhir-type))
    ((SqlServer) (sqlserver-type fhir-type))
    ((Sqlite) (sqlite-type fhir-type))))

; String/code-ish FHIR types map to NVARCHAR(MAX) on SqlServer, which cannot be
; an index key. Inline search columns ARE indexed, so they need a bounded type.
(define (sqlserver-stringish-type? fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") #t)
    (else #f)))

; Column type for inline (indexed) search columns: like db-type, but on
; SqlServer string/code types are bounded to NVARCHAR(450) so they can be
; indexed (matches db-search-text-type used by the separate search tables).
(define (db-inline-type fhir-type db)
  (case db
    ((SqlServer) (if (sqlserver-stringish-type? fhir-type) "NVARCHAR(450)" (sqlserver-type fhir-type)))
    (else (db-type fhir-type db))))

(define (db-now db)
  (case db
    ((PostgreSql) "NOW()")
    ((SqlServer) "SYSUTCDATETIME()")
    ((Sqlite) "(datetime('now'))")))

(define (db-content-type db)
  (case db
    ((PostgreSql) "BYTEA")
    ((SqlServer) "VARBINARY(MAX)")
    ((Sqlite) "BLOB")))

(define (db-timestamp-type db)
  (case db
    ((PostgreSql) "TIMESTAMPTZ")
    ((SqlServer) "DATETIME2")
    ((Sqlite) "TEXT")))

;; (quoted on SQLite too: a resource may be named by a word of SQL -- Group)
(define (db-public-table rn db)
  (case db
    ((PostgreSql Sqlite) ($ "\"" rn "\""))
    ((SqlServer) ($ "[" rn "]"))))

(define (db-schema-prefix schema db)
  (case db
    ((PostgreSql) ($ "\"" schema "\"."))
    ((SqlServer) ($ "[" schema "]."))
    ((Sqlite) ($ schema "_"))))

(define (db-schema-create schema db)
  (case db
    ((PostgreSql) ($"CREATE SCHEMA IF NOT EXISTS \"" schema "\";
"))
    ((SqlServer) ($"IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = '" schema "') EXEC('CREATE SCHEMA [" schema "]');
"))
    ((Sqlite) "")))

;; Quote a column name for raw SQL (DDL, migration scripts)
(define (db-quote-col col db)
  (case db
    ((PostgreSql) ($ "\"" col "\""))
    ((SqlServer) ($ "[" col "]"))
    ((Sqlite) col)))

;; Quote a column name inside a C# string literal (needs escaped quotes)
(define (db-quote-col-cs col db)
  (case db
    ((PostgreSql) ($ "\\\"" col "\\\""))
    ((SqlServer) ($ "[" col "]"))
    ((Sqlite) col)))

(define (db-id-type db)
  (case db
    ((PostgreSql SqlServer) "VARCHAR(64)")
    ((Sqlite) "TEXT")))

(define (db-create-table db)
  (case db
    ((PostgreSql Sqlite) "CREATE TABLE IF NOT EXISTS ")
    ((SqlServer) "CREATE TABLE ")))

(define (db-union-open db)
  (case db
    ((PostgreSql SqlServer) "(")
    ((Sqlite) "")))

(define (db-union-close db)
  (case db
    ((PostgreSql SqlServer) ")")
    ((Sqlite) "")))

(define (db-create-index db)
  (case db
    ((PostgreSql Sqlite) "CREATE INDEX IF NOT EXISTS ")
    ((SqlServer) "CREATE INDEX ")))

;; The add-column clause of an ALTER TABLE. T-SQL has no COLUMN keyword there.
(define (db-add-column db)
  (case db
    ((PostgreSql Sqlite) "ADD COLUMN ")
    ((SqlServer) "ADD ")))

(define (db-search-text-type db)
  (case db
    ((PostgreSql Sqlite) "TEXT")
    ((SqlServer) "NVARCHAR(450)")))

(define (db-search-display-type db)
  (case db
    ((PostgreSql Sqlite) "TEXT")
    ((SqlServer) "NVARCHAR(MAX)")))

(define (db-using db)
  (case db
    ((PostgreSql) "Npgsql")
    ((SqlServer) "Microsoft.Data.SqlClient")
    ((Sqlite) "Microsoft.Data.Sqlite")))

(define (db-command-type db)
  (case db
    ((PostgreSql) "NpgsqlCommand")
    ((SqlServer) "SqlCommand")
    ((Sqlite) "SqliteCommand")))

(define (db-folder db)
  (case db
    ((PostgreSql) "postgresql")
    ((SqlServer) "sqlserver")
    ((Sqlite) "sqlite")))

(define (db-namespace db)
  (case db
    ((PostgreSql) "PostgreSql")
    ((SqlServer) "SqlServer")
    ((Sqlite) "Sqlite")))

(define (search-humanname-field search)
  (let* ((paths-node (select-children "paths" search))
         (first-paths (if (node-list-empty? paths-node)
                          #f
                          (node-list-first paths-node)))
         (path-nodes (if first-paths
                          (select-children "path" first-paths)
                          (empty-node-list)))
         (first-path (if (node-list-empty? path-nodes)
                         #f
                         (node-list-first path-nodes))))
    (if first-path
        (let* ((parts-node (select-children "parts" first-path))
               (first-parts (if (node-list-empty? parts-node)
                                #f
                                (node-list-first parts-node)))
               (part-nodes (if first-parts
                                (select-children "part" first-parts)
                                (empty-node-list)))
               (part-list (node-list->list part-nodes))
               (part-count (length part-list)))
          (cond
            ((and (= part-count 1)
                  (let ((ref (% "ref" (car part-list))))
                    (or (string-ci=? ref "patient.name")
                        (string-ci=? ref "practitioner.name")
                        (string-ci=? ref "relatedperson.name")
                        (string-ci=? ref "person.name"))))
             "all")
            ((= part-count 2)
             (let ((ref2 (% "ref" (cadr part-list))))
               (cond
                 ((string-ci=? ref2 "humanname.family") "family")
                 ((string-ci=? ref2 "humanname.given") "given")
                 ((string-ci=? ref2 "humanname.prefix") "prefix")
                 ((string-ci=? ref2 "humanname.suffix") "suffix")
                 ((string-ci=? ref2 "humanname.text") "text")
                 (else #f))))
            (else #f)))
        #f)))
