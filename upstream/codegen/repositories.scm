; Repositories generator - generates Fire/Repositories/{Resource}Repository.cs
; Called from (element resource ...) context
; Uses helpers from models.scm (filter, kebab-to-pascal, search-path-parts, etc.)
; Uses helpers from migration.scm (search-table-name, inline-searches, table-searches)

; ===================================================================
; DB-abstraction helpers
; ===================================================================

; Binary stores its raw bytes in a separate resource_data table only on
; PostgreSql. On Sqlite/SqlServer the data stays inline in content, so the
; resource_data join/column/WithData-reader must be suppressed there.
(define (binary-data-table? rn db)
    (and (string=? rn "Binary") (case db ((PostgreSql) #t) (else #f))))

; Parameter placeholder: $1 or @p1
(define (pp n db)
    (case db
        ((PostgreSql) ($ "$"(number->string n)))
        ((SqlServer Sqlite) ($ "@p"(number->string n)))))

; Parameter prefix for C# interpolated strings: $ or @p
(define (pp-prefix db)
    (case db ((PostgreSql)"$") ((SqlServer Sqlite)"@p")))

; Table name: schema.table or schema_table (with quoting escaped for C# strings)
(define (tbl schema table db)
    (case db
        ((PostgreSql) ($ "\\\"" schema "\\\"." table))
        ((SqlServer) ($ "[" schema "]." table))
        ((Sqlite) ($ schema "_" table))))

; Public table name: quoted resource name (escaped for C# strings)
(define (tbl-public rn db)
    (case db
        ((PostgreSql) ($ "\\\""rn"\\\""))
        ((SqlServer) ($ "["rn"]"))
        ((Sqlite) rn)))

; Table name for C# interpolated strings: schema.{var} or schema_{var}
(define (tbl-interp schema var-name db)
    (case db
        ((PostgreSql) ($ "\\\"" schema "\\\".{" var-name "}"))
        ((SqlServer) ($ "[" schema "].{" var-name "}"))
        ((Sqlite) ($ schema "_{" var-name "}"))))

; SELECT lock suffix
(define (select-lock db)
    (case db ((PostgreSql)" FOR UPDATE") ((SqlServer Sqlite)"")))

; Connection opening
(define (open-conn db)
    (case db
        ((PostgreSql)"        await using var conn = await _dataSource.OpenConnectionAsync();")
        ((SqlServer)"        await using var conn = new SqlConnection(_connectionString);
        await conn.OpenAsync();")
        ((Sqlite)"        await using var conn = new SqliteConnection(_connectionString);
        await conn.OpenAsync();")))

; New command: new XxxCommand(
(define (new-cmd db)
    ($ "new "(db-command-type db)"("))

; Param type name
(define (param-type db)
    (case db
        ((PostgreSql)"NpgsqlParameter")
        ((SqlServer)"SqlParameter")
        ((Sqlite)"SqliteParameter")))

; New param: new NpgsqlParameter { Value = xxx }
(define (new-param value-expr db)
    ($ "new "(param-type db)" { Value = " value-expr " }"))

; New timestamp param
(define (new-ts-param value-expr db)
    (case db
        ((PostgreSql) ($ "new NpgsqlParameter { Value = " value-expr ", NpgsqlDbType = NpgsqlDbType.TimestampTz }"))
        ((SqlServer) ($ "new SqlParameter { Value = " value-expr ", SqlDbType = SqlDbType.DateTimeOffset }"))
        ((Sqlite) ($ "new SqliteParameter { Value = " value-expr " }"))))

; Param type copy in search (for reusing params in count vs select query)
(define (param-copy-typed db)
    (case db
        ((PostgreSql) ($"
                if (parameters[i].NpgsqlDbType != NpgsqlDbType.Unknown)
                    p.NpgsqlDbType = parameters[i].NpgsqlDbType;"))
        ((SqlServer Sqlite)"")))

; AddWithValue call - positional for PG, named for others
(define (awv n expr db)
    (case db
        ((PostgreSql) ($"
            cmd.Parameters.AddWithValue(" expr ");"))
        ((SqlServer Sqlite) ($"
            cmd.Parameters.AddWithValue(\""(pp n db)"\", " expr ");"))))

; Reader type name
(define (reader-type db)
    (case db
        ((PostgreSql)"NpgsqlDataReader")
        ((SqlServer)"SqlDataReader")
        ((Sqlite)"SqliteDataReader")))

; Connection + transaction opening. Inside a DbTransactionScope (transaction bundle) the method
; uses the connection and transaction of the scope and owns neither; every commit and rollback in
; such a method is guarded with txScope == null, the scope commits or rolls back once at the end.
(define (open-conn-tx db)
    (case db
        ((PostgreSql)"        var txScope = DbTransactionScope.Current;
        await using var ownConn = txScope == null ? await _dataSource.OpenConnectionAsync() : null;
        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        await using var ownTx = txScope == null ? await conn.BeginTransactionAsync() : null;
        var tx = txScope == null ? ownTx! : (TxType)txScope.Transaction;")
        ((SqlServer)"        var txScope = DbTransactionScope.Current;
        await using var ownConn = txScope == null ? new SqlConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        await using var ownTx = txScope == null ? (TxType)await conn.BeginTransactionAsync() : null;
        var tx = txScope == null ? ownTx! : (TxType)txScope.Transaction;")
        ((Sqlite)"        var txScope = DbTransactionScope.Current;
        await using var ownConn = txScope == null ? new SqliteConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        using var ownTx = txScope == null ? conn.BeginTransaction() : null;
        var tx = txScope == null ? ownTx! : (TxType)txScope.Transaction;")))

; Connection opening for the reads a transaction bundle can reach (read, version read, search,
; delete without history). Inside a DbTransactionScope they run on its connection and within its
; transaction, so they see the writes of earlier bundle entries; SqlClient and Microsoft.Data.Sqlite
; also reject a command without the pending transaction. Outside a scope tx is null.
(define (open-conn-scoped db)
    ($"        var txScope = DbTransactionScope.Current;
"
    (case db
        ((PostgreSql)"        await using var ownConn = txScope == null ? await _dataSource.OpenConnectionAsync() : null;
")
        ((SqlServer)"        await using var ownConn = txScope == null ? new SqlConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
")
        ((Sqlite)"        await using var ownConn = txScope == null ? new SqliteConnection(_connectionString) : null;
        if (ownConn != null) await ownConn.OpenAsync();
"))
"        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        var tx = (TxType?)txScope?.Transaction;"))

; Table-level lock hint (goes after table name in SQL Server)
(define (table-lock-hint db)
    (case db ((SqlServer)" WITH (UPDLOCK, ROWLOCK)") (else "")))

; Transaction type name
(define (db-transaction-type db)
    (case db
        ((PostgreSql)"NpgsqlTransaction")
        ((SqlServer)"SqlTransaction")
        ((Sqlite)"SqliteTransaction")))

; Single AddWithValue line (no leading newline), with configurable command name
(define (add-param-line-for cmd-name n expr db)
    (case db
        ((PostgreSql) ($"            " cmd-name ".Parameters.AddWithValue(" expr ");"))
        ((SqlServer Sqlite) ($"            " cmd-name ".Parameters.AddWithValue(\""(pp n db)"\", " expr ");"))))

(define (add-param-line n expr db) (add-param-line-for "cmd" n expr db))

; 8-space indent version for non-block contexts (Read, VRead, etc.)
(define (add-param-8-for cmd-name n expr db)
    (case db
        ((PostgreSql) ($"        " cmd-name ".Parameters.AddWithValue(" expr ");"))
        ((SqlServer Sqlite) ($"        " cmd-name ".Parameters.AddWithValue(\""(pp n db)"\", " expr ");"))))

(define (add-param-8 n expr db) (add-param-8-for "cmd" n expr db))


; Bind parameters from a runtime list of objects — positional for PG, named for others
(define (bind-args-list cmd-name args-name db)
    (case db
        ((PostgreSql)
            ($"            foreach (var __a in " args-name ") " cmd-name ".Parameters.AddWithValue(__a);"))
        ((SqlServer Sqlite)
            ($"            for (int __i = 0; __i < " args-name ".Count; __i++) " cmd-name ".Parameters.AddWithValue($\"@p{__i + 1}\", " args-name "[__i]);"))))

; Pagination: LIMIT/OFFSET vs OFFSET/FETCH
; count-expr and offset-expr are C# interpolation expressions like "count" or "offset"
(define (db-paginate count-expr offset-expr db)
    (case db
        ((PostgreSql Sqlite) ($ "LIMIT {" count-expr "} OFFSET {" offset-expr "}"))
        ((SqlServer) ($ "OFFSET {" offset-expr "} ROWS FETCH NEXT {" count-expr "} ROWS ONLY"))))

; Pagination for SQL params (e.g. $1, @p1)
(define (db-paginate-params count-param offset-param db)
    (case db
        ((PostgreSql Sqlite) ($ "LIMIT " count-param " OFFSET " offset-param))
        ((SqlServer) ($ "OFFSET " offset-param " ROWS FETCH NEXT " count-param " ROWS ONLY"))))

; Simple LIMIT 1 (used after ORDER BY)
(define (db-limit-1 db)
    (case db
        ((PostgreSql Sqlite)"LIMIT 1")
        ((SqlServer)"OFFSET 0 ROWS FETCH NEXT 1 ROWS ONLY")))

; SELECT [TOP 1] prefix for subqueries without ORDER BY
(define (db-select-top1 db)
    (case db
        ((PostgreSql Sqlite)"SELECT")
        ((SqlServer)"SELECT TOP 1")))

; Trailing LIMIT 1 for subqueries without ORDER BY (empty for SqlServer since TOP 1 is used)
(define (db-trailing-limit1 db)
    (case db
        ((PostgreSql Sqlite)" LIMIT 1")
        ((SqlServer)"")))

; ===================================================================
; Helper functions
; ===================================================================

; Column list: "id, version_id, last_updated, content, active, gender"
(define (repo-column-list inline-list has-history db)
    ($"id, version_id, last_updated, content"
        (apply $ (map (lambda (s) ($", "(db-quote-col-cs (search-col s) db))) inline-list))))

; Prefixed column list for SELECT: "r.id, r.version_id, r.last_updated, r.content, r.active, r.gender"
(define (repo-column-list-prefixed prefix inline-list has-history db)
    ($ prefix ".id, " prefix ".version_id, " prefix ".last_updated, " prefix ".content"
        (apply $ (map (lambda (s) ($", " prefix "."(db-quote-col-cs (search-col s) db))) inline-list))))

; Parameter placeholders: "$1, $2, $3, $4, $5, $6" or "@p1, @p2, ..."
(define (repo-param-list inline-list has-history db)
    (let ((pre (pp-prefix db)))
    ($ pre "1, " pre "2, " pre "3, " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", " pre (number->string idx) (loop (cdr items) (+ idx 1))))))))

; History INSERT column list: "id, version_id, last_updated, content, active, gender, history_operation"
(define (repo-history-column-list inline-list db)
    ($"id, version_id, last_updated, content"
        (apply $ (map (lambda (s) ($", "(db-quote-col-cs (search-col s) db))) inline-list))
        ", history_operation"))

; History INSERT params: "$1, $2, $3, $4, $5, $6, 'UPDATE'" or "@p1, @p2, ..."
(define (repo-history-param-list inline-list operation db)
    (let ((pre (pp-prefix db)))
    ($ pre "1, " pre "2, " pre "3, " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", " pre (number->string idx) (loop (cdr items) (+ idx 1)))))
        ", '" operation "'")))

; UPDATE SET clause: "version_id = $2, last_updated = $3, content = $4, ..."
(define (repo-update-set inline-list has-history db)
    (let ((pre (pp-prefix db)))
    ($"version_id = " pre "2, last_updated = " pre "3, content = " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", "(db-quote-col-cs (search-col (car items)) db)" = " pre (number->string idx) (loop (cdr items) (+ idx 1))))))))

; AddWithValue calls for inline params
(define (repo-addwithvalue inline-list var-prefix start-idx db)
    (let loop ((items inline-list) (idx start-idx))
        (if (null? items)""
            ($ (case db
                ((PostgreSql) ($"
            cmd.Parameters.AddWithValue((object?)" var-prefix (search-query-camel (car items))" ?? DBNull.Value);"))
                ((SqlServer Sqlite) ($"
            cmd.Parameters.AddWithValue(\""(pp idx db)"\", (object?)" var-prefix (search-query-camel (car items))" ?? DBNull.Value);")))
                (loop (cdr items) (+ idx 1))))))

; Reader column fields for inline params (starting at index 4)
(define (repo-reader-fields inline-list has-history)
    (let loop ((items inline-list) (idx 4))
        (if (null? items)""
            (let* ((s (car items))
                   (prop (search-property s))
                   (prop-type (if prop (% "type" prop)"code"))
                   (reader-method (case prop-type
                       (("boolean")"GetBoolean")
                       (("integer" "positiveInt" "unsignedInt")"GetInt32")
                       (("integer64")"GetInt64")
                       (("decimal")"GetDecimal")
                       (("instant" "dateTime")"GetDateTime")
                       (else "GetString")))
                   ; Same missing Kind as last_updated - see (repo-utc-helper). These values only
                   ; travel back into the column they came from today, but nothing says a caller
                   ; may not format one, and an unlabelled instant is a trap either way.
                   (open (if (string=? reader-method "GetDateTime") "Utc(" ""))
                   (close (if (string=? reader-method "GetDateTime") ")" "")))
                ($",
            reader.IsDBNull("(number->string idx)") ? null : " open "reader." reader-method "("(number->string idx)")" close
                    (loop (cdr items) (+ idx 1)))))))

; Constructor args for inline params: ", active, gender"
(define (repo-constructor-inline-args inline-list)
    (apply $ (map (lambda (s) ($", "(search-query-camel s))) inline-list)))

; Destructure tuple: "var (active, gender) = ..." or "var status = ..." for single
(define (repo-destructure-inline inline-list)
    (if (= 1 (length inline-list))
        ($"var "(search-query-camel (car inline-list)))
        ($"var ("
            (let loop ((items inline-list) (first? #t))
                (if (null? items)""
                    ($ (if first? "" ", ") (search-query-camel (car items)) (loop (cdr items) #f))))
            ")")))

; Get C# extractor call for a string search index
(define (repo-string-extractor-call rn search all-hn-all)
    (cond
        ; HumanName-all → shared method (prefer "name")
        ((and (search-is-humanname? search)
              (equal? "all"(search-humanname-field search)))
            (let* ((name-search (filter (lambda (s) (string=? "name"(% "name" s))) all-hn-all))
                   (primary (if (null? name-search) (car all-hn-all) (car name-search))))
                ($ rn "SearchExtractor.Extract"(kebab-to-pascal (% "name" primary))"(content)")))
        ; HumanName-field
        ((search-is-humanname? search)
            ($ rn "SearchExtractor.Extract"(kebab-to-pascal (% "name" search))"(content)"))
        ; Address-all
        ((and (search-is-address? search)
              (= 1 (length (search-path-parts search))))
            ($ rn "SearchExtractor.Extract"(kebab-to-pascal (% "name" search))"(content)"))
        ; Address-field → ExtractAddressField with field name
        ((search-is-address? search)
            (let* ((prop-last (search-property-last search))
                   (field-name (if prop-last (% "name" prop-last)"")))
                ($ rn "SearchExtractor.ExtractAddressField(content, \"" field-name "\")")))
        ; Address-through-backbone all → named method
        ((and (search-is-address-through-backbone? search)
              (search-last-is-address? search))
            ($ rn "SearchExtractor.Extract"(kebab-to-pascal (% "name" search))"(content)"))
        ; Address-through-backbone field → ExtractAddressFieldThroughBackbone with field name
        ((search-is-address-through-backbone? search)
            (let* ((prop-last (search-property-last search))
                   (field-name (if prop-last (% "name" prop-last)"")))
                ($ rn "SearchExtractor.ExtractAddressFieldThroughBackbone(content, \"" field-name "\")")))
        ; Other
        (else
            ($ rn "SearchExtractor.Extract"(kebab-to-pascal (% "name" search))"(content)"))))

; ===================================================================
; Interface generation
; ===================================================================

(define (repo-interface rn inline-list has-history)
    (file ($ "I"rn"Repository.cs")
        ($
"using System.Text.Json.Nodes;
using "(project-name)".Search;

namespace "(project-name)".Repositories;

public interface I"rn"Repository
{
"
"    Task<"rn"Resource> CreateAsync(JsonNode content, string? clientId = null);
    Task<"rn"Resource?> ReadAsync(string id);
"
    (if has-history
        ($
"    Task<"rn"Resource?> VersionReadAsync(string id, int versionId);
    Task<"rn"Resource?> UpdateAsync(string id, JsonNode content, int? ifMatchVersion = null);
    Task<"rn"Resource?> PatchAsync(string id, JsonArray patchOps, int? ifMatchVersion = null);
")
        ($
"    Task<"rn"Resource?> UpdateAsync(string id, JsonNode content);
    Task<"rn"Resource?> PatchAsync(string id, JsonArray patchOps);
"       )
    )
"    Task<""bool> DeleteAsync(string id);
"
    ; Only the provenance of a rolled back version gets removed without a trace, so only
    ; Provenance carries the method that does it.
    (if (string=? rn "Provenance") ($
"    Task<""bool> HardDeleteAsync(string id);
"   )"")
    (if has-history ($
"    Task<""bool> IsDeletedAsync(string id);
    Task<"rn"Resource?> RollbackAsync(string id, int currentVersionId);
    Task<"rn"Resource?> UndeleteAsync(string id);
    Task<""int> UndeleteAllAsync();
"   )"")
"    Task<(List<"rn"Resource> resources, int total)> SearchAsync(IQueryCollection queryParams, int count, int offset);
"
    (if has-history ($
"    Task<(List<"rn"HistoryRow> rows, int total)> InstanceHistoryAsync(string id, int count, int offset);
    Task<(List<"rn"HistoryRow> rows, int total)> TypeHistoryAsync(int count, int offset);
"   )"")
"    Task<""List<""string>> AutocompleteAsync(string param, string value, string property, int? count, int? offset);
"
"    Task CreateSearchAsync(string parameter);
    Task DropSearchAsync(string parameter);
    Task<""int> PurgeAsync(DateTime? before);
}
")))

; ===================================================================
; DB-specific class helpers (P, NewParam, NewTsParam, CopyParam)
; These allow search code to use abstract DbParameter types
; ===================================================================

(define (repo-db-class-helpers db)
    ($
"    // -- DB helpers --

    private static string P(int n) => $\""
    (pp-prefix db)
    "{n}\";
"
    (case db
        ((PostgreSql)
"
    private static ParamType NewParam(int index, object value) => new ParamType { Value = value };
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType { Value = value, NpgsqlDbType = NpgsqlDbType.TimestampTz };
    private static ParamType CopyParam(ParamType source)
    {
        var p = new ParamType { Value = source.Value };
        if (source.NpgsqlDbType != NpgsqlDbType.Unknown)
            p.NpgsqlDbType = source.NpgsqlDbType;
        return p;
    }

")
        ((SqlServer)
"
    private static ParamType NewParam(int index, object value) => new ParamType($\"@p{index}\", value);
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType($\"@p{index}\", value) { SqlDbType = SqlDbType.DateTime2 };
    private static ParamType CopyParam(ParamType source) => new ParamType(source.ParameterName, source.Value) { SqlDbType = source.SqlDbType };

")
        ((Sqlite)
"
    private static ParamType NewParam(int index, object value) => new ParamType($\"@p{index}\", value);
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType($\"@p{index}\", value);
    private static ParamType CopyParam(ParamType source) => new ParamType(source.ParameterName, source.Value);

"))))

; ===================================================================
; Main repository function
; ===================================================================

(define (repository)
    (let* ((resource (current-node))
           (rn (name-of resource))
           (has-history (history? resource))
           (schema rn)
           (inlines (inline-searches resource))
           (inline-list (node-list->list inlines))
           (raw-table-search-list (node-list->list (table-searches resource)))
           ; Filter to supported types only (excludes composite, uri)
           (all-table-search-list (filter (lambda (s)
               (let ((t (% "type" s)))
                   (or (string=? "string" t)
                       (string=? "token" t)
                       (string=? "date" t)
                       (string=? "reference" t)
                       (string=? "number" t)
                       (string=? "quantity" t)))) raw-table-search-list))
           (string-searches (filter (lambda (s) (string=? "string"(% "type" s))) all-table-search-list))
           (token-searches (filter (lambda (s) (and (string=? "token"(% "type" s)) (not (search-is-inline? s)))) all-table-search-list))
           (date-searches (filter (lambda (s) (and (string=? "date"(% "type" s)) (not (search-is-inline-instant? s)))) all-table-search-list))
           (instant-searches (filter (lambda (s) (search-is-inline-instant? s)) all-table-search-list))
           (ref-searches (filter (lambda (s) (string=? "reference"(% "type" s))) all-table-search-list))
           (number-searches (filter (lambda (s) (string=? "number"(% "type" s))) all-table-search-list))
           (quantity-searches (filter (lambda (s) (string=? "quantity"(% "type" s))) all-table-search-list))
           (hn-all (filter (lambda (s)
               (and (search-is-humanname? s) (equal? "all"(search-humanname-field s))))
               string-searches)))
        (make directory path: "Repositories"
        (apply sosofo-append
            (repo-interface rn inline-list has-history)
            (map (lambda (db)
                (make directory path: (db-folder db)
                (file ($ rn "Repository.cs")
                    ($
                        (repo-header rn schema inline-list db)
                        (repo-create rn schema inline-list has-history db)
                        (repo-read rn schema inline-list has-history db)
                        (if has-history (repo-vread rn schema inline-list has-history db)"")
                        (repo-update rn schema inline-list has-history db)
                        (repo-patch rn schema inline-list has-history db)
                        (repo-delete rn schema inline-list has-history db)
                        (if (string=? rn "Provenance") (repo-hard-delete rn schema has-history db)"")
                        (if has-history (repo-is-deleted schema db)"")
                        (if has-history (repo-rollback rn schema inline-list db)"")
                        (if has-history (repo-undelete rn schema inline-list db)"")
                        (if has-history (repo-undelete-all rn schema inline-list db)"")
                        (repo-search rn schema inline-list all-table-search-list has-history db)
                        (if has-history (repo-instance-history rn schema db)"")
                        (if has-history (repo-type-history rn schema db)"")
                        (repo-autocomplete schema string-searches token-searches date-searches db)
                        (repo-create-search rn schema all-table-search-list inline-list string-searches token-searches date-searches ref-searches number-searches hn-all db)
                        (repo-drop-search schema all-table-search-list db)
                        (repo-purge rn schema has-history db)
                        (repo-read-resource-helper rn inline-list has-history db)
                        (repo-insert-search-indexes rn schema string-searches token-searches date-searches instant-searches ref-searches number-searches quantity-searches hn-all db)
                        (repo-delete-search-indexes schema all-table-search-list db)
                        (repo-insert-helpers schema db)
                        (if (null? quantity-searches) "" (repo-insert-quantity-helper schema db))
                        (repo-search-helpers rn schema inline-list all-table-search-list string-searches token-searches date-searches number-searches ref-searches db)
                        (repo-db-class-helpers db)
                        "}
"
                    )))) target-databases)))))

; ===================================================================
; Section: Header + class open + constructor
; ===================================================================

(define (repo-header rn schema inline-list db)
    ($
"using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using "(project-name)".Search;
using "(project-name)".Repositories;
using "(db-using db)";
"
    (case db
        ((PostgreSql)"using NpgsqlTypes;
using ConnType = Npgsql.NpgsqlConnection;
using TxType = Npgsql.NpgsqlTransaction;
using CmdType = Npgsql.NpgsqlCommand;
using ReaderType = Npgsql.NpgsqlDataReader;
using ParamType = Npgsql.NpgsqlParameter;
")
        ((SqlServer)"using System.Data;
using ConnType = Microsoft.Data.SqlClient.SqlConnection;
using TxType = Microsoft.Data.SqlClient.SqlTransaction;
using CmdType = Microsoft.Data.SqlClient.SqlCommand;
using ReaderType = Microsoft.Data.SqlClient.SqlDataReader;
using ParamType = Microsoft.Data.SqlClient.SqlParameter;
")
        ((Sqlite)"using ConnType = Microsoft.Data.Sqlite.SqliteConnection;
using TxType = Microsoft.Data.Sqlite.SqliteTransaction;
using CmdType = Microsoft.Data.Sqlite.SqliteCommand;
using ReaderType = Microsoft.Data.Sqlite.SqliteDataReader;
using ParamType = Microsoft.Data.Sqlite.SqliteParameter;
"))
"
namespace "(project-name)".Repositories."(db-namespace db)";

public class "rn"Repository : I"rn"Repository
{
"
    (case db
        ((PostgreSql)
            ($"    private readonly NpgsqlDataSource _dataSource;

    public "rn"Repository(NpgsqlDataSource dataSource)
    {
        _dataSource = dataSource;
    }

"))
        ((SqlServer Sqlite)
            ($"    private readonly string _connectionString;

    public "rn"Repository(string connectionString)
    {
        _connectionString = connectionString;
    }

")))))

; ===================================================================
; Section: CreateAsync
; ===================================================================

(define (repo-create rn schema inline-list has-history db)
    ($
"    public async Task<"rn"Resource> CreateAsync(JsonNode content, string? clientId = null)
    {
        var id = clientId ?? Guid.NewGuid().ToString();
        var lastUpdated = DateTime.UtcNow;

"
        (open-conn-tx db)
"

"
    (if has-history
        ($"        // Check history for previous versions (update-as-create after delete)
        var versionId = 1;
        if (clientId != null)
        {
            await using (var vCmd = "(new-cmd db)"
                \"SELECT COALESCE(MAX(version_id), 0) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)"\", conn, tx))
            {
"(add-param-line-for "vCmd" 1 "id" db)"
                var maxVersion = Convert.ToInt32(await vCmd.ExecuteScalarAsync());
                if (maxVersion > 0) versionId = maxVersion + 1;
            }
        }

        ")
        "        var versionId = 1;
")
    rn "SearchExtractor.InjectMeta(content, id, versionId, lastUpdated);
"(if (null? inline-list)""($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"))
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"        // Binary: extract data before storing
        byte[]? binaryData = null;
        JsonNode? binaryDataNode = null;
        var dataElement = content[\"data\"];
        if (dataElement != null)
        {
            binaryDataNode = dataElement.DeepClone();
            binaryData = Convert.FromBase64String(dataElement.GetValue<""string>());
            content[\"data\"] = null;
        }
")
        "")
"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl-public rn db)" ("(repo-column-list inline-list has-history db)") \" +
            \"VALUES ("(repo-param-list inline-list has-history db)")\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "versionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

"
    (if has-history
        ; No history row for the version that has just become the current one - not on a
        ; re-create after delete either. The history table holds superseded versions; the
        ; current version lives in the public table and the history queries read it from
        ; there. A row for it would show the version twice and collide with the primary key
        ; as soon as the next write archives that version.
        ""
        "")
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($ "        // Binary: store raw bytes separately
        if (binaryData != null)
        {
            await using (var dataCmd = new NpgsqlCommand(
                \"INSERT INTO \\\"Binary\\\".resource_data (resource_id, version_id, data) VALUES ($1, $2, $3)\", conn, tx))
            {
                dataCmd.Parameters.AddWithValue(id);
                dataCmd.Parameters.AddWithValue(versionId);
                dataCmd.Parameters.Add(new NpgsqlParameter { Value = binaryData, NpgsqlDbType = NpgsqlDbType.Bytea });
                await dataCmd.ExecuteNonQueryAsync();
            }
            content[\"data\"] = binaryDataNode;
            contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);
        }

")
        "")
"        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, versionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: ReadAsync
; ===================================================================

(define (repo-read rn schema inline-list has-history db)
    ($
"    public async Task<"rn"Resource?> ReadAsync(string id)
    {
"
    (open-conn-scoped db)"
        await using var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list has-history db)" FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx);
"(add-param-8 1 "id" db)"

        await using var reader = await cmd.ExecuteReaderAsync();
        if (!await reader.ReadAsync()) return null;

        var resource = Read"rn"Resource(reader);
        await reader.CloseAsync();
"
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"
        // Binary: inject data from resource_data
        await using var dataCmd = new NpgsqlCommand(
            \"SELECT data FROM \\\"Binary\\\".resource_data WHERE resource_id = $1 AND version_id = $2\", conn, tx);
        dataCmd.Parameters.AddWithValue(id);
        dataCmd.Parameters.AddWithValue(resource.versionId);
        await using var dataReader = await dataCmd.ExecuteReaderAsync();
        if (await dataReader.ReadAsync())
        {
            var bytes = dataReader.GetFieldValue<""byte[]>(0);
            await dataReader.CloseAsync();
            var parsed = JsonNode.Parse(resource.content)!;
            parsed[\"data\"] = Convert.ToBase64String(bytes);
            resource = new BinaryResource(resource.id, resource.versionId, resource.lastUpdated,
                JsonSerializer.SerializeToUtf8Bytes(parsed));
        }

")
        "")
"        return resource;
    }

"))

; ===================================================================
; Section: VersionReadAsync
; ===================================================================

(define (repo-vread rn schema inline-list has-history db)
    ($
"    public async Task<"rn"Resource?> VersionReadAsync(string id, int versionId)
    {
        // Check current table first
"
    (open-conn-scoped db)"
        await using var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list has-history db)" FROM "(tbl-public rn db)" \" +
            \"WHERE id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx);
"(add-param-8 1 "id" db)"
"(add-param-8 2 "versionId" db)"

        await using var reader = await cmd.ExecuteReaderAsync();
        if (await reader.ReadAsync())
        {
            var resource = Read"rn"Resource(reader);
            await reader.CloseAsync();
"
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"
            // Binary: inject data from resource_data
            await using var dataCmd = new NpgsqlCommand(
                \"SELECT data FROM \\\"Binary\\\".resource_data WHERE resource_id = $1 AND version_id = $2\", conn, tx);
            dataCmd.Parameters.AddWithValue(id);
            dataCmd.Parameters.AddWithValue(versionId);
            await using var dataReader = await dataCmd.ExecuteReaderAsync();
            if (await dataReader.ReadAsync())
            {
                var bytes = dataReader.GetFieldValue<""byte[]>(0);
                await dataReader.CloseAsync();
                var parsed = JsonNode.Parse(resource.content)!;
                parsed[\"data\"] = Convert.ToBase64String(bytes);
                resource = new BinaryResource(resource.id, resource.versionId, resource.lastUpdated,
                    JsonSerializer.SerializeToUtf8Bytes(parsed));
            }

")
        "")
"            return resource;
        }
        await reader.CloseAsync();

        // Check history table
        await using var histCmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list has-history db)" FROM "(tbl schema "resource_history" db)" \" +
            \"WHERE id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx);
"(add-param-8-for "histCmd" 1 "id" db)"
"(add-param-8-for "histCmd" 2 "versionId" db)"

        await using var histReader = await histCmd.ExecuteReaderAsync();
        if (await histReader.ReadAsync())
        {
            var resource = Read"rn"Resource(histReader);
            await histReader.CloseAsync();
"
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"
            // Binary: inject data from resource_data (history)
            await using var histDataCmd = new NpgsqlCommand(
                \"SELECT data FROM \\\"Binary\\\".resource_data WHERE resource_id = $1 AND version_id = $2\", conn, tx);
            histDataCmd.Parameters.AddWithValue(id);
            histDataCmd.Parameters.AddWithValue(versionId);
            await using var histDataReader = await histDataCmd.ExecuteReaderAsync();
            if (await histDataReader.ReadAsync())
            {
                var bytes = histDataReader.GetFieldValue<""byte[]>(0);
                await histDataReader.CloseAsync();
                var parsed = JsonNode.Parse(resource.content)!;
                parsed[\"data\"] = Convert.ToBase64String(bytes);
                resource = new BinaryResource(resource.id, resource.versionId, resource.lastUpdated,
                    JsonSerializer.SerializeToUtf8Bytes(parsed));
            }

")
        "")
"            return resource;
        }

        return null;
    }

"))

; ===================================================================
; Section: UpdateAsync
; ===================================================================

(define (repo-update rn schema inline-list has-history db)
    (if has-history
        (repo-update-versioned rn schema inline-list db)
        (repo-update-non-versioned rn schema inline-list db)))

(define (repo-update-versioned rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> UpdateAsync(string id, JsonNode content, int? ifMatchVersion = null)
    {
"
    (open-conn-tx db)"

        // Read current
        "rn"Resource? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = Read"rn"Resource(reader);
        }

        if (current == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        if (ifMatchVersion.HasValue && current.versionId != ifMatchVersion.Value)
        {
            if (txScope == null) await tx.RollbackAsync();
            throw new ConcurrencyException(current.versionId);
        }

        // Copy current to history
        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl schema "resource_history" db)" (" (repo-history-column-list inline-list db)") \" +
            \"VALUES (" (repo-history-param-list inline-list "UPDATE" db)")\", conn, tx))
        {
" (add-param-line 1 "current.id" db)"
" (add-param-line 2 "current.versionId" db)"
" (add-param-line 3 "current.lastUpdated" db)"
" (add-param-line 4 "current.content" db)
            (repo-addwithvalue inline-list "current." 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        var newVersionId = current.versionId + 1;
        var lastUpdated = DateTime.UtcNow;
        "rn"SearchExtractor.InjectMeta(content, id, newVersionId, lastUpdated);
" (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"))
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"        // Binary: extract data before storing
        byte[]? binaryData = null;
        JsonNode? binaryDataNode = null;
        var dataElement = content[\"data\"];
        if (dataElement != null)
        {
            binaryDataNode = dataElement.DeepClone();
            binaryData = Convert.FromBase64String(dataElement.GetValue<""string>());
            content[\"data\"] = null;
        }
")
        "")
"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

        // Update resource
        await using (var cmd = "(new-cmd db)"
            \"UPDATE "(tbl-public rn db)" SET "(repo-update-set inline-list #t db)" \" +
            \"WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "newVersionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

"
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        "        // Binary: store raw bytes separately
        if (binaryData != null)
        {
            await using (var dataCmd = new NpgsqlCommand(
                \"INSERT INTO \\\"Binary\\\".resource_data (resource_id, version_id, data) VALUES ($1, $2, $3)\", conn, tx))
            {
                dataCmd.Parameters.AddWithValue(id);
                dataCmd.Parameters.AddWithValue(newVersionId);
                dataCmd.Parameters.Add(new NpgsqlParameter { Value = binaryData, NpgsqlDbType = NpgsqlDbType.Bytea });
                await dataCmd.ExecuteNonQueryAsync();
            }
            content[\"data\"] = binaryDataNode;
            contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);
        }

"
        "")
"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

(define (repo-update-non-versioned rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> UpdateAsync(string id, JsonNode content)
    {
"
       (open-conn-tx db)"

        // Read current version
        int currentVersionId = 0;
        await using (var cmd = "(new-cmd db)"
            \"SELECT version_id FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"       (add-param-line 1 "id" db)"
            var result = await cmd.ExecuteScalarAsync();
            if (result == null)
            {
                if (txScope == null) await tx.RollbackAsync();
                return null;
            }
            currentVersionId = Convert.ToInt32(result);
        }

        var newVersionId = currentVersionId + 1;
        var lastUpdated = DateTime.UtcNow;
        "rn"SearchExtractor.InjectMeta(content, id, newVersionId, lastUpdated);
"       (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"       ))
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        ($"        // Binary: extract data before storing
        byte[]? binaryData = null;
        JsonNode? binaryDataNode = null;
        var dataElement = content[\"data\"];
        if (dataElement != null)
        {
            binaryDataNode = dataElement.DeepClone();
            binaryData = Convert.FromBase64String(dataElement.GetValue<""string>());
            content[\"data\"] = null;
        }
")
        "")
"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

        // Update resource
        await using (var cmd = "(new-cmd db)"
            \"UPDATE "(tbl-public rn db)" SET "(repo-update-set inline-list #f db)" \" +
            \"WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "newVersionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

"
    (if (and (string=? "Binary" rn) (string=? (symbol->string db)"PostgreSql"))
        "        // Binary: store raw bytes separately
        if (binaryData != null)
        {
            await using (var dataCmd = new NpgsqlCommand(
                \"INSERT INTO \\\"Binary\\\".resource_data (resource_id, version_id, data) VALUES ($1, $2, $3)\", conn, tx))
            {
                dataCmd.Parameters.AddWithValue(id);
                dataCmd.Parameters.AddWithValue(newVersionId);
                dataCmd.Parameters.Add(new NpgsqlParameter { Value = binaryData, NpgsqlDbType = NpgsqlDbType.Bytea });
                await dataCmd.ExecuteNonQueryAsync();
            }
            content[\"data\"] = binaryDataNode;
            contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);
        }

"
        "")
"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: PatchAsync
; ===================================================================

(define (repo-patch rn schema inline-list has-history db)
    (if has-history
        (repo-patch-versioned rn schema inline-list db)
        (repo-patch-non-versioned rn schema inline-list db)))

(define (repo-patch-versioned rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> PatchAsync(string id, JsonArray patchOps, int? ifMatchVersion = null)
    {
"
    (open-conn-tx db)"

        // Read current
        "rn"Resource? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = Read"rn"Resource(reader);
        }

        if (current == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        if (ifMatchVersion.HasValue && current.versionId != ifMatchVersion.Value)
        {
            if (txScope == null) await tx.RollbackAsync();
            throw new ConcurrencyException(current.versionId);
        }

        // Copy current to history
        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl schema "resource_history" db)" ("(repo-history-column-list inline-list db)") \" +
            \"VALUES ("(repo-history-param-list inline-list "UPDATE" db)")\", conn, tx))
        {
"(add-param-line 1 "current.id" db)"
"(add-param-line 2 "current.versionId" db)"
"(add-param-line 3 "current.lastUpdated" db)"
"(add-param-line 4 "current.content" db)
            (repo-addwithvalue inline-list "current." 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        var newVersionId = current.versionId + 1;
        var lastUpdated = DateTime.UtcNow;
        var content = JsonNode.Parse(current.content)!;
        JsonPatcher.Apply(content, patchOps);
        "rn"SearchExtractor.InjectMeta(content, id, newVersionId, lastUpdated);
"(if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"))"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

        // Update resource
        await using (var cmd = "(new-cmd db)"
            \"UPDATE "(tbl-public rn db)" SET "(repo-update-set inline-list #t db)" \" +
            \"WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "newVersionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

(define (repo-patch-non-versioned rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> PatchAsync(string id, JsonArray patchOps)
    {
"
    (open-conn-tx db)"

        // Read current
        "rn"Resource? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #f db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = Read"rn"Resource(reader);
        }

        if (current == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        var newVersionId = current.versionId + 1;
        var lastUpdated = DateTime.UtcNow;
        var content = JsonNode.Parse(current.content)!;
        JsonPatcher.Apply(content, patchOps);
        "rn"SearchExtractor.InjectMeta(content, id, newVersionId, lastUpdated);
"(if (null? inline-list)""($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"))"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

        // Update resource
        await using (var cmd = "(new-cmd db)"
            \"UPDATE "(tbl-public rn db)" SET "(repo-update-set inline-list #f db)" \" +
            \"WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "newVersionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: DeleteAsync
; ===================================================================

(define (repo-delete rn schema inline-list has-history db)
    (if has-history
        (repo-delete-versioned rn schema inline-list db)
        (repo-delete-non-versioned rn schema db)))

(define (repo-delete-versioned rn schema inline-list db)
    ($
"    public async Task<""bool> DeleteAsync(string id)
    {
"
    (open-conn-tx db)"

        "rn"Resource? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = Read"rn"Resource(reader);
        }

        if (current == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return false;
        }

        // Copy to history as DELETE
        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl schema "resource_history" db)" ("(repo-history-column-list inline-list db)") \" +
            \"VALUES ("(repo-history-param-list inline-list "DELETE" db)")\", conn, tx))
        {
"(add-param-line 1 "current.id" db)"
"(add-param-line 2 "current.versionId" db)"
"(add-param-line 3 "current.lastUpdated" db)"
"(add-param-line 4 "current.content" db)
            (repo-addwithvalue inline-list "current." 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        // Delete from resource (CASCADE deletes search indexes)
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await cmd.ExecuteNonQueryAsync();
        }

        if (txScope == null) await tx.CommitAsync();
        return true;
    }

"))

(define (repo-delete-non-versioned rn schema db)
    ($
"    public async Task<""bool> DeleteAsync(string id)
    {
"
    (open-conn-scoped db)"
        await using var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx);
"(add-param-8 1 "id" db)"
        var rows = await cmd.ExecuteNonQueryAsync();
        return rows > 0;
    }

"))

; ===================================================================
; Section: IsDeletedAsync
; ===================================================================

(define (repo-is-deleted schema db)
    ($
"    public async Task<""bool> IsDeletedAsync(string id)
    {
"        (open-conn-scoped db)
"
        await using var cmd = "(new-cmd db)"
            \"SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE'\", conn, tx);
"(add-param-8 1 "id" db)"
        return Convert.ToInt64(await cmd.ExecuteScalarAsync()) > 0;
    }

"))

; ===================================================================
; Section: HardDeleteAsync
; ===================================================================

; Removes a resource without leaving a trace - no tombstone, no history. A rollback uses it to
; drop the provenance that was written for the version it removes, so that the data ends up
; exactly as it was before that version existed.
(define (repo-hard-delete rn schema has-history db)
    ($
"    public async Task<""bool> HardDeleteAsync(string id)
    {
"
    (open-conn-tx db)"

        var deleted = false;
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            deleted = await cmd.ExecuteNonQueryAsync() > 0;
        }
"
    (if has-history ($"
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            if (await cmd.ExecuteNonQueryAsync() > 0) deleted = true;
        }
") "")
    (if (binary-data-table? rn db) ($"
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await cmd.ExecuteNonQueryAsync();
        }
") "")
"
        if (txScope == null) await tx.CommitAsync();
        return deleted;
    }

"))

; ===================================================================
; Section: RollbackAsync
; ===================================================================

(define (repo-rollback rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> RollbackAsync(string id, int currentVersionId)
    {
"
    (open-conn-tx db)"

        "rn"Resource? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = Read"rn"Resource(reader);
        }

        if (current == null || current.versionId != currentVersionId)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        // Delete current from resource (CASCADE deletes search indexes)
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await cmd.ExecuteNonQueryAsync();
        }

        // Find the most recent history entry - the DELETE row included, because it is the
        // version the resource stood in when it was deleted.
        "rn"Resource? previous = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl schema "resource_history" db)" \" +
            \"WHERE id = "(pp 1 db)" ORDER BY version_id DESC "(db-limit-1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                previous = Read"rn"Resource(reader);
        }

        if (previous == null)
        {
            // No prior history to restore: rollback effectively deleted the resource.
            // Return the just-deleted state so the handler reports 200 (with last-known
            // content) instead of 404 (which mis-reports a state change as failure).
            if (txScope == null) await tx.CommitAsync();
            return current;
        }

        // Which operation wrote that version? A DELETE row means the resource was deleted at
        // that point, so this is where the rollback ends: the current row is already gone and
        // the delete row stays where it is. Undoing the version leaves exactly the state that
        // preceded it - a deleted resource.
        var previousIsDelete = false;
        await using (var cmd = "(new-cmd db)"
            \"SELECT history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "previous.versionId" db)"
            previousIsDelete = (await cmd.ExecuteScalarAsync()) as string == \"DELETE\";
        }

        if (previousIsDelete)
        {
            if (txScope == null) await tx.CommitAsync();
            return previous;
        }

        // Remove previous from history
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "previous.versionId" db)"
            await cmd.ExecuteNonQueryAsync();
        }

        // Insert previous as current
        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl-public rn db)" ("(repo-column-list inline-list #t db)") \" +
            \"VALUES ("(repo-param-list inline-list #t db)")\", conn, tx))
        {
"(add-param-line 1 "previous.id" db)"
"(add-param-line 2 "previous.versionId" db)"
"(add-param-line 3 "previous.lastUpdated" db)"
"(add-param-line 4 "previous.content" db)
            (repo-addwithvalue inline-list "previous." 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        await InsertSearchIndexesAsync(conn, tx, id, JsonNode.Parse(previous.content)!);
        if (txScope == null) await tx.CommitAsync();

        return previous;
    }

"))

; ===================================================================
; Section: UndeleteAsync
; ===================================================================

(define (repo-undelete rn schema inline-list db)
    ($
"    public async Task<"rn"Resource?> UndeleteAsync(string id)
    {
"
    (open-conn-tx db)"

        // Check resource doesn't already exist
        await using (var cmd = "(new-cmd db)"
            \""(db-select-top1 db)" 1 FROM "(tbl-public rn db)" WHERE id = "(pp 1 db) (db-trailing-limit1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
            {
                if (txScope == null) await tx.RollbackAsync();
                return null; // Already exists, not deleted
            }
        }

        // Find the latest history entry, the DELETE row included: it carries the state the
        // resource was in when it was deleted. Skipping it restored the version before the
        // last edit and silently dropped that edit.
        "rn"Resource? previous = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl schema "resource_history" db)" \" +
            \"WHERE id = "(pp 1 db)" ORDER BY version_id DESC "(db-limit-1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                previous = Read"rn"Resource(reader);
        }

        if (previous == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        // Re-insert with new version
        int maxVersionId = 0;
        await using (var cmd = "(new-cmd db)"
            \"SELECT COALESCE(MAX(version_id), 0) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            maxVersionId = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        var newVersionId = maxVersionId + 1;
        var lastUpdated = DateTime.UtcNow;
        var content = JsonNode.Parse(previous.content)!;
        "rn"SearchExtractor.InjectMeta(content, id, newVersionId, lastUpdated);
"   (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "rn"SearchExtractor.ExtractInlineParams(content);
"))"        var contentBytes = System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(content);

        await using (var cmd = "(new-cmd db)"
            \"INSERT INTO "(tbl-public rn db)" ("(repo-column-list inline-list #t db)") \" +
            \"VALUES ("(repo-param-list inline-list #t db)")\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "newVersionId" db)"
"(add-param-line 3 "lastUpdated" db)"
"(add-param-line 4 "contentBytes" db)
            (repo-addwithvalue inline-list "" 5 db)
"
            await cmd.ExecuteNonQueryAsync();
        }

        // No history row for the restored version: it is the current one now and the
        // history queries read the current version from the public table.
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return new "rn"Resource(id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: UndeleteAllAsync
; ===================================================================

(define (repo-undelete-all rn schema inline-list db)
    ($
"    public async Task<""int> UndeleteAllAsync()
    {
"    (open-conn db)"

        // Find all deleted resource IDs (in history but not in resource table)
        var deletedIds = new System.Collections.Generic.List<""string>();
        await using (var cmd = "(new-cmd db)"
            \"SELECT DISTINCT h.id FROM "(tbl schema "resource_history" db)" h \" +
            \"LEFT JOIN "(tbl-public rn db)" r ON h.id = r.id \" +
            \"WHERE r.id IS NULL\", conn))
        {
            await using var reader = await cmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                deletedIds.Add(reader.GetString(0));
        }

        var count = 0;
        foreach (var id in deletedIds)
        {
            var result = await UndeleteAsync(id);
            if (result != null) count++;
        }

        return count;
    }

"))

; ===================================================================
; Section: SearchAsync
; ===================================================================

(define (repo-search rn schema inline-list all-table-search-list has-history db)
    ($
"    public async Task<(List<"rn"Resource> resources, int total)> SearchAsync(
        IQueryCollection queryParams, int count, int offset)
    {
        var whereClauses = new List<""string>();
        var joins = new List<""string>();
        var parameters = new List<""ParamType>();
        var paramIndex = 1;
        var aliasIndex = 0;
        var sortParam = \"\";

        foreach (var param in queryParams)
        {
            var key = param.Key;
            if (key == \"_count\" || key == \"_offset\" || key == \"_total\" ||
                key == \"_include\" || key == \"_revinclude\" ||
                key == \"_include:iterate\" || key == \"_revinclude:iterate\" ||
                key == \"_summary\" || key == \"_elements\")
                continue;

            // Skip chained params, _has, and composite params (handled by handler;
            // its names, as handlers.scm has them)
            if (key.Contains('.') || key.StartsWith(\"_has:\") ||
                key == \"code-value-quantity\" || key == \"code-value-concept\" ||
                key == \"component-code-value-quantity\" || key == \"component-code-value-concept\")
                continue;

            if (key == \"_sort\")
            {
                sortParam = param.Value.ToString();
                continue;
            }
"
        ; Reverse-existence filter cases (per-project; see existsfilter.scm)
        (repo-exists-filter-cases rn db)
"
            // Handle modifiers
            var modifier = \"\";
            if (key.Contains(':'))
            {
                var parts = key.Split(':', 2);
                key = parts[0];
                modifier = parts[1];
            }

            // Handle :missing modifier
            if (modifier == \"missing\")
            {
                var isMissing = param.Value.ToString().Equals(\"true\", StringComparison.OrdinalIgnoreCase);
                AddMissingSearch(key, isMissing, joins, whereClauses, ref aliasIndex);
                continue;
            }

            // Handle :not modifier
            if (modifier == \"not\")
            {
                AddNotSearch(key, param.Value.ToString(), joins, whereClauses, parameters, ref paramIndex, ref aliasIndex);
                continue;
            }

            // Each StringValues entry is a repeated parameter (AND).
            // Within each entry, comma-separated values are OR.
            foreach (var singleValue in param.Value)
            {
                if (singleValue == null) continue;
                var orValues = singleValue.Split(',').Select(v => v.Trim()).Where(v => v.Length > 0).ToArray();
                if (orValues.Length == 0)
                {
                    // An _id without a usable value must not widen the search. Dropping it silently
                    // turns a caller-bound query into everything the tenant filter still allows, so
                    // the criterion matches nothing instead. Other parameters keep the old behaviour:
                    // an empty filter field there means the filter was not set.
                    if (key == \"_id\")
                        whereClauses.Add(\"1 = 0\");
                    continue;
                }

                switch (key)
                {
                    case \"_id\":
                        if (orValues.Length == 1)
                        {
                            whereClauses.Add($\"r.id = {P(paramIndex)}\");
                            parameters.Add(NewParam(paramIndex, orValues[0]));
                            paramIndex++;
                        }
                        else
                        {
                            var idInParams = new List<""string>();
                            foreach (var v in orValues)
                            {
                                idInParams.Add($\"{P(paramIndex)}\");
                                parameters.Add(NewParam(paramIndex, v));
                                paramIndex++;
                            }
                            whereClauses.Add($\"r.id IN ({string.Join(\", \", idInParams)})\");
                        }
                        break;

"
        ; Inline search param cases
        (apply $ (map (lambda (s) (repo-search-case-inline s db)) inline-list))
        ; _lastUpdated inline case (uses r.last_updated directly, no JOIN)
        (repo-search-case-last-updated rn)
        ; Table search param cases
        (apply $ (map (lambda (s) (repo-search-case-table s schema)) all-table-search-list))
"                    default:
                        throw new ArgumentException($\"Unknown search parameter: {key}\");
                }
            }
        }

        var whereClause = whereClauses.Count > 0 ? \"WHERE \" + string.Join(\" AND \", whereClauses) : \"\";
        var joinClause = string.Join(\" \", joins);

        // Determine sort order
        var orderBy = BuildOrderBy(sortParam);

        // Count total
        var countSql = $\"SELECT COUNT(DISTINCT r.id) FROM "(tbl-public rn db)" r {joinClause} {whereClause}\";
        int total;
"
    (open-conn-scoped db)
"
        await using (var cmd = new CmdType(countSql, conn, tx))
        {
            for (int i = 0; i < parameters.Count; i++)
                cmd.Parameters.Add(parameters[i]);
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        if (count == 0)
            return (new List<"rn"Resource>(), total);

        // Fetch page
        string selectSql;
        if (joins.Count > 0)
        {
            // Use subquery to avoid DISTINCT + ORDER BY conflict
            selectSql = $\"SELECT "(repo-column-list-prefixed "r2" inline-list has-history db)(if (binary-data-table? rn db) ", rd.data" "")" \" +
                        $\"FROM "(tbl-public rn db)" r2 "(if (binary-data-table? rn db) ($ "LEFT JOIN "(tbl schema "resource_data" db)" rd ON rd.resource_id = r2.id AND rd.version_id = r2.version_id ") "")"WHERE r2.id IN (\" +
                        $\"SELECT DISTINCT r.id FROM "(tbl-public rn db)" r {joinClause} {whereClause}\" +
                        $\") ORDER BY {orderBy.Replace(\"r.\", \"r2.\")} "(db-paginate "count" "offset" db)"\";
        }
        else
        {
            selectSql = $\"SELECT "(repo-column-list-prefixed "r" inline-list has-history db)(if (binary-data-table? rn db) ", rd.data" "")" \" +
                        $\"FROM "(tbl-public rn db)" r "(if (binary-data-table? rn db) ($ "LEFT JOIN "(tbl schema "resource_data" db)" rd ON rd.resource_id = r.id AND rd.version_id = r.version_id ") "")"{whereClause} \" +
                        $\"ORDER BY {orderBy} "(db-paginate "count" "offset" db)"\";
        }

        var resources = new List<"rn"Resource>();
        await using (var cmd = new CmdType(selectSql, conn, tx))
        {
            for (int i = 0; i < parameters.Count; i++)
                cmd.Parameters.Add(CopyParam(parameters[i]));
            await using var reader = await cmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                resources.Add(Read"rn"Resource"(if (binary-data-table? rn db) "WithData" "")"(reader));
        }

        return (resources, total);
    }

"))

; --- Search case for inline bool param ---
(define (repo-search-case-inline-bool sn col)
    ($
"                    case \"" sn "\":
                        if (orValues.Length == 1)
                        {
                            whereClauses.Add($\"r."col" = {P(paramIndex)}\");
                            parameters.Add(NewParam(paramIndex, bool.Parse(orValues[0])));
                            paramIndex++;
                        }
                        else
                        {
                            var orParts = new List<""string>();
                            foreach (var v in orValues)
                            {
                                orParts.Add($\"r."col" = {P(paramIndex)}\");
                                parameters.Add(NewParam(paramIndex, bool.Parse(v)));
                                paramIndex++;
                            }
                            whereClauses.Add($\"({string.Join(\" OR \", orParts)})\");
                        }
                        break;

"))

; --- Search case for inline string/code param ---
; An inline token matches exactly, like its subtable counterpart, and :contains loosens the
; value side the same way (substring, ignoring case). Without it a filter field over a code
; only ever answers whoever already knows the whole code - and a column holding a failure
; code is read by someone looking for the kind of failure, not for one spelling of it.
(define (repo-search-case-inline-string sn col)
    ($
"                    case \"" sn "\":
                        if (modifier == \"contains\")
                        {
                            var containsParts = new List<""string>();
                            foreach (var v in orValues)
                            {
                                containsParts.Add($\"LOWER(r."col") LIKE {P(paramIndex)}\");
                                parameters.Add(NewParam(paramIndex, $\"%{v.ToLowerInvariant()}%\"));
                                paramIndex++;
                            }
                            whereClauses.Add(containsParts.Count == 1 ? containsParts[0] : $\"({string.Join(\" OR \", containsParts)})\");
                        }
                        else if (orValues.Length == 1)
                        {
                            whereClauses.Add($\"r."col" = {P(paramIndex)}\");
                            parameters.Add(NewParam(paramIndex, orValues[0]));
                            paramIndex++;
                        }
                        else
                        {
                            var inParams = new List<""string>();
                            foreach (var v in orValues)
                            {
                                inParams.Add($\"{P(paramIndex)}\");
                                parameters.Add(NewParam(paramIndex, v));
                                paramIndex++;
                            }
                            whereClauses.Add($\"r."col" IN ({string.Join(\", \", inParams)})\");
                        }
                        break;

"))

; --- Search case for inline param (dispatches by type) ---
(define (repo-search-case-inline search db)
    (let* ((sn (% "name" search))
           (col (db-quote-col-cs (search-col search) db))
           (prop (search-property search))
           (prop-type (if prop (% "type" prop)"code")))
        (cond
            ((string=? "boolean" prop-type) (repo-search-case-inline-bool sn col))
            ((string=? "instant" prop-type) (repo-search-case-inline-instant sn col))
            ((member prop-type '("integer" "positiveInt" "unsignedInt" "integer64" "decimal"))
                (repo-search-case-inline-number sn col prop-type))
            (else (repo-search-case-inline-string sn col)))))

; --- Search case for inline instant param ---
(define (repo-search-case-inline-instant sn col)
    ($"                    case \"" sn "\":
                        SearchQueryBuilder.AddInlineInstantSearch(\"r." col "\", \"" sn "\", whereClauses, parameters, ref paramIndex, orValues);
                        break;

"))

; --- Search case for inline number param ---
(define (repo-search-case-inline-number sn col prop-type)
    (let ((method (case prop-type
                    (("integer" "positiveInt" "unsignedInt") "AddInlineIntSearch")
                    (("integer64") "AddInlineLongSearch")
                    (("decimal") "AddInlineDecimalSearch")
                    (else "AddInlineIntSearch"))))
        ($"                    case \"" sn "\":
                        SearchQueryBuilder." method "(\"r." col "\", \"" sn "\", whereClauses, parameters, ref paramIndex, orValues);
                        break;

")))

; --- Search case for _lastUpdated (inline on r.last_updated) ---
(define (repo-search-case-last-updated rn)
    ($"                    case \"_lastUpdated\":
                        SearchQueryBuilder.AddInlineInstantSearch(\"r.last_updated\", \"_lastUpdated\", whereClauses, parameters, ref paramIndex, orValues);
                        break;

"))

; --- Search case for table search param ---
(define (repo-search-case-table search schema)
    (let* ((sn (% "name" search))
           (tn (search-table-name search))
           (type (% "type" search))
           (is-string (string=? "string" type))
           (is-token (string=? "token" type))
           (helper (cond
               ((string=? "string" type)"AddStringSearchOr")
               ((string=? "token" type)"AddTokenSearchOr")
               ((string=? "date" type)"AddDateSearchOr")
               ((string=? "reference" type)"AddReferenceSearchOr")
               ((string=? "number" type)"AddNumberSearchOr")
               ((string=? "quantity" type)"AddQuantitySearchOr")
               (else #f))))
        (if helper
            (if (or is-string is-token)
                ($"                    case \"" sn "\":
                        SearchQueryBuilder." helper "(\"" schema "\", joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                            \"" tn "\", orValues, modifier);
                        break;

")
                ($"                    case \"" sn "\":
                        SearchQueryBuilder." helper "(\"" schema "\", joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                            \"" tn "\", orValues);
                        break;

"))
            "")))

; ===================================================================
; Section: InstanceHistoryAsync
; ===================================================================

; The total counts the current row, the history rows and one extra entry per DELETE row,
; and it has to count the current one rather than assume it: a deleted resource has no row
; in the public table any more, so a hard-coded 1 + COUNT(history) reported one entry more
; than the bundle carries. A DELETE row yields two entries - see repo-history-union.
(define (repo-instance-history rn schema db)
    ($
"    public async Task<(List<"rn"HistoryRow> rows, int total)> InstanceHistoryAsync(string id, int count, int offset)
    {
"
    (open-conn db)
"
        int total;
        await using (var cmd = new CmdType(
            \"SELECT (SELECT COUNT(*) FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE')\", conn))
        {
"(add-param-8 1 "id" db)"
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        var sql = "(if (binary-data-table? rn db) ($ "\"SELECT h.id, h.version_id, h.last_updated, h.content, h.history_operation, rd.data FROM (\" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM " (tbl-public rn db) " WHERE id = " (pp 1 db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, history_operation FROM " (tbl schema "resource_history" db) " WHERE id = " (pp 1 db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM " (tbl schema "resource_history" db) " WHERE id = " (pp 1 db) " AND history_operation = 'DELETE'" (db-union-close db) "\" +
                  \") h LEFT JOIN " (tbl schema "resource_data" db) " rd ON rd.resource_id = h.id AND rd.version_id = h.version_id \" +
                  \"ORDER BY h.version_id DESC, h.history_operation DESC " (db-paginate-params (pp 2 db) (pp 3 db) db) "\"") ($ "\""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY version_id DESC, history_operation DESC "(db-paginate-params (pp 2 db) (pp 3 db) db) "\""))";

        await using var cmd2 = new CmdType(sql, conn);
"(add-param-8-for "cmd2" 1 "id" db)"
"(add-param-8-for "cmd2" 2 "count" db)"
"(add-param-8-for "cmd2" 3 "offset" db)
"

        var rows = new List<"rn"HistoryRow>();
        await using var reader = await cmd2.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
"(if (binary-data-table? rn db) ($
"            rows.Add(Read"rn"HistoryRowWithData(reader));
") ($
"            rows.Add(new "rn"HistoryRow(
                reader.GetString(0),
                reader.GetInt32(1),
                Utc(reader.GetDateTime(2)),
                reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3),
                reader.GetString(4)));
"))"        }
        return (rows, total);
    }

"))

; ===================================================================
; Section: TypeHistoryAsync
; ===================================================================

(define (repo-type-history rn schema db)
    ($
"    public async Task<(List<"rn"HistoryRow> rows, int total)> TypeHistoryAsync(int count, int offset)
    {
"       (open-conn db)
"
        int total;
        await using (var cmd = new CmdType(
            \"SELECT (SELECT COUNT(*) FROM "(tbl-public rn db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE history_operation = 'DELETE')\", conn))
        {
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        var sql = "(if (binary-data-table? rn db) ($ "\"SELECT h.id, h.version_id, h.last_updated, h.content, h.history_operation, rd.data FROM (\" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM " (tbl-public rn db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, history_operation FROM " (tbl schema "resource_history" db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM " (tbl schema "resource_history" db) " WHERE history_operation = 'DELETE'" (db-union-close db) "\" +
                  \") h LEFT JOIN " (tbl schema "resource_data" db) " rd ON rd.resource_id = h.id AND rd.version_id = h.version_id \" +
                  \"ORDER BY h.last_updated DESC, h.history_operation DESC " (db-paginate-params (pp 1 db) (pp 2 db) db) "\"") ($ "\""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY last_updated DESC, history_operation DESC "(db-paginate-params (pp 1 db) (pp 2 db) db) "\""))";

        await using var cmd2 = new CmdType(sql, conn);
"(add-param-8-for "cmd2" 1 "count" db)"
"(add-param-8-for "cmd2" 2 "offset" db)
"

        var rows = new List<"rn"HistoryRow>();
        await using var reader = await cmd2.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
"(if (binary-data-table? rn db) ($
"            rows.Add(Read"rn"HistoryRowWithData(reader));
") ($
"            rows.Add(new "rn"HistoryRow(
                reader.GetString(0),
                reader.GetInt32(1),
                Utc(reader.GetDateTime(2)),
                reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3),
                reader.GetString(4)));
"))"        }
        return (rows, total);
    }

"))

; ===================================================================
; Section: AutocompleteAsync
; ===================================================================

(define (repo-autocomplete-resolve schema string-searches token-searches date-searches db)
    ($
"    private static (string? tableName, string column, bool isToken) ResolveSearchTable(string name)
    {
        var tableName = name switch
        {
"
        (apply $ (map (lambda (s)
            ($"            \""(% "name" s)"\" => \""(search-table-name s)"\",
"))
            string-searches))
        (apply $ (map (lambda (s)
            (let ((sn (% "name" s)))
                (if (search-is-inline? s)
                    ""
                    ($"            \"" sn "\" => \""(search-table-name s)"\",
"))))
            token-searches))
        (apply $ (map (lambda (s)
            ($"            \""(% "name" s)"\" => \""(search-table-name s)"\",
"))
            date-searches))
"            _ => (string?)null
        };
        var column = name switch
        {
"
        (apply $ (map (lambda (s) ($"            \""(% "name" s)"\" => \"value\",
"))
            string-searches))
        (apply $ (map (lambda (s)
            (let ((sn (% "name" s)))
                (if (search-is-inline? s)""
                    ; Token-Index befuellt nur system+value (display bleibt leer),
                    ; daher liefert Autocomplete fuer alle Token-Params die value-Spalte (den Code).
                    ($"            \"" sn "\" => \"value\",
"))))
            token-searches))
        (apply $ (map (lambda (s)
            ($"            \""(% "name" s)"\" => \""
                (case db
                    ((PostgreSql) "TO_CHAR(value_low, 'YYYY-MM-DD')")
                    ((Sqlite)     "STRFTIME('%Y-%m-%d', value_low)")
                    ((SqlServer)  "FORMAT(value_low, 'yyyy-MM-dd')"))
                "\",
"))
            date-searches))
"            _ => \"value\"
        };
        var isToken = name switch
        {
"
        (apply $ (map (lambda (s)
            (let ((sn (% "name" s)))
                (if (search-is-inline? s)""
                    ($"            \"" sn "\" => true,
"))))
            token-searches))
"            _ => false
        };
        return (tableName, column, isToken);
    }

"))

(define (repo-autocomplete schema string-searches token-searches date-searches db)
    ($
    (repo-autocomplete-resolve schema string-searches token-searches date-searches db)
"    public async Task<""List<""string>> AutocompleteAsync(string param, string value, string property, int? count, int? offset)
    {
"       (open-conn db)
"
        var (propTable, propColumn, propIsToken) = ResolveSearchTable(property);
        if (propTable == null && property != \"_id\") return new List<""string>();

        if (param == \"_id\" && propTable != null)
        {
            await using var cmd = new CmdType(
"
    (case db
        ((PostgreSql Sqlite)
            ($"                $\"SELECT DISTINCT {propColumn} FROM "(tbl-interp schema "propTable" db)" WHERE resource_id = {P(1)} AND {propColumn} IS NOT NULL\", conn);
"))
        ((SqlServer)
            ($"                $\"SELECT DISTINCT {propColumn} FROM "(tbl-interp schema "propTable" db)" WHERE resource_id = {P(1)} AND {propColumn} IS NOT NULL\", conn);
")))
    (add-param-8-for "cmd" 1 "value" db)
"
            var results = new List<""string>();
            await using var reader = await cmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                results.Add(reader.GetString(0));
            return results;
        }

        var paging = (count, offset) switch
        {
            (not null, not null) => "
    (case db
        ((PostgreSql Sqlite)
            ($" $\" LIMIT {count} OFFSET {offset}\","))
        ((SqlServer)
            ($" $\" OFFSET {offset} ROWS FETCH NEXT {count} ROWS ONLY\",")))
"
            (not null, null) => "
    (case db
        ((PostgreSql Sqlite)
            ($" $\" LIMIT {count}\","))
        ((SqlServer)
            ($" $\" OFFSET 0 ROWS FETCH NEXT {count} ROWS ONLY\",")))
"
            (null, not null) => "
    (case db
        ((PostgreSql Sqlite)
            ($" $\" LIMIT -1 OFFSET {offset}\","))
        ((SqlServer)
            ($" $\" OFFSET {offset} ROWS\",")))
"
            _ => \"\"
        };

        if (property == \"_id\")
        {
            var (paramTable0, paramColumn0, paramIsToken0) = ResolveSearchTable(param);
            if (paramTable0 == null) return new List<""string>();

            string whereSql0;
            var whereArgs0 = new List<""object>();
            if (paramIsToken0 && value.Contains('|'))
            {
                var pipe = value.IndexOf('|');
                var sys = pipe == 0 ? null : value.Substring(0, pipe);
                var code = pipe == value.Length - 1 ? null : value.Substring(pipe + 1);
                var conds = new List<""string>();
                if (sys != null) { conds.Add($\"system = {P(conds.Count + 1)}\"); whereArgs0.Add(sys); }
                if (code != null) { conds.Add($\"value = {P(conds.Count + 1)}\"); whereArgs0.Add(code); }
                whereSql0 = string.Join(\" AND \", conds);
            }
            else
            {
                whereSql0 = $\"{paramColumn0} IS NOT NULL AND LOWER({paramColumn0}) LIKE {P(1)}\";
                whereArgs0.Add($\"{value.ToLowerInvariant()}%\");
            }

            await using var cmd4 = new CmdType(
                $\"SELECT DISTINCT resource_id FROM "(tbl-interp schema "paramTable0" db)" WHERE {whereSql0} ORDER BY resource_id{paging}\", conn);
"
    (bind-args-list "cmd4" "whereArgs0" db)
"
            var results4 = new List<""string>();
            await using var reader4 = await cmd4.ExecuteReaderAsync();
            while (await reader4.ReadAsync())
                results4.Add(reader4.GetString(0));
            return results4;
        }

        if (param == property)
        {
            string whereSql1;
            var whereArgs1 = new List<""object>();
            if (propIsToken && value.Contains('|'))
            {
                var pipe = value.IndexOf('|');
                var sys = pipe == 0 ? null : value.Substring(0, pipe);
                var code = pipe == value.Length - 1 ? null : value.Substring(pipe + 1);
                var conds = new List<""string>();
                if (sys != null) { conds.Add($\"system = {P(conds.Count + 1)}\"); whereArgs1.Add(sys); }
                if (code != null) { conds.Add($\"value = {P(conds.Count + 1)}\"); whereArgs1.Add(code); }
                whereSql1 = string.Join(\" AND \", conds);
            }
            else
            {
                whereSql1 = $\"{propColumn} IS NOT NULL AND LOWER({propColumn}) LIKE {P(1)}\";
                whereArgs1.Add($\"{value.ToLowerInvariant()}%\");
            }

            await using var cmd2 = new CmdType(
                $\"SELECT DISTINCT {propColumn} FROM "(tbl-interp schema "propTable" db)" WHERE {whereSql1} ORDER BY {propColumn}{paging}\", conn);
"
    (bind-args-list "cmd2" "whereArgs1" db)
"
            var results2 = new List<""string>();
            await using var reader2 = await cmd2.ExecuteReaderAsync();
            while (await reader2.ReadAsync())
                results2.Add(reader2.GetString(0));
            return results2;
        }

        var (paramTable, paramColumn, paramIsToken) = ResolveSearchTable(param);
        if (paramTable == null) return new List<""string>();

        string whereSql2;
        var whereArgs2 = new List<""object>();
        if (paramIsToken && value.Contains('|'))
        {
            var pipe = value.IndexOf('|');
            var sys = pipe == 0 ? null : value.Substring(0, pipe);
            var code = pipe == value.Length - 1 ? null : value.Substring(pipe + 1);
            var conds = new List<""string>();
            if (sys != null) { conds.Add($\"system = {P(conds.Count + 1)}\"); whereArgs2.Add(sys); }
            if (code != null) { conds.Add($\"value = {P(conds.Count + 1)}\"); whereArgs2.Add(code); }
            whereSql2 = string.Join(\" AND \", conds);
        }
        else
        {
            whereSql2 = $\"{paramColumn} IS NOT NULL AND LOWER({paramColumn}) LIKE {P(1)}\";
            whereArgs2.Add($\"{value.ToLowerInvariant()}%\");
        }

        await using var cmd3 = new CmdType(
            $\"SELECT DISTINCT {propColumn} FROM "(tbl-interp schema "propTable" db)" WHERE resource_id IN (SELECT resource_id FROM "(tbl-interp schema "paramTable" db)" WHERE {whereSql2}) AND {propColumn} IS NOT NULL ORDER BY {propColumn}{paging}\", conn);
"
    (bind-args-list "cmd3" "whereArgs2" db)
"
        var results3 = new List<""string>();
        await using var reader3 = await cmd3.ExecuteReaderAsync();
        while (await reader3.ReadAsync())
            results3.Add(reader3.GetString(0));
        return results3;
    }

"))

; ===================================================================
; Section: CreateSearchAsync
; ===================================================================

; DDL for a search table (C# string context)
(define (search-table-ddl schema tn stype rn db)
    ($ (db-create-table db) (tbl schema tn db)
       " (id INT NOT NULL, resource_id " (db-id-type db)
       " NOT NULL REFERENCES " (tbl-public rn db) "(id) ON DELETE CASCADE, "
       (case stype
           (("string") ($"value " (db-search-text-type db)))
           (("token") ($"system " (db-search-text-type db) ", value " (db-search-text-type db) ", display " (db-search-display-type db)))
           (("date") ($"value_low " (db-timestamp-type db) ", value_high " (db-timestamp-type db)))
           (("reference") ($"target_type " (db-search-text-type db) ", target_id " (db-search-text-type db) ", target_version " (db-search-display-type db)))
           (("number") ($"value " (db-type "decimal" db)))
           (("quantity") ($"value " (db-type "decimal" db) ", system " (db-search-text-type db) ", code " (db-search-text-type db) ", unit " (db-search-text-type db)))
           (else ""))
       ", PRIMARY KEY (id, resource_id))"))

; Index DDL for a search table (C# string context)
(define (search-index-ddl schema tn stype sn db)
    (let ((ci (db-create-index db)))
    (case stype
        (("string") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"))
        (("token") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"
                     (if (or (string=? "identifier" sn) (string=? "language" sn))
                         ($"; " ci "ix_" schema "_" tn "_system ON " (tbl schema tn db) " (system)")
                         "")))
        (("date") ($"; " ci "ix_" schema "_" tn "_value_low ON " (tbl schema tn db) " (value_low)"
                    "; " ci "ix_" schema "_" tn "_value_high ON " (tbl schema tn db) " (value_high)"))
        (("reference") ($"; " ci "ix_" schema "_" tn "_reference ON " (tbl schema tn db) " (target_id, target_type)"))
        (("number" "quantity") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"))
        (else ""))))

; Re-index call for a search parameter
(define (search-reindex-call rn stype sn search hn-all)
    (let ((tn (search-table-name search)))
    (case stype
        (("string")
            ($"                    await InsertStringIndexAsync(conn, tx!, \""tn"\", id, "(repo-string-extractor-call rn search hn-all)");
"))
        (("token")
            (cond
                ((search-is-cast? search)
                    ($"                    await InsertTokenIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))
                ((and (search-is-contactpoint? search) (string=? "telecom" sn))
                    ($"                    await InsertTokenIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.ExtractTelecom(content));
"))
                ((search-is-contactpoint? search)
                    ($"                    await InsertTokenIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.ExtractTelecom(content, \""sn"\"));
"))
                ((search-has-variants? search)
                    (let ((method ($ "Extract"(kebab-to-pascal sn)))
                          (var-name (string-replace sn "-" "_")))
                    ($"                    var "var-name" = "rn"SearchExtractor."method"(content);
                    if ("var-name".HasValue)
                        await InsertTokenIndexAsync(conn, tx!, \""tn"\", id, new List<(string?, string?)> { "var-name".Value });
")))
                (else
                    ($"                    await InsertTokenIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))))
        (("date")
            (let ((method ($ "Extract"(kebab-to-pascal sn)))
                  (var-name (string-replace sn "-" "_")))
            ($"                    var "var-name" = "rn"SearchExtractor."method"(content);
                    if ("var-name".HasValue)
                        await InsertDateIndexAsync(conn, tx!, \""tn"\", id, "var-name".Value.low, "var-name".Value.high);
")))
        (("reference")
            (let ((parts (search-path-parts search)))
            (if (and (<= (length parts) 1) (not (search-is-cast? search)))
                (let* ((prop (search-property search))
                       (fhir-field (if prop (% "name" prop) sn)))
                ($"                    await InsertReferenceIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.ExtractReference(content, \""fhir-field"\"));
"))
                ($"                    await InsertReferenceIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))))
        (("number")
            ($"                    await InsertNumberIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))
        (("quantity")
            ($"                    await InsertQuantityIndexAsync(conn, tx!, \""tn"\", id, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))
        (else ""))))

; Deconstruction of the inline tuple that keeps only one of its values: the backfill of a
; single column has no use for the others, and naming them would leave unread locals behind.
(define (repo-destructure-inline-one inline-list target)
    (if (= 1 (length inline-list))
        ($"var "(search-query-camel target))
        ($"var ("
            (let loop ((items inline-list) (first? #t))
                (if (null? items)""
                    ($ (if first? "" ", ")
                       (if (string=? (% "name" (car items)) (% "name" target)) (search-query-camel target) "_")
                       (loop (cdr items) #f))))
            ")")))

; AddWithValue line at the indent of the backfill loop.
(define (add-param-line-20 cmd-name n expr db)
    (case db
        ((PostgreSql) ($"                    " cmd-name ".Parameters.AddWithValue(" expr ");"))
        ((SqlServer Sqlite) ($"                    " cmd-name ".Parameters.AddWithValue(\""(pp n db)"\", " expr ");"))))

; An inline search parameter is a column on the resource table, so there is no table to create -
; the column comes with the schema or, for one added later, with the upgrade DDL. What is left is
; the fill, and it matters just as much as it does for a subtable: an inline parameter added to an
; existing installation would otherwise read NULL for every row written before it and silently
; drop those rows out of every filter and sort over it.
(define (repo-create-search-inline rn inline-list db)
    (apply $ (map (lambda (s)
        (let ((sn (% "name" s))
              (var (search-query-camel s))
              (col (db-quote-col-cs (search-col s) db)))
        ($"            case \""sn"\":
            {
                var resources = new List<""(string id, JsonNode content)>();
                await using (var readCmd = "(new-cmd db)"
                    \"SELECT id, content FROM "(tbl-public rn db)"\", conn, tx))
                {
                    await using var reader = await readCmd.ExecuteReaderAsync();
                    while (await reader.ReadAsync())
                        resources.Add((reader.GetString(0), JsonNode.Parse(reader.GetFieldValue<""byte[]>(1))!));
                }
                foreach (var (id, content) in resources)
                {
                    "(repo-destructure-inline-one inline-list s)" = "rn"SearchExtractor.ExtractInlineParams(content);
                    await using var updCmd = "(new-cmd db)"
                        \"UPDATE "(tbl-public rn db)" SET "col" = "(pp 1 db)" WHERE id = "(pp 2 db)"\", conn, tx);
"(add-param-line-20 "updCmd" 1 ($"(object?)"var" ?? DBNull.Value") db)"
"(add-param-line-20 "updCmd" 2 "id" db)"
                    await updCmd.ExecuteNonQueryAsync();
                }
                break;
            }
"       ))) inline-list)))

; CreateSearchAsync and DropSearchAsync join a DbTransactionScope like the write methods: a
; versioned upgrade step or the one-time data migration adds a search parameter within its
; transaction, so the index is built from the uncommitted content and rolls back with the step.
(define (repo-create-search rn schema all-table-search-list inline-list string-searches token-searches date-searches ref-searches number-searches hn-all db)
    ($
"    public async Task CreateSearchAsync(string parameter)
    {
"   (open-conn-scoped db)"
        switch (parameter)
        {
"   (apply $ (map (lambda (s)
        (let* ((sn (% "name" s))
               (stype (% "type" s))
               (tn (search-table-name s)))
        ($"            case \""sn"\":
            {
                await using (var cmd = "(new-cmd db)"
                    \""(search-table-ddl schema tn stype rn db)(search-index-ddl schema tn stype sn db)"\", conn, tx))
                    await cmd.ExecuteNonQueryAsync();
                var resources = new List<""(string id, JsonNode content)>();
                await using (var readCmd = "(new-cmd db)"
                    \"SELECT id, content FROM "(tbl-public rn db)"\", conn, tx))
                {
                    await using var reader = await readCmd.ExecuteReaderAsync();
                    while (await reader.ReadAsync())
                        resources.Add((reader.GetString(0), JsonNode.Parse(reader.GetFieldValue<""byte[]>(1))!));
                }
                foreach (var (id, content) in resources)
                {
"                   (search-reindex-call rn stype sn s hn-all)
"                }
                break;
            }
"       ))) all-table-search-list))
   (repo-create-search-inline rn inline-list db)
"            default:
                throw new ArgumentException($\"Unknown search parameter: {parameter}\");
        }
    }

"))

; ===================================================================
; Section: DropSearchAsync
; ===================================================================

; The table name is selected through a whitelist switch over the known
; search-parameter names (never interpolated from the caller-supplied value)
; so an attacker-controlled "parameter" cannot be injected into the DDL.
(define (repo-drop-search schema all-table-search-list db)
    ($
"    public async Task DropSearchAsync(string parameter)
    {
"   (open-conn-scoped db)"
        switch (parameter)
        {
"   (apply $ (map (lambda (s)
        (let* ((sn (% "name" s))
               (tn (search-table-name s)))
        ($"            case \""sn"\":
                await using (var cmd = "(new-cmd db)"
                    \"DROP TABLE IF EXISTS "(tbl schema tn db)"\", conn, tx))
                    await cmd.ExecuteNonQueryAsync();
                break;
"       ))) all-table-search-list))
"            default:
                throw new ArgumentException($\"Unknown search parameter: {parameter}\");
        }
    }

"))

; ===================================================================
; Section: PurgeAsync
; ===================================================================

; A resource counts as deleted when it has history but no row in the live table - the same
; test UndeleteAllAsync uses. One that was restored with $undelete, or recreated under the
; same id, is therefore left alone with its history intact: the previous version selected
; every id that had ever seen a DELETE and stripped the past off resources that are very
; much alive. 'before' bounds the selection by the instant of the deletion - the newest
; history_timestamp of that id, the moment the DELETE row was written - which is what gives
; an operator a grace period in which an accidental delete can still be taken back. Not
; last_updated: the DELETE row copies it from the live row, so it names the resource's last
; change, and a resource written once and deleted days later was purged at the next run.
(define (purge-before-param db)
    (case db
        ((PostgreSql) "new NpgsqlParameter { Value = before.Value, NpgsqlDbType = NpgsqlDbType.TimestampTz }")
        ((SqlServer) "new SqlParameter(\"@p1\", before.Value) { SqlDbType = SqlDbType.DateTime2 }")
        ((Sqlite) "new SqliteParameter(\"@p1\", before.Value)")))

(define (repo-purge rn schema has-history db)
    (if has-history
        ($
"    public async Task<""int> PurgeAsync(DateTime? before)
    {
"   (open-conn db)"

        var selection =
            \"SELECT h.id FROM "(tbl schema "resource_history" db)" h \" +
            \"LEFT JOIN "(tbl-public rn db)" r ON h.id = r.id \" +
            \"WHERE r.id IS NULL AND h.history_operation = 'DELETE' \" +
            \"GROUP BY h.id\";
        if (before != null)
            selection += \" HAVING MAX(h.history_timestamp) < "(pp 1 db)"\";

        // Counted before the delete and over ids, not rows: the number an operator cares
        // about is how many resources went, not how many versions they had between them.
        int count;
        await using (var cmd = "(new-cmd db)"$\"SELECT COUNT(*) FROM ({selection}) t\", conn))
        {
            if (before != null)
                cmd.Parameters.Add("(purge-before-param db)");
            count = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        if (count == 0)
            return 0;

        await using (var cmd = "(new-cmd db)"$\"DELETE FROM "(tbl schema "resource_history" db)" WHERE id IN ({selection})\", conn))
        {
            if (before != null)
                cmd.Parameters.Add("(purge-before-param db)");
            await cmd.ExecuteNonQueryAsync();
        }

        return count;
    }

    // -- Private helpers --

")
        ($
"    public async Task<""int> PurgeAsync(DateTime? before)
    {
        return await Task.FromResult(0);
    }

    // -- Private helpers --

")))

; ===================================================================
; Section: ReadResource helper
; ===================================================================

; Every timestamp reaches the database as DateTime.UtcNow, but only PostgreSQL gives it back
; that way: TIMESTAMPTZ carries the zone, so Npgsql returns Kind=Utc. SQL Server's DATETIME2 and
; SQLite's TEXT carry none, so the driver hands back Kind=Unspecified - the right instant with no
; label on it. Formatting that with "zzz" then stamps the server's local offset onto digits that
; are already UTC (an hour on the clock in Berlin becomes an hour that never happened), and
; ToUniversalTime() subtracts that offset outright. Both are what the Last-Modified header, the
; If-Modified-Since comparison and a history bundle's response.lastModified are built from, so
; every one of them ran up to a full offset early on those two backends. Saying Kind=Utc here is
; not a conversion, it is restoring the fact that got lost in the column - and on PostgreSQL it
; changes nothing.
(define (repo-utc-helper)
"    private static DateTime Utc(DateTime value) => DateTime.SpecifyKind(value, DateTimeKind.Utc);

")

(define (repo-read-resource-helper rn inline-list has-history db)
    ($
    (repo-utc-helper)
"    private static "rn"Resource Read"rn"Resource(ReaderType reader)
    {
        return new "rn"Resource(
            reader.GetString(0),
            reader.GetInt32(1),
            Utc(reader.GetDateTime(2)),
            reader.GetFieldValue<""byte[]>(3)"(repo-reader-fields inline-list has-history)");
    }

"
    (if (binary-data-table? rn db) ($
"    private static "rn"Resource Read"rn"ResourceWithData(ReaderType reader)
    {
        var resource = Read"rn"Resource(reader);
        if (!reader.IsDBNull("(number->string (+ 4 (length inline-list)))"))
        {
            var bytes = reader.GetFieldValue<""byte[]>("(number->string (+ 4 (length inline-list)))");
            var parsed = JsonNode.Parse(resource.content)!;
            parsed[\"data\"] = Convert.ToBase64String(bytes);
            resource = new "rn"Resource(resource.id, resource.versionId, resource.lastUpdated,
                JsonSerializer.SerializeToUtf8Bytes(parsed));
        }
        return resource;
    }

    private static "rn"HistoryRow Read"rn"HistoryRowWithData(ReaderType reader)
    {
        var content = reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3);
        if (content != null && !reader.IsDBNull(5))
        {
            var bytes = reader.GetFieldValue<""byte[]>(5);
            var parsed = JsonNode.Parse(content)!;
            parsed[\"data\"] = Convert.ToBase64String(bytes);
            content = JsonSerializer.SerializeToUtf8Bytes(parsed);
        }
        return new "rn"HistoryRow(
            reader.GetString(0),
            reader.GetInt32(1),
            Utc(reader.GetDateTime(2)),
            content,
            reader.GetString(4));
    }

") "")))

; ===================================================================
; Section: InsertSearchIndexesAsync
; ===================================================================

(define (repo-insert-search-indexes rn schema string-searches token-searches date-searches instant-searches ref-searches number-searches quantity-searches hn-all db)
    ($
"    private async Task InsertSearchIndexesAsync(ConnType conn, TxType tx, string resourceId, JsonNode content)
    {
"
        ; String indexes
        (if (null? string-searches)""
            ($"        // String indexes
"
            (apply $ (map (lambda (s)
                ($"        await InsertStringIndexAsync(conn, tx, \""(search-table-name s)"\", resourceId, "(repo-string-extractor-call rn s hn-all)");
"))
                string-searches))))
        ; Token indexes
        (if (null? token-searches)""
            ($"
        // Token indexes
"
            (apply $ (map (lambda (s) (repo-token-index-line rn s)) token-searches))))
        ; Date indexes
        (if (null? date-searches)""
            ($"
        // Date indexes
"
            (apply $ (map (lambda (s) (repo-date-index-line rn s)) date-searches))))
        ; Reference indexes
        (if (null? ref-searches)""
            ($"
        // Reference indexes
"
            (apply $ (map (lambda (s) (repo-reference-index-line rn s)) ref-searches))))
        ; Number indexes
        (if (null? number-searches)""
            ($"
        // Number indexes
"
            (apply $ (map (lambda (s) (repo-number-index-line rn s)) number-searches))))
        ; Quantity indexes
        (if (null? quantity-searches)""
            ($"
        // Quantity indexes
"
            (apply $ (map (lambda (s) (repo-quantity-index-line rn s)) quantity-searches))))
"    }

"))

; Quantity index line for InsertSearchIndexesAsync
(define (repo-quantity-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search)))
        ($"        await InsertQuantityIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
")))

; The insert helper of a quantity table: emitted for a repository that has one.
(define (repo-insert-quantity-helper schema db)
    ($
"    private static async Task InsertQuantityIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""(decimal value, string? system, string? code, string? unit)> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = new CmdType(
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, value, system, code, unit) VALUES ({P(1)}, {P(2)}, {P(3)}, {P(4)}, {P(5)}, {P(6)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewParam(3, values[i].value));
            cmd.Parameters.Add(NewParam(4, (object?)values[i].system ?? DBNull.Value));
            cmd.Parameters.Add(NewParam(5, (object?)values[i].code ?? DBNull.Value));
            cmd.Parameters.Add(NewParam(6, (object?)values[i].unit ?? DBNull.Value));
            await cmd.ExecuteNonQueryAsync();
        }
    }

"))

; Number index line for InsertSearchIndexesAsync
(define (repo-number-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search)))
        ($"        await InsertNumberIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
")))

; Token index line for InsertSearchIndexesAsync
(define (repo-token-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search)))
        (cond
            ; One type of a choice: a list, as the others at the end
            ((search-is-cast? search)
                ($"        await InsertTokenIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))
            ; ContactPoint/telecom (no filter)
            ((and (search-is-contactpoint? search) (string=? "telecom" sn))
                ($"        await InsertTokenIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.ExtractTelecom(content));
"))
            ; ContactPoint with filter (email, phone)
            ((search-is-contactpoint? search)
                ($"        await InsertTokenIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.ExtractTelecom(content, \"" sn "\"));
"))
            ; Variant (returns nullable single)
            ((search-has-variants? search)
                (let ((method ($ "Extract"(kebab-to-pascal sn)))
                      (var-name (string-replace sn "-" "_")))
                    ($"        var " var-name " = "rn"SearchExtractor." method "(content);
        if (" var-name ".HasValue)
            await InsertTokenIndexAsync(conn, tx, \"" tn "\", resourceId, new List<(string?, string?)> { " var-name ".Value });
")))
            ; All others (identifier, codeableconcept, code-thru-coll, etc.)
            (else
                ($"        await InsertTokenIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
")))))

; Date index line for InsertSearchIndexesAsync
(define (repo-date-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search))
           (method ($ "Extract"(kebab-to-pascal sn)))
           (var-name (string-replace sn "-" "_")))
        ($"        var " var-name " = "rn"SearchExtractor." method "(content);
        if (" var-name ".HasValue)
            await InsertDateIndexAsync(conn, tx, \"" tn "\", resourceId, " var-name ".Value.low, " var-name ".Value.high);
")))

; Reference index line for InsertSearchIndexesAsync
(define (repo-reference-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search))
           (parts (search-path-parts search)))
        (if (and (<= (length parts) 1) (not (search-is-cast? search)))
            ; Direct reference → ExtractReference(content, "fhirFieldName")
            (let* ((prop (search-property search))
                   (fhir-field (if prop (% "name" prop) sn)))
                ($"        await InsertReferenceIndexAsync(conn, tx, \"" tn "\", resourceId,
            "rn"SearchExtractor.ExtractReference(content, \"" fhir-field "\"));
"))
            ; Backbone reference
            ($"        await InsertReferenceIndexAsync(conn, tx, \"" tn "\", resourceId,
            "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
"))))

; ===================================================================
; Section: DeleteSearchIndexesAsync
; ===================================================================

(define (repo-delete-search-indexes schema all-table-search-list db)
    ($
"    private static async Task DeleteSearchIndexesAsync(ConnType conn, TxType tx, string resourceId)
    {
        var tables = new string[]
        {
"
        (let loop ((items all-table-search-list) (first? #t))
            (if (null? items)""
                (let ((tn (search-table-name (car items))))
                    ($ (if first? "" ",
")
                       "            \"" tn "\""
                       (loop (cdr items) #f)))))
"
        };

        foreach (var table in tables)
        {
            await using var cmd = new CmdType($\"DELETE FROM "(tbl-interp schema "table" db)" WHERE resource_id = {P(1)}\", conn, tx);
            cmd.Parameters.Add(NewParam(1, resourceId));
            await cmd.ExecuteNonQueryAsync();
        }
    }

"))

; ===================================================================
; Section: Insert helper methods
; ===================================================================

(define (repo-insert-helpers schema db)
    ($
"    private static async Task InsertStringIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""string> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = new CmdType(
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, value) VALUES ({P(1)}, {P(2)}, {P(3)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewParam(3, values[i]));
            await cmd.ExecuteNonQueryAsync();
        }
    }

    private static async Task InsertTokenIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<(string? system, string? value)> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = new CmdType(
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, system, value) VALUES ({P(1)}, {P(2)}, {P(3)}, {P(4)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewParam(3, (object?)values[i].system ?? DBNull.Value));
            cmd.Parameters.Add(NewParam(4, (object?)values[i].value ?? DBNull.Value));
            await cmd.ExecuteNonQueryAsync();
        }
    }

    private static async Task InsertDateIndexAsync(ConnType conn, TxType tx, string table, string resourceId, DateTime low, DateTime high)
    {
        await using var cmd = new CmdType(
            $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, value_low, value_high) VALUES (0, {P(1)}, {P(2)}, {P(3)})\", conn, tx);
        cmd.Parameters.Add(NewParam(1, resourceId));
        cmd.Parameters.Add(NewTsParam(2, low));
        cmd.Parameters.Add(NewTsParam(3, high));
        await cmd.ExecuteNonQueryAsync();
    }

    private static async Task InsertReferenceIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<(string? type, string? id, string? version)> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = new CmdType(
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, target_type, target_id, target_version) VALUES ({P(1)}, {P(2)}, {P(3)}, {P(4)}, {P(5)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewParam(3, (object?)values[i].type ?? DBNull.Value));
            cmd.Parameters.Add(NewParam(4, (object?)values[i].id ?? DBNull.Value));
            cmd.Parameters.Add(NewParam(5, (object?)values[i].version ?? DBNull.Value));
            await cmd.ExecuteNonQueryAsync();
        }
    }

    private static async Task InsertNumberIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""decimal?> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = new CmdType(
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, value) VALUES ({P(1)}, {P(2)}, {P(3)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewParam(3, (object?)values[i] ?? DBNull.Value));
            await cmd.ExecuteNonQueryAsync();
        }
    }

"))

; ===================================================================
; Section: Search helper methods
; ===================================================================

(define (repo-search-helpers rn schema inline-list all-table-search-list string-searches token-searches date-searches number-searches ref-searches db)
    ($
"    // -- Search helpers --

    private static string NextAlias(ref int aliasIndex) => $\"s{aliasIndex++}\";

"
        (repo-build-order-by schema inline-list all-table-search-list string-searches token-searches date-searches number-searches ref-searches db)
        (repo-add-missing-search schema inline-list all-table-search-list db)
        (repo-add-not-search rn schema inline-list string-searches
            (filter (lambda (s) (string=? "token" (% "type" s))) all-table-search-list) db)
    ))

; --- Chained sort helpers ---
; Emit switch cases for a target resource:
;   "refName:TargetRn.inlineParam"   → subquery joining target main table
;   "refName:TargetRn._lastUpdated"  → subquery joining target main table
;   "refName:TargetRn.tableParam"    → subquery joining target search-index table
(define (chain-sort-cases-for-target src-schema src-tbl src-name tgt-rn tgt-elem db)
    (let* ((tgt-inline (node-list->list (inline-searches tgt-elem)))
           (tgt-table-s (node-list->list (table-searches tgt-elem)))
           (tgt-sortable-table (filter (lambda (s)
               (let ((t (% "type" s)))
                   (or (string=? "string" t)
                       (string=? "token" t)
                       (string=? "date" t)
                       (string=? "number" t)))) tgt-table-s)))
      ($
        ; Inline searches on target
        (apply $ (map (lambda (s)
            ($"                \""src-name":"tgt-rn"."(% "name" s)"\" => \"("
               (db-select-top1 db)" t."(db-quote-col-cs (search-col s) db)
               " FROM "(tbl src-schema src-tbl db)" src JOIN "(tbl-public tgt-rn db)" t ON t.id = src.target_id"
               " WHERE src.resource_id = r.id"(db-trailing-limit1 db)")\",
")) tgt-inline))
        ; _lastUpdated on target
        ($"                \""src-name":"tgt-rn"._lastUpdated\" => \"("
           (db-select-top1 db)" t.last_updated"
           " FROM "(tbl src-schema src-tbl db)" src JOIN "(tbl-public tgt-rn db)" t ON t.id = src.target_id"
           " WHERE src.resource_id = r.id"(db-trailing-limit1 db)")\",
")
        ; Table searches on target (string/token → value, date → value_low, number → value)
        (apply $ (map (lambda (s)
            (let ((col (if (string=? "date" (% "type" s)) "value_low" "value")))
              ($"                \""src-name":"tgt-rn"."(% "name" s)"\" => \"("
                 (db-select-top1 db)" c."col
                 " FROM "(tbl src-schema src-tbl db)" src JOIN "(tbl tgt-rn (search-table-name s) db)" c ON c.resource_id = src.target_id"
                 " WHERE src.resource_id = r.id"(db-trailing-limit1 db)")\",
"))) tgt-sortable-table)))))

; Emit chain-sort switch cases for one reference search param of the source resource
(define (chain-sort-cases-for-ref src-schema ref-search db)
    (let* ((src-tbl (search-table-name ref-search))
           (src-name (% "name" ref-search))
           (paths-node (select-children "paths" ref-search))
           (path-nodes (if (node-list-empty? paths-node) (empty-node-list)
                          (select-children "path" (node-list-first paths-node))))
           (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
           (targets-node (if first-path (select-children "targets" first-path) (empty-node-list)))
           (target-nodes (if (node-list-empty? targets-node) '()
                            (node-list->list (children (node-list-first targets-node))))))
      (apply $ (map (lambda (tgt)
          (let* ((tgt-rn (% "resource" tgt))
                 (tgt-elem (if tgt-rn (element-with-id (downcase-string tgt-rn)) #f)))
            (if (and tgt-elem (active? tgt-elem))
                (chain-sort-cases-for-target src-schema src-tbl src-name tgt-rn tgt-elem db)
                "")))
          target-nodes))))

; --- BuildOrderBy is defined in orderby.scm (per-project; see map.dsl) ---

; --- AddMissingSearch ---
(define (repo-add-missing-search schema inline-list all-table-search-list db)
    ($
"    private static void AddMissingSearch(string key, bool isMissing, List<""string> joins, List<""string> wheres, ref int aliasIndex)
    {
        switch (key)
        {
"
        ; Inline params → direct column IS NULL / IS NOT NULL
        (apply $ (map (lambda (s)
            ($"            case \""(% "name" s)"\":
                wheres.Add(isMissing ? \"r."(db-quote-col-cs (search-col s) db)" IS NULL\" : \"r."(db-quote-col-cs (search-col s) db)" IS NOT NULL\");
                break;
"))
            inline-list))
"            case \"_lastUpdated\":
                wheres.Add(isMissing ? \"r.last_updated IS NULL\" : \"r.last_updated IS NOT NULL\");
                break;
"
"            default:
            {
                var table = key switch
                {
"
        ; All table searches
        (apply $ (map (lambda (s)
            ($"                    \""(% "name" s)"\" => \""(search-table-name s)"\",
"))
            all-table-search-list))
"                    _ => (string?)null
                };
                if (table == null) break;
                var alias = NextAlias(ref aliasIndex);
                if (isMissing)
                {
                    joins.Add($\"LEFT JOIN "(tbl-interp schema "table" db)" {alias} ON {alias}.resource_id = r.id\");
                    wheres.Add($\"{alias}.resource_id IS NULL\");
                }
                else
                {
                    joins.Add($\"JOIN "(tbl-interp schema "table" db)" {alias} ON {alias}.resource_id = r.id\");
                }
                break;
            }
        }
    }

"))

; --- AddNotSearch ---
(define (repo-add-not-search rn schema inline-list string-searches token-searches db)
    ($
"    private static void AddNotSearch(string key, string value, List<""string> joins, List<""string> wheres,
        List<""ParamType> parameters, ref int paramIndex, ref int aliasIndex)
    {
        switch (key)
        {
"
        ; Inline params
        (apply $ (map (lambda (s)
            (let* ((sn (% "name" s))
                   (col (db-quote-col-cs (search-col s) db))
                   (prop (search-property s))
                   (prop-type (if prop (% "type" prop)"code")))
                (cond
                    ((string=? "boolean" prop-type)
                        ($"            case \"" sn "\":
                wheres.Add($\"(r." col " IS NULL OR r." col " != {P(paramIndex)})\");
                parameters.Add(NewParam(paramIndex, bool.Parse(value)));
                paramIndex++;
                break;
"))
                    ((member prop-type '("integer" "positiveInt" "unsignedInt"))
                        ($"            case \"" sn "\":
                if (!int.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out int "(search-col s)"NotParsed))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '" sn "'\");
                wheres.Add($\"(r." col " IS NULL OR r." col " != {P(paramIndex)})\");
                parameters.Add(NewParam(paramIndex, "(search-col s)"NotParsed));
                paramIndex++;
                break;
"))
                    ((string=? "integer64" prop-type)
                        ($"            case \"" sn "\":
                if (!long.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out long "(search-col s)"NotParsed))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '" sn "'\");
                wheres.Add($\"(r." col " IS NULL OR r." col " != {P(paramIndex)})\");
                parameters.Add(NewParam(paramIndex, "(search-col s)"NotParsed));
                paramIndex++;
                break;
"))
                    ((string=? "decimal" prop-type)
                        ($"            case \"" sn "\":
                if (!decimal.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out decimal "(search-col s)"NotParsed))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '" sn "'\");
                wheres.Add($\"(r." col " IS NULL OR r." col " != {P(paramIndex)})\");
                parameters.Add(NewParam(paramIndex, "(search-col s)"NotParsed));
                paramIndex++;
                break;
"))
                    (else
                        ($"            case \"" sn "\":
                wheres.Add($\"(r." col " IS NULL OR r." col " != {P(paramIndex)})\");
                parameters.Add(NewParam(paramIndex, value));
                paramIndex++;
                break;
")))))
            inline-list))
"            default:
            {
                var stringTable = key switch
                {
"
        ; String searches for :not
        (apply $ (map (lambda (s)
            ($"                    \""(% "name" s)"\" => \""(search-table-name s)"\",
"))
            string-searches))
"                    _ => (string?)null
                };
                if (stringTable != null)
                {
                    var alias = NextAlias(ref aliasIndex);
                    joins.Add($\"LEFT JOIN "(tbl-interp schema "stringTable" db)" {alias} ON {alias}.resource_id = r.id AND LOWER({alias}.value) LIKE {P(paramIndex)}\");
                    wheres.Add($\"{alias}.resource_id IS NULL\");
                    parameters.Add(NewParam(paramIndex, $\"{value.ToLowerInvariant()}%\"));
                    paramIndex++;
                    break;
                }
                var tokenTable = key switch
                {
"
        ; Token searches for :not
        (apply $ (map (lambda (s)
            ($"                    \""(% "name" s)"\" => \""(search-table-name s)"\",
"))
            token-searches))
"                    _ => (string?)null
                };
                if (tokenTable != null)
                {
                    SearchQueryBuilder.AddTokenNotSearch(\""rn"\", joins, wheres, parameters, ref paramIndex, ref aliasIndex, tokenTable, value);
                    break;
                }
                break;
            }
        }
    }

"))


; ===================================================================
; Section: SearchQueryBuilder (one per backend, generated from resources)
; ===================================================================

(define (search-query-builders)
    (make directory path: "Repositories"
    (apply sosofo-append
        (map (lambda (db)
            (make directory path: (db-folder db)
            (file "SearchQueryBuilder.cs"
                ($ (sqb-header db) (sqb-body db)"}
"))))
            target-databases))))

(define (sqb-header db)
    ($
"using "(db-using db)";
"
    (case db
        ((PostgreSql)"using NpgsqlTypes;
using ParamType = Npgsql.NpgsqlParameter;
")
        ((SqlServer)"using System.Data;
using ParamType = Microsoft.Data.SqlClient.SqlParameter;
")
        ((Sqlite)"using ParamType = Microsoft.Data.Sqlite.SqliteParameter;
"))
"
namespace "(project-name)".Repositories."(db-namespace db)";

internal static class SearchQueryBuilder
{
"))

(define (sqb-body db)
    ($
    (sqb-schema-table db)
    (sqb-helpers db)
    (sqb-add-string-search-or db)
    (sqb-add-token-search-or db)
    (sqb-add-token-not-search db)
    (sqb-add-date-search-or db)
    (sqb-add-inline-instant-search db)
    (sqb-add-inline-number-searches db)
    (sqb-add-number-search-or db)
    (if (any-quantity-table-search?) (sqb-add-quantity-search-or db) "")
    (sqb-add-reference-search-or db)))

(define (sqb-schema-table db)
    (case db
        ((PostgreSql)
"    private static string SchemaTable(string schema, string table) => $\"\\\"\" + schema + \"\\\"\" + \".\" + table;

")
        ((SqlServer)
"    private static string SchemaTable(string schema, string table) => $\"[{schema}].{table}\";

")
        ((Sqlite)
"    private static string SchemaTable(string schema, string table) => $\"{schema}_{table}\";

")))

(define (sqb-helpers db)
    ($
"    private static string P(int n) => $\""
    (pp-prefix db)
    "{n}\";

    private static string NextAlias(ref int aliasIndex) => $\"s{aliasIndex++}\";
"
    (case db
        ((PostgreSql)
"
    private static ParamType NewParam(int index, object value) => new ParamType { Value = value };
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType { Value = value, NpgsqlDbType = NpgsqlDbType.TimestampTz };

")
        ((SqlServer)
"
    private static ParamType NewParam(int index, object value) => new ParamType($\"@p{index}\", value);
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType($\"@p{index}\", value) { SqlDbType = SqlDbType.DateTime2 };

")
        ((Sqlite)
"
    private static ParamType NewParam(int index, object value) => new ParamType($\"@p{index}\", value);
    private static ParamType NewTsParam(int index, DateTime value) => new ParamType($\"@p{index}\", value);

"))))

(define (sqb-add-string-search-or db)
    ($
"    internal static void AddStringSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values, string modifier)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            switch (modifier)
            {
                case \"exact\":
                    orParts.Add($\"{alias}.value = {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, value));
                    paramIndex++;
                    break;
                case \"contains\":
                    orParts.Add($\"LOWER({alias}.value) LIKE {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, $\"%{value.ToLowerInvariant()}%\"));
                    paramIndex++;
                    break;
                default:
                    orParts.Add($\"LOWER({alias}.value) LIKE {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, $\"{value.ToLowerInvariant()}%\"));
                    paramIndex++;
                    break;
            }
        }

        wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

; Token-Werte treffen exakt; der einzige Modifier, der die Wertseite lockert, ist :contains
; (Teilstring, ohne Beachtung der Schreibung, wie bei String-Parametern). Er ist kein FHIR-Standard
; für Token, aber die einzige Form, in der ein Suchfeld über eine E-Mail-Adresse oder Telefonnummer
; so suchen kann wie über einen Namen. Ein System-Anteil (system|value) bleibt auch dann exakt,
; Teilstring gibt es nur auf dem Wert. Jeder andere Modifier trifft weiter exakt.
(define (sqb-add-token-search-or db)
    ($
"    internal static void AddTokenSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values, string modifier = \"\")
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");
        var contains = modifier == \"contains\";

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            if (value.Contains('|'))
            {
                var parts = value.Split('|', 2);
                var conditions = new List<""string>();
                if (!string.IsNullOrEmpty(parts[0]))
                {
                    conditions.Add($\"{alias}.system = {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, parts[0]));
                    paramIndex++;
                }
                if (!string.IsNullOrEmpty(parts[1]))
                {
                    conditions.Add(TokenValueCondition(alias, paramIndex, contains));
                    parameters.Add(NewParam(paramIndex, TokenValueParam(parts[1], contains)));
                    paramIndex++;
                }
                if (conditions.Count > 0)
                    orParts.Add($\"({string.Join(\" AND \", conditions)})\");
            }
            else
            {
                orParts.Add(TokenValueCondition(alias, paramIndex, contains));
                parameters.Add(NewParam(paramIndex, TokenValueParam(value, contains)));
                paramIndex++;
            }
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

    private static string TokenValueCondition(string alias, int paramIndex, bool contains) =>
        contains ? $\"LOWER({alias}.value) LIKE {P(paramIndex)}\" : $\"{alias}.value = {P(paramIndex)}\";

    private static string TokenValueParam(string value, bool contains) =>
        contains ? $\"%{value.ToLowerInvariant()}%\" : value;

"))

(define (sqb-add-token-not-search db)
    ($
"    internal static void AddTokenNotSearch(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string value)
    {
        var alias = NextAlias(ref aliasIndex);
        if (value.Contains('|'))
        {
            var parts = value.Split('|', 2);
            var conditions = new List<""string> { $\"{alias}.resource_id = r.id\" };
            if (!string.IsNullOrEmpty(parts[0]))
            {
                conditions.Add($\"{alias}.system = {P(paramIndex)}\");
                parameters.Add(NewParam(paramIndex, parts[0]));
                paramIndex++;
            }
            if (!string.IsNullOrEmpty(parts[1]))
            {
                conditions.Add($\"{alias}.value = {P(paramIndex)}\");
                parameters.Add(NewParam(paramIndex, parts[1]));
                paramIndex++;
            }
            joins.Add($\"LEFT JOIN {SchemaTable(schema, table)} {alias} ON {string.Join(\" AND \", conditions)}\");
        }
        else
        {
            joins.Add($\"LEFT JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id AND {alias}.value = {P(paramIndex)}\");
            parameters.Add(NewParam(paramIndex, value));
            paramIndex++;
        }
        wheres.Add($\"{alias}.resource_id IS NULL\");
    }

"))

(define (sqb-add-date-search-or db)
    ($
"    internal static void AddDateSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            var prefix = \"eq\";
            var dateStr = value;

            if (value.Length >= 2 && char.IsLetter(value[0]) && char.IsLetter(value[1]))
            {
                prefix = value[..2];
                dateStr = value[2..];
            }

            (DateTime low, DateTime high) range;
            try
            {
                range = FhirDateHelper.ParseFhirDateRange(dateStr);
            }
            catch
            {
                continue;
            }
            var (low, high) = range;

            switch (prefix)
            {
                case \"eq\":
                    orParts.Add($\"({alias}.value_low >= {P(paramIndex)} AND {alias}.value_high <= {P(paramIndex + 1)})\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    parameters.Add(NewTsParam(paramIndex + 1, high));
                    paramIndex += 2;
                    break;
                case \"ne\":
                    orParts.Add($\"NOT ({alias}.value_low >= {P(paramIndex)} AND {alias}.value_high <= {P(paramIndex + 1)})\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    parameters.Add(NewTsParam(paramIndex + 1, high));
                    paramIndex += 2;
                    break;
                case \"gt\":
                    orParts.Add($\"{alias}.value_low > {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, high));
                    paramIndex++;
                    break;
                case \"lt\":
                    orParts.Add($\"{alias}.value_high < {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    paramIndex++;
                    break;
                case \"ge\":
                    orParts.Add($\"{alias}.value_high >= {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    paramIndex++;
                    break;
                case \"le\":
                    orParts.Add($\"{alias}.value_low <= {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, high));
                    paramIndex++;
                    break;
                case \"sa\":
                    orParts.Add($\"{alias}.value_low > {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, high));
                    paramIndex++;
                    break;
                case \"eb\":
                    orParts.Add($\"{alias}.value_high < {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    paramIndex++;
                    break;
            }
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

(define (sqb-add-inline-instant-search db)
    ($
"    internal static void AddInlineInstantSearch(string column, string paramName, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, string[] values)
    {
        var orParts = new List<""string>();
        foreach (var value in values)
        {
            var prefix = \"eq\";
            var dateStr = value;

            if (value.Length >= 2 && char.IsLetter(value[0]) && char.IsLetter(value[1]))
            {
                prefix = value[..2];
                dateStr = value[2..];
            }

            DateTime low, high;
            try { (low, high) = FhirDateHelper.ParseFhirDateRange(dateStr); }
            catch (FormatException) { throw new ArgumentException($\"Invalid value '{value}' for date search parameter '{paramName}'\"); }

            switch (prefix)
            {
                case \"eq\":
                    orParts.Add($\"({column} >= {P(paramIndex)} AND {column} <= {P(paramIndex + 1)})\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    parameters.Add(NewTsParam(paramIndex + 1, high));
                    paramIndex += 2;
                    break;
                case \"gt\":
                    orParts.Add($\"{column} > {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, high));
                    paramIndex++;
                    break;
                case \"lt\":
                    orParts.Add($\"{column} < {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    paramIndex++;
                    break;
                case \"ge\":
                    orParts.Add($\"{column} >= {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, low));
                    paramIndex++;
                    break;
                case \"le\":
                    orParts.Add($\"{column} <= {P(paramIndex)}\");
                    parameters.Add(NewTsParam(paramIndex, high));
                    paramIndex++;
                    break;
            }
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

; Inline number search helpers — one per numeric type.
; FHIR number prefixes supported: eq (default), ne, gt, lt, ge, le.
(define (sqb-add-inline-number-searches db)
    ($
        (sqb-add-inline-number-search "AddInlineIntSearch" "int" "int.TryParse" db)
        (sqb-add-inline-number-search "AddInlineLongSearch" "long" "long.TryParse" db)
        (sqb-add-inline-number-search "AddInlineDecimalSearch" "decimal" "decimal.TryParse" db)))

(define (sqb-add-inline-number-search method cs-type parse-method db)
    ($
"    internal static void " method "(string column, string paramName, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, string[] values)
    {
        var orParts = new List<""string>();
        foreach (var value in values)
        {
            var prefix = \"eq\";
            var numStr = value;

            if (value.Length >= 2 && char.IsLetter(value[0]) && char.IsLetter(value[1])
                && (value.Length == 2 || !char.IsLetter(value[2])))
            {
                prefix = value[..2];
                numStr = value[2..];
            }

            if (!" parse-method "(numStr, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out " cs-type " parsed))
                throw new ArgumentException($\"Invalid value '{value}' for number search parameter '{paramName}'\");

            var op = prefix switch
            {
                \"eq\" => \"=\",
                \"ne\" => \"<"">\",
                \"gt\" => \">\",
                \"lt\" => \"<""\",
                \"ge\" => \">=\",
                \"le\" => \"<""=\",
                _ => throw new ArgumentException($\"Unsupported prefix '{prefix}' for number search parameter '{paramName}'\")
            };

            orParts.Add($\"{column} {op} {P(paramIndex)}\");
            parameters.Add(NewParam(paramIndex, parsed));
            paramIndex++;
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

(define (sqb-add-number-search-or db)
    ($
"    internal static void AddNumberSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            var prefix = \"eq\";
            var numStr = value;

            if (value.Length >= 2 && char.IsLetter(value[0]) && char.IsLetter(value[1])
                && (value.Length == 2 || !char.IsLetter(value[2])))
            {
                prefix = value[..2];
                numStr = value[2..];
            }

            if (!decimal.TryParse(numStr, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out decimal parsed))
                throw new ArgumentException($\"Invalid value '{value}' for number search parameter\");

            var op = prefix switch
            {
                \"eq\" => \"=\",
                \"ne\" => \"<"">\",
                \"gt\" => \">\",
                \"lt\" => \"<""\",
                \"ge\" => \">=\",
                \"le\" => \"<""=\",
                _ => throw new ArgumentException($\"Unsupported prefix '{prefix}' for number search parameter\")
            };

            orParts.Add($\"{alias}.value {op} {P(paramIndex)}\");
            parameters.Add(NewParam(paramIndex, parsed));
            paramIndex++;
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

; Does any active resource of the model have a quantity search? The query
; helper is emitted only then.
(define (any-quantity-table-search?)
    (let loop ((rs (node-list->list (select-elements (descendants (tree-root (current-node))) "resource"))))
        (cond ((null? rs) #f)
              ((and (active? (car rs)) (not (node-list-empty? (quantity-table-searches (car rs))))) #t)
              (else (loop (cdr rs))))))

; Quantity search: [prefix]number, or [prefix]number|system|code.
; Prefixes eq (default), ne, gt, lt, ge, le as for a number, and ap: within
; a tenth of the value. With a system, system and code must both match; with
; a code alone (number||code) the code or the unit may.
(define (sqb-add-quantity-search-or db)
    ($
"    internal static void AddQuantitySearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            var pieces = value.Split('|');
            if (pieces.Length != 1 && pieces.Length != 3)
                throw new ArgumentException($\"Invalid value '{value}' for quantity search parameter: number or number|system|code\");

            var prefix = \"eq\";
            var numStr = pieces[0];

            if (numStr.Length >= 2 && char.IsLetter(numStr[0]) && char.IsLetter(numStr[1])
                && (numStr.Length == 2 || !char.IsLetter(numStr[2])))
            {
                prefix = numStr[..2];
                numStr = numStr[2..];
            }

            if (!decimal.TryParse(numStr, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out decimal parsed))
                throw new ArgumentException($\"Invalid value '{value}' for quantity search parameter\");

            string condition;
            if (prefix == \"ap\")
            {
                var margin = Math.Abs(parsed) * 0.1m;
                condition = $\"{alias}.value >= {P(paramIndex)} AND {alias}.value <""= {P(paramIndex + 1)}\";
                parameters.Add(NewParam(paramIndex, parsed - margin));
                parameters.Add(NewParam(paramIndex + 1, parsed + margin));
                paramIndex += 2;
            }
            else
            {
                var op = prefix switch
                {
                    \"eq\" => \"=\",
                    \"ne\" => \"<"">\",
                    \"gt\" => \">\",
                    \"lt\" => \"<""\",
                    \"ge\" => \">=\",
                    \"le\" => \"<""=\",
                    _ => throw new ArgumentException($\"Unsupported prefix '{prefix}' for quantity search parameter\")
                };
                condition = $\"{alias}.value {op} {P(paramIndex)}\";
                parameters.Add(NewParam(paramIndex, parsed));
                paramIndex++;
            }

            if (pieces.Length == 3 && pieces[2].Length > 0)
            {
                if (pieces[1].Length > 0)
                {
                    condition += $\" AND {alias}.system = {P(paramIndex)} AND {alias}.code = {P(paramIndex + 1)}\";
                    parameters.Add(NewParam(paramIndex, pieces[1]));
                    parameters.Add(NewParam(paramIndex + 1, pieces[2]));
                    paramIndex += 2;
                }
                else
                {
                    condition += $\" AND ({alias}.code = {P(paramIndex)} OR {alias}.unit = {P(paramIndex + 1)})\";
                    parameters.Add(NewParam(paramIndex, pieces[2]));
                    parameters.Add(NewParam(paramIndex + 1, pieces[2]));
                    paramIndex += 2;
                }
            }

            orParts.Add($\"({condition})\");
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

(define (sqb-add-reference-search-or db)
    ($
"    internal static void AddReferenceSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            // the value by its last parts, as the index has a reference
            // (AddReference): Type/id, an absolute URL's base left out, a
            // version behind /_history/ asked for only when it is named
            var parts = value.Split('/');
            int n = parts.Length;
            string? version = null;
            if (n >= 3 && parts[n - 2] == \"_history\")
            {
                version = parts[n - 1];
                n -= 2;
            }
            var part = n >= 2
                ? $\"{alias}.target_type = {P(paramIndex)} AND {alias}.target_id = {P(paramIndex + 1)}\"
                : $\"{alias}.target_id = {P(paramIndex)}\";
            if (n >= 2)
            {
                parameters.Add(NewParam(paramIndex, parts[n - 2]));
                parameters.Add(NewParam(paramIndex + 1, parts[n - 1]));
                paramIndex += 2;
            }
            else
            {
                parameters.Add(NewParam(paramIndex, version == null ? value : parts[0]));
                paramIndex++;
            }
            if (version != null)
            {
                part += $\" AND {alias}.target_version = {P(paramIndex)}\";
                parameters.Add(NewParam(paramIndex, version));
                paramIndex++;
            }
            orParts.Add(n >= 2 || version != null ? $\"({part})\" : part);
        }

        if (orParts.Count > 0)
            wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"   )
)
