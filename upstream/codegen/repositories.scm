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

; The base class is written for no resource in particular: what a data table needs is in it,
; and asked for when the server runs -- HasDataTable, which a resource's class answers
; (Binary: on PostgreSQL). data-table? is whether the code is to be written at all, and
; data-guard puts the question around a block of it.
(define (data-table? rn db) (or (r-generic? rn) (binary-data-table? rn db)))
(define (data-guard rn code)
    (if (r-generic? rn)
        ($ "        if (HasDataTable)
        {
" code "
        }
")
        code))

; Before a content is stored: its data taken out (the row keeps "data": null).
(define (repo-data-take rn)
    (if (r-generic? rn) ($
"        // a data table: the bytes are kept there, not in the content
        byte[]? binaryData = null;
        JsonNode? binaryDataNode = null;
        if (HasDataTable && content[\"data\"] is { } dataElement)
        {
            binaryDataNode = dataElement.DeepClone();
            binaryData = Convert.FromBase64String(dataElement.GetValue<""string>());
            content[\"data\"] = null;
        }
")
        ""))

; After the row is written: the bytes stored under the version, and the content that is
; answered given its data back.
(define (repo-data-store rn schema db version)
    (if (r-generic? rn) ($
"        if (binaryData != null)
        {
            await using (var dataCmd = "(new-cmd db)"
                \"INSERT INTO "(tbl schema "resource_data" db)" (resource_id, version_id, data) VALUES ("(pp 1 db)", "(pp 2 db)", "(pp 3 db)")\", conn, tx))
            {
    "(add-param-line-for "dataCmd" 1 "id" db)"
    "(add-param-line-for "dataCmd" 2 version db)"
    "(add-param-line-for "dataCmd" 3 "binaryData" db)"
                await dataCmd.ExecuteNonQueryAsync();
            }
            content[\"data\"] = binaryDataNode;
            contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);
        }

")
        ""))

; A resource that was read: with its data, where the data lies in a table.
(define (repo-data-read rn indent var)
    (if (r-generic? rn) ($ indent var " = await WithDataAsync(conn, tx, " var ");
")
        ""))

; What the base class has for a data table.
(define (repo-data-members has-history db)
    (let ((rn repo-generic-rn) (schema repo-generic-rn)) ($
"    // -- A data table --

    // Whether the resource keeps its data in a table of its own (Binary on PostgreSQL: raw
    // bytes in resource_data, a row a version, the content with \"data\": null).
    protected virtual bool HasDataTable => false;

    private string DataColumn => HasDataTable ? \", rd.data\" : \"\";
    private string DataJoin(string alias) => HasDataTable
        ? $\"LEFT JOIN "(tbl schema "resource_data" db)" rd ON rd.resource_id = {alias}.id AND rd.version_id = {alias}.version_id \"
        : \"\";

    protected async Task<""TResource> WithDataAsync(ConnType conn, TxType? tx, TResource resource)
    {
        if (!HasDataTable) return resource;
        await using var dataCmd = "(new-cmd db)"
            \"SELECT data FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx);
"(add-param-8-for "dataCmd" 1 "resource.id" db)"
"(add-param-8-for "dataCmd" 2 "resource.versionId" db)"
        if (await dataCmd.ExecuteScalarAsync() is not byte[] bytes) return resource;
        return WithData(resource, bytes);
    }

    private TResource WithData(TResource resource, byte[] bytes)
    {
        var parsed = JsonNode.Parse(resource.content)!;
        parsed[\"data\"] = Convert.ToBase64String(bytes);
        return MakeResource(resource.id, resource.versionId, resource.lastUpdated,
            JsonSerializer.SerializeToUtf8Bytes(parsed), InlineOf(resource));
    }

    // A row of a search that joined the data table: the data is the column behind the
    // resource's own.
    private TResource ReadResourceWithData(ReaderType reader)
    {
        var resource = ReadResource(reader);
        var at = 4 + InlineColumns.Length;
        return reader.IsDBNull(at) ? resource : WithData(resource, reader.GetFieldValue<""byte[]>(at));
    }

"
    (if has-history ($
"    private TRow ReadHistoryRowWithData(ReaderType reader)
    {
        var content = reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3);
        if (content != null && !reader.IsDBNull(5))
        {
            var bytes = reader.GetFieldValue<""byte[]>(5);
            var parsed = JsonNode.Parse(content)!;
            parsed[\"data\"] = Convert.ToBase64String(bytes);
            content = JsonSerializer.SerializeToUtf8Bytes(parsed);
        }
        return MakeRow(
            reader.GetString(0),
            reader.GetInt32(1),
            Utc(reader.GetDateTime(2)),
            content,
            reader.GetString(4));
    }

") ""))))

; Parameter placeholder: $1 or @p1
(define (pp n db)
    (db3 db (lambda (db) (case db
        ((PostgreSql) ($ "$"(number->string n)))
        ((SqlServer Sqlite) ($ "@p"(number->string n)))))))

; Parameter prefix for C# interpolated strings: $ or @p
(define (pp-prefix db)
    (db3 db (lambda (db) (case db ((PostgreSql)"$") ((SqlServer Sqlite)"@p")))))

; Table name: schema.table or schema_table (with quoting escaped for C# strings)
(define (tbl schema table db)
    (db3 db (lambda (db) (case db
        ((PostgreSql) ($ "\\\"" schema "\\\"." table))
        ((SqlServer) ($ "[" schema "]." table))
        ((Sqlite) ($ schema "_" table))))))

; Public table name: quoted resource name (escaped for C# strings)
(define (tbl-public rn db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) ($ "\\\""rn"\\\""))
        ((SqlServer) ($ "["rn"]"))))))

; Table name for C# interpolated strings: schema.{var} or schema_{var}
(define (tbl-interp schema var-name db)
    (db3 db (lambda (db) (case db
        ((PostgreSql) ($ "\\\"" schema "\\\".{" var-name "}"))
        ((SqlServer) ($ "[" schema "].{" var-name "}"))
        ((Sqlite) ($ schema "_{" var-name "}"))))))

; SELECT lock suffix
(define (select-lock db)
    (db3 db (lambda (db) (case db ((PostgreSql)" FOR UPDATE") ((SqlServer Sqlite)"")))))

; Connection opening
(define (open-conn db)
    "        await using var conn = await Db.OpenAsync();")

; The same where the generator wrote the alias: new CmdType(
(define (new-cmd-alias db)
    "Db.Cmd(")

; New command: new XxxCommand(
(define (new-cmd db)
    "Db.Cmd(")


; Connection + transaction opening. Inside a DbTransactionScope (transaction bundle) the method
; uses the connection and transaction of the scope and owns neither; every commit and rollback in
; such a method is guarded with txScope == null, the scope commits or rolls back once at the end.
(define (open-conn-tx db)
    "        var txScope = DbTransactionScope.Current;
        await using var ownConn = txScope == null ? await Db.OpenAsync() : null;
        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        await using var ownTx = txScope == null ? await conn.BeginTransactionAsync() : null;
        var tx = txScope == null ? ownTx! : (TxType)txScope.Transaction;")

; Connection opening for the reads a transaction bundle can reach (read, version read, search,
; delete without history). Inside a DbTransactionScope they run on its connection and within its
; transaction, so they see the writes of earlier bundle entries; SqlClient and Microsoft.Data.Sqlite
; also reject a command without the pending transaction. Outside a scope tx is null.
(define (open-conn-scoped db)
    "        var txScope = DbTransactionScope.Current;
        await using var ownConn = txScope == null ? await Db.OpenAsync() : null;
        var conn = txScope == null ? ownConn! : (ConnType)txScope.Connection;
        var tx = (TxType?)txScope?.Transaction;")

; Table-level lock hint (goes after table name in SQL Server)
(define (table-lock-hint db)
    (db3 db (lambda (db) (case db ((SqlServer)" WITH (UPDLOCK, ROWLOCK)") (else "")))))


; Single AddWithValue line (no leading newline), with configurable command name
(define (add-param-line-for cmd-name n expr db)
    ($"            Db.AddValue(" cmd-name ", "(number->string n)", " expr ");"))

(define (add-param-line n expr db) (add-param-line-for "cmd" n expr db))

; 8-space indent version for non-block contexts (Read, VRead, etc.)
(define (add-param-8-for cmd-name n expr db)
    ($"        Db.AddValue(" cmd-name ", "(number->string n)", " expr ");"))

(define (add-param-8 n expr db) (add-param-8-for "cmd" n expr db))


; Bind parameters from a runtime list of objects — positional for PG, named for others
(define (bind-args-list cmd-name args-name db)
    ($"            for (int __i = 0; __i < " args-name ".Count; __i++) Db.Add(" cmd-name ", __i + 1, " args-name "[__i]);"))

; Pagination: LIMIT/OFFSET vs OFFSET/FETCH
; count-expr and offset-expr are C# interpolation expressions like "count" or "offset"
(define (db-paginate count-expr offset-expr db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) ($ "LIMIT {" count-expr "} OFFSET {" offset-expr "}"))
        ((SqlServer) ($ "OFFSET {" offset-expr "} ROWS FETCH NEXT {" count-expr "} ROWS ONLY"))))))

; Pagination for SQL params (e.g. $1, @p1)
(define (db-paginate-params count-param offset-param db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) ($ "LIMIT " count-param " OFFSET " offset-param))
        ((SqlServer) ($ "OFFSET " offset-param " ROWS FETCH NEXT " count-param " ROWS ONLY"))))))

; Simple LIMIT 1 (used after ORDER BY)
(define (db-limit-1 db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite)"LIMIT 1")
        ((SqlServer)"OFFSET 0 ROWS FETCH NEXT 1 ROWS ONLY")))))

; SELECT [TOP 1] prefix for subqueries without ORDER BY
(define (db-select-top1 db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite)"SELECT")
        ((SqlServer)"SELECT TOP 1")))))

; Trailing LIMIT 1 for subqueries without ORDER BY (empty for SqlServer since TOP 1 is used)
(define (db-trailing-limit1 db)
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite)" LIMIT 1")
        ((SqlServer)"")))))

; ===================================================================
; Helper functions
; ===================================================================

; Column list: "id, version_id, last_updated, content, active, gender"
(define (repo-column-list inline-list has-history db)
    (if (generic-inline? inline-list)
        "id, version_id, last_updated, content\\uE004C\\uE005"
        ($"id, version_id, last_updated, content"
        (apply $ (map (lambda (s) ($", "(db-quote-col-cs (search-col s) db))) inline-list)))))

; Prefixed column list for SELECT: "r.id, r.version_id, r.last_updated, r.content, r.active, r.gender"
(define (repo-column-list-prefixed prefix inline-list has-history db)
    (if (generic-inline? inline-list)
        ($ prefix ".id, " prefix ".version_id, " prefix ".last_updated, " prefix ".content\\uE004C" prefix ".\\uE005")
        ($ prefix ".id, " prefix ".version_id, " prefix ".last_updated, " prefix ".content"
        (apply $ (map (lambda (s) ($", " prefix "."(db-quote-col-cs (search-col s) db))) inline-list)))))

; Parameter placeholders: "$1, $2, $3, $4, $5, $6" or "@p1, @p2, ..."
(define (repo-param-list inline-list has-history db)
    (if (generic-inline? inline-list)
        (let ((pre (pp-prefix db))) ($ pre "1, " pre "2, " pre "3, " pre "4\\uE004P5\\uE005"))
        (let ((pre (pp-prefix db)))
    ($ pre "1, " pre "2, " pre "3, " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", " pre (number->string idx) (loop (cdr items) (+ idx 1)))))))))

; History INSERT column list: "id, version_id, last_updated, content, active, gender, history_operation"
(define (repo-history-column-list inline-list db)
    (if (generic-inline? inline-list)
        "id, version_id, last_updated, content\\uE004C\\uE005, history_operation"
        ($"id, version_id, last_updated, content"
        (apply $ (map (lambda (s) ($", "(db-quote-col-cs (search-col s) db))) inline-list))
        ", history_operation")))

; History INSERT params: "$1, $2, $3, $4, $5, $6, 'UPDATE'" or "@p1, @p2, ..."
(define (repo-history-param-list inline-list operation db)
    (if (generic-inline? inline-list)
        (let ((pre (pp-prefix db))) ($ pre "1, " pre "2, " pre "3, " pre "4\\uE004P5\\uE005, '" operation "'"))
        (let ((pre (pp-prefix db)))
    ($ pre "1, " pre "2, " pre "3, " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", " pre (number->string idx) (loop (cdr items) (+ idx 1)))))
        ", '" operation "'"))))

; UPDATE SET clause: "version_id = $2, last_updated = $3, content = $4, ..."
(define (repo-update-set inline-list has-history db)
    (if (generic-inline? inline-list)
        (let ((pre (pp-prefix db))) ($ "version_id = " pre "2, last_updated = " pre "3, content = " pre "4\\uE004U5\\uE005"))
        (let ((pre (pp-prefix db)))
    ($"version_id = " pre "2, last_updated = " pre "3, content = " pre "4"
        (let loop ((items inline-list) (idx 5))
            (if (null? items)""
                ($", "(db-quote-col-cs (search-col (car items)) db)" = " pre (number->string idx) (loop (cdr items) (+ idx 1)))))))))

; AddWithValue calls for inline params
(define (repo-addwithvalue inline-list var-prefix start-idx db)
    (if (generic-inline? inline-list)
        ($"
            AddInline(cmd, " (number->string start-idx) ", " (if (string=? var-prefix "") "inline" ($ "InlineOf(" (substring var-prefix 0 (- (string-length var-prefix) 1)) ")")) ");")
        (let loop ((items inline-list) (idx start-idx))
            (if (null? items)""
                ($"
            Db.AddValue(cmd, "(number->string idx)", (object?)" var-prefix (search-query-camel (car items))" ?? DBNull.Value);"
                    (loop (cdr items) (+ idx 1)))))))

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
    (if (generic-inline? inline-list)
        ", inline"
        (apply $ (map (lambda (s) ($", "(search-query-camel s))) inline-list))))

; Destructure tuple: "var (active, gender) = ..." or "var status = ..." for single
(define (repo-destructure-inline inline-list)
    (if (generic-inline? inline-list)
        "var inline"
        (if (= 1 (length inline-list))
        ($"var "(search-query-camel (car inline-list)))
        ($"var ("
            (let loop ((items inline-list) (first? #t))
                (if (null? items)""
                    ($ (if first? "" ", ") (search-query-camel (car items)) (loop (cdr items) #f))))
            ")"))))

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

; What every repository offers, written once for all resources: the members with the
; resource's record and history row as type parameters -- with versions, and without.
(define (repo-interface-members res row has-history)
    ($
"    Task<"res"> CreateAsync(JsonNode content, string? clientId = null);
    Task<"res"?> ReadAsync(string id);
"
    (if has-history
        ($
"    Task<"res"?> VersionReadAsync(string id, int versionId);
    Task<"res"?> UpdateAsync(string id, JsonNode content, int? ifMatchVersion = null);
    Task<"res"?> PatchAsync(string id, JsonArray patchOps, int? ifMatchVersion = null);
")
        ($
"    Task<"res"?> UpdateAsync(string id, JsonNode content);
    Task<"res"?> PatchAsync(string id, JsonArray patchOps);
"       )
    )
"    Task<""bool> DeleteAsync(string id);
"
    (if has-history ($
"    Task<""bool> IsDeletedAsync(string id);
    Task<"res"?> RollbackAsync(string id, int currentVersionId);
    Task<"res"?> UndeleteAsync(string id);
    Task<""int> UndeleteAllAsync();
"   )"")
"    Task<(List<"res"> resources, int total)> SearchAsync(IQueryCollection queryParams, int count, int offset);
"
    (if has-history ($
"    Task<(List<"row"> rows, int total)> InstanceHistoryAsync(string id, int count, int offset);
    Task<(List<"row"> rows, int total)> TypeHistoryAsync(int count, int offset);
"   )"")
"    Task<""List<""string>> AutocompleteAsync(string param, string value, string property, int? count, int? offset);
"
"    Task CreateSearchAsync(string parameter);
    Task DropSearchAsync(string parameter);
    Task<""int> PurgeAsync(DateTime? before);
"))

(define (repo-generic-interfaces)
    (file "IResourceRepository.cs" ($
"using System.Text.Json.Nodes;

namespace "(project-name)".Repositories;

// What a stored resource is known by, whatever columns of its own it has beside.
public interface IResourceRecord
{
    string id { get; }
    int versionId { get; }
    DateTime lastUpdated { get; }
    byte[] content { get; }
}

public interface IHistoryRow
{
    string id { get; }
    int versionId { get; }
    DateTime lastUpdated { get; }
    byte[]? content { get; }
    string historyOperation { get; }
}

// The repository of a resource that keeps its versions ...
public interface IVersionedResourceRepository<""TResource, TRow>
{
" (repo-interface-members "TResource" "TRow" #t)
"}

// ... and of one that does not.
public interface IResourceRepository<""TResource>
{
" (repo-interface-members "TResource" "" #f)
"}
")))

(define (repo-interface rn inline-list has-history)
    (file ($ "I"rn"Repository.cs")
        ($
"using System.Text.Json.Nodes;
using "(project-name)".Search;

namespace "(project-name)".Repositories;

public interface I"rn"Repository : "
    (if has-history
        ($ "IVersionedResourceRepository<" rn "Resource, " rn "HistoryRow>")
        ($ "IResourceRepository<" rn "Resource>"))
"
{
"
    ; Only the provenance of a rolled back version gets removed without a trace, so only
    ; Provenance carries the method that does it.
    (if (string=? rn "Provenance") ($
"    Task<""bool> HardDeleteAsync(string id);
"   )"")
"}
")))

; ===================================================================
; DB-specific class helpers (P, NewParam, NewTsParam, CopyParam)
; These allow search code to use abstract DbParameter types
; ===================================================================

(define (repo-db-class-helpers db)
    "    // -- DB helpers --

    private static string P(int n) => Db.P(n);
    private static ParamType NewParam(int index, object value) => Db.Param(index, value);
    private static ParamType NewTsParam(int index, DateTime value) => Db.TsParam(index, value);
    private static ParamType CopyParam(ParamType source) => Db.CopyParam(source);

")

; ===================================================================
; The base classes: what does not depend on a resource's search parameters
; ===================================================================
; Create, read, update, patch, delete, rollback, undelete, the histories, the purge and the
; helpers that write an index row are the same for every resource but for its name and the
; columns of its own. They are written ONCE, into ResourceRepository (and
; UnversionedResourceRepository, for a resource without history), by running the very
; sections below with a resource that is none: the name mark, the columns `generic-inline`.
;   - a type the section names answers the class's type parameter, a call into the resource's
;     extractor a method the resource's own class overrides (r-res, r-read, r-inject ...);
;   - inside an SQL string the resource's name is the mark , and a list over its columns
;      kind argument  (C a column list with a prefix, P placeholders from a number
;     on, U the SET clause from a number on); the class's Sql puts the running resource's in.
(define generic-inline '("*"))
(define (generic-inline? inline-list) (equal? inline-list generic-inline))
(define repo-generic-rn "\\uE003")
(define (r-generic? rn) (string=? rn repo-generic-rn))
(define (r-res rn) (if (r-generic? rn) "TResource" ($ rn "Resource")))
(define (r-row rn) (if (r-generic? rn) "TRow" ($ rn "HistoryRow")))
(define (r-read rn) (if (r-generic? rn) "ReadResource" ($ "Read" rn "Resource")))
(define (r-new-res rn) (if (r-generic? rn) "MakeResource(" ($ "new " rn "Resource(")))
(define (r-new-row rn) (if (r-generic? rn) "MakeRow(" ($ "new " rn "HistoryRow(")))
(define (r-extract-inline rn) (if (r-generic? rn) "ExtractInline(content)" ($ rn "SearchExtractor.ExtractInlineParams(content)")))
(define (r-inject rn) (if (r-generic? rn) "InjectMeta" ($ rn "SearchExtractor.InjectMeta")))

; The base class of a resource's repository, with its records.
(define (repo-base-of rn)
    (if (history? (current-node))
        ($ "ResourceRepository<" rn "Resource, " rn "HistoryRow>")
        ($ "UnversionedResourceRepository<" rn "Resource>")))

(define (repo-base-head has-history)
    ($
"using System.Collections.Concurrent;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using "(project-name)".Search;
using ConnType = System.Data.Common.DbConnection;
using TxType = System.Data.Common.DbTransaction;
using CmdType = System.Data.Common.DbCommand;
using ReaderType = System.Data.Common.DbDataReader;
using ParamType = System.Data.Common.DbParameter;

namespace "(project-name)".Repositories;

// What every resource's repository does the same way: all that does not depend on its search
// parameters. A resource's own class says what it is called, which columns of its own it has
// and how its records are made, and keeps the methods that go by search parameter.
//
// In the SQL here the resource's name is the mark , and a list over its own columns
//  kind argument ; Sql puts the running resource's in (and Db.Cmd, behind it, the
// database's text).
public abstract class "(if has-history ($ "ResourceRepository<""TResource, TRow>
    where TResource : class, IResourceRecord
    where TRow : class, IHistoryRow") ($ "UnversionedResourceRepository<""TResource>
    where TResource : class, IResourceRecord"))"
{
    protected abstract string Name { get; }
    protected abstract string[] InlineColumns { get; }
    protected abstract object?[] ExtractInline(JsonNode content);
    protected abstract object?[] InlineOf(TResource resource);
    protected abstract TResource MakeResource(string id, int versionId, DateTime lastUpdated, byte[] content, object?[] inline);
"(if has-history "    protected abstract TRow MakeRow(string id, int versionId, DateTime lastUpdated, byte[]? content, string historyOperation);
" "")"    protected abstract TResource ReadResource(ReaderType reader);
    protected abstract void InjectMeta(JsonNode content, string id, int versionId, DateTime lastUpdated);
    // A filter a project adds to the search, and the order.
    protected virtual bool AddOwnFilter(KeyValuePair<""string, Microsoft.Extensions.Primitives.StringValues> param, string key, List<""string> joins,
        List<""string> whereClauses, List<""ParamType> parameters, ref int paramIndex, ref int aliasIndex) => false;
    protected abstract string OrderBy(string sortParam);

    // The values of the resource's own columns as parameters first, first + 1, ...
    protected static void AddInline(CmdType cmd, int first, object?[] inline)
    {
        for (var i = 0; i < inline.Length; i++)
            Db.AddValue(cmd, first + i, inline[i] ?? DBNull.Value);
    }

    protected CmdType Cmd(string sql, ConnType conn, TxType? tx = null) => Db.Cmd(Sql(sql), conn, tx);

    protected async Task ExecuteAsync(ConnType conn, TxType? tx, string sql)
    {
        await using var cmd = Cmd(sql, conn, tx);
        await cmd.ExecuteNonQueryAsync();
    }

    // A search table made (ddl) and filled: index writes the rows of one resource, and is
    // called for every stored one.
    protected async Task CreateSearchTableAsync(ConnType conn, TxType? tx, string ddl, Func<""string, JsonNode, Task> index)
    {
        await ExecuteAsync(conn, tx, ddl);
        var resources = new List<""(string id, JsonNode content)>();
        await using (var readCmd = Cmd(\"SELECT id, content FROM \\uE000\\\"\\uE003\\\"\\uE001\\\"\\uE003\\\"\\uE001[\\uE003]\\uE002\", conn, tx))
        {
            await using var reader = await readCmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                resources.Add((reader.GetString(0), JsonNode.Parse(reader.GetFieldValue<""byte[]>(1))!));
        }
        foreach (var (id, content) in resources)
            await index(id, content);
    }

    private readonly ConcurrentDictionary<""string, string> _sql = new();

    // sql with the resource's name and the lists over its columns put in. A text is worked
    // out once (the texts here are few; the bound is for what a caller might compose).
    protected string Sql(string sql)
    {
        if (sql.IndexOf('') < 0 && sql.IndexOf('') < 0) return sql;
        if (_sql.TryGetValue(sql, out var known)) return known;
        var made = Resolve(sql);
        if (_sql.Count < 4096) _sql[sql] = made;
        return made;
    }

    // A command of a text composed for one request (a search, with its conditions and its
    // page): worked out each time, never kept.
    protected CmdType CmdOnce(string sql, ConnType conn, TxType? tx = null) => Db.Cmd(Resolve(sql), conn, tx);

    protected string Resolve(string sql)
    {
        var sb = new StringBuilder(sql.Length + 64);
        var columns = InlineColumns;
        for (var i = 0; i < sql.Length; i++)
        {
            var c = sql[i];
            if (c == '') { sb.Append(Name); continue; }
            if (c != '') { sb.Append(c); continue; }
            var end = sql.IndexOf('', i);
            var kind = sql[i + 1];
            var argument = sql.Substring(i + 2, end - i - 2);
            for (var k = 0; k < columns.Length; k++)
            {
                sb.Append(\", \");
                if (kind == 'C') sb.Append(argument).Append(Db.Col(columns[k]));
                else if (kind == 'P') sb.Append(Db.P(int.Parse(argument) + k));
                else sb.Append(Db.Col(columns[k])).Append(\" = \").Append(Db.P(int.Parse(argument) + k));
            }
            i = end;
        }
        return sb.ToString();
    }

"))

; The sections of a base class, run for no resource in particular. What they declare private
; a resource's class must reach, and their commands go through the class's own Cmd.
(define (repo-base-body has-history)
    (let ((rn repo-generic-rn) (schema repo-generic-rn) (inline-list generic-inline) (db 'Any))
        (string-replace (string-replace (string-replace
            ($
                (repo-create rn schema inline-list has-history db)
                (repo-read rn schema inline-list has-history db)
                (if has-history (repo-vread rn schema inline-list has-history db) "")
                (repo-update rn schema inline-list has-history db)
                (repo-patch rn schema inline-list has-history db)
                (repo-delete rn schema inline-list has-history db)
                (if has-history (repo-is-deleted schema db) "")
                (if has-history (repo-rollback rn schema inline-list db) "")
                (if has-history (repo-undelete rn schema inline-list db) "")
                (if has-history (repo-undelete-all rn schema inline-list db) "")
                (if has-history (repo-instance-history rn schema db) "")
                (if has-history (repo-type-history rn schema db) "")
                (repo-purge rn schema has-history db)
                ; (their texts are composed for the request)
                (string-replace
                    ($ (repo-search rn schema inline-list '() has-history db)
                       (repo-autocomplete-body schema db))
                    "Db.Cmd(" "CmdOnce(")
                (repo-utc-helper)
                (repo-insert-helpers schema db)
                (repo-insert-quantity-helper schema db)
                (repo-insert-date-list-helper schema db)
                (repo-db-class-helpers db)
                (repo-data-members has-history db))
            "Db.Cmd(" "Cmd(")
            "    private " "    protected ")
            ; (the helpers that write an index row name the resource's tables: no longer static)
            "    protected static async Task Insert" "    protected async Task Insert")))

(define (repo-base-files)
    (sosofo-append
        (file "ResourceRepository.cs" ($ (repo-base-head #t) (repo-base-body #t) (repo-base-parameters #t) "}
" (repo-search-parameter-record)))
        (file "UnversionedResourceRepository.cs" ($ (repo-base-head #f) (repo-base-body #f) (repo-base-parameters #f) "}
"))))

; A resource's own class beside its base: what it is called, its columns, its records.
(define (repo-overrides rn inline-list has-history)
    (let ((names (map search-query-camel inline-list))
          (types (map (lambda (s) (let ((prop (search-property s))) ($ (csharp-type (if prop (% "type" prop) "code")) "?"))) inline-list)))
        ($
"    protected override string Name => \"" rn "\";
    protected override string[] InlineColumns { get; } = new string[] {" (apply $ (map (lambda (s) ($ " \"" (search-col s) "\",")) inline-list)) " };
    protected override object?[] ExtractInline(JsonNode content)
    {
"       (if (null? inline-list)
            ($"        return Array.Empty<""object?>();
")
            ($"        " (repo-destructure-inline inline-list) " = " rn "SearchExtractor.ExtractInlineParams(content);
        return new object?[] {" (apply $ (map (lambda (n) ($ " " n ",")) names)) " };
"))
"    }
    protected override object?[] InlineOf(" rn "Resource resource) => new object?[] {" (apply $ (map (lambda (n) ($ " resource." n ",")) names)) " };
    protected override " rn "Resource MakeResource(string id, int versionId, DateTime lastUpdated, byte[] content, object?[] inline)
        => new(id, versionId, lastUpdated, content"
        (let loop ((ts types) (i 0))
            (if (null? ts) "" ($ ", (" (car ts) ")inline[" (number->string i) "]" (loop (cdr ts) (+ i 1)))))
        ");
"       (if has-history ($
"    protected override " rn "HistoryRow MakeRow(string id, int versionId, DateTime lastUpdated, byte[]? content, string historyOperation)
        => new(id, versionId, lastUpdated, content, historyOperation);
") "")
"    protected override " rn "Resource ReadResource(ReaderType reader) => Read" rn "Resource(reader);
    protected override void InjectMeta(JsonNode content, string id, int versionId, DateTime lastUpdated)
        => " rn "SearchExtractor.InjectMeta(content, id, versionId, lastUpdated);
    protected override string OrderBy(string sortParam) => BuildOrderBy(sortParam);
"       (if (string=? rn "Binary")
"    // (its bytes lie in a table of their own there)
    protected override bool HasDataTable => Db.Kind == DbKind.PostgreSql;
" "")
"
")))

; A resource's part of a search: the conditions of its own parameters, and the filters a
; project adds (existsfilter.scm writes them for the loop over the query: its `continue`
; is this method's `true`).
(define (repo-search-own rn schema inline-list all-table-search-list db)
    (let ((own (repo-exists-filter-cases rn db)))
        ($
            (if (string=? own "") "" ($
"    protected override bool AddOwnFilter(KeyValuePair<""string, Microsoft.Extensions.Primitives.StringValues> param, string key, List<""string> joins,
        List<""string> whereClauses, List<""ParamType> parameters, ref int paramIndex, ref int aliasIndex)
    {
"               (string-replace own "continue;" "return true;")
"        return false;
    }

"))
        )))


; What goes by search parameter, once for every resource: it asks the resource's list.
(define (repo-base-parameters has-history)
    (let ((rn repo-generic-rn) (schema repo-generic-rn) (db 'Any))
        ($ "    // -- The resource's search parameters --

    // What the resource may be searched by: Parameters, in the resource's own class, names
    // each parameter, its kind and where it is kept. IndexAsync writes the rows of a
    // resource's search tables -- of all of them, or of the parameter `only` names.
    protected abstract SearchParameter[] Parameters { get; }
    protected abstract Task IndexAsync(ConnType conn, TxType tx, string? only, string resourceId, JsonNode content);

    private Dictionary<""string, SearchParameter>? _parameters;
    protected SearchParameter? Parameter(string name)
    {
        _parameters ??= Parameters.ToDictionary(p => p.Name);
        return _parameters.TryGetValue(name, out var found) ? found : null;
    }

    protected static string NextAlias(ref int aliasIndex) => $\"s{aliasIndex++}\";

    protected Task InsertSearchIndexesAsync(ConnType conn, TxType tx, string resourceId, JsonNode content)
        => IndexAsync(conn, tx, null, resourceId, content);

    protected async Task DeleteSearchIndexesAsync(ConnType conn, TxType tx, string resourceId)
    {
        foreach (var parameter in Parameters)
        {
            if (!parameter.IsTable) continue;
            var table = parameter.Target;
            await using var cmd = Cmd($\"DELETE FROM " (tbl-interp schema "table" db) " WHERE resource_id = {P(1)}\", conn, tx);
            cmd.Parameters.Add(NewParam(1, resourceId));
            await cmd.ExecuteNonQueryAsync();
        }
    }

    // The condition of one parameter of the resource's own; false: it has none of that name.
    protected bool AddSearch(string key, string modifier, string[] orValues, List<""string> joins, List<""string> whereClauses,
        List<""ParamType> parameters, ref int paramIndex, ref int aliasIndex)
    {
        if (Parameter(key) is not { } p) return false;
        var column = p.IsTable ? \"\" : \"r.\" + Db.Col(p.Target);
        switch (p.Kind)
        {
            case 's':
                SearchQueryBuilder.AddStringSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues, modifier);
                break;
            case 't':
                SearchQueryBuilder.AddTokenSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues, modifier);
                break;
            case 'u':
                SearchQueryBuilder.AddUriSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues, modifier);
                break;
            case 'd':
                SearchQueryBuilder.AddDateSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues);
                break;
            case 'r':
                SearchQueryBuilder.AddReferenceSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues);
                break;
            case 'n':
                SearchQueryBuilder.AddNumberSearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues);
                break;
" (if (any-quantity-table-search?) "            case 'q':
                SearchQueryBuilder.AddQuantitySearchOr(Name, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex,
                    p.Target, orValues);
                break;
" "") "            case 'B':
                if (orValues.Length == 1)
                {
                    whereClauses.Add($\"{column} = {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, bool.Parse(orValues[0])));
                    paramIndex++;
                }
                else
                {
                    var orParts = new List<""string>();
                    foreach (var v in orValues)
                    {
                        orParts.Add($\"{column} = {P(paramIndex)}\");
                        parameters.Add(NewParam(paramIndex, bool.Parse(v)));
                        paramIndex++;
                    }
                    whereClauses.Add($\"({string.Join(\" OR \", orParts)})\");
                }
                break;
            case 'I':
                SearchQueryBuilder.AddInlineInstantSearch(column, key, whereClauses, parameters, ref paramIndex, orValues);
                break;
            case 'N':
                SearchQueryBuilder.AddInlineIntSearch(column, key, whereClauses, parameters, ref paramIndex, orValues);
                break;
            case 'L':
                SearchQueryBuilder.AddInlineLongSearch(column, key, whereClauses, parameters, ref paramIndex, orValues);
                break;
            case 'M':
                SearchQueryBuilder.AddInlineDecimalSearch(column, key, whereClauses, parameters, ref paramIndex, orValues);
                break;
            default:
                // a code or a text in a column: matched exactly, and :contains loosens the value
                // side (substring, ignoring case)
                if (modifier == \"contains\")
                {
                    var containsParts = new List<""string>();
                    foreach (var v in orValues)
                    {
                        containsParts.Add($\"LOWER({column}) LIKE {P(paramIndex)}\");
                        parameters.Add(NewParam(paramIndex, $\"%{v.ToLowerInvariant()}%\"));
                        paramIndex++;
                    }
                    whereClauses.Add(containsParts.Count == 1 ? containsParts[0] : $\"({string.Join(\" OR \", containsParts)})\");
                }
                else if (orValues.Length == 1)
                {
                    whereClauses.Add($\"{column} = {P(paramIndex)}\");
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
                    whereClauses.Add($\"{column} IN ({string.Join(\", \", inParams)})\");
                }
                break;
        }
        return true;
    }

    protected void MissingSearch(string key, bool isMissing, List<""string> joins, List<""string> wheres, ref int aliasIndex)
    {
        if (key == \"_lastUpdated\")
        {
            wheres.Add(isMissing ? \"r.last_updated IS NULL\" : \"r.last_updated IS NOT NULL\");
            return;
        }
        if (Parameter(key) is not { } p) return;
        if (!p.IsTable)
        {
            wheres.Add(isMissing ? $\"r.{Db.Col(p.Target)} IS NULL\" : $\"r.{Db.Col(p.Target)} IS NOT NULL\");
            return;
        }
        var table = p.Target;
        var alias = NextAlias(ref aliasIndex);
        if (isMissing)
        {
            joins.Add($\"LEFT JOIN " (tbl-interp schema "table" db) " {alias} ON {alias}.resource_id = r.id\");
            wheres.Add($\"{alias}.resource_id IS NULL\");
        }
        else
        {
            joins.Add($\"JOIN " (tbl-interp schema "table" db) " {alias} ON {alias}.resource_id = r.id\");
        }
    }

    protected void NotSearch(string key, string value, List<""string> joins, List<""string> wheres,
        List<""ParamType> parameters, ref int paramIndex, ref int aliasIndex)
    {
        if (Parameter(key) is not { } p) return;
        if (!p.IsTable)
        {
            var column = \"r.\" + Db.Col(p.Target);
            object parsed = value;
            if (p.Kind == 'B')
                parsed = bool.Parse(value);
            else if (p.Kind == 'N')
            {
                if (!int.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out int number))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '{key}'\");
                parsed = number;
            }
            else if (p.Kind == 'L')
            {
                if (!long.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out long number))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '{key}'\");
                parsed = number;
            }
            else if (p.Kind == 'M')
            {
                if (!decimal.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out decimal number))
                    throw new ArgumentException($\"Invalid value '{value}' for number search parameter '{key}'\");
                parsed = number;
            }
            wheres.Add($\"({column} IS NULL OR {column} != {P(paramIndex)})\");
            parameters.Add(NewParam(paramIndex, parsed));
            paramIndex++;
            return;
        }
        if (p.Kind == 's')
        {
            var stringTable = p.Target;
            var alias = NextAlias(ref aliasIndex);
            joins.Add($\"LEFT JOIN " (tbl-interp schema "stringTable" db) " {alias} ON {alias}.resource_id = r.id AND LOWER({alias}.value) LIKE {P(paramIndex)}\");
            wheres.Add($\"{alias}.resource_id IS NULL\");
            parameters.Add(NewParam(paramIndex, $\"{value.ToLowerInvariant()}%\"));
            paramIndex++;
        }
        else if (p.Kind == 't')
            SearchQueryBuilder.AddTokenNotSearch(Name, joins, wheres, parameters, ref paramIndex, ref aliasIndex, p.Target, value);
    }

    // What autocomplete reads of a parameter: its table, the expression of its value, and
    // whether a value may name a system.
    protected (string? tableName, string column, bool isToken) ResolveSearchTable(string name)
    {
        var p = Parameter(name);
        return p?.Kind switch
        {
            's' => (p.Target, \"value\", false),
            't' => (p.Target, \"value\", true),
            'd' => (p.Target, \"" (db3 db (lambda (db) (case db
                    ((PostgreSql) "TO_CHAR(value_low, 'YYYY-MM-DD')")
                    ((Sqlite)     "STRFTIME('%Y-%m-%d', value_low)")
                    ((SqlServer)  "FORMAT(value_low, 'yyyy-MM-dd')")))) "\", false),
            _ => (null, \"value\", false)
        };
    }

    // A parameter's table with its indexes, as the schema has them.
    private string SearchTableDdl(SearchParameter p)
    {
        var table = p.Target;
        return p.Kind switch
        {
            's' or 'u' => $\"" (search-table-ddl schema "{table}" "string" rn db)(search-index-ddl schema "{table}" "string" "" db) "\",
            't' => p.Name is \"identifier\" or \"language\"
                ? $\"" (search-table-ddl schema "{table}" "token" rn db)(search-index-ddl schema "{table}" "token" "identifier" db) "\"
                : $\"" (search-table-ddl schema "{table}" "token" rn db)(search-index-ddl schema "{table}" "token" "" db) "\",
            'd' => $\"" (search-table-ddl schema "{table}" "date" rn db)(search-index-ddl schema "{table}" "date" "" db) "\",
            'r' => $\"" (search-table-ddl schema "{table}" "reference" rn db)(search-index-ddl schema "{table}" "reference" "" db) "\",
            'n' => $\"" (search-table-ddl schema "{table}" "number" rn db)(search-index-ddl schema "{table}" "number" "" db) "\",
            _ => $\"" (search-table-ddl schema "{table}" "quantity" rn db)(search-index-ddl schema "{table}" "quantity" "" db) "\"
        };
    }

    // A search parameter added to a database that is in use: its table made and filled from
    // every stored resource, or -- a column of the resource's own, which the schema or an
    // upgrade step has made -- the column filled. Like the writes it joins a transaction that
    // is open, so a step's index is made of the step's content and goes with it.
    public async Task CreateSearchAsync(string parameter)
    {
" (open-conn-scoped db) "
        if (Parameter(parameter) is not { } p) throw new ArgumentException($\"Unknown search parameter: {parameter}\");
        if (p.IsTable)
        {
            await CreateSearchTableAsync(conn, tx, SearchTableDdl(p), (id, content) => IndexAsync(conn, tx!, parameter, id, content));
            return;
        }
        var position = Array.IndexOf(InlineColumns, p.Target);
        var resources = new List<""(string id, JsonNode content)>();
        await using (var readCmd = Cmd(\"SELECT id, content FROM " (tbl-public rn db) "\", conn, tx))
        {
            await using var reader = await readCmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                resources.Add((reader.GetString(0), JsonNode.Parse(reader.GetFieldValue<""byte[]>(1))!));
        }
        foreach (var (id, content) in resources)
        {
            await using var updCmd = Cmd($\"UPDATE " (tbl-public rn db) " SET {Db.Col(p.Target)} = {P(1)} WHERE id = {P(2)}\", conn, tx);
            Db.AddValue(updCmd, 1, ExtractInline(content)[position] ?? DBNull.Value);
            Db.AddValue(updCmd, 2, id);
            await updCmd.ExecuteNonQueryAsync();
        }
    }

    // The table is the one the resource's own list names for the parameter, never text of the
    // caller's.
    public async Task DropSearchAsync(string parameter)
    {
" (open-conn-scoped db) "
        if (Parameter(parameter) is not { IsTable: true } p) throw new ArgumentException($\"Unknown search parameter: {parameter}\");
        var table = p.Target;
        await ExecuteAsync(conn, tx, $\"DROP TABLE IF EXISTS " (tbl-interp schema "table" db) "\");
    }

")))

(define (repo-search-parameter-record)
    "
// A search parameter of a resource: its name, its kind and where it is kept -- a search table
// (the kind in lower case: s string, t token, d date, r reference, n number, q quantity,
// u uri) or a column of the resource's own (B boolean, C code or text, I instant, N integer,
// L long, M decimal).
public sealed record SearchParameter(string Name, char Kind, string Target)
{
    public bool IsTable => char.IsLower(Kind);
}
")

; The kind of a parameter in a resource's list (see SearchParameter).
(define (search-parameter-kind s inline?)
    (if inline?
        (let* ((prop (search-property s))
               (prop-type (if prop (% "type" prop) "code")))
            (cond
                ((string=? "boolean" prop-type) "B")
                ((string=? "instant" prop-type) "I")
                ((member prop-type '("integer" "positiveInt" "unsignedInt")) "N")
                ((string=? "integer64" prop-type) "L")
                ((string=? "decimal" prop-type) "M")
                (else "C")))
        (let ((type (% "type" s)))
            (cond
                ((string=? "string" type) "s")
                ((string=? "token" type) "t")
                ((string=? "date" type) "d")
                ((string=? "reference" type) "r")
                ((string=? "number" type) "n")
                ((string=? "quantity" type) "q")
                (else "u")))))

; A line of IndexAsync: the rows of one parameter, written when all are or when it is the
; one asked for.
(define (repo-index-only s lines)
    ($"        if (only is null or \""(% "name" s)"\")
        {
" lines "        }
"))

; A resource's own part: the list of its parameters, and how the rows of each are made.
(define (repo-own-parameters rn inline-list all-table-search-list string-searches token-searches date-searches ref-searches number-searches quantity-searches uri-searches hn-all)
    ($
"    private static readonly SearchParameter[] _parameters =
    {
"
        (apply $ (map (lambda (s)
            ($"        new(\""(% "name" s)"\", '"(search-parameter-kind s #t)"', \""(search-col s)"\"),
")) inline-list))
        (apply $ (map (lambda (s)
            ($"        new(\""(% "name" s)"\", '"(search-parameter-kind s #f)"', \""(search-table-name s)"\"),
")) all-table-search-list))
"    };
    protected override SearchParameter[] Parameters => _parameters;

    protected override async Task IndexAsync(ConnType conn, TxType tx, string? only, string resourceId, JsonNode content)
    {
"
        (apply $ (map (lambda (s) (repo-index-only s
            ($"        await InsertStringIndexAsync(conn, tx, \""(search-table-name s)"\", resourceId, "(repo-string-extractor-call rn s hn-all)");
"))) string-searches))
        (apply $ (map (lambda (s) (repo-index-only s (repo-token-index-line rn s))) token-searches))
        (apply $ (map (lambda (s) (repo-index-only s (repo-date-index-line rn s))) date-searches))
        (apply $ (map (lambda (s) (repo-index-only s (repo-reference-index-line rn s))) ref-searches))
        (apply $ (map (lambda (s) (repo-index-only s (repo-number-index-line rn s))) number-searches))
        (apply $ (map (lambda (s) (repo-index-only s (repo-quantity-index-line rn s))) quantity-searches))
        (apply $ (map (lambda (s) (repo-index-only s
            ($"        await InsertStringIndexAsync(conn, tx, \""(search-table-name s)"\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal (% "name" s))"(content));
"))) uri-searches))
"        await Task.CompletedTask;
    }

"))

; ===================================================================
; Main repository function
; ===================================================================

; A resource has ONE repository, for every database (Db picks the dialect when the server
; starts) -- but Binary, whose bytes lie in a table of their own on PostgreSQL alone: its
; repositories differ in more than their SQL and are written for each database as before.
(define (single-repository? rn) #t)

(define (repo-folder db content)
    content)

; For the program: Db told which database it is, and a resource's repository made for it.
; `source` is the C# expression the repositories of db are made with (a data source on
; PostgreSQL, the connection string elsewhere).
(define (repo-db-use db source)
    ($"        " (project-name) ".Repositories.Db.Use" (db-namespace db) "(" source ");
"))
(define (repo-new rn db source)
    (if (single-repository? rn)
        ($ "new " (project-name) ".Repositories." rn "Repository()")
        ($ "new " (project-name) ".Repositories." (db-namespace db) "." rn "Repository(" source ")")))

(define (repository)
    (let* ((resource (current-node))
           (rn (name-of resource))
           (has-history (history? resource))
           (schema rn)
           (inlines (inline-searches resource))
           (inline-list (node-list->list inlines))
           (raw-table-search-list (node-list->list (table-searches resource)))
           ; Filter to supported types only (excludes composite, special)
           (all-table-search-list (filter (lambda (s)
               (let ((t (% "type" s)))
                   (or (string=? "string" t)
                       (string=? "token" t)
                       (string=? "date" t)
                       (string=? "reference" t)
                       (string=? "number" t)
                       (string=? "quantity" t)
                       (string=? "uri" t)))) raw-table-search-list))
           (string-searches (filter (lambda (s) (string=? "string"(% "type" s))) all-table-search-list))
           (token-searches (filter (lambda (s) (and (string=? "token"(% "type" s)) (not (search-is-inline? s)))) all-table-search-list))
           (date-searches (filter (lambda (s) (and (string=? "date"(% "type" s)) (not (search-is-inline-instant? s)))) all-table-search-list))
           (instant-searches (filter (lambda (s) (search-is-inline-instant? s)) all-table-search-list))
           (ref-searches (filter (lambda (s) (string=? "reference"(% "type" s))) all-table-search-list))
           (number-searches (filter (lambda (s) (string=? "number"(% "type" s))) all-table-search-list))
           (quantity-searches (filter (lambda (s) (string=? "quantity"(% "type" s))) all-table-search-list))
           (uri-searches (filter (lambda (s) (string=? "uri"(% "type" s))) all-table-search-list))
           (hn-all (filter (lambda (s)
               (and (search-is-humanname? s) (equal? "all"(search-humanname-field s))))
               string-searches)))
        (make directory path: "Repositories"
        (apply sosofo-append
            (repo-interface rn inline-list has-history)
            (map (lambda (db)
                (repo-folder db
                (file ($ rn "Repository.cs")
                        ; its own class beside the base: the methods that go by search parameter
                        ($
                            (repo-header rn schema inline-list db)
                            (repo-overrides rn inline-list has-history)
                            (if (string=? rn "Provenance") (repo-hard-delete rn schema has-history db)"")
                            (repo-search-own rn schema inline-list all-table-search-list db)
                            (repo-own-parameters rn inline-list all-table-search-list string-searches token-searches date-searches ref-searches number-searches quantity-searches uri-searches hn-all)
                            (repo-read-resource-helper rn inline-list has-history db)
                            (repo-build-order-by schema inline-list all-table-search-list string-searches token-searches date-searches number-searches ref-searches db)
                            "}
")))) '(Any))))))

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
using ConnType = System.Data.Common.DbConnection;
using TxType = System.Data.Common.DbTransaction;
using CmdType = System.Data.Common.DbCommand;
using ReaderType = System.Data.Common.DbDataReader;
using ParamType = System.Data.Common.DbParameter;

namespace "(project-name)".Repositories;

// The resource's own part of its repository: what goes by search parameter, and what the
// base class asks of a resource. One class for every database (the dialect is Db's).
public class "rn"Repository : "(repo-base-of rn)", I"rn"Repository
{
"))

; ===================================================================
; Section: CreateAsync
; ===================================================================

(define (repo-create rn schema inline-list has-history db)
    ($
"    public async Task<"(r-res rn)"> CreateAsync(JsonNode content, string? clientId = null)
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
    (r-inject rn) "(content, id, versionId, lastUpdated);
"(if (null? inline-list)""($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
"))
    (repo-data-take rn)
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
    (repo-data-store rn schema db "versionId")
"        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, versionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: ReadAsync
; ===================================================================

(define (repo-read rn schema inline-list has-history db)
    ($
"    public async Task<"(r-res rn)"?> ReadAsync(string id)
    {
"
    (open-conn-scoped db)"
        await using var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list has-history db)" FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)"\", conn, tx);
"(add-param-8 1 "id" db)"

        await using var reader = await cmd.ExecuteReaderAsync();
        if (!await reader.ReadAsync()) return null;

        var resource = "(r-read rn)"(reader);
        await reader.CloseAsync();
"
    (repo-data-read rn "        " "resource")
"        return resource;
    }

"))

; ===================================================================
; Section: VersionReadAsync
; ===================================================================

(define (repo-vread rn schema inline-list has-history db)
    ($
"    public async Task<"(r-res rn)"?> VersionReadAsync(string id, int versionId)
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
            var resource = "(r-read rn)"(reader);
            await reader.CloseAsync();
"
    (repo-data-read rn "            " "resource")
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
            var resource = "(r-read rn)"(histReader);
            await histReader.CloseAsync();
"
    (repo-data-read rn "            " "resource")
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
"    public async Task<"(r-res rn)"?> UpdateAsync(string id, JsonNode content, int? ifMatchVersion = null)
    {
"
    (open-conn-tx db)"

        // Read current
        "(r-res rn)"? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = "(r-read rn)"(reader);
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
        "(r-inject rn)"(content, id, newVersionId, lastUpdated);
" (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
"))
    (repo-data-take rn)
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
    (repo-data-store rn schema db "newVersionId")
"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

(define (repo-update-non-versioned rn schema inline-list db)
    ($
"    public async Task<"(r-res rn)"?> UpdateAsync(string id, JsonNode content)
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
        "(r-inject rn)"(content, id, newVersionId, lastUpdated);
"       (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
"       ))
    (repo-data-take rn)
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
    (repo-data-store rn schema db "newVersionId")
"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

; ===================================================================
; Section: PatchAsync
; ===================================================================

(define (repo-patch rn schema inline-list has-history db)
    (if has-history
        (repo-patch-versioned rn schema inline-list db)
        (repo-patch-non-versioned rn schema inline-list db)))

; A PATCH on a type whose data lies in a table of its own (Binary): the
; patch is applied to the content WITH its data, and the new version gets
; its row there, as a create and an update give it one. Without these the
; patched version had no data at all.
(define (repo-patch-binary-read rn schema db)
    (if (data-table? rn db) (data-guard rn ($
"        // Binary: the stored content has no data; the patch sees it
        await using (var readCmd = "(new-cmd db)"
                \"SELECT data FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx))
        {
"(add-param-line-for "readCmd" 1 "id" db)"
"(add-param-line-for "readCmd" 2 "current.versionId" db)"
            if (await readCmd.ExecuteScalarAsync() is byte[] storedData)
                content[\"data\"] = Convert.ToBase64String(storedData);
        }
")) ""))

(define (repo-patch-binary-take rn db) (repo-data-take rn))

(define (repo-patch-binary-store rn schema db) (repo-data-store rn schema db "newVersionId"))

(define (repo-patch-versioned rn schema inline-list db)
    ($
"    public async Task<"(r-res rn)"?> PatchAsync(string id, JsonArray patchOps, int? ifMatchVersion = null)
    {
"
    (open-conn-tx db)"

        // Read current
        "(r-res rn)"? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = "(r-read rn)"(reader);
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
"(repo-patch-binary-read rn schema db)"        JsonPatcher.Apply(content, patchOps);
        "(r-inject rn)"(content, id, newVersionId, lastUpdated);
"(if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
"))(repo-patch-binary-take rn db)"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

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

"(repo-patch-binary-store rn schema db)"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
    }

"))

(define (repo-patch-non-versioned rn schema inline-list db)
    ($
"    public async Task<"(r-res rn)"?> PatchAsync(string id, JsonArray patchOps)
    {
"
    (open-conn-tx db)"

        // Read current
        "(r-res rn)"? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #f db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = "(r-read rn)"(reader);
        }

        if (current == null)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null;
        }

        var newVersionId = current.versionId + 1;
        var lastUpdated = DateTime.UtcNow;
        var content = JsonNode.Parse(current.content)!;
"(repo-patch-binary-read rn schema db)"        JsonPatcher.Apply(content, patchOps);
        "(r-inject rn)"(content, id, newVersionId, lastUpdated);
"(if (null? inline-list)""($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
"))(repo-patch-binary-take rn db)"        var contentBytes = JsonSerializer.SerializeToUtf8Bytes(content);

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

"(repo-patch-binary-store rn schema db)"        // Rebuild search indexes
        await DeleteSearchIndexesAsync(conn, tx, id);
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
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

        "(r-res rn)"? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = "(r-read rn)"(reader);
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
    (open-conn-scoped db)
    ; no history to keep the bytes for: they go with the resource
    (if (data-table? rn db) (data-guard rn ($"
        await using (var dataCmd = "(new-cmd db)"
            \"DELETE FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)"\", conn, tx))
        {
"(add-param-line-for "dataCmd" 1 "id" db)"
            await dataCmd.ExecuteNonQueryAsync();
        }
")) "")
"
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
"    public async Task<"(r-res rn)"?> RollbackAsync(string id, int currentVersionId)
    {
"
    (open-conn-tx db)"

        "(r-res rn)"? current = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl-public rn db) (table-lock-hint db)" WHERE id = "(pp 1 db) (select-lock db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                current = "(r-read rn)"(reader);
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
"
    ; the bytes of the version that is undone go with it: no foreign key takes
    ; them, and left behind they would stand in the way of the next version
    ; with that number
    (if (data-table? rn db) (data-guard rn ($"
        await using (var cmd = "(new-cmd db)"
            \"DELETE FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
"(add-param-line 2 "currentVersionId" db)"
            await cmd.ExecuteNonQueryAsync();
        }
")) "")
"
        // Find the most recent history entry - the DELETE row included, because it is the
        // version the resource stood in when it was deleted.
        "(r-res rn)"? previous = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl schema "resource_history" db)" \" +
            \"WHERE id = "(pp 1 db)" ORDER BY version_id DESC "(db-limit-1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                previous = "(r-read rn)"(reader);
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
"(repo-data-read rn "            " "previous")"            if (txScope == null) await tx.CommitAsync();
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
"    public async Task<"(r-res rn)"?> UndeleteAsync(string id)
    {
"
    (open-conn-tx db)"

        // Check resource doesn't already exist. The reader is closed before the
        // rollback: rolling back under an open reader threw, and a resource that
        // is there was answered with 503 instead of not-found.
        bool exists;
        await using (var cmd = "(new-cmd db)"
            \""(db-select-top1 db)" 1 FROM "(tbl-public rn db)" WHERE id = "(pp 1 db) (db-trailing-limit1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            exists = await reader.ReadAsync();
        }
        if (exists)
        {
            if (txScope == null) await tx.RollbackAsync();
            return null; // Already exists, not deleted
        }

        // Find the latest history entry, the DELETE row included: it carries the state the
        // resource was in when it was deleted. Skipping it restored the version before the
        // last edit and silently dropped that edit.
        "(r-res rn)"? previous = null;
        await using (var cmd = "(new-cmd db)"
            \"SELECT "(repo-column-list inline-list #t db)" FROM "(tbl schema "resource_history" db)" \" +
            \"WHERE id = "(pp 1 db)" ORDER BY version_id DESC "(db-limit-1 db)"\", conn, tx))
        {
"(add-param-line 1 "id" db)"
            await using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
                previous = "(r-read rn)"(reader);
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
        "(r-inject rn)"(content, id, newVersionId, lastUpdated);
"   (if (null? inline-list)"" ($"        "(repo-destructure-inline inline-list)" = "(r-extract-inline rn)";
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

"(if (data-table? rn db) (data-guard rn ($
"            // the restored version gets the bytes of the version it restores -
            // without them it came back with no data at all.
            byte[]? binaryData = null;
            await using (var readCmd = "(new-cmd db)"
                \"SELECT data FROM "(tbl schema "resource_data" db)" WHERE resource_id = "(pp 1 db)" AND version_id = "(pp 2 db)"\", conn, tx))
            {
    "(add-param-line-for "readCmd" 1 "id" db)"
    "(add-param-line-for "readCmd" 2 "previous.versionId" db)"
                binaryData = await readCmd.ExecuteScalarAsync() as byte[];
            }
            if (binaryData != null)
            {
                await using (var dataCmd = "(new-cmd db)"
                    \"INSERT INTO "(tbl schema "resource_data" db)" (resource_id, version_id, data) VALUES ("(pp 1 db)", "(pp 2 db)", "(pp 3 db)")\", conn, tx))
                {
        "(add-param-line-for "dataCmd" 1 "id" db)"
        "(add-param-line-for "dataCmd" 2 "newVersionId" db)"
        "(add-param-line-for "dataCmd" 3 "binaryData" db)"
                    await dataCmd.ExecuteNonQueryAsync();
                }
                content[\"data\"] = Convert.ToBase64String(binaryData);
                contentBytes = System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(content);
            }")) "")"        // No history row for the restored version: it is the current one now and the
        // history queries read the current version from the public table.
        await InsertSearchIndexesAsync(conn, tx, id, content);
        if (txScope == null) await tx.CommitAsync();

        return "(r-new-res rn)"id, newVersionId, lastUpdated, contentBytes"(repo-constructor-inline-args inline-list)");
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
"    public async Task<(List<"(r-res rn)"> resources, int total)> SearchAsync(
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
        (if (r-generic? rn)
"            if (AddOwnFilter(param, key, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex)) continue;
"
            (repo-exists-filter-cases rn db))
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
                "(if (r-generic? rn) "MissingSearch" "AddMissingSearch")"(key, isMissing, joins, whereClauses, ref aliasIndex);
                continue;
            }

            // Handle :not modifier
            if (modifier == \"not\")
            {
                "(if (r-generic? rn) "NotSearch" "AddNotSearch")"(key, param.Value.ToString(), joins, whereClauses, parameters, ref paramIndex, ref aliasIndex);
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
        (if (r-generic? rn) "" (apply $ (map (lambda (s) (repo-search-case-inline s db)) inline-list)))
        ; _lastUpdated inline case (uses r.last_updated directly, no JOIN)
        (repo-search-case-last-updated rn)
        ; Table search param cases
        (apply $ (map (lambda (s) (repo-search-case-table s schema)) all-table-search-list))
"                    default:
"       (if (r-generic? rn)
"                        if (AddSearch(key, modifier, orValues, joins, whereClauses, parameters, ref paramIndex, ref aliasIndex)) break;
" "")
"                        throw new ArgumentException($\"Unknown search parameter: {key}\");
                }
            }
        }

        var whereClause = whereClauses.Count > 0 ? \"WHERE \" + string.Join(\" AND \", whereClauses) : \"\";
        var joinClause = string.Join(\" \", joins);

        // Determine sort order
        var orderBy = "(if (r-generic? rn) "OrderBy" "BuildOrderBy")"(sortParam);

        // Count total
        var countSql = $\"SELECT COUNT(DISTINCT r.id) FROM "(tbl-public rn db)" r {joinClause} {whereClause}\";
        int total;
"
    (open-conn-scoped db)
"
        await using (var cmd = "(new-cmd-alias db)"countSql, conn, tx))
        {
            for (int i = 0; i < parameters.Count; i++)
                cmd.Parameters.Add(parameters[i]);
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        if (count == 0)
            return (new List<"(r-res rn)">(), total);

        // Fetch page
        string selectSql;
        if (joins.Count > 0)
        {
            // Use subquery to avoid DISTINCT + ORDER BY conflict
            selectSql = $\"SELECT "(repo-column-list-prefixed "r2" inline-list has-history db)(if (r-generic? rn) "{DataColumn}" "")" \" +
                        $\"FROM "(tbl-public rn db)" r2 "(if (r-generic? rn) "{DataJoin(\"r2\")}" "")"WHERE r2.id IN (\" +
                        $\"SELECT DISTINCT r.id FROM "(tbl-public rn db)" r {joinClause} {whereClause}\" +
                        $\") ORDER BY {orderBy.Replace(\"r.\", \"r2.\")} "(db-paginate "count" "offset" db)"\";
        }
        else
        {
            selectSql = $\"SELECT "(repo-column-list-prefixed "r" inline-list has-history db)(if (r-generic? rn) "{DataColumn}" "")" \" +
                        $\"FROM "(tbl-public rn db)" r "(if (r-generic? rn) "{DataJoin(\"r\")}" "")"{whereClause} \" +
                        $\"ORDER BY {orderBy} "(db-paginate "count" "offset" db)"\";
        }

        var resources = new List<"(r-res rn)">();
        await using (var cmd = "(new-cmd-alias db)"selectSql, conn, tx))
        {
            for (int i = 0; i < parameters.Count; i++)
                cmd.Parameters.Add(CopyParam(parameters[i]));
            await using var reader = await cmd.ExecuteReaderAsync();
            while (await reader.ReadAsync())
                resources.Add("(if (r-generic? rn) "HasDataTable ? ReadResourceWithData(reader) : ReadResource(reader)" ($ "Read"rn"Resource(reader)"))");
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
               ((string=? "uri" type)"AddUriSearchOr")
               (else #f))))
        (if helper
            (if (or is-string is-token (string=? "uri" type))
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
"    public async Task<(List<"(r-row rn)"> rows, int total)> InstanceHistoryAsync(string id, int count, int offset)
    {
"
    (open-conn db)
"
        int total;
        await using (var cmd = "(new-cmd-alias db)"
            \"SELECT (SELECT COUNT(*) FROM "(tbl-public rn db)" WHERE id = "(pp 1 db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE')\", conn))
        {
"(add-param-8 1 "id" db)"
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        var sql = "(if (r-generic? rn) ($ "HasDataTable
            ? \"SELECT h.id, h.version_id, h.last_updated, h.content, h.history_operation, rd.data FROM (\" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM " (tbl-public rn db) " WHERE id = " (pp 1 db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, history_operation FROM " (tbl schema "resource_history" db) " WHERE id = " (pp 1 db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM " (tbl schema "resource_history" db) " WHERE id = " (pp 1 db) " AND history_operation = 'DELETE'" (db-union-close db) "\" +
                  \") h LEFT JOIN " (tbl schema "resource_data" db) " rd ON rd.resource_id = h.id AND rd.version_id = h.version_id \" +
                  \"ORDER BY h.version_id DESC, h.history_operation DESC " (db-paginate-params (pp 2 db) (pp 3 db) db) "\"
            : \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY version_id DESC, history_operation DESC "(db-paginate-params (pp 2 db) (pp 3 db) db) "\"") ($ "\""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE id = "(pp 1 db)" AND history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY version_id DESC, history_operation DESC "(db-paginate-params (pp 2 db) (pp 3 db) db) "\""))";

        await using var cmd2 = "(new-cmd-alias db)"sql, conn);
"(add-param-8-for "cmd2" 1 "id" db)"
"(add-param-8-for "cmd2" 2 "count" db)"
"(add-param-8-for "cmd2" 3 "offset" db)
"

        var rows = new List<"(r-row rn)">();
        await using var reader = await cmd2.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
"(if (r-generic? rn) "            if (HasDataTable) { rows.Add(ReadHistoryRowWithData(reader)); continue; }
" "")
"            rows.Add("(r-new-row rn)"
                reader.GetString(0),
                reader.GetInt32(1),
                Utc(reader.GetDateTime(2)),
                reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3),
                reader.GetString(4)));
        }
        return (rows, total);
    }

"))

; ===================================================================
; Section: TypeHistoryAsync
; ===================================================================

(define (repo-type-history rn schema db)
    ($
"    public async Task<(List<"(r-row rn)"> rows, int total)> TypeHistoryAsync(int count, int offset)
    {
"       (open-conn db)
"
        int total;
        await using (var cmd = "(new-cmd-alias db)"
            \"SELECT (SELECT COUNT(*) FROM "(tbl-public rn db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)") + (SELECT COUNT(*) FROM "(tbl schema "resource_history" db)" WHERE history_operation = 'DELETE')\", conn))
        {
            total = Convert.ToInt32(await cmd.ExecuteScalarAsync());
        }

        var sql = "(if (r-generic? rn) ($ "HasDataTable
            ? \"SELECT h.id, h.version_id, h.last_updated, h.content, h.history_operation, rd.data FROM (\" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM " (tbl-public rn db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, history_operation FROM " (tbl schema "resource_history" db) (db-union-close db) " \" +
                  \"UNION ALL \" +
                  \"" (db-union-open db) "SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM " (tbl schema "resource_history" db) " WHERE history_operation = 'DELETE'" (db-union-close db) "\" +
                  \") h LEFT JOIN " (tbl schema "resource_data" db) " rd ON rd.resource_id = h.id AND rd.version_id = h.version_id \" +
                  \"ORDER BY h.last_updated DESC, h.history_operation DESC " (db-paginate-params (pp 1 db) (pp 2 db) db) "\"
            : \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY last_updated DESC, history_operation DESC "(db-paginate-params (pp 1 db) (pp 2 db) db) "\"") ($ "\""(db-union-open db)"SELECT id, version_id, last_updated, content, 'CURRENT' as history_operation FROM "(tbl-public rn db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, history_operation FROM "(tbl schema "resource_history" db) (db-union-close db)" \" +
                  \"UNION ALL \" +
                  \""(db-union-open db)"SELECT id, version_id, last_updated, content, 'TOMBSTONE' as history_operation FROM "(tbl schema "resource_history" db)" WHERE history_operation = 'DELETE'"(db-union-close db)" \" +
                  \"ORDER BY last_updated DESC, history_operation DESC "(db-paginate-params (pp 1 db) (pp 2 db) db) "\""))";

        await using var cmd2 = "(new-cmd-alias db)"sql, conn);
"(add-param-8-for "cmd2" 1 "count" db)"
"(add-param-8-for "cmd2" 2 "offset" db)
"

        var rows = new List<"(r-row rn)">();
        await using var reader = await cmd2.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
"(if (r-generic? rn) "            if (HasDataTable) { rows.Add(ReadHistoryRowWithData(reader)); continue; }
" "")
"            rows.Add("(r-new-row rn)"
                reader.GetString(0),
                reader.GetInt32(1),
                Utc(reader.GetDateTime(2)),
                reader.IsDBNull(3) ? null : reader.GetFieldValue<""byte[]>(3),
                reader.GetString(4)));
        }
        return (rows, total);
    }

"))

; ===================================================================
; Section: AutocompleteAsync
; ===================================================================


(define (repo-autocomplete-body schema db)
    ($
"    public async Task<""List<""string>> AutocompleteAsync(string param, string value, string property, int? count, int? offset)
    {
"       (open-conn db)
"
        var (propTable, propColumn, propIsToken) = ResolveSearchTable(property);
        if (propTable == null && property != \"_id\") return new List<""string>();

        if (param == \"_id\" && propTable != null)
        {
            await using var cmd = "(new-cmd-alias db)"
"
    ($"                $\"SELECT DISTINCT {propColumn} FROM "(tbl-interp schema "propTable" db)" WHERE resource_id = {P(1)} AND {propColumn} IS NOT NULL\", conn);
")
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
    " $\""
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) " LIMIT {count} OFFSET {offset}")
        ((SqlServer) " OFFSET {offset} ROWS FETCH NEXT {count} ROWS ONLY"))))
    "\","
"
            (not null, null) => "
    " $\""
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) " LIMIT {count}")
        ((SqlServer) " OFFSET 0 ROWS FETCH NEXT {count} ROWS ONLY"))))
    "\","
"
            (null, not null) => "
    " $\""
    (db3 db (lambda (db) (case db
        ((PostgreSql Sqlite) " LIMIT -1 OFFSET {offset}")
        ((SqlServer) " OFFSET {offset} ROWS"))))
    "\","
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

            await using var cmd4 = "(new-cmd-alias db)"
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

            await using var cmd2 = "(new-cmd-alias db)"
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

        await using var cmd3 = "(new-cmd-alias db)"
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
           (("string" "uri") ($"value " (db-search-text-type db)))
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
        (("string" "uri") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"))
        (("token") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"
                     (if (or (string=? "identifier" sn) (string=? "language" sn))
                         ($"; " ci "ix_" schema "_" tn "_system ON " (tbl schema tn db) " (system)")
                         "")))
        (("date") ($"; " ci "ix_" schema "_" tn "_value_low ON " (tbl schema tn db) " (value_low)"
                    "; " ci "ix_" schema "_" tn "_value_high ON " (tbl schema tn db) " (value_high)"))
        (("reference") ($"; " ci "ix_" schema "_" tn "_reference ON " (tbl schema tn db) " (target_id, target_type)"))
        (("number" "quantity") ($"; " ci "ix_" schema "_" tn "_value ON " (tbl schema tn db) " (value)"))
        (else ""))))


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
    "Db.TsParam(1, before.Value)")

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
"
    (if (data-table? rn db) (data-guard rn ($"
        // the bytes of the versions that go (no foreign key takes them)
        await using (var cmd = "(new-cmd db)"$\"DELETE FROM "(tbl schema "resource_data" db)" WHERE resource_id IN ({selection})\", conn))
        {
            if (before != null)
                cmd.Parameters.Add("(purge-before-param db)");
            await cmd.ExecuteNonQueryAsync();
        }
")) "")
"
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
    ; (a single repository has it from its base class)
    ""
"    private static "rn"Resource Read"rn"Resource(ReaderType reader)
    {
        return new "rn"Resource(
            reader.GetString(0),
            reader.GetInt32(1),
            Utc(reader.GetDateTime(2)),
            reader.GetFieldValue<""byte[]>(3)"(repo-reader-fields inline-list has-history)");
    }

"))

; ===================================================================
; Section: InsertSearchIndexesAsync
; ===================================================================


; Quantity index line for InsertSearchIndexesAsync
(define (repo-quantity-index-line rn search)
    (let* ((sn (% "name" search))
           (tn (search-table-name search)))
        ($"        await InsertQuantityIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor.Extract"(kebab-to-pascal sn)"(content));
")))

; The insert helper of a date table with a row for every item of a list:
; emitted for a repository that has one (search-is-date-list?).
(define (repo-insert-date-list-helper schema db)
    ($
"    private static async Task InsertDateListIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""(DateTime low, DateTime high)> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = "(new-cmd-alias db)"
                $\"INSERT INTO "(tbl-interp schema "table" db)" (id, resource_id, value_low, value_high) VALUES ({P(1)}, {P(2)}, {P(3)}, {P(4)})\", conn, tx);
            cmd.Parameters.Add(NewParam(1, i));
            cmd.Parameters.Add(NewParam(2, resourceId));
            cmd.Parameters.Add(NewTsParam(3, values[i].low));
            cmd.Parameters.Add(NewTsParam(4, values[i].high));
            await cmd.ExecuteNonQueryAsync();
        }
    }

"))

; The insert helper of a quantity table: emitted for a repository that has one.
(define (repo-insert-quantity-helper schema db)
    ($
"    private static async Task InsertQuantityIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""(decimal value, string? system, string? code, string? unit)> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = "(new-cmd-alias db)"
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
        (if (search-is-date-list? search)
            ; a list: a row for every item that has a date
            ($"        await InsertDateListIndexAsync(conn, tx, \"" tn "\", resourceId, "rn"SearchExtractor." method "(content));
")
        ($"        var " var-name " = "rn"SearchExtractor." method "(content);
        if (" var-name ".HasValue)
            await InsertDateIndexAsync(conn, tx, \"" tn "\", resourceId, " var-name ".Value.low, " var-name ".Value.high);
"))))

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
; Section: Insert helper methods
; ===================================================================

(define (repo-insert-helpers schema db)
    ($
"    private static async Task InsertStringIndexAsync(ConnType conn, TxType tx, string table, string resourceId, List<""string> values)
    {
        for (int i = 0; i < values.Count; i++)
        {
            await using var cmd = "(new-cmd-alias db)"
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
            await using var cmd = "(new-cmd-alias db)"
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
        await using var cmd = "(new-cmd-alias db)"
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
            await using var cmd = "(new-cmd-alias db)"
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
            await using var cmd = "(new-cmd-alias db)"
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


; --- Sorting through a reference ---

; The keys every active resource offers to a chained sort, as data (SortTargets.cs).
(define (sort-target-keys)
    (apply $ (map (lambda (r)
        (let ((rn (name-of r)))
            ($
                (apply $ (map (lambda (s)
                    ($"        [\"" rn "." (% "name" s) "\"] = ('i', \"" (search-col s) "\"),
"))
                    (node-list->list (inline-searches r))))
                (apply $ (map (lambda (s)
                    (let ((t (% "type" s)))
                        (if (or (string=? "string" t) (string=? "token" t) (string=? "date" t) (string=? "number" t))
                            ($"        [\"" rn "." (% "name" s) "\"] = ('" (if (string=? "date" t) "d" "t") "', \"" (search-table-name s) "\"),
")
                            "")))
                    (node-list->list (table-searches r)))))))
        (node-list->list (active-resources)))))

(define (sort-target-types)
    (apply $ (map (lambda (r) ($"        \"" (name-of r) "\",
")) (node-list->list (active-resources)))))

(define (sort-targets-file)
    (file "SortTargets.cs" ($
"namespace "(project-name)".Repositories;

// What a resource may be sorted by through a reference to it (_sort=subject:Patient.family):
// a column of its own, the time of its last change, or a search table of strings, tokens,
// numbers (the value) or dates (the lower bound). The key of a search is put together here
// when it is asked for.
internal static class SortTargets
{
    // \"<""Type>.<""parameter>\": 'i' and the column, 't' or 'd' and the search table
    private static readonly Dictionary<""string, (char kind, string name)> Keys = new()
    {
"(sort-target-keys)"    };

    public static readonly string[] AllTypes =
    {
"(sort-target-types)"    };

    private static readonly HashSet<""string> Types = new(AllTypes);

    public static bool Known(string type) => Types.Contains(type);

    // The column the key `parameter` of `target` sorts by, reached through the reference whose
    // rows lie in `table` of `schema`; null when the target has no such key.
    public static string? Expression(string schema, string table, string target, string parameter)
    {
        var select = Db.Kind == DbKind.SqlServer ? \"SELECT TOP 1\" : \"SELECT\";
        var limit = Db.Kind == DbKind.SqlServer ? \"\" : \" LIMIT 1\";
        var source = Db.SchemaTable(schema, table);
        if (parameter == \"_lastUpdated\")
            return Types.Contains(target)
                ? $\"({select} t.last_updated FROM {source} src JOIN {Db.Table(target)} t ON t.id = src.target_id WHERE src.resource_id = r.id{limit})\"
                : null;
        if (!Keys.TryGetValue(target + \".\" + parameter, out var key)) return null;
        return key.kind == 'i'
            ? $\"({select} t.{Db.Col(key.name)} FROM {source} src JOIN {Db.Table(target)} t ON t.id = src.target_id WHERE src.resource_id = r.id{limit})\"
            : $\"({select} c.{(key.kind == 'd' ? \"value_low\" : \"value\")} FROM {source} src JOIN {Db.SchemaTable(target, key.name)} c ON c.resource_id = src.target_id WHERE src.resource_id = r.id{limit})\";
    }
}
")))

; The active types a reference parameter may point to, by its first path.
(define (ref-sort-targets ref-search)
    (let* ((paths-node (select-children "paths" ref-search))
           (path-nodes (if (node-list-empty? paths-node) (empty-node-list)
                          (select-children "path" (node-list-first paths-node))))
           (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
           (targets-node (if first-path (select-children "targets" first-path) (empty-node-list)))
           (target-nodes (if (node-list-empty? targets-node) '()
                            (node-list->list (children (node-list-first targets-node))))))
        (filter (lambda (rn) (let ((e (if rn (element-with-id (downcase-string rn)) #f))) (and e (active? e))))
                (map (lambda (tgt) (% "resource" tgt)) target-nodes))))

; A resource's part of it: its reference parameters with their tables and the types each may
; point to, and the key put together from them.
(define (repo-chain-sort schema ref-searches)
    ($
"    // The reference parameters a sort may go through: the table of each, and the types it may
    // point to.
    private static readonly Dictionary<""string, (string table, string[] targets)> SortReferences = new()
    {
"
    (apply $ (map (lambda (ref-s)
        (let ((targets (ref-sort-targets ref-s))
              (all (length (node-list->list (node-list-filter active? (children (parent (current-node))))))))
            ; (a reference to any resource: the list of all of them, kept once)
            ($"        [\"" (% "name" ref-s) "\"] = (\"" (search-table-name ref-s) "\", "
                (if (and (> all 8) (= (length targets) all))
                    "SortTargets.AllTypes"
                    ($ "new string[] {" (apply $ (map (lambda (rn) ($ " \"" rn "\",")) targets)) " }"))
                "),
")))
        ref-searches))
"    };

    // The sort key \"reference:Type.parameter\"; null when it is none of this resource's.
    private static string? ChainSort(string key)
    {
        var colon = key.IndexOf(':');
        var dot = colon < 0 ? -1 : key.IndexOf('.', colon + 1);
        if (dot < 0) return null;
        if (!SortReferences.TryGetValue(key[..colon], out var reference)) return null;
        var target = key[(colon + 1)..dot];
        if (Array.IndexOf(reference.targets, target) < 0) return null;
        return SortTargets.Expression(\"" schema "\", reference.table, target, key[(dot + 1)..]);
    }

"))

; --- BuildOrderBy is defined in orderby.scm (per-project; see map.dsl) ---


; ===================================================================
; Section: SearchQueryBuilder (one per backend, generated from resources)
; ===================================================================

; The builder of the single repositories and Db beside it; a builder for each database is
; written only for the resources that still have a repository for each (single-repository?).
(define (search-query-builders)
    (make directory path: "Repositories"
    (apply sosofo-append
        (file "Db.cs" (db-runtime-cs))
        (repo-generic-interfaces)
        (sort-targets-file)
        (repo-base-files)
        (list (file "SearchQueryBuilder.cs"
            ($ (sqb-header 'Any) (sqb-body 'Any)"}
"))))))

(define (db-runtime-cs) ($
"using System.Data.Common;
using System.Text;

namespace "(project-name)".Repositories;

public enum DbKind { PostgreSql, Sqlite, SqlServer }

// The database the server runs on, chosen once when it starts. A repository is written once for
// all three; what differs between them is asked here.
//
// Where the three want another TEXT inside an SQL string, the string holds all three between
// marks --  postgresql  sqlite  sqlserver  -- and Sql takes the one of
// this database when the command is made. So each database is sent exactly the text a repository
// of its own would have sent.
public static class Db
{
    public static DbKind Kind { get; private set; }
    private static Npgsql.NpgsqlDataSource? _dataSource;
    private static string _connectionString = \"\";
    private static readonly string[] Named = Enumerable.Range(0, 257).Select(i => \"@p\" + i).ToArray();
    private static readonly string[] Numbered = Enumerable.Range(0, 257).Select(i => \"$\" + i).ToArray();

    public static void UsePostgreSql(Npgsql.NpgsqlDataSource dataSource) { Kind = DbKind.PostgreSql; _dataSource = dataSource; }
    public static void UseSqlServer(string connectionString) { Kind = DbKind.SqlServer; _connectionString = connectionString; }
    public static void UseSqlite(string connectionString) { Kind = DbKind.Sqlite; _connectionString = connectionString; }

    public static async Task<""DbConnection> OpenAsync()
    {
        switch (Kind)
        {
            case DbKind.PostgreSql:
                return await _dataSource!.OpenConnectionAsync();
            case DbKind.SqlServer:
            {
                var conn = new Microsoft.Data.SqlClient.SqlConnection(_connectionString);
                await conn.OpenAsync();
                return conn;
            }
            default:
            {
                var conn = new Microsoft.Data.Sqlite.SqliteConnection(_connectionString);
                await conn.OpenAsync();
                return conn;
            }
        }
    }

    public static DbCommand Cmd(string sql, DbConnection conn, DbTransaction? tx = null) => Kind switch
    {
        DbKind.PostgreSql => new Npgsql.NpgsqlCommand(Sql(sql), (Npgsql.NpgsqlConnection)conn, (Npgsql.NpgsqlTransaction?)tx),
        DbKind.SqlServer => new Microsoft.Data.SqlClient.SqlCommand(Sql(sql), (Microsoft.Data.SqlClient.SqlConnection)conn, (Microsoft.Data.SqlClient.SqlTransaction?)tx),
        _ => new Microsoft.Data.Sqlite.SqliteCommand(Sql(sql), (Microsoft.Data.Sqlite.SqliteConnection)conn, (Microsoft.Data.Sqlite.SqliteTransaction?)tx)
    };

    // The n-th placeholder as the database writes it, and the name its parameter has.
    public static string P(int n) => Kind == DbKind.PostgreSql
        ? (n < Numbered.Length ? Numbered[n] : \"$\" + n)
        : Name(n);
    private static string Name(int n) => n < Named.Length ? Named[n] : \"@p\" + n;

    // The n-th parameter of a command: by its place on PostgreSQL, by its name elsewhere.
    public static void Add(DbCommand cmd, int n, object? value)
    {
        switch (Kind)
        {
            case DbKind.PostgreSql: ((Npgsql.NpgsqlCommand)cmd).Parameters.AddWithValue(value!); break;
            case DbKind.SqlServer: ((Microsoft.Data.SqlClient.SqlCommand)cmd).Parameters.AddWithValue(Name(n), value); break;
            default: ((Microsoft.Data.Sqlite.SqliteCommand)cmd).Parameters.AddWithValue(Name(n), value); break;
        }
    }

    // The same - but on SQL Server a time as DATETIME2: AddWithValue alone takes a DateTime for
    // the old DATETIME, whose steps are a 300th of a second, and last_updated and every instant
    // in a column of its own were stored rounded to that.
    public static void AddValue(DbCommand cmd, int n, object? value)
    {
        if (Kind != DbKind.SqlServer) { Add(cmd, n, value); return; }
        var p = ((Microsoft.Data.SqlClient.SqlCommand)cmd).Parameters.AddWithValue(Name(n), value);
        if (value is DateTime) p.SqlDbType = System.Data.SqlDbType.DateTime2;
    }

    public static DbParameter Param(int index, object value) => Kind switch
    {
        DbKind.PostgreSql => new Npgsql.NpgsqlParameter { Value = value },
        DbKind.SqlServer => new Microsoft.Data.SqlClient.SqlParameter(Name(index), value),
        _ => new Microsoft.Data.Sqlite.SqliteParameter(Name(index), value)
    };

    public static DbParameter TsParam(int index, DateTime value) => Kind switch
    {
        DbKind.PostgreSql => new Npgsql.NpgsqlParameter { Value = value, NpgsqlDbType = NpgsqlTypes.NpgsqlDbType.TimestampTz },
        DbKind.SqlServer => new Microsoft.Data.SqlClient.SqlParameter(Name(index), value) { SqlDbType = System.Data.SqlDbType.DateTime2 },
        _ => new Microsoft.Data.Sqlite.SqliteParameter(Name(index), value)
    };

    public static DbParameter CopyParam(DbParameter source)
    {
        switch (Kind)
        {
            case DbKind.PostgreSql:
            {
                var from = (Npgsql.NpgsqlParameter)source;
                var p = new Npgsql.NpgsqlParameter { Value = from.Value };
                if (from.NpgsqlDbType != NpgsqlTypes.NpgsqlDbType.Unknown)
                    p.NpgsqlDbType = from.NpgsqlDbType;
                return p;
            }
            case DbKind.SqlServer:
            {
                var from = (Microsoft.Data.SqlClient.SqlParameter)source;
                return new Microsoft.Data.SqlClient.SqlParameter(from.ParameterName, from.Value) { SqlDbType = from.SqlDbType };
            }
            default:
                return new Microsoft.Data.Sqlite.SqliteParameter(source.ParameterName, source.Value);
        }
    }

    // A resource's own table, and a column of it, as each database names them in upstream's SQL.
    public static string Table(string name) => Kind == DbKind.SqlServer ? \"[\" + name + \"]\" : \"\\\"\" + name + \"\\\"\";
    public static string Col(string name) => Kind switch
    {
        DbKind.PostgreSql => \"\\\"\" + name + \"\\\"\",
        DbKind.SqlServer => \"[\" + name + \"]\",
        _ => name
    };

    // A table of a resource's schema; SQLite has no schemas, the schema is the front of the name.
    public static string SchemaTable(string schema, string table) => Kind switch
    {
        DbKind.PostgreSql => \"\\\"\" + schema + \"\\\"\" + \".\" + table,
        DbKind.SqlServer => \"[\" + schema + \"].\" + table,
        _ => schema + \"_\" + table
    };

    private const char Open = '', Next = '', Close = '';

    // sql with, of every marked place, the text of this database. A marked place may lie within
    // another one's text (a placeholder inside a paging clause).
    public static string Sql(string sql)
    {
        var at = sql.IndexOf(Open);
        if (at < 0) return sql;
        var which = (int)Kind;
        var sb = new StringBuilder(sql.Length);
        sb.Append(sql, 0, at);
        Span<""int> part = stackalloc int[16];
        int depth = 0, skipped = 0, from = at;
        for (var i = at; i < sql.Length; i++)
        {
            var c = sql[i];
            if (c != Open && c != Next && c != Close) continue;
            if (skipped == 0) sb.Append(sql, from, i - from);
            from = i + 1;
            if (c == Open)
            {
                part[depth++] = 0;
                if (which != 0) skipped++;
            }
            else if (c == Next)
            {
                var was = part[depth - 1] == which;
                var now = ++part[depth - 1] == which;
                if (was && !now) skipped++;
                else if (!was && now) skipped--;
            }
            else
            {
                if (part[depth - 1] != which) skipped--;
                depth--;
            }
        }
        sb.Append(sql, from, sql.Length - from);
        return sb.ToString();
    }
}
"))

(define (sqb-header db)
    ($
"using ParamType = System.Data.Common.DbParameter;

namespace "(project-name)".Repositories;

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
    (if (any-uri-table-search?) (sqb-add-uri-search-or db) "")
    (sqb-add-reference-search-or db)))

(define (sqb-schema-table db)
    "    private static string SchemaTable(string schema, string table) => Db.SchemaTable(schema, table);

")

(define (sqb-helpers db)
    "    private static string P(int n) => Db.P(n);

    private static string NextAlias(ref int aliasIndex) => $\"s{aliasIndex++}\";

    private static ParamType NewParam(int index, object value) => Db.Param(index, value);
    private static ParamType NewTsParam(int index, DateTime value) => Db.TsParam(index, value);

")

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

; Does any active resource of the model have a uri search? The query helper
; is emitted only then.
(define (any-uri-table-search?)
    (let loop ((rs (node-list->list (select-elements (descendants (tree-root (current-node))) "resource"))))
        (cond ((null? rs) #f)
              ((and (active? (car rs)) (not (node-list-empty? (uri-table-searches (car rs))))) #t)
              (else (loop (cdr rs))))))

; The first n characters of a column, n written by the caller in C#.
(define (db-left-cs column-cs count-cs db)
    (db3 db (lambda (db) (case db
        ((Sqlite) ($ "SUBSTR(" column-cs ", 1, " count-cs ")"))
        (else ($ "LEFT(" column-cs ", " count-cs ")"))))))

; Uri search: the whole value, as it is written (no case folding, no
; prefix). :below takes what stands at or under the value, :above what the
; value stands at or under -- by path steps, a slash between them.
(define (sqb-add-uri-search-or db)
    ($
"    internal static void AddUriSearchOr(string schema, List<""string> joins, List<""string> wheres, List<""ParamType> parameters,
        ref int paramIndex, ref int aliasIndex, string table, string[] values, string modifier)
    {
        var alias = NextAlias(ref aliasIndex);
        joins.Add($\"JOIN {SchemaTable(schema, table)} {alias} ON {alias}.resource_id = r.id\");

        var orParts = new List<""string>();
        foreach (var value in values)
        {
            switch (modifier)
            {
                case \"below\":
                {
                    var under = value.TrimEnd('/') + \"/\";
                    orParts.Add($\"({alias}.value = {P(paramIndex)} OR " (db-left-cs "{alias}.value" "{under.Length}" db) " = {P(paramIndex + 1)})\");
                    parameters.Add(NewParam(paramIndex, value));
                    parameters.Add(NewParam(paramIndex + 1, under));
                    paramIndex += 2;
                    break;
                }
                case \"above\":
                {
                    // the value and every step above it, down to its authority
                    var steps = new List<""string> { value };
                    var floor = value.IndexOf(\"://\", StringComparison.Ordinal);
                    floor = floor < 0 ? 0 : floor + 3;
                    for (var at = value.LastIndexOf('/'); at > floor; at = value.LastIndexOf('/', at - 1))
                    {
                        var step = value[..at];
                        if (step.Length > 0 && !step.EndsWith('/')) steps.Add(step);
                    }
                    var marks = new List<""string>();
                    foreach (var step in steps)
                    {
                        marks.Add($\"{P(paramIndex)}\");
                        parameters.Add(NewParam(paramIndex, step));
                        paramIndex++;
                    }
                    orParts.Add($\"{alias}.value IN ({string.Join(\", \", marks)})\");
                    break;
                }
                default:
                    orParts.Add($\"{alias}.value = {P(paramIndex)}\");
                    parameters.Add(NewParam(paramIndex, value));
                    paramIndex++;
                    break;
            }
        }

        wheres.Add(orParts.Count == 1 ? orParts[0] : $\"({string.Join(\" OR \", orParts)})\");
    }

"))

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
