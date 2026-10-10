; Boilerplate generator - generates invariant and cross-resource files
; Called from (element resources ...) context

(define (boilerplate)
    (sosofo-append
        (boilerplate-csproj)
        (boilerplate-appsettings)
        (boilerplate-fhir-response)
        (result)
        (routing)
        (boilerplate-fhir-xml-serializer)
        (boilerplate-json-patcher)
        (boilerplate-fhir-validator)
        (boilerplate-fhir-date-helper)
        (boilerplate-program-cs)
        (boilerplate-database-cs)
        (database-providers)
        (boilerplate-seed-data-loader)
        (boilerplate-search-helper)
        (boilerplate-composite-search-helper)
        (make directory path: "Handlers"
            (boilerplate-bundle-handler)
            (boilerplate-metadata-handler)
            (boilerplate-search-parameter-handler)
            (boilerplate-system-history-handler))
        (make directory path: "Repositories"
            (boilerplate-concurrency-exception))
        (make directory path: "wwwroot"
            (make directory path: "js"
                (bundle-js)
                (operationoutcome-js))
            (make directory path: "css"
                (empty-sosofo)))))

; --- Fire.csproj ---

(define (boilerplate-csproj)
    (file ($ (project-name) ".csproj") ($
<![CDATA["<Project Sdk=\"Microsoft.NET.Sdk.Web\">

  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>

  <ItemGroup>
    <ProjectReference Include=\""(model-ref)"/"(project-name)".Model.csproj\" />
  </ItemGroup>

  <ItemGroup>
    <PackageReference Include=\"Npgsql\" Version=\"9.0.3\" />
    <PackageReference Include=\"Microsoft.Data.SqlClient\" Version=\"5.2.2\" />
    <PackageReference Include=\"Microsoft.Data.Sqlite\" Version=\"9.0.0\" />
    <PackageReference Include=\"SQLitePCLRaw.lib.e_sqlite3\" Version=\"2.1.12\" />
    <PackageReference Include=\"Microsoft.Extensions.Hosting.WindowsServices\" Version=\"9.0.0\" />
  </ItemGroup>

</Project>
"]]>)))

; --- appsettings.json ---

(define (boilerplate-appsettings)
    (file "appsettings.json" ($
"{
  \"Logging\": {
    \"LogLevel\": {
      \"Default\": \"Information\",
      \"Microsoft.AspNetCore\": \"Warning\"
    }
  },
  \"Kestrel\": {
    \"Endpoints\": {
      \"MyHttpEndpoint\": {
        \"Url\": \"http://localhost:5080\"
      }
    }
  },
  \"AllowedHosts\": \"*\",
  \"externalUrl\": \"http://localhost:5080/" (route-prefix) "\",
  \"Origins\": [\"*\"],
  \"DatabaseType\": \"PostgreSql\",
  \"ConnectionString\": \"Host=localhost;Database=" (database-name) ";Username=postgres;Password=postgres\""
(if (fhir-seed-path) ($",
  \"SeedDataPath\": \"" (fhir-seed-path) "\"") "")
",
  \"ReseedSecret\": \"00000000-0000-0000-0000-000000000000\"
}
")))


; --- FhirResponse.cs ---

(define (boilerplate-fhir-response)
    (file "FhirResponse.cs" ($
"using System.Text.Json.Nodes;

namespace "(project-name)";

public static class FhirResponse
{
    public static JsonObject OperationOutcome(string severity, string code, string diagnostics)
    {
        return new JsonObject
        {
            [\"resourceType\"] = \"OperationOutcome\",
            [\"issue\"] = new JsonArray
            {
                new JsonObject
                {
                    [\"severity\"] = severity,
                    [\"code\"] = code,
                    [\"diagnostics\"] = diagnostics
                }
            }
        };
    }

    public static JsonObject OperationOutcome(List<""string> diagnostics)
    {
        return new JsonObject
        {
            [\"resourceType\"] = \"OperationOutcome\",
            [\"issue\"] = new JsonArray(diagnostics.Select(d => (JsonNode)new JsonObject
            {
                [\"severity\"] = \"error\",
                [\"code\"] = \"structure\",
                [\"diagnostics\"] = d
            }).ToArray())
        };
    }

    public static JsonObject SearchBundle(
        List<""JsonObject> entries, int total, string selfUrl, int count, int offset)
    {
        var bundle = new JsonObject
        {
            [\"resourceType\"] = \"Bundle\",
            [\"type\"] = \"searchset\",
            [\"link\"] = BuildSearchLinks(selfUrl, total, count, offset)
        };

        if (total >= 0)
            bundle[\"total\"] = total;

        bundle[\"entry\"] = new JsonArray(entries.Select(e => (JsonNode)e).ToArray());

        return bundle;
    }

    public static JsonObject HistoryBundle(List<""JsonObject> entries, int total, string selfUrl, int count, int offset)
    {
        var bundle = new JsonObject
        {
            [\"resourceType\"] = \"Bundle\",
            [\"type\"] = \"history\",
            [\"total\"] = total,
            [\"link\"] = BuildSearchLinks(selfUrl, total, count, offset)
        };

        if (entries.Count > 0)
            bundle[\"entry\"] = new JsonArray(entries.Select(e => (JsonNode)e).ToArray());

        return bundle;
    }

    public static JsonObject SearchEntry(JsonNode resource, string fullUrl)
    {
        return new JsonObject
        {
            [\"fullUrl\"] = fullUrl,
            [\"resource\"] = resource.DeepClone(),
            [\"search\"] = new JsonObject { [\"mode\"] = \"match\" }
        };
    }

    public static JsonObject IncludeEntry(JsonNode resource, string fullUrl)
    {
        return new JsonObject
        {
            [\"fullUrl\"] = fullUrl,
            [\"resource\"] = resource.DeepClone(),
            [\"search\"] = new JsonObject { [\"mode\"] = \"include\" }
        };
    }

    public static JsonObject HistoryEntry(JsonNode resource, string fullUrl, string method, string url, string status, string lastModified)
    {
        return new JsonObject
        {
            [\"fullUrl\"] = fullUrl,
            [\"resource\"] = resource.DeepClone(),
            [\"request\"] = new JsonObject
            {
                [\"method\"] = method,
                [\"url\"] = url
            },
            [\"response\"] = new JsonObject
            {
                [\"status\"] = status,
                [\"lastModified\"] = lastModified
            }
        };
    }

    public static JsonObject HistoryDeleteEntry(string fullUrl, string method, string url, string status, string lastModified)
    {
        return new JsonObject
        {
            [\"fullUrl\"] = fullUrl,
            [\"request\"] = new JsonObject
            {
                [\"method\"] = method,
                [\"url\"] = url
            },
            [\"response\"] = new JsonObject
            {
                [\"status\"] = status,
                [\"lastModified\"] = lastModified
            }
        };
    }

    private static JsonArray BuildSearchLinks(string selfUrl, int total, int count, int offset)
    {
        var links = new JsonArray
        {
            new JsonObject { [\"relation\"] = \"self\", [\"url\"] = selfUrl }
        };

        if (offset + count < total)
        {
            var nextUrl = ReplaceOffsetInUrl(selfUrl, offset + count, count);
            links.Add(new JsonObject { [\"relation\"] = \"next\", [\"url\"] = nextUrl });
        }

        if (offset > 0)
        {
            var prevOffset = Math.Max(0, offset - count);
            var prevUrl = ReplaceOffsetInUrl(selfUrl, prevOffset, count);
            links.Add(new JsonObject { [\"relation\"] = \"previous\", [\"url\"] = prevUrl });
        }

        return links;
    }

    private static string ReplaceOffsetInUrl(string url, int newOffset, int count)
    {
        var uri = new UriBuilder(url);
        var query = System.Web.HttpUtility.ParseQueryString(uri.Query);
        query[\"_offset\"] = newOffset.ToString();
        query[\"_count\"] = count.ToString();
        uri.Query = query.ToString();
        return uri.ToString();
    }
}
")))

; --- FhirXmlSerializer.cs ---

(define (boilerplate-fhir-xml-serializer)
    (file "FhirXmlSerializer.cs" ($
"using System.Security;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace "(project-name)";

public static class FhirXmlSerializer
{
    private const string FhirNamespace = \"http://hl7.org/fhir\";
    private const string XhtmlNamespace = \"http://www.w3.org/1999/xhtml\";

    public static string ToXml(JsonNode node)
    {
        var sb = new StringBuilder();
        sb.AppendLine(\"<""?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>\");

        if (node is JsonObject obj)
        {
            var resourceType = obj[\"resourceType\"]?.GetValue<""string>();
            if (resourceType != null)
            {
                sb.Append('<').Append(resourceType).Append(\" xmlns=\\\"\").Append(FhirNamespace).AppendLine(\"\\\">\");
                SerializeProperties(sb, obj, 1);
                sb.Append(\"</\").Append(resourceType).AppendLine(\">\");
            }
        }

        return sb.ToString();
    }

    private static void SerializeProperties(StringBuilder sb, JsonObject obj, int depth)
    {
        var indent = new string(' ', depth * 2);

        foreach (var prop in obj)
        {
            if (prop.Key == \"resourceType\") continue;
            if (prop.Key.StartsWith(\"_\")) continue;

            var shadowKey = \"_\" + prop.Key;
            var shadow = obj.ContainsKey(shadowKey) ? obj[shadowKey] : null;

            SerializeProperty(sb, prop.Key, prop.Value, shadow, depth, indent);
        }

        // Handle standalone shadow properties (e.g., _birthDate without birthDate)
        foreach (var prop in obj)
        {
            if (!prop.Key.StartsWith(\"_\")) continue;
            var baseKey = prop.Key.Substring(1);
            if (obj.ContainsKey(baseKey)) continue;
            if (baseKey == \"\") continue;

            SerializeShadowOnly(sb, baseKey, prop.Value, depth, indent);
        }
    }

    private static void SerializeProperty(StringBuilder sb, string name, JsonNode? value, JsonNode? shadow, int depth, string indent)
    {
        if (value == null) return;

        if (value is JsonArray arr)
        {
            var shadowArr = shadow as JsonArray;
            for (int i = 0; i < arr.Count; i++)
            {
                var itemShadow = shadowArr != null && i < shadowArr.Count ? shadowArr[i] : null;
                SerializeSingleValue(sb, name, arr[i], itemShadow, depth, indent);
            }
        }
        else
        {
            SerializeSingleValue(sb, name, value, shadow, depth, indent);
        }
    }

    private static void SerializeSingleValue(StringBuilder sb, string name, JsonNode? value, JsonNode? shadow, int depth, string indent)
    {
        if (value == null)
        {
            if (shadow != null)
                SerializeShadowOnly(sb, name, shadow, depth, indent);
            return;
        }

        if (name == \"div\")
        {
            sb.Append(indent);
            var divStr = value.GetValue<""string>();
            sb.AppendLine(divStr);
            return;
        }

        if (value is JsonObject childObj)
        {
            if (name == \"contained\")
            {
                sb.Append(indent).Append(\"<""contained>\");
                var containedType = childObj[\"resourceType\"]?.GetValue<""string>();
                if (containedType != null)
                {
                    sb.AppendLine();
                    sb.Append(indent).Append(\"  <\").Append(containedType).AppendLine(\">\");
                    SerializeProperties(sb, childObj, depth + 2);
                    sb.Append(indent).Append(\"  </\").Append(containedType).AppendLine(\">\");
                }
                sb.Append(indent).AppendLine(\"<""/contained>\");
            }
            else
            {
                sb.Append(indent).Append('<').Append(name).Append('>');
                sb.AppendLine();
                SerializeProperties(sb, childObj, depth + 1);
                sb.Append(indent).Append(\"</\").Append(name).AppendLine(\">\");
            }
        }
        else
        {
            var strValue = FormatPrimitive(value);
            sb.Append(indent).Append('<').Append(name).Append(\" value=\\\"\").Append(SecurityElement.Escape(strValue)).Append('\\\"');

            if (shadow is JsonObject shadowObj && shadowObj.Count > 0)
            {
                sb.AppendLine(\">\");
                SerializeProperties(sb, shadowObj, depth + 1);
                sb.Append(indent).Append(\"</\").Append(name).AppendLine(\">\");
            }
            else
            {
                sb.AppendLine(\"/>\");
            }
        }
    }

    private static void SerializeShadowOnly(StringBuilder sb, string name, JsonNode? shadow, int depth, string indent)
    {
        if (shadow == null) return;

        if (shadow is JsonArray shadowArr)
        {
            foreach (var item in shadowArr)
            {
                if (item is JsonObject shadowObj && shadowObj.Count > 0)
                {
                    sb.Append(indent).Append('<').Append(name).AppendLine(\">\");
                    SerializeProperties(sb, shadowObj, depth + 1);
                    sb.Append(indent).Append(\"</\").Append(name).AppendLine(\">\");
                }
            }
        }
        else if (shadow is JsonObject shadowObjSingle && shadowObjSingle.Count > 0)
        {
            sb.Append(indent).Append('<').Append(name).AppendLine(\">\");
            SerializeProperties(sb, shadowObjSingle, depth + 1);
            sb.Append(indent).Append(\"</\").Append(name).AppendLine(\">\");
        }
    }

    private static string FormatPrimitive(JsonNode value)
    {
        var kind = value.GetValueKind();
        return kind switch
        {
            JsonValueKind.True => \"true\",
            JsonValueKind.False => \"false\",
            JsonValueKind.Number => value.ToJsonString(),
            _ => value.GetValue<""string>()
        };
    }
}
")))

; --- ConcurrencyException.cs ---

(define (boilerplate-concurrency-exception)
    (file "ConcurrencyException.cs" ($
"namespace " (project-name) ".Repositories;

public class ConcurrencyException : Exception
{
    public int CurrentVersion { get; }
    public ConcurrencyException(int currentVersion) : base($\"Version conflict: current version is {currentVersion}\")
    {
        CurrentVersion = currentVersion;
    }
}
")))

; --- JsonPatcher.cs ---

(define (boilerplate-json-patcher)
    (file "JsonPatcher.cs" ($
"using System.Text.Json.Nodes;

namespace "(project-name)";

public static class JsonPatcher
{
    public static void Apply(JsonNode document, JsonArray patch)
    {
        foreach (var op in patch)
        {
            var operation = op?[\"op\"]?.GetValue<""string>()
                ?? throw new InvalidOperationException(\"Missing 'op' in patch operation\");
            var path = op[\"path\"]?.GetValue<""string>()
                ?? throw new InvalidOperationException(\"Missing 'path' in patch operation\");

            switch (operation)
            {
                case \"add\":
                    Add(document, path, GetValue(op));
                    break;
                case \"remove\":
                    Remove(document, path);
                    break;
                case \"replace\":
                    Replace(document, path, GetValue(op));
                    break;
                case \"move\":
                    Move(document, path, GetFrom(op));
                    break;
                case \"copy\":
                    Copy(document, path, GetFrom(op));
                    break;
                case \"test\":
                    Test(document, path, GetValue(op));
                    break;
                default:
                    throw new InvalidOperationException($\"Unknown patch operation: {operation}\");
            }
        }
    }

    private static JsonNode? GetValue(JsonNode op)
    {
        if (op[\"value\"] == null && op.AsObject().ContainsKey(\"value\"))
            return null;
        if (!op.AsObject().ContainsKey(\"value\"))
            throw new InvalidOperationException(\"Missing 'value' in patch operation\");
        return op[\"value\"]?.DeepClone();
    }

    private static string GetFrom(JsonNode op)
    {
        return op[\"from\"]?.GetValue<""string>()
            ?? throw new InvalidOperationException(\"Missing 'from' in patch operation\");
    }

    private static (JsonNode parent, string key, int index) ResolvePath(JsonNode document, string path)
    {
        if (string.IsNullOrEmpty(path))
            throw new InvalidOperationException(\"Empty path\");

        var segments = ParsePointer(path);
        JsonNode current = document;

        for (int i = 0; i < segments.Length - 1; i++)
        {
            current = ResolveSegment(current, segments[i]);
        }

        var lastSegment = segments[^1];
        if (current is JsonArray arr)
        {
            if (lastSegment == \"-\")
                return (current, \"-\", arr.Count);
            if (int.TryParse(lastSegment, out var idx))
                return (current, lastSegment, idx);
            throw new InvalidOperationException($\"Invalid array index: {lastSegment}\");
        }

        return (current, lastSegment, -1);
    }

    private static JsonNode ResolveSegment(JsonNode current, string segment)
    {
        if (current is JsonArray arr)
        {
            if (!int.TryParse(segment, out var idx) || idx < 0 || idx >= arr.Count)
                throw new InvalidOperationException($\"Invalid array index: {segment}\");
            return arr[idx] ?? throw new InvalidOperationException($\"Null element at index {idx}\");
        }

        if (current is JsonObject obj)
        {
            return obj[segment] ?? throw new InvalidOperationException($\"Path not found: {segment}\");
        }

        throw new InvalidOperationException(\"Cannot traverse scalar value\");
    }

    private static JsonNode? ResolvePointer(JsonNode document, string path)
    {
        if (path == \"\") return document;
        var segments = ParsePointer(path);
        JsonNode? current = document;

        foreach (var segment in segments)
        {
            if (current is JsonArray arr)
            {
                if (!int.TryParse(segment, out var idx) || idx < 0 || idx >= arr.Count)
                    throw new InvalidOperationException($\"Invalid array index: {segment}\");
                current = arr[idx];
            }
            else if (current is JsonObject obj)
            {
                current = obj[segment];
            }
            else
            {
                throw new InvalidOperationException($\"Cannot traverse path: {segment}\");
            }
        }

        return current;
    }

    private static string[] ParsePointer(string pointer)
    {
        if (!pointer.StartsWith(\"/\"))
            throw new InvalidOperationException($\"Invalid JSON Pointer: {pointer}\");

        return pointer[1..].Split('/').Select(s =>
            s.Replace(\"~1\", \"/\").Replace(\"~0\", \"~\")).ToArray();
    }

    private static void Add(JsonNode document, string path, JsonNode? value)
    {
        var (parent, key, index) = ResolvePath(document, path);

        if (parent is JsonArray arr)
        {
            if (key == \"-\")
                arr.Add(value);
            else if (index >= 0 && index <= arr.Count)
                arr.Insert(index, value);
            else
                throw new InvalidOperationException($\"Array index out of bounds: {index}\");
        }
        else if (parent is JsonObject obj)
        {
            obj[key] = value;
        }
    }

    private static void Remove(JsonNode document, string path)
    {
        var (parent, key, index) = ResolvePath(document, path);

        if (parent is JsonArray arr)
        {
            if (index < 0 || index >= arr.Count)
                throw new InvalidOperationException($\"Array index out of bounds: {index}\");
            arr.RemoveAt(index);
        }
        else if (parent is JsonObject obj)
        {
            if (!obj.ContainsKey(key))
                throw new InvalidOperationException($\"Path not found: {path}\");
            obj.Remove(key);
        }
    }

    private static void Replace(JsonNode document, string path, JsonNode? value)
    {
        var (parent, key, index) = ResolvePath(document, path);

        if (parent is JsonArray arr)
        {
            if (index < 0 || index >= arr.Count)
                throw new InvalidOperationException($\"Array index out of bounds: {index}\");
            arr[index] = value;
        }
        else if (parent is JsonObject obj)
        {
            if (!obj.ContainsKey(key))
                throw new InvalidOperationException($\"Path not found: {path}\");
            obj[key] = value;
        }
    }

    private static void Move(JsonNode document, string path, string from)
    {
        var value = ResolvePointer(document, from)?.DeepClone();
        Remove(document, from);
        Add(document, path, value);
    }

    private static void Copy(JsonNode document, string path, string from)
    {
        var value = ResolvePointer(document, from)?.DeepClone();
        Add(document, path, value);
    }

    private static void Test(JsonNode document, string path, JsonNode? value)
    {
        var current = ResolvePointer(document, path);
        if (value == null && current == null) return;
        if (value == null || current == null)
            throw new InvalidOperationException($\"Test failed at {path}\");

        if (current.ToJsonString() != value.ToJsonString())
            throw new InvalidOperationException($\"Test failed at {path}\");
    }
}
")))

; --- FhirDateHelper.cs ---

(define (boilerplate-fhir-date-helper)
    (file "FhirDateHelper.cs" ($
"namespace " (project-name) ";

internal static class FhirDateHelper
{
    public static (DateTime low, DateTime high) ParseFhirDateRange(string dateStr)
    {
        if (dateStr.Contains('T'))
        {
            var dt = DateTime.Parse(dateStr, null, System.Globalization.DateTimeStyles.RoundtripKind);
            if (dt.Kind == DateTimeKind.Unspecified)
                dt = DateTime.SpecifyKind(dt, DateTimeKind.Utc);
            return (dt.ToUniversalTime(), dt.ToUniversalTime());
        }

        var parts = dateStr.Split('-');
        return parts.Length switch
        {
            1 => (new DateTime(int.Parse(parts[0]), 1, 1, 0, 0, 0, DateTimeKind.Utc),
                  new DateTime(int.Parse(parts[0]), 12, 31, 23, 59, 59, 999, DateTimeKind.Utc)),
            2 => (new DateTime(int.Parse(parts[0]), int.Parse(parts[1]), 1, 0, 0, 0, DateTimeKind.Utc),
                  new DateTime(int.Parse(parts[0]), int.Parse(parts[1]),
                      DateTime.DaysInMonth(int.Parse(parts[0]), int.Parse(parts[1])), 23, 59, 59, 999, DateTimeKind.Utc)),
            _ => (new DateTime(int.Parse(parts[0]), int.Parse(parts[1]), int.Parse(parts[2]), 0, 0, 0, DateTimeKind.Utc),
                  new DateTime(int.Parse(parts[0]), int.Parse(parts[1]), int.Parse(parts[2]), 23, 59, 59, 999, DateTimeKind.Utc))
        };
    }
}
")))

; --- Program.cs ---
; Has variance: routes for each active resource

(define (boilerplate-program-cs)
    (file "Program.cs" ($
"using System.Text.Json;
using " (project-name) ";
using " (project-name) ".Handlers;
using " (project-name) ".Repositories;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.DependencyInjection;

var builder = WebApplication.CreateBuilder(args);

builder.Host.UseWindowsService();

var dbType = builder.Configuration[\"DatabaseType\"] ?? \"PostgreSql\";
var connectionString = builder.Configuration[\"ConnectionString\"]
    ?? \"Host=localhost;Database=" (database-name) ";Username=postgres;Password=postgres\";

IDatabaseProvider databaseProvider;
switch (dbType)
{
"
(apply $ (map (lambda (db)
    ($"    case \"" (db-namespace db) "\":
    {
"
    (case db
        ((PostgreSql)
            ($"        var dataSource = Npgsql.NpgsqlDataSource.Create(connectionString);
        databaseProvider = new " (project-name) ".Repositories." (db-namespace db) ".DatabaseProvider(dataSource, connectionString);
"
            (for-active-resources (lambda (r)
                ($"        builder.Services.AddSingleton<""I" (name-of r) "Repository>(new " (project-name) ".Repositories." (db-namespace db) "." (name-of r) "Repository(dataSource));
")))))
        ((SqlServer Sqlite)
            ($"        databaseProvider = new " (project-name) ".Repositories." (db-namespace db) ".DatabaseProvider(connectionString);
"
            (for-active-resources (lambda (r)
                ($"        builder.Services.AddSingleton<""I" (name-of r) "Repository>(new " (project-name) ".Repositories." (db-namespace db) "." (name-of r) "Repository(connectionString));
"))))))
"        break;
    }
"))
    target-databases))
"    default:
        throw new Exception($\"Unsupported database type: {dbType}\");
"
"}
// BundleHandler opens the shared transaction of a transaction bundle through the provider.
builder.Services.AddSingleton<""IDatabaseProvider>(databaseProvider);
// Resource types and other literal path segments are case-sensitive, see CaseSensitiveRouting.cs.
builder.Services.AddCaseSensitiveRouting();

builder.Services.Configure<""Microsoft.AspNetCore.Http.Json.JsonOptions>(options =>
{
    options.SerializerOptions.PropertyNamingPolicy = null;
    options.SerializerOptions.DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull;
});

var allowedOrigins = builder.Configuration.GetSection(\"Origins\").Get<""string[]>() ?? new[] { \"*\" };
builder.Services.AddCors(options => options.AddDefaultPolicy(policy =>
{
    if (allowedOrigins.Contains(\"*\"))
        policy.AllowAnyOrigin().AllowAnyHeader().AllowAnyMethod()
              .WithExposedHeaders(\"Location\", \"ETag\", \"Last-Modified\", \"Content-Location\");
    else
        policy.WithOrigins(allowedOrigins).AllowAnyHeader().AllowAnyMethod()
              .AllowCredentials()
              .WithExposedHeaders(\"Location\", \"ETag\", \"Last-Modified\", \"Content-Location\");
}));

builder.Services.Configure<""ForwardedHeadersOptions>(options =>
    options.ForwardedHeaders = ForwardedHeaders.XForwardedFor | ForwardedHeaders.XForwardedProto);

builder.WebHost.ConfigureKestrel(options => options.Limits.MaxRequestBodySize = 128 * 1024 * 1024);

var app = builder.Build();

// Robust startup: the database may not be reachable the instant this process
// starts (container/service ordering). Retry initialization with backoff
// instead of crashing, so the server comes up as soon as the DB is available.
var dbMaxAttempts = builder.Configuration.GetValue<""int?>(\"Startup:DbMaxAttempts\") ?? 60;
var dbRetryDelaySeconds = builder.Configuration.GetValue<""int?>(\"Startup:DbRetryDelaySeconds\") ?? 2;
for (var attempt = 1; ; attempt++)
{
    try
    {
        await Database.InitializeAsync(databaseProvider, app.Services);
        break;
    }
    catch (Exception ex) when (attempt < dbMaxAttempts)
    {
        Console.Error.WriteLine($\"[Startup] Database initialization attempt {attempt}/{dbMaxAttempts} failed: {ex.Message}. Retrying in {dbRetryDelaySeconds}s...\");
        await Task.Delay(TimeSpan.FromSeconds(dbRetryDelaySeconds));
    }
}
"
(if generate-datamigration
    "
// The one-time migration of a legacy database deliberately runs OUTSIDE the retry loop, and
// therefore exactly once. The loop above covers a database that is not reachable yet at startup;
// a failing migration is not that kind of transient state. The run clears its own half-filled
// target tables on a second attempt, but sixty silent retries would bury the actual cause in the
// log. If the migration fails, startup ends here.
await DataMigration.RunIfNeededAsync(dbType, connectionString, app.Services);
"
    "")
"
app.UseForwardedHeaders();
app.UseCors();
app.Use(async (ctx, next) => { await next(); if (ctx.Response.StatusCode != 304) Console.WriteLine($\"{ctx.Request.Method} {ctx.Request.Path}{ctx.Request.QueryString} {ctx.Response.StatusCode}\"); });

// Translate unhandled infrastructure/database failures into a well-formed
// OperationOutcome with an appropriate status (503/504) instead of a naked,
// empty 500. Genuine bugs still surface as 500, but never leak internals.
app.Use(async (ctx, next) =>
{
    try
    {
        await next();
    }
    catch (Exception ex) when (ex is not OperationCanceledException || !ctx.RequestAborted.IsCancellationRequested)
    {
        var (status, code, diagnostics) = ex switch
        {
            System.Data.Common.DbException => (503, \"transient\", \"The data store is currently unavailable. Please retry later.\"),
            System.Net.Sockets.SocketException => (503, \"transient\", \"The data store is currently unavailable. Please retry later.\"),
            TimeoutException => (504, \"timeout\", \"A timeout occurred while accessing the data store. Please retry later.\"),
            _ => (500, \"exception\", \"An unexpected internal error occurred.\")
        };
        Console.Error.WriteLine($\"[UnhandledException] {ctx.Request.Method} {ctx.Request.Path} -> {status}: {ex}\");
        if (ctx.Response.HasStarted)
            throw;
        ctx.Response.Clear();
        await FhirResult.Error(ctx, \"error\", code, diagnostics, status).ExecuteAsync(ctx);
    }
});
app.UseDefaultFiles();
app.UseStaticFiles();

"
(for-active-resources (lambda (r)
    ($"" (name-of r) "Handlers.Configure(builder.Configuration);
")))
"
"
(for-active-resources (lambda (resource)
    (resource-routes resource #f)))
"// System-level history
app.MapGet(\"/" (route-prefix) "/_history\", (Delegate)SystemHistoryHandler.Get);

// Metadata endpoint
app.MapGet(\"/" (route-prefix) "/metadata\", MetadataHandler.Get);

// SearchParameter endpoint
app.MapGet(\"/" (route-prefix) "/SearchParameter\", SearchParameterHandler.Get);

// Bundle endpoint
app.MapPost(\"/" (route-prefix) "\", (Delegate)BundleHandler.ProcessBundle);

// Admin endpoints (no authorization — server sits behind a facade)
app.MapGet(\"/admin/undelete/{type}\", async (string type, HttpContext ctx) =>
{
    var count = 0;
    switch (type)
    {
"
(for-active-resources (lambda (resource)
    (let ((rn (name-of resource))
          (has-history (history? resource)))
        (if has-history
            ($"        case \"" rn "\":
        {
            var repo = ctx.RequestServices.GetRequiredService<""I" rn "Repository>();
            count = await repo.UndeleteAllAsync();
            break;
        }
")
            ""))))
"    }
    return FhirResult.Error(ctx, \"information\", \"informational\", $\"{count} {type} resource(s) restored\", 200);
});

app.MapGet(\"/admin/indexes/create\", async (HttpContext ctx) =>
{
    await databaseProvider.ExecuteSqlAsync(databaseProvider.CreateIndexesSql);
    return FhirResult.Error(ctx, \"information\", \"informational\", \"Indexes created\", 200);
});

app.MapGet(\"/admin/indexes/drop\", async (HttpContext ctx) =>
{
    await databaseProvider.ExecuteSqlAsync(databaseProvider.DropIndexesSql);
    return FhirResult.Error(ctx, \"information\", \"informational\", \"Indexes dropped\", 200);
});

app.MapGet(\"/admin/seed\", async (HttpContext ctx) =>
{
    var seedDir = SeedDataLoader.ResolveSeedDir(builder.Configuration[\"SeedDataPath\"]);
    await SeedDataLoader.LoadAsync(seedDir, app.Services);
    return FhirResult.Error(ctx, \"information\", \"informational\", \"Seed data loaded\", 200);
});

// Dev/test: drop and recreate one resource kind from its seed file.
// Guarded by an unguessable secret in the path (capability URL). Disabled
// (404) unless ReseedSecret is configured.
app.MapGet(\"/admin/{secret}/reseed/{type}\", async (string secret, string type, HttpContext ctx) =>
{
    var configured = builder.Configuration[\"ReseedSecret\"];
    if (string.IsNullOrEmpty(configured) || secret != configured)
        return Results.NotFound();
    var truncateSql = databaseProvider.ReseedTruncateSql(type);
    if (truncateSql == null)
        return FhirResult.Error(ctx, \"error\", \"not-supported\", $\"Unknown resource type {type}\", 400);
    var seedDir = SeedDataLoader.ResolveSeedDir(builder.Configuration[\"SeedDataPath\"]);
    await databaseProvider.ExecuteSqlAsync(truncateSql);
    var (created, updated, failed) = await SeedDataLoader.LoadResourceTypeAsync(seedDir, app.Services, type);
    var severity = failed > 0 ? \"warning\" : \"information\";
    return FhirResult.Error(ctx, severity, \"informational\", $\"{type} reseeded: {created} created, {updated} updated, {failed} failed\", 200);
});

// Liveness: the process is up. Does NOT touch the database, so an unavailable
// database never triggers container restarts.
app.MapGet(\"/healthz\", () => Results.Text(\"healthy\", \"text/plain\"));

// Readiness: verifies the database is reachable before routing traffic here.
app.MapGet(\"/readyz\", async () =>
{
    try
    {
        await databaseProvider.ExecuteScalarAsync(\"SELECT 1\");
        return Results.Text(\"ready\", \"text/plain\");
    }
    catch
    {
        return Results.StatusCode(503);
    }
});

app.Run();
")))

; --- Database.cs ---

(define (boilerplate-database-cs)
    (file "Database.cs" ($
"using " (project-name) ".Repositories;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Configuration;

namespace " (project-name) ";

public static class Database
{
    private const string SoftwareVersion = \"" (software-version) "\";

    public static async Task InitializeAsync(IDatabaseProvider db, IServiceProvider? services = null)
    {
        await db.EnsureDatabaseExists(\"" (database-name) "\");
        var version = await GetDatabaseVersion(db, services);
        if (version > new Version(SoftwareVersion))
            throw new Exception($\"Database version {version} is newer than software version {SoftwareVersion}\");
" (upgrade-version-checks)
"    }

    private static async Task<""Version> GetDatabaseVersion(IDatabaseProvider db, IServiceProvider? services)
    {
        var versionStr = await ReadDbVersion(db);
        if (versionStr != null)
            return new Version(versionStr);

        var legacyVersion = await ReadLegacyDbVersion(db);
        if (legacyVersion != null)
        {
            Console.WriteLine($\"Found legacy database version {legacyVersion}, migrating to {SoftwareVersion}...\");
            Console.WriteLine(\"Creating new schema alongside legacy data...\");
            await db.ExecuteSqlAsync(db.InitialSchema);
            await db.ExecuteSqlAsync(db.CreateDbInfoTableSql);
            await db.ExecuteSqlAsync(db.InsertVersionSql(SoftwareVersion));
            return new Version(SoftwareVersion);
        }

        var hasFireMeta = await HasFireMetaSchema(db);
        if (!hasFireMeta)
        {
            await CreateAllSchemas(db, services);
            return new Version(SoftwareVersion);
        }

        await db.ExecuteSqlAsync(db.CreateDbInfoTableSql);
        await db.ExecuteSqlAsync(db.InsertVersionSql(\"0.0.0\"));
        return new Version(\"0.0.0\");
    }

    private static async Task<""string?> ReadDbVersion(IDatabaseProvider db)
    {
        try { return await db.ExecuteScalarAsync(db.SelectVersionSql) as string; }
        catch { return null; }
    }

    private static async Task<""string?> ReadLegacyDbVersion(IDatabaseProvider db)
    {
        try { return await db.ExecuteScalarAsync(db.SelectLegacyVersionSql) as string; }
        catch { return null; }
    }

    private static async Task<""bool> HasFireMetaSchema(IDatabaseProvider db)
    {
        try { return Convert.ToInt32(await db.ExecuteScalarAsync(db.HasFireMetaSchemaSql)) > 0; }
        catch { return false; }
    }

    private static async Task CreateAllSchemas(IDatabaseProvider db, IServiceProvider? services)
    {
        Console.WriteLine(\"Creating database schema...\");
        await db.ExecuteSqlAsync(db.InitialSchema);

        if (services != null)
            await ImportSeedData(services);

        await db.ExecuteSqlAsync(db.CreateDbInfoTableSql);
        await db.ExecuteSqlAsync(db.InsertVersionSql(SoftwareVersion));
        Console.WriteLine($\"Database initialized at version {SoftwareVersion}.\");
    }

    // Seeding goes through the single SeedDataLoader implementation. The seed
    // directory defaults to the \"seed\" folder shipped next to the binary
    // (overridable via the SeedDataPath configuration setting).
    private static async Task ImportSeedData(IServiceProvider services)
    {
        await SeedDataLoader.LoadAsync(SeedDir(services), services);
    }

    private static Task ImportSeedBundle(string resourceType, IServiceProvider services)
        => SeedDataLoader.LoadResourceTypeAsync(SeedDir(services), services, resourceType);

    // Apply a named transaction bundle from the seed directory as part of a
    // versioned upgrade step (e.g. to introduce a new seed resource without
    // overwriting operator-customized existing ones). Missing file = skip.
    private static Task ApplyUpgradeBundle(string fileName, IServiceProvider services)
        => SeedDataLoader.ApplyBundleAsync(SeedDir(services), services, fileName);

    private static string SeedDir(IServiceProvider services)
        => SeedDataLoader.ResolveSeedDir(services.GetService<""IConfiguration>()?[\"SeedDataPath\"]);

    // One versioned upgrade step is one database transaction: its schema SQL, the runtime work of
    // its actions (seed bundles, new search indexes, data migrations) and the new version number
    // commit together or not at all. Everything inside joins the DbTransactionScope - the provider,
    // the repositories, a transaction bundle and the hand-written migrations. A failing step leaves
    // the database on the previous version, and the next start runs the step again. The version is
    // written last; it used to be written before the runtime work, so a failing data migration left
    // the step marked as done and it never ran again.
    private static async Task Upgrade(string version, IDatabaseProvider db, IServiceProvider? services)
    {
        Console.WriteLine($\"Upgrading database to {version}...\");
        await using var scope = await DbTransactionScope.BeginMaintenanceAsync(db);
        using (scope.Enter())
        {
            var upgradeSql = db.UpgradeSql(version);
            if (!string.IsNullOrWhiteSpace(upgradeSql))
                await db.ExecuteSqlAsync(upgradeSql);
" (upgrade-seed-imports)
"            await db.ExecuteSqlAsync(db.UpdateVersionSql(version));

            // A PostgreSQL transaction that hit an error rejects every later statement, and a
            // COMMIT on it rolls back without an error. The probe turns that into an exception
            // instead of a step that looks committed but is not.
            await db.ExecuteScalarAsync(\"SELECT 1\");
        }
        await scope.CommitAsync();
        Console.WriteLine($\"Database upgraded to {version}.\");
    }
}
")))

; ===================================================================
; FhirValidator - primitive type validation
; ===================================================================

(define (boilerplate-fhir-validator)
    (file "FhirValidator.cs" ($
"using System.Text.RegularExpressions;

namespace " (project-name) ";

public static partial class FhirValidator
{
    private static readonly Regex IdRegex = CreateIdRegex();
    private static readonly Regex DateRegex = CreateDateRegex();
    private static readonly Regex DateTimeRegex = CreateDateTimeRegex();
    private static readonly Regex TimeRegex = CreateTimeRegex();
    private static readonly Regex InstantRegex = CreateInstantRegex();

    [GeneratedRegex(@\"^[A-Za-z0-9\\-\\.]{1,64}$\")]
    private static partial Regex CreateIdRegex();

    [GeneratedRegex(@\"^(\\d{4})(-\\d{2}(-\\d{2})?)?$\")]
    private static partial Regex CreateDateRegex();

    [GeneratedRegex(@\"^(\\d{4})(-\\d{2}(-\\d{2}(T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:\\d{2})?)?)?)?$\")]
    private static partial Regex CreateDateTimeRegex();

    [GeneratedRegex(@\"^\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?$\")]
    private static partial Regex CreateTimeRegex();

    [GeneratedRegex(@\"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:\\d{2})$\")]
    private static partial Regex CreateInstantRegex();

    public static string? ValidateId(string? v) =>
        v != null && !IdRegex.IsMatch(v) ? \"Invalid id format\" : null;

    public static string? ValidateDate(string? v) =>
        v != null && !DateRegex.IsMatch(v) ? \"Invalid date format\" : null;

    public static string? ValidateDateTime(string? v) =>
        v != null && !DateTimeRegex.IsMatch(v) ? \"Invalid dateTime format\" : null;

    public static string? ValidateInstant(string? v) =>
        v != null && !InstantRegex.IsMatch(v) ? \"Invalid instant format\" : null;

    public static string? ValidateTime(string? v) =>
        v != null && !TimeRegex.IsMatch(v) ? \"Invalid time format\" : null;

    public static string? ValidatePositiveInt(int? v) =>
        v.HasValue && v.Value < 1 ? \"Must be positive\" : null;

    public static string? ValidateUnsignedInt(int? v) =>
        v.HasValue && v.Value < 0 ? \"Must be non-negative\" : null;
}
")))

; ===================================================================
; SeedDataLoader - load FHIR Bundle seed data
; ===================================================================

(define (boilerplate-seed-data-loader)
    (file "SeedDataLoader.cs" ($
"using System.Text.Json;
using System.Text.Json.Nodes;
using "(project-name)".Handlers;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Configuration;

namespace "(project-name)";

public static class SeedDataLoader
{
    // Single source of truth for the seed directory: the configured SeedDataPath
    // if set, otherwise the \"seed\" folder shipped next to the binary.
    public static string ResolveSeedDir(string? configuredPath) =>
        string.IsNullOrEmpty(configuredPath) ? Path.Combine(AppContext.BaseDirectory, \"seed\") : configuredPath;

    // Replay every seed bundle through the bundle handler as a searchset
    // (upsert by id). Returns (created, updated, failed) from the responses.
    public static async Task<""(int created, int updated, int failed)> LoadAsync(string path, IServiceProvider services)
    {
        var totals = (created: 0, updated: 0, failed: 0);
        if (!Directory.Exists(path)) return totals;

        var baseUrl = services.GetService<""IConfiguration>()?[\"externalUrl\"] ?? \"\";
        foreach (var file in Directory.GetFiles(path, \"*.json\").OrderBy(f => f))
        {
            JsonNode? node;
            try { node = JsonNode.Parse(await File.ReadAllTextAsync(file)); }
            catch { continue; }
            if (node == null || node[\"resourceType\"]?.GetValue<""string>() != \"Bundle\") continue;
            var bundle = node.AsObject();
            // Transaction/batch bundles in the seed dir are upgrade bundles, applied
            // explicitly by version steps (ApplyBundleAsync) — never by the blanket
            // searchset replay, which would mis-process their request verbs.
            var srcType = bundle[\"type\"]?.GetValue<""string>();
            if (srcType == \"transaction\" || srcType == \"batch\") continue;
            bundle[\"type\"] = \"searchset\";
            Tally(await BundleHandler.ProcessBundleObject(bundle, baseUrl, services), ref totals);
        }
        return totals;
    }

    // Replay only the entries of a single type (across all seed files) as a
    // searchset upsert. Used by the reseed endpoint.
    public static async Task<""(int created, int updated, int failed)> LoadResourceTypeAsync(string path, IServiceProvider services, string resourceType)
    {
        var totals = (created: 0, updated: 0, failed: 0);
        if (!Directory.Exists(path)) return totals;

        var baseUrl = services.GetService<""IConfiguration>()?[\"externalUrl\"] ?? \"\";
        var filtered = new JsonArray();
        foreach (var file in Directory.GetFiles(path, \"*.json\").OrderBy(f => f))
        {
            JsonNode? node;
            try { node = JsonNode.Parse(await File.ReadAllTextAsync(file)); }
            catch { continue; }
            // Skip upgrade bundles (transaction/batch); they are applied explicitly
            // by version steps, not by the searchset reseed path.
            var srcType = node?[\"type\"]?.GetValue<""string>();
            if (srcType == \"transaction\" || srcType == \"batch\") continue;
            var entries = node?[\"entry\"]?.AsArray();
            if (entries == null) continue;
            foreach (var entry in entries)
            {
                if (entry?[\"resource\"]?[\"resourceType\"]?.GetValue<""string>() == resourceType)
                    filtered.Add(entry!.DeepClone());
            }
        }
        if (filtered.Count == 0) return totals;
        var typeBundle = new JsonObject { [\"resourceType\"] = \"Bundle\", [\"type\"] = \"searchset\", [\"entry\"] = filtered };
        Tally(await BundleHandler.ProcessBundleObject(typeBundle, baseUrl, services), ref totals);
        return totals;
    }

    // Apply a single named seed file as a transaction bundle, honouring each
    // entry's request verb (POST/PUT/PATCH/DELETE) and ifNoneExist. Unlike the
    // searchset replays above this does NOT force upsert, so an upgrade can add
    // resources conditionally without overwriting operator-customized ones.
    // A missing file is skipped (logged); a present-but-invalid or failing
    // bundle propagates (transaction bundles are atomic). Returns the totals.
    public static async Task<""(int created, int updated, int failed)> ApplyBundleAsync(string path, IServiceProvider services, string fileName)
    {
        var totals = (created: 0, updated: 0, failed: 0);
        var file = Path.Combine(path, fileName);
        if (!File.Exists(file))
        {
            Console.WriteLine($\"Upgrade bundle '{fileName}' not found in seed directory '{path}'; skipping.\");
            return totals;
        }

        var baseUrl = services.GetService<""IConfiguration>()?[\"externalUrl\"] ?? \"\";
        var node = JsonNode.Parse(await File.ReadAllTextAsync(file));
        if (node == null || node[\"resourceType\"]?.GetValue<""string>() != \"Bundle\")
            throw new InvalidDataException($\"Upgrade bundle '{fileName}' is not a FHIR Bundle.\");
        var bundle = node.AsObject();
        var type = bundle[\"type\"]?.GetValue<""string>();
        if (type != \"transaction\" && type != \"batch\")
            bundle[\"type\"] = \"transaction\";
        Tally(await BundleHandler.ProcessBundleObject(bundle, baseUrl, services), ref totals);
        return totals;
    }

    private static void Tally(JsonObject responseBundle, ref (int created, int updated, int failed) totals)
    {
        var entries = responseBundle[\"entry\"]?.AsArray();
        if (entries == null) return;
        foreach (var e in entries)
        {
            var status = e?[\"response\"]?[\"status\"]?.GetValue<""string>() ?? \"\";
            if (status.StartsWith(\"201\")) totals.created++;
            else if (status.StartsWith(\"200\")) totals.updated++;
            else totals.failed++;
        }
    }
}
")))

; ===================================================================
; BundleHandler - cross-resource bundle processing
; ===================================================================

(define (boilerplate-bundle-handler)
    (file "BundleHandler.cs"($
"using System.Text.Json;
using System.Text.Json.Nodes;
using " (project-name) ".Model;
using " (project-name) ".Model.Validator;
using " (project-name) ".Search;
using " (project-name) ".Repositories;
using Microsoft.Extensions.DependencyInjection;

namespace " (project-name) ".Handlers;

public static class BundleHandler
{
    public static async Task<""IResult> ProcessBundle(HttpContext ctx)
    {
        JsonNode content;
        try
        {
            using var doc = await JsonDocument.ParseAsync(ctx.Request.Body);
            content = JsonNode.Parse(doc.RootElement.GetRawText())!;
        }
        catch
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid JSON\", 400);
        }

        var resourceType = content[\"resourceType\"]?.GetValue<""string>();
        if (resourceType != \"Bundle\")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Expected Bundle resource\", 400);
        }

        var bundleType = content[\"type\"]?.GetValue<""string>();
        if (bundleType != \"batch\" && bundleType != \"transaction\" && bundleType != \"searchset\")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Unsupported bundle type: {bundleType}\", 400);
        }

        try
        {
            var responseBundle = await ProcessBundleObject(content, GetBaseUrl(ctx), ctx.RequestServices);
            return FhirResult.Bundle(ctx, responseBundle);
        }
        catch (BundleException ex)
        {
            if (ex.Outcome != null)
                return FhirResult.Resource(ctx, JsonSerializer.SerializeToUtf8Bytes(ex.Outcome), 400);
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }
    }

    public sealed class BundleException : Exception
    {
        public JsonObject? Outcome { get; }
        public BundleException(string message) : base(message) { }
        public BundleException(JsonObject outcome) : base(string.Empty) { Outcome = outcome; }
    }

    // In-process bundle processing (no HttpContext) — used by the HTTP wrapper
    // above and by SeedDataLoader. transaction = atomic (throws BundleException
    // on validation/processing failure); batch and searchset = non-atomic.
    // searchset entries are upserted by id (PUT semantics).
    public static async Task<""JsonObject> ProcessBundleObject(JsonNode content, string baseUrl, IServiceProvider sp)
    {
        var bundleType = content[\"type\"]?.GetValue<""string>();
        var entries = content[\"entry\"]?.AsArray();
        var responseType = bundleType == \"transaction\" ? \"transaction-response\" : \"batch-response\";

        var responseEntries = new JsonArray();
        if (entries == null || entries.Count == 0)
            return new JsonObject { [\"resourceType\"] = \"Bundle\", [\"type\"] = responseType, [\"entry\"] = responseEntries };

        if (bundleType == \"transaction\")
        {
            foreach (var entry in entries)
            {
                var obj = entry?.AsObject();
                if (obj == null) continue;
                var error = ValidateTransactionEntry(obj);
                if (error != null)
                    throw new BundleException(error);
            }
        }

        // A transaction runs all entries in one database transaction: every repository operation
        // joins the scope, a failing entry throws BundleException, and disposing the uncommitted
        // scope rolls back the entries processed before it. A transaction bundle applied inside a
        // scope that is already open - an upgrade bundle within a versioned upgrade step - joins
        // that scope instead: its entries become part of the step, and the owner of the scope
        // commits or rolls back.
        await using var txScope = bundleType == \"transaction\" && DbTransactionScope.Current == null
            ? await DbTransactionScope.BeginAsync(sp.GetRequiredService<""IDatabaseProvider>())
            : null;
        using var enteredScope = txScope?.Enter();

        foreach (var entry in entries)
        {
            JsonObject responseEntry;
            if (bundleType == \"searchset\")
            {
                var resource = entry?[\"resource\"];
                var rt = resource?[\"resourceType\"]?.GetValue<""string>();
                var rid = resource?[\"id\"]?.GetValue<""string>();
                responseEntry = (resource == null || rt == null || rid == null)
                    ? MakeErrorResponse(\"400\", \"searchset entry missing resource/resourceType/id\")
                    : await ProcessPut(rt, rid, resource, baseUrl, sp);
            }
            else
            {
                responseEntry = await ProcessEntry(entry?.AsObject(), baseUrl, sp);
            }

            if (bundleType == \"transaction\")
            {
                var status = responseEntry[\"response\"]?[\"status\"]?.GetValue<""string>() ?? \"\";
                if (status.StartsWith(\"4\") || status.StartsWith(\"5\"))
                {
                    var diagnostics = responseEntry[\"response\"]?[\"outcome\"]?.ToString() ?? responseEntry.ToJsonString();
                    throw new BundleException($\"Transaction processing failed: {diagnostics}\");
                }
            }

            responseEntries.Add(responseEntry);
        }

        if (txScope != null)
            await txScope.CommitAsync();

        return new JsonObject { [\"resourceType\"] = \"Bundle\", [\"type\"] = responseType, [\"entry\"] = responseEntries };
    }

    private static JsonObject? ValidateTransactionEntry(JsonObject entry)
    {
        var request = entry[\"request\"]?.AsObject();
        if (request == null)
            return FhirResponse.OperationOutcome(\"error\", \"invalid\", \"Bundle entry missing request\");

        var method = request[\"method\"]?.GetValue<""string>();
        var url = request[\"url\"]?.GetValue<""string>();
        if (method == null || url == null)
            return FhirResponse.OperationOutcome(\"error\", \"invalid\", \"Bundle entry missing method or url\");

        if (method == \"POST\" || method == \"PUT\")
        {
            var resource = entry[\"resource\"];
            if (resource == null)
                return FhirResponse.OperationOutcome(\"error\", \"invalid\", $\"Bundle entry missing resource for {method}\");

            var resType = resource[\"resourceType\"]?.GetValue<""string>();
            if (resType == null)
                return FhirResponse.OperationOutcome(\"error\", \"invalid\", \"Missing resourceType\");

            var expectedType = url.Split('/')[0].Split('?')[0];
            if (resType != expectedType)
                return FhirResponse.OperationOutcome(\"error\", \"invalid\", $\"Resource type mismatch: expected {expectedType}, got {resType}\");

            // Boolean field validation
            var activeNode = resource[\"active\"];
            if (activeNode != null && activeNode.GetValueKind() != JsonValueKind.True && activeNode.GetValueKind() != JsonValueKind.False)
                return FhirResponse.OperationOutcome(\"error\", \"invalid\", \"Field 'active' must be a boolean\");

            // Per-resource required field validation
"       (for-active-resources (lambda (resource)
             (let* (
                (rn (name-of resource))
                (props-node (select-children "properties" resource))
                (all-props (if (node-list-empty? props-node) (empty-node-list) (children (node-list-first props-node))))
                (notnull-props (node-list->list (node-list-filter (lambda (p) (and (true? "notnull" p) (not (string=? "id" (% "name" p))))) all-props)))
                )
                (if (null? notnull-props) "" ($"            if (resType == \"" rn "\")
            {
"               (apply $ (map (lambda (p)
                    ($"                if (resource[\"" (% "name" p) "\"] == null)
                    return FhirResponse.OperationOutcome(\"error\", \"required\", \"Missing required field: " (% "name" p) "\");
"))
                    notnull-props))
"            }
"               ))
            )
        ))
"        }

        return null;
    }

    private static async Task<""JsonObject> ProcessEntry(JsonObject? entry, string baseUrl, IServiceProvider sp)
    {
        if (entry == null)
            return MakeErrorResponse(\"400\", \"Invalid entry\");

        var request = entry[\"request\"]?.AsObject();
        if (request == null)
            return MakeErrorResponse(\"400\", \"Entry missing request\");

        var method = request[\"method\"]?.GetValue<""string>();
        var url = request[\"url\"]?.GetValue<""string>();
        if (method == null || url == null)
            return MakeErrorResponse(\"400\", \"Entry missing method or url\");

        var urlParts = url.Split('?', 2);
        var pathPart = urlParts[0];
        var queryString = urlParts.Length > 1 ? urlParts[1] : null;
        var segments = pathPart.Split('/');
        var resourceType = segments[0];
        var resourceId = segments.Length > 1 ? segments[1] : null;
        var entryResource = entry[\"resource\"];
        var ifNoneExist = request[\"ifNoneExist\"]?.GetValue<""string>();
        var ifMatchVersion = ParseIfMatch(request[\"ifMatch\"]?.GetValue<""string>());

        try
        {
            switch (method)
            {
                case \"POST\":
                    return await ProcessPost(resourceType, entryResource, ifNoneExist, baseUrl, sp);
                case \"GET\":
                    return await ProcessGet(resourceType, resourceId, queryString, baseUrl, sp);
                case \"PUT\":
                    return await ProcessPut(resourceType, resourceId, entryResource, baseUrl, sp, ifMatchVersion);
                case \"DELETE\":
                    return await ProcessDelete(resourceType, resourceId, sp);
                default:
                    return MakeErrorResponse(\"400\", $\"Unsupported method: {method}\");
            }
        }
        catch (Exception ex)
        {
            return MakeErrorResponse(\"400\", ex.Message);
        }
    }

    private static async Task<""JsonObject> ProcessPost(string resourceType, JsonNode? resource, string? ifNoneExist, string baseUrl, IServiceProvider sp)
    {
        if (resource == null)
            return MakeErrorResponse(\"400\", \"Missing resource\");

        var resType = resource[\"resourceType\"]?.GetValue<""string>();
        if (resType == null)
            return MakeErrorResponse(\"400\", \"Missing resourceType\");
        if (resType != resourceType)
            return MakeErrorResponse(\"400\", $\"Resource type mismatch: expected {resourceType}, got {resType}\");

        var validationError = ValidateResourceModel(resourceType, resource);
        if (validationError != null) return validationError;

        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) 
            (let ((rn (name-of r))) ($
"            case \""rn"\":
            {
                var repo = sp.GetRequiredService<""I" rn "Repository>();
                if (ifNoneExist != null)
                {
                    var searchParams = System.Web.HttpUtility.ParseQueryString(ifNoneExist);
                    var qc = new QueryCollection(
                        searchParams.AllKeys.Where(k => k != null).ToDictionary(k => k!, k => new Microsoft.Extensions.Primitives.StringValues(searchParams[k])));
                    var (existing, total) = await repo.SearchAsync(qc, 2, 0);
                    if (total == 1)
                        return MakeResponse(\"200\", null, JsonNode.Parse(existing[0].content));
                    if (total > 1)
                        return MakeErrorResponse(\"412\", \"Multiple matches found for conditional create\");
                }
                var result = await repo.CreateAsync(resource);
                return MakeResponse(\"201\", $\"{baseUrl}/" rn "/{result.id}/_history/{result.versionId}\", JsonNode.Parse(result.content));
            }
"        ))))
"            default:
                return MakeErrorResponse(\"404\", $\"Unknown resource type: {resourceType}\");
        }
    }

    private static async Task<""JsonObject> ProcessGet(string resourceType, string? resourceId, string? queryString, string baseUrl, IServiceProvider sp)
    {
        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (let ((rn (name-of r)))
        ($
"            case \"" rn "\":
            {
                var repo = sp.GetRequiredService<""I"rn"Repository>();
                if (resourceId != null)
                {
                    var result = await repo.ReadAsync(resourceId);
                    if (result == null)
                        return MakeErrorResponse(\"404\", $\""rn"/{resourceId} not found\");
                    return MakeResponse(\"200\", null, JsonNode.Parse(result.content));
                }
                else
                {
                    var searchParams = System.Web.HttpUtility.ParseQueryString(queryString ?? \"\");
                    var qc = new QueryCollection(
                        searchParams.AllKeys.Where(k => k != null).ToDictionary(k => k!, k => new Microsoft.Extensions.Primitives.StringValues(searchParams[k])));
                    var (resources, total) = await repo.SearchAsync(qc, 10, 0);
                    var searchEntries = resources.Select(r =>
                        FhirResponse.SearchEntry(JsonNode.Parse(r.content)!, $\"{baseUrl}/"rn"/{r.id}\")).ToList();
                    var searchBundle = FhirResponse.SearchBundle(searchEntries, total, $\"{baseUrl}/"rn"?{queryString}\", 10, 0);
                    return MakeResponse(\"200\", null, searchBundle);
                }
            }
"))))
"            default:
                return MakeErrorResponse(\"404\", $\"Unknown resource type: {resourceType}\");
        }
    }

    // Same reading as the If-Match header of a single PUT: W/\"3\" or \"3\"; anything else is ignored.
    private static int? ParseIfMatch(string? ifMatch)
    {
        if (ifMatch == null) return null;
        var versionStr = ifMatch.Replace(\"W/\\\"\", \"\").Replace(\"\\\"\", \"\");
        return int.TryParse(versionStr, out var v) ? v : null;
    }

    private static async Task<""JsonObject> ProcessPut(string resourceType, string? resourceId, JsonNode? resource, string baseUrl, IServiceProvider sp, int? ifMatchVersion = null)
    {
        if (resource == null)
            return MakeErrorResponse(\"400\", \"Missing resource\");
        if (resourceId == null)
            return MakeErrorResponse(\"400\", \"PUT requires resource ID in URL\");

        var validationError = ValidateResourceModel(resourceType, resource);
        if (validationError != null) return validationError;

        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (let ((rn (name-of r))
            (has-history (history? r)))
                (if has-history
                    ($
"            case \"" rn "\":
            {
                var repo = sp.GetRequiredService<""I" rn "Repository>();
                try
                {
                    var result = await repo.UpdateAsync(resourceId, resource, ifMatchVersion);
                    if (result == null)
                    {
                        result = await repo.CreateAsync(resource, resourceId);
                        return MakeResponse(\"201\", $\"{baseUrl}/" rn "/{result.id}/_history/{result.versionId}\", JsonNode.Parse(result.content));
                    }
                    return MakeResponse(\"200\", $\"{baseUrl}/" rn "/{result.id}/_history/{result.versionId}\", JsonNode.Parse(result.content));
                }
                catch (ConcurrencyException)
                {
                    return MakeErrorResponse(\"409\", \"Version conflict\");
                }
            }
"                   )
                    ($
"            case \"" rn "\":
            {
                var repo = sp.GetRequiredService<""I" rn "Repository>();
                var result = await repo.UpdateAsync(resourceId, resource);
                if (result == null)
                {
                    result = await repo.CreateAsync(resource, resourceId);
                    return MakeResponse(\"201\", $\"{baseUrl}/" rn "/{result.id}\", JsonNode.Parse(result.content));
                }
                return MakeResponse(\"200\", $\"{baseUrl}/" rn "/{result.id}\", JsonNode.Parse(result.content));
            }
"                   )
                )
            )))
"            default:
                return MakeErrorResponse(\"404\", $\"Unknown resource type: {resourceType}\");
        }
    }

    private static async Task<""JsonObject> ProcessDelete(string resourceType, string? resourceId, IServiceProvider sp)
    {
        if (resourceId == null)
            return MakeErrorResponse(\"400\", \"DELETE requires resource ID in URL\");

        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (let ((rn (name-of r))) ($
"            case \""rn"\":
            {
                var repo = sp.GetRequiredService<""I"rn"Repository>();
                var deleted = await repo.DeleteAsync(resourceId);
                if (!deleted) return MakeErrorResponse(\"404\", $\""rn"/{resourceId} not found\");
                return MakeResponse(\"204\", null, null);
            }
"       ))))
"            default:
                return MakeErrorResponse(\"404\", $\"Unknown resource type: {resourceType}\");
        }
    }

    private static JsonObject MakeResponse(string status, string? location, JsonNode? resource)
    {
        var entry = new JsonObject();
        var response = new JsonObject { [\"status\"] = status };
        if (location != null)
            response[\"location\"] = location;
        entry[\"response\"] = response;
        if (resource != null)
            entry[\"resource\"] = resource.DeepClone();
        return entry;
    }

    private static JsonObject MakeErrorResponse(string status, string message)
    {
        return new JsonObject
        {
            [\"response\"] = new JsonObject { [\"status\"] = status },
            [\"resource\"] = (JsonNode)FhirResponse.OperationOutcome(\"error\", \"invalid\", message)
        };
    }

    private static JsonObject MakeValidationErrorResponse(List<""string> errors)
    {
        return new JsonObject
        {
            [\"response\"] = new JsonObject { [\"status\"] = \"400\" },
            [\"resource\"] = (JsonNode)FhirResponse.OperationOutcome(errors)
        };
    }

    private static JsonObject? ValidateResourceModel(string resourceType, JsonNode resource)
    {
        try
        {
            Resource? model = resourceType switch
            {
"   (for-active-resources (lambda (r) (let ((rn (name-of r))) ($
"                \"" rn "\" => resource.Deserialize<""" rn ">(),
"))))
"                _ => null,
            };
            if (model == null) return null;
            var errors = ModelValidator.ValidateResource(model);
            if (errors.Count > 0)
                return MakeValidationErrorResponse(errors);
            return null;
        }
        catch (JsonException ex)
        {
            return MakeErrorResponse(\"400\", ex.Message);
        }
    }

    private static string GetBaseUrl(HttpContext ctx)
    {
        return $\"{ctx.Request.Scheme}://{ctx.Request.Host}/" (route-prefix) "\";
    }
}
"    ))
)

; ===================================================================
; SearchHelper - cross-resource search dispatch and chain/include resolution
; ===================================================================

(define (boilerplate-search-helper)
    (file "SearchHelper.cs" ($
"using System.Text.Json.Nodes;
using " (project-name) ".Search;
using " (project-name) ".Repositories;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Primitives;

namespace " (project-name) ";

public static class SearchHelper
{
    public static async Task<(List<""JsonNode> contents, List<""string> ids, int total)> SearchAsync(
        IServiceProvider sp, string resourceType, IQueryCollection query, int count, int offset)
    {
        switch (resourceType)
        {
"       (for-active-resources (lambda (r) (let ((rn (name-of r))) ($
"            case \""rn"\":
            {
                var repo = sp.GetRequiredService<""I"rn"Repository>();
                var (resources, total) = await repo.SearchAsync(query, count, offset);
                return (resources.Select(r => (JsonNode)JsonNode.Parse(r.content)!).ToList(), resources.Select(r => r.id).ToList(), total);
            }
"       ))))
"        }
        return (new List<""JsonNode>(), new List<""string>(), 0);
    }

    public static async Task<""JsonNode?> ReadAsync(IServiceProvider sp, string resourceType, string id)
    {
        switch (resourceType)
        {
"        (for-active-resources (lambda (r) (let ((rn (name-of r)))
        ($
"            case \""rn"\":
            {
                var repo = sp.GetRequiredService<""I"rn"Repository>();
                var resource = await repo.ReadAsync(id);
                return resource?.content != null ? JsonNode.Parse(resource.content) : null;
            }
"       ))))
"        }
        return null;
    }

    public static string? GetReferencePropertyName(string resourceType, string searchParamName)
    {
        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (search-helper-ref-prop-resource-case r)))
"        }
        return null;
    }

    public static string[] GetActiveTargetTypes(string resourceType, string searchParamName)
    {
        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (search-helper-targets-resource-case r)))
"        }
        return Array.Empty<""string>();
    }

    public static async Task<""IQueryCollection> ResolveForwardChainsAsync(
        IServiceProvider sp, string resourceType, IQueryCollection query)
    {
        var chainParams = query.Where(p => p.Key.Contains('.') && !p.Key.StartsWith(\"_\")).ToList();
        if (chainParams.Count == 0) return query;

        var modified = new Dictionary<""string, StringValues>();
        foreach (var param in query)
        {
            if (!param.Key.Contains('.') || param.Key.StartsWith(\"_\"))
                modified[param.Key] = param.Value;
        }

        var chainGroups = new Dictionary<""string, List<""(string chainKey, StringValues value)>>();
        foreach (var param in chainParams)
        {
            var dotIndex = param.Key.IndexOf('.');
            var refPart = param.Key.Substring(0, dotIndex);
            var baseRefParam = refPart.Contains(':') ? refPart.Split(':')[0] : refPart;
            if (!chainGroups.ContainsKey(baseRefParam))
                chainGroups[baseRefParam] = new List<""(string, StringValues)>();
            chainGroups[baseRefParam].Add((param.Key, param.Value));
        }

        foreach (var group in chainGroups)
        {
            var baseRefParam = group.Key;
            HashSet<""string>? matchedRefValues = null;

            foreach (var (chainKey, value) in group.Value)
            {
                var dotIndex = chainKey.IndexOf('.');
                var refPart = chainKey.Substring(0, dotIndex);
                var remainingChain = chainKey.Substring(dotIndex + 1);

                string refParamName;
                string? explicitTargetType = null;
                if (refPart.Contains(':'))
                {
                    var parts = refPart.Split(':', 2);
                    refParamName = parts[0];
                    explicitTargetType = parts[1];
                }
                else
                {
                    refParamName = refPart;
                }

                var targetTypes = explicitTargetType != null
                    ? new[] { explicitTargetType }
                    : GetActiveTargetTypes(resourceType, refParamName);

                var chainRefValues = new HashSet<""string>();

                foreach (var targetType in targetTypes)
                {
                    var targetQuery = new Dictionary<""string, StringValues>
                    {
                        [remainingChain] = value
                    };

                    var targetQC = new QueryCollection(targetQuery);
                    if (remainingChain.Contains('.'))
                    {
                        var resolved = await ResolveForwardChainsAsync(sp, targetType, targetQC);
                        targetQC = resolved as QueryCollection ?? new QueryCollection(
                            resolved.ToDictionary(k => k.Key, k => k.Value));
                    }

                    var (contents, ids, total) = await SearchAsync(sp, targetType, targetQC, 10000, 0);
                    foreach (var id in ids)
                        chainRefValues.Add($\"{targetType}/{id}\");
                }

                if (matchedRefValues == null)
                    matchedRefValues = chainRefValues;
                else
                    matchedRefValues.IntersectWith(chainRefValues);
            }

            if (matchedRefValues != null && matchedRefValues.Count > 0)
                modified[baseRefParam] = new StringValues(string.Join(\",\", matchedRefValues));
            else
                modified[\"_id\"] = new StringValues(\"__no_match__\");
        }

        return new QueryCollection(modified);
    }

    public static async Task<""string[]?> ResolveReverseChainsAsync(
        IServiceProvider sp, IQueryCollection query)
    {
        var hasParams = query.Where(p => p.Key.StartsWith(\"_has:\")).ToList();
        if (hasParams.Count == 0) return null;

        string[]? resultIds = null;
        foreach (var param in hasParams)
        {
            var parts = param.Key.Split(':', 4);
            if (parts.Length <"" 4) continue;

            var targetResourceType = parts[1];
            var referenceParam = parts[2];
            var searchParam = parts[3];

            var targetQuery = new Dictionary<""string, StringValues>
            {
                [searchParam] = param.Value
            };
            var targetQC = new QueryCollection(targetQuery);

            var (contents, ids, total) = await SearchAsync(sp, targetResourceType, targetQC, 10000, 0);

            var propertyName = GetReferencePropertyName(targetResourceType, referenceParam);
            if (propertyName == null) continue;

            var referencedIds = new HashSet<""string>();
            foreach (var content in contents)
            {
                ExtractReferenceIds(content, propertyName, referencedIds);
            }

            if (resultIds == null)
                resultIds = referencedIds.ToArray();
            else
                resultIds = resultIds.Intersect(referencedIds).ToArray();
        }

        return resultIds;
    }

    private static void ExtractReferenceIds(JsonNode content, string propertyName, HashSet<""string> ids)
    {
        var nodes = new List<""JsonNode?> { content };
        foreach (var segment in propertyName.Split('.'))
        {
            var next = new List<""JsonNode?>();
            foreach (var node in nodes)
            {
                if (node == null) continue;
                var child = node[segment];
                if (child == null) continue;
                if (child is JsonArray arr)
                    foreach (var item in arr) next.Add(item);
                else
                    next.Add(child);
            }
            nodes = next;
        }
        foreach (var refNode in nodes)
        {
            var refStr = refNode?[\"reference\"]?.GetValue<""string>();
            if (refStr != null && refStr.Contains('/'))
                ids.Add(refStr.Split('/').Last());
        }
    }

    public static async Task<""List<""JsonObject>> ResolveIncludesAsync(
        IServiceProvider sp, string resourceType, List<""JsonNode> primaryResults,
        List<""string> primaryIds, string baseUrl, IQueryCollection query)
    {
        var included = new List<""JsonObject>();
        var seen = new HashSet<""string>();

        var includeParams = new List<""string>();
        if (query.TryGetValue(\"_include\", out var incValues))
            foreach (var v in incValues) if (v != null) foreach (var item in v.Split(',')) includeParams.Add(item.Trim());

        foreach (var inc in includeParams)
        {
            var parts = inc.Split(':');
            if (parts.Length <"" 2) continue;

            var sourceType = parts[0];
            var paramName = parts[1];
            var targetTypeFilter = parts.Length > 2 ? parts[2] : null;

            if (sourceType != resourceType) continue;

            if (paramName == \"*\")
            {
                var allRefProps = GetAllReferencePropertyNames(resourceType);
                foreach (var refProp in allRefProps)
                    await IncludeFromProperty(sp, primaryResults, refProp, null, baseUrl, included, seen);
                continue;
            }

            var propertyName = GetReferencePropertyName(sourceType, paramName);
            if (propertyName == null) continue;
            await IncludeFromProperty(sp, primaryResults, propertyName, targetTypeFilter, baseUrl, included, seen);
        }

        // _include:iterate (second pass on included results)
        if (query.TryGetValue(\"_include:iterate\", out var iterValues))
        {
            var iterResults = included.Select(e => e[\"resource\"]!).ToList();
            foreach (var v in iterValues)
            {
                if (v == null) continue;
                foreach (var item in v.Split(','))
                {
                    var inc = item.Trim();
                    var parts = inc.Split(':');
                    if (parts.Length <"" 2) continue;
                    var srcType = parts[0];
                    var pName = parts[1];
                    var propName = GetReferencePropertyName(srcType, pName);
                    if (propName == null) continue;
                    await IncludeFromProperty(sp, iterResults, propName, null, baseUrl, included, seen);
                }
            }
        }

        // _revinclude
        var revincludeParams = new List<""string>();
        if (query.TryGetValue(\"_revinclude\", out var revIncValues))
            foreach (var v in revIncValues) if (v != null) foreach (var item in v.Split(',')) revincludeParams.Add(item.Trim());

        foreach (var revInc in revincludeParams)
        {
            var parts = revInc.Split(':');
            if (parts.Length <"" 2) continue;

            var searchingType = parts[0];
            var refParamName = parts[1];

            if (primaryIds.Count == 0) continue;

            var refValues = primaryIds.Select(id => $\"{resourceType}/{id}\").ToArray();
            var searchQuery = new Dictionary<""string, StringValues>
            {
                [refParamName] = new StringValues(string.Join(\",\", refValues))
            };
            var qc = new QueryCollection(searchQuery);
            var (contents, ids, total) = await SearchAsync(sp, searchingType, qc, 10000, 0);

            for (int i = 0; i <"" contents.Count; i++)
            {
                var entryKey = $\"{searchingType}/{ids[i]}\";
                if (seen.Add(entryKey))
                    included.Add(FhirResponse.IncludeEntry(contents[i], $\"{baseUrl}/{searchingType}/{ids[i]}\"));
            }
        }

        // _revinclude:iterate (search from included results)
        if (query.TryGetValue(\"_revinclude:iterate\", out var revIterValues))
        {
            var iterIds = new List<""(string? rt, string? rid)>();
            foreach (var e in included)
            {
                var res = e[\"resource\"];
                var rt = res?[\"resourceType\"]?.GetValue<""string>();
                var rid = res?[\"id\"]?.GetValue<""string>();
                iterIds.Add((rt, rid));
            }

            foreach (var v in revIterValues)
            {
                if (v == null) continue;
                foreach (var item in v.Split(','))
                {
                    var revInc = item.Trim();
                    var parts = revInc.Split(':');
                    if (parts.Length <"" 2) continue;
                    var searchingType = parts[0];
                    var refParamName = parts[1];

                    var sourceType = GetActiveTargetTypes(searchingType, refParamName).FirstOrDefault();
                    if (sourceType == null) continue;

                    var matchingIds = iterIds
                        .Where(x => x.rt == sourceType)
                        .Select(x => $\"{sourceType}/{x.rid}\")
                        .ToArray();
                    if (matchingIds.Length == 0) continue;

                    var searchQuery = new Dictionary<""string, StringValues>
                    {
                        [refParamName] = new StringValues(string.Join(\",\", matchingIds))
                    };
                    var qc = new QueryCollection(searchQuery);
                    var (contents, ids, total) = await SearchAsync(sp, searchingType, qc, 10000, 0);
                    for (int i = 0; i <"" contents.Count; i++)
                    {
                        var entryKey = $\"{searchingType}/{ids[i]}\";
                        if (seen.Add(entryKey))
                            included.Add(FhirResponse.IncludeEntry(contents[i], $\"{baseUrl}/{searchingType}/{ids[i]}\"));
                    }
                }
            }
        }

        return included;
    }

    private static async Task IncludeFromProperty(IServiceProvider sp, List<""JsonNode> results,
        string propertyName, string? targetTypeFilter, string baseUrl, List<""JsonObject> included, HashSet<""string> seen)
    {
        foreach (var result in results)
        {
            var nodes = new List<""JsonNode?> { result };
            foreach (var segment in propertyName.Split('.'))
            {
                var next = new List<""JsonNode?>();
                foreach (var node in nodes)
                {
                    if (node == null) continue;
                    var child = node[segment];
                    if (child == null) continue;
                    if (child is JsonArray arr)
                        foreach (var item in arr) next.Add(item);
                    else
                        next.Add(child);
                }
                nodes = next;
            }

            foreach (var refNode in nodes)
            {
                var refStr = refNode?[\"reference\"]?.GetValue<""string>();
                if (refStr == null || !refStr.Contains('/')) continue;
                var slashIdx = refStr.IndexOf('/');
                var refType = refStr.Substring(0, slashIdx);
                var refId = refStr.Substring(slashIdx + 1);

                if (targetTypeFilter != null && refType != targetTypeFilter) continue;

                var entryKey = $\"{refType}/{refId}\";
                if (!seen.Add(entryKey)) continue;

                var content = await ReadAsync(sp, refType, refId);
                if (content != null)
                    included.Add(FhirResponse.IncludeEntry(content, $\"{baseUrl}/{refType}/{refId}\"));
            }
        }
    }

    public static string[] GetAllReferencePropertyNames(string resourceType)
    {
        switch (resourceType)
        {
"
        (for-active-resources (lambda (r) (let* ((rn (name-of r))
           (ref-searches (reference-searches-of r))
           (ref-search-list (node-list->list ref-searches))
           (dot-paths (filter (lambda (n) n)
               (map (lambda (s) (search-dot-path (search-path-parts s)))
                   ref-search-list))))
        (if (null? dot-paths) ""
            ($"            case \"" rn "\": return new[] { "
                (apply $ (map (lambda (n) ($ "\"" n "\", ")) dot-paths))
                "};
")))))
"        }
        return Array.Empty<""string>();
    }
}
"   )
))

; Build dot-separated property path from search path parts
; e.g. (part ref=link, part ref=other) → "link.other"
(define (search-dot-path parts)
    (if (null? parts) #f
        (let loop ((ps parts) (result ""))
            (if (null? ps) (if (string=? "" result) #f result)
                (let* ((p (car ps))
                       (r (% "ref" p))
                       (prop (if r (element-with-id r) #f))
                       (pn (if prop (% "name" prop) #f)))
                    (if pn
                        (loop (cdr ps) (if (string=? "" result) pn ($ result "." pn)))
                        #f))))))

(define (search-helper-ref-prop-resource-case resource)
    (let* ((rn (name-of resource))
           (ref-searches (reference-searches-of resource))
           (ref-search-list (node-list->list ref-searches)))
        (if (null? ref-search-list) ""
            ($
"            case \"" rn "\":
                switch (searchParamName)
                {
"
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (parts (search-path-parts s))
                           (dot-path (search-dot-path parts)))
                        (if dot-path
                            ($"                    case \"" sn "\": return \"" dot-path "\";
")
                            "")))
                    ref-search-list))
"                }
                break;
"))))

(define (search-helper-targets-resource-case resource)
    (let* ((rn (name-of resource))
           (ref-searches (reference-searches-of resource))
           (ref-search-list (node-list->list ref-searches)))
        (if (null? ref-search-list) ""
            ($
"            case \"" rn "\":
                switch (searchParamName)
                {
"
                (apply $ (map (lambda (s) (search-helper-target-case s)) ref-search-list))
"                }
                break;
"))))

(define (search-helper-target-case search)
    (let* ((sn (% "name" search))
           (paths-node (select-children "paths" search))
           (path-nodes (if (node-list-empty? paths-node) (empty-node-list)
                          (select-children "path" (node-list-first paths-node))))
           (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
           (targets-node (if first-path (select-children "targets" first-path) (empty-node-list)))
           (target-nodes (if (node-list-empty? targets-node) '()
                            (node-list->list (children (node-list-first targets-node)))))
           (active-target-names (filter (lambda (name) name)
               (map (lambda (t)
                   (let ((target-name (% "resource" t)))
                       (if (node-list-some? (lambda (r) (and (active? r) (string=? (name-of r) target-name)))
                                            (active-resources))
                           target-name
                           #f)))
                   target-nodes))))
        (if (null? active-target-names) ""
            ($"                    case \"" sn "\": return new[] { "
                (apply $ (map (lambda (n) ($ "\"" n "\", ")) active-target-names))
                "};
"))))

; ===================================================================
; CompositeSearchHelper - in-memory composite search filtering
; ===================================================================

(define (boilerplate-composite-search-helper)
    (file "CompositeSearchHelper.cs" ($
"using System.Text.Json.Nodes;

namespace "(project-name)";

public static class CompositeSearchHelper
{
    private static readonly string[] CompositeParamNames = {
        \"code-value-quantity\", \"code-value-concept\",
        \"component-code-value-quantity\", \"component-code-value-concept\"
    };

    public static bool HasCompositeParams(IQueryCollection query)
    {
        return query.Any(p => CompositeParamNames.Contains(p.Key));
    }

    public static (string? codeParamName, string? codeValue) ExtractCodeComponent(string paramName, string compositeValue)
    {
        var dollarIdx = compositeValue.IndexOf('$');
        if (dollarIdx <"" 0) return (null, null);

        var codePart = compositeValue.Substring(0, dollarIdx);
        var codeParamName = paramName.StartsWith(\"component-\") ? \"component-code\" : \"code\";
        return (codeParamName, codePart);
    }

    public static bool MatchesComposite(JsonNode resource, string paramName, string compositeValue)
    {
        var dollarIdx = compositeValue.IndexOf('$');
        if (dollarIdx <"" 0) return true;

        var codePart = compositeValue.Substring(0, dollarIdx);
        var valuePart = compositeValue.Substring(dollarIdx + 1);

        return paramName switch
        {
            \"code-value-quantity\" => MatchesToken(resource[\"code\"], codePart) &&
                                     MatchesQuantity(resource[\"valueQuantity\"], valuePart),
            \"code-value-concept\" => MatchesToken(resource[\"code\"], codePart) &&
                                    MatchesToken(resource[\"valueCodeableConcept\"], valuePart),
            \"component-code-value-quantity\" => MatchesComponentCodeValueQuantity(resource, codePart, valuePart),
            \"component-code-value-concept\" => MatchesComponentCodeValueConcept(resource, codePart, valuePart),
            _ => true
        };
    }

    private static bool MatchesComponentCodeValueQuantity(JsonNode resource, string codePart, string valuePart)
    {
        var components = resource[\"component\"] as JsonArray;
        if (components == null) return false;
        return components.Any(c =>
            MatchesToken(c?[\"code\"], codePart) && MatchesQuantity(c?[\"valueQuantity\"], valuePart));
    }

    private static bool MatchesComponentCodeValueConcept(JsonNode resource, string codePart, string valuePart)
    {
        var components = resource[\"component\"] as JsonArray;
        if (components == null) return false;
        return components.Any(c =>
            MatchesToken(c?[\"code\"], codePart) && MatchesToken(c?[\"valueCodeableConcept\"], valuePart));
    }

    private static bool MatchesToken(JsonNode? codeableConcept, string tokenValue)
    {
        if (codeableConcept == null) return false;
        var coding = codeableConcept[\"coding\"] as JsonArray;
        if (coding == null) return false;

        var pipeIdx = tokenValue.IndexOf('|');
        if (pipeIdx >= 0)
        {
            var system = tokenValue.Substring(0, pipeIdx);
            var code = tokenValue.Substring(pipeIdx + 1);
            return coding.Any(c =>
                (string.IsNullOrEmpty(system) || c?[\"system\"]?.GetValue<""string>() == system) &&
                c?[\"code\"]?.GetValue<""string>() == code);
        }
        else
        {
            return coding.Any(c => c?[\"code\"]?.GetValue<""string>() == tokenValue);
        }
    }

    private static bool MatchesQuantity(JsonNode? quantity, string quantityValue)
    {
        if (quantity == null) return false;
        var valueNode = quantity[\"value\"];
        if (valueNode == null) return false;

        decimal value;
        try { value = valueNode.GetValue<""decimal>(); }
        catch { return false; }

        string prefix = \"eq\";
        string numStr = quantityValue;

        if (quantityValue.Length > 2 && char.IsLetter(quantityValue[0]) && char.IsLetter(quantityValue[1]))
        {
            prefix = quantityValue.Substring(0, 2);
            numStr = quantityValue.Substring(2);
        }

        if (!decimal.TryParse(numStr, System.Globalization.NumberStyles.Any,
            System.Globalization.CultureInfo.InvariantCulture, out var target))
            return false;

        return prefix switch
        {
            \"eq\" => value == target,
            \"ne\" => value != target,
            \"gt\" => value > target,
            \"lt\" => value <"" target,
            \"ge\" => value >= target,
            \"le\" => value <""= target,
            _ => false
        };
    }
}
")))

; ===================================================================
; SearchParameterHandler - serves SearchParameter resources
; ===================================================================

; Build FHIRPath expression from search path parts, prefixed with resource name
; e.g. Patient + (part ref=patient.name, part ref=humanname.family) → "Patient.name.family"
(define (search-fhirpath resource-name parts)
    (let ((dot-path (search-dot-path parts)))
        (if dot-path ($ resource-name "." dot-path) #f)))

(define (boilerplate-search-parameter-handler)
    (file "SearchParameterHandler.cs" ($
"using System.Text.Json;
using System.Text.Json.Nodes;

namespace "(project-name)".Handlers;

public static class SearchParameterHandler
{
    private static readonly Dictionary<""string, byte[]> _byBase = Build();

    public static IResult Get(HttpContext ctx)
    {
        var baseParam = ctx.Request.Query[\"base\"].FirstOrDefault();
        if (baseParam is null || !_byBase.TryGetValue(baseParam, out var bundle))
            return FhirResult.Resource(ctx, EmptyBundle());
        return FhirResult.Resource(ctx, bundle);
    }

    private static byte[] EmptyBundle()
    {
        var bundle = new JsonObject
        {
            [\"resourceType\"] = \"Bundle\",
            [\"type\"] = \"searchset\",
            [\"total\"] = 0,
            [\"entry\"] = new JsonArray()
        };
        return JsonSerializer.SerializeToUtf8Bytes(bundle);
    }

    private static Dictionary<""string, byte[]> Build()
    {
        var all = new List<""(string baseName, JsonObject sp)>();
"
(for-active-resources (lambda (resource)
    (let* ((rn (name-of resource))
           (searches-node (select-children "searches" resource))
           (all-searches (if (node-list-empty? searches-node) '()
                            (node-list->list (children (node-list-first searches-node)))))
           (non-composite-searches (filter (lambda (s) (not (string=? "composite" (% "type" s)))) all-searches)))
        (apply $ (map (lambda (s)
            (let* ((sn (% "name" s))
                   (st (% "type" s))
                   (parts (search-path-parts s))
                   (fhirpath (search-fhirpath rn parts)))
                (if fhirpath
                    ($"        Add(all, \"" rn "\", \"" sn "\", \"" st "\", \"" fhirpath "\");
")
                    "")))
            non-composite-searches)))))
"
        var result = new Dictionary<""string, byte[]>();
        foreach (var group in all.GroupBy(x => x.baseName))
        {
            var entries = new JsonArray();
            foreach (var (_, sp) in group)
                entries.Add(new JsonObject { [\"resource\"] = sp });
            var bundle = new JsonObject
            {
                [\"resourceType\"] = \"Bundle\",
                [\"type\"] = \"searchset\",
                [\"total\"] = entries.Count,
                [\"entry\"] = entries
            };
            result[group.Key] = JsonSerializer.SerializeToUtf8Bytes(bundle);
        }
        return result;
    }

    private static void Add(List<""(string, JsonObject)> all, string baseName, string code, string type, string expression)
    {
        all.Add((baseName, new JsonObject
        {
            [\"resourceType\"] = \"SearchParameter\",
            [\"code\"] = code,
            [\"base\"] = new JsonArray(baseName),
            [\"type\"] = type,
            [\"expression\"] = expression
        }));
    }
}
")))

; ===================================================================
; MetadataHandler - CapabilityStatement
; ===================================================================

(define (boilerplate-metadata-handler)
    (file "MetadataHandler.cs" ($
"using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace " (project-name) ".Handlers;

public static class MetadataHandler
{
    private static readonly byte[] _capabilityStatement = Build();

    public static IResult Get(HttpContext ctx) => FhirResult.Resource(ctx, _capabilityStatement);

    private static byte[] Build()
    {
        var cs = new JsonObject
        {
            [\"resourceType\"] = \"CapabilityStatement\",
            [\"status\"] = \"active\",
            [\"kind\"] = \"instance\",
            [\"fhirVersion\"] = \"5.0.0\",
            [\"format\"] = new JsonArray(\"json\", \"xml\"),
            [\"rest\"] = new JsonArray
            {
                new JsonObject
                {
                    [\"mode\"] = \"server\",
                    [\"resource\"] = new JsonArray
                    {
"
(for-active-resources (lambda (resource)
    (let* ((rn (name-of resource))
           (has-history (history? resource))
           (searches-node (select-children "searches" resource))
           (all-searches (if (node-list-empty? searches-node) '()
                            (node-list->list (children (node-list-first searches-node)))))
           (non-composite-searches (filter (lambda (s) (not (string=? "composite" (% "type" s)))) all-searches)))
        ($"                        new JsonObject
                        {
                            [\"type\"] = \"" rn "\",
                            [\"interaction\"] = new JsonArray
                            {
                                new JsonObject { [\"code\"] = \"read\" },
"
    (if has-history
        "                                new JsonObject { [\"code\"] = \"vread\" },
"
        "")
"                                new JsonObject { [\"code\"] = \"update\" },
                                new JsonObject { [\"code\"] = \"patch\" },
                                new JsonObject { [\"code\"] = \"delete\" },
                                new JsonObject { [\"code\"] = \"create\" },
                                new JsonObject { [\"code\"] = \"search-type\" },
"
    (if has-history
        "                                new JsonObject { [\"code\"] = \"history-instance\" },
                                new JsonObject { [\"code\"] = \"history-type\" },
"
        "")
"                            },
                            [\"versioning\"] = \"" (if has-history "versioned" "no-version") "\",
                            [\"conditionalCreate\"] = true,
                            [\"conditionalUpdate\"] = true,
                            [\"conditionalDelete\"] = \"single\",
                            [\"searchParam\"] = new JsonArray
                            {
"
    (apply $ (map (lambda (s)
        ($"                                new JsonObject { [\"name\"] = \"" (% "name" s) "\", [\"type\"] = \"" (% "type" s) "\" },
"))
        non-composite-searches))
"                            }
                        },
"))))
"                    },
                    [\"interaction\"] = new JsonArray
                    {
                        new JsonObject { [\"code\"] = \"transaction\" },
                        new JsonObject { [\"code\"] = \"batch\" }
                    }
                }
            }
        };

        return JsonSerializer.SerializeToUtf8Bytes(cs);
    }
}
")))

; --- SystemHistoryHandler ---

(define (boilerplate-system-history-handler)
    (file "SystemHistoryHandler.cs" ($
"using System.Text.Json;
using System.Text.Json.Nodes;
using "(project-name)".Repositories;

namespace "(project-name)".Handlers;

public static class SystemHistoryHandler
{
    public static async Task<""IResult> Get(HttpContext ctx)
    {
        var count = 20;
        var offset = 0;
        if (ctx.Request.Query.TryGetValue(\"_count\", out var countStr) && int.TryParse(countStr, out var c))
            count = Math.Min(c, 1000);
        if (ctx.Request.Query.TryGetValue(\"_offset\", out var offsetStr) && int.TryParse(offsetStr, out var o))
            offset = o;

        var allRows = new List<""(string resourceType, string id, int versionId, DateTime lastUpdated, byte[]? content, string historyOperation)>();

"
    (for-active-resources (lambda (resource)
        (let ((rn (name-of resource))
              (has-history (history? resource)))
            (if has-history
                ($"        {
            var repo = ctx.RequestServices.GetRequiredService<""I" rn "Repository>();
            var (" (downcase-string rn) "Rows, _) = await repo.TypeHistoryAsync(10000, 0);
            allRows.AddRange(" (downcase-string rn) "Rows.Select(r => (\"" rn "\", r.id, r.versionId, r.lastUpdated, r.content, r.historyOperation)));
        }

")
                ""))))
"        // Sort by lastUpdated descending
        allRows.Sort((a, b) => b.lastUpdated.CompareTo(a.lastUpdated));

        var total = allRows.Count;
        var paged = allRows.Skip(offset).Take(count).ToList();

        var baseUrl = $\"{ctx.Request.Scheme}://{ctx.Request.Host}/fhir\";
        var entries = paged.Select(r =>
        {
            // A DELETE row stands for two entries: the tombstone the query synthesizes as
            // TOMBSTONE, and the row itself, which still carries the state the resource was
            // in when it was deleted and is rendered as the version it is.
            if (r.historyOperation == \"TOMBSTONE\")
            {
                return FhirResponse.HistoryDeleteEntry(
                    $\"{baseUrl}/{r.resourceType}/{r.id}\",
                    \"DELETE\", $\"{r.resourceType}/{r.id}\",
                    \"204\",
                    r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
            }
            return FhirResponse.HistoryEntry(
                JsonNode.Parse(r.content)!,
                $\"{baseUrl}/{r.resourceType}/{r.id}\",
                \"PUT\",
                $\"{r.resourceType}/{r.id}\",
                \"200\",
                r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
        }).ToList();

        var selfUrl = $\"{baseUrl}/_history{ctx.Request.QueryString}\";
        var bundle = FhirResponse.HistoryBundle(entries, total, selfUrl, count, offset);

        return FhirResult.Bundle(ctx, bundle);
    }
}
")))

; --- HTML documentation header/footer ---

(define (html-header title prev up next) ($
"<""!DOCTYPE HTML>
<""html xml:lang='en' xmlns='http://www.w3.org/1999/xhtml' lang='en'>
<""head>
    <""title>"title"<""/title>
    <""meta name='viewport' content='width=device-width, initial-scale=1.0' charset='utf-8' />
    <""link rel='stylesheet' href='css/fhir.css' />
    <""link rel='stylesheet' href='css/fhir-pub.css' />
    <""link rel='stylesheet' href='css/fire.css' />
    <""link rel='stylesheet' href='css/bootstrap.css' />
    <""link rel='stylesheet' href='css/bootstrap-fhir.css' />
    <""link rel='stylesheet' href='css/project.css' />
    <""link rel='stylesheet' href='css/jquery-ui.css' />
    <""link rel='stylesheet' href='css/modules.css' />
<""/head>
<""body>
    <""div id='segment-navbar' class='segment'>
        <""div class='container'>
            <""nav class='navbar navbar-inverse'>
                <""div class='container'>
                    <""div class='nav-collapse collapse navbar-inverse-collapse'>
                        <""ul class='nav navbar-nav'>
"   (if prev ($
"                            <""li><""a href='"prev".html'>Prev<""/a><""/li>
"   )"")
    (if up ($
"                            <""li><""a href='"up".html'>Up<""/a><""/li>
"   )"")
"                            <""li><""a href='index.html'>Home<""/a><""/li>
                            <""li><""a href='documentation.html'>TOC<""/a><""/li>
"   (if next ($
"                            <""li><""a href='"next".html'>Next<""/a><""/li>
"   )"")
"                            <""li><""a href='resourcelist.html'><""span style='color: yellow'>Resource Types<""/span><""/a><""/li>
                        <""/ul>
                    <""/div>
                <""/div>
            <""/nav>
        <""/div>
    <""/div>
    <""div class='container'>
        <""div class='row'>
            <""div class='col-12'>
                <""p><""/p>
"))

(define (html-footer) ($
"            <""/div>
        <""/div>
    <""/div>
<""/body>
<""/html>"))
