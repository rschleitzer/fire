; Handlers generator - generates Fire/Handlers/{Resource}Handlers.cs
; Called from (element resource ...) context
; Uses helpers from migration.scm (inline-searches)

; ===================================================================
; Main handler function
; ===================================================================

; A resource may be named Task, and a handler sees the model's classes beside
; System.Threading.Tasks: with it active, the plain Task of a signature is named in full, and
; so is the model's class in its own handler. Without it nothing changes.
(define (handler-plain-task)
    (if (member "Task" (map name-of (node-list->list (node-list-filter active?
            (children (if (string=? (gi (current-node)) "resources") (current-node) (parent (current-node))))))))
        "System.Threading.Tasks.Task" "Task"))
; (Task and Endpoint are also names of what ASP.NET brings into a handler's file.)
(define (handler-model-class rn)
    (if (string=? rn "*") "TModel"
        (case rn (("Task" "Endpoint") ($ (project-name) ".Model." rn)) (else rn))))

; The handlers of every resource are ONE class (two: with and without versions), generic over
; what a resource brings -- its repository, its records, its model class and, through a type
; of its own, its name and its validator. A resource's file is that type and the name its
; handlers always had, as an alias of the class made for it: {Resource}Handlers.
; (The type is {Resource}HandlerType: "Type" alone met a model class -- a Document's type
; beside a resource DocumentType.)
(define generic-rn "*")
(define (h-generic? rn) (string=? rn generic-rn))
(define (h-repo rn) (if (h-generic? rn) "TRepo" ($ "I" rn "Repository")))
(define (h-res rn) (if (h-generic? rn) "TResource" ($ rn "Resource")))
(define (h-validate rn) (if (h-generic? rn) "T.Validate" ($ "ModelValidator.Validate" rn)))
(define (h-name rn) (if (h-generic? rn) "T.Name" ($ "\"" rn "\"")))
(define (h-in rn) (if (h-generic? rn) "{T.Name}" rn))
(define (h-class rn has-history)
    (if has-history
        ($
"public static class ResourceHandlers<""T, TRepo, TResource, TRow, TModel>
    where T : IResourceType<""TModel>
    where TRepo : IVersionedResourceRepository<""TResource, TRow>
    where TResource : IResourceRecord
    where TRow : IHistoryRow
    where TModel : class")
        ($
"public static class UnversionedResourceHandlers<""T, TRepo, TResource, TModel>
    where T : IResourceType<""TModel>
    where TRepo : IResourceRepository<""TResource>
    where TResource : IResourceRecord
    where TModel : class")))

(define (handler-class-text rn has-history)
    ($
        (handler-header rn has-history)
        (handler-create rn has-history)
        (handler-read rn has-history)
        (if has-history (handler-vread rn) (handler-vread-non-versioned rn))
        (handler-update rn has-history)
        (handler-conditional-update rn has-history)
        (handler-patch rn has-history)
        (handler-delete rn)
        (handler-conditional-delete rn)
        (if has-history (handler-rollback rn) "")
        (if has-history (handler-undelete rn) "")
        (handler-search rn)
        (handler-post-search rn)
        (if has-history (handler-instance-history rn) "")
        (if has-history (handler-type-history rn) "")
        (handler-autocomplete rn)
        (handler-import rn)
        (handler-create-search rn)
        (handler-drop-search rn)
        (handler-purge rn)
        (handler-get-base-url)))

; The two classes, and what a resource's type answers.
(define (generic-handlers)
    (sosofo-append
        (file "ResourceHandlers.cs" (handler-class-text generic-rn #t))
        (file "UnversionedResourceHandlers.cs" (handler-class-text generic-rn #f))
        (file "IResourceType.cs" ($
"namespace " (project-name) ".Handlers;

// What the handlers ask of a resource: its name, and its model's validator.
public interface IResourceType<""TModel>
{
    static abstract string Name { get; }
    static abstract List<""string> Validate(TModel model);
}
"))))

(define (handler)
    (let* ((resource (current-node))
           (rn (name-of resource))
           (has-history (history? resource))
           (model-class ($ (project-name) ".Model." rn))
           (p (project-name)))
        (file ($ rn "Handlers.cs")
            ($
"global using " rn "Handlers = " p ".Handlers." (if has-history "ResourceHandlers" "UnversionedResourceHandlers") "<""" p ".Handlers." rn "HandlerType, " p ".Repositories.I" rn "Repository, " p ".Search." rn "Resource, " (if has-history ($ p ".Search." rn "HistoryRow, ") "") model-class ">;

namespace " p ".Handlers;

public sealed class " rn "HandlerType : IResourceType<""" model-class ">
{
    public static string Name => \"" rn "\";
    public static List<""string> Validate(" model-class " model) => " p ".Model.Validator.ModelValidator.Validate" rn "(model);
}
"))))

; ===================================================================
; Section: Header
; ===================================================================

(define (handler-header rn has-history)
    ($
"using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Extensions.Primitives;
using " (project-name) ".Model;
using " (project-name) ".Model.Validator;
using " (project-name) ".Search;
using " (project-name) ".Repositories;

namespace " (project-name) ".Handlers;

"(h-class rn has-history)"
{
    public static void Configure(IConfiguration configuration)
    {
    }

"))

; ===================================================================
; Section: Create
; ===================================================================

(define (handler-create rn has-history)
    ($
"    public static async Task<""IResult> Create(HttpContext ctx, "(h-repo rn)" repo)
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
        if (resourceType == null)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Missing required field: resourceType\", 400);
        }
        if (resourceType != "(h-name rn)")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Resource type mismatch: expected "(h-in rn)", got {resourceType}\", 400);
        }

"
        (handler-model-validation rn)
"
        // Handle conditional create (If-None-Exist header)
        var ifNoneExist = ctx.Request.Headers[\"If-None-Exist\"].FirstOrDefault();
        if (ifNoneExist != null)
        {
            var searchParams = System.Web.HttpUtility.ParseQueryString(ifNoneExist);
            var queryCollection = new QueryCollection(
                searchParams.AllKeys.Where(k => k != null).ToDictionary(k => k!, k => new Microsoft.Extensions.Primitives.StringValues(searchParams[k])));
            var (existing, total) = await repo.SearchAsync(queryCollection, 2, 0);
            if (total == 1)
            {
                return FhirResult.Resource(ctx, existing[0].content, 200);
            }
            if (total > 1)
            {
                return FhirResult.Error(ctx, \"error\", \"duplicate\", \"Multiple matches found for conditional create\", 412);
            }
        }

        "(h-res rn)" resource;
        try
        {
            resource = await repo.CreateAsync(content);
        }
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }

        var baseUrl = GetBaseUrl(ctx);
"
    "        ctx.Response.Headers[\"Location\"] = $\"{baseUrl}/"(h-in rn)"/{resource.id}/_history/{resource.versionId}\";
        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
"
"
        return await HandlePrefer(ctx, resource.content, 201);
    }

"))

; --- Model deserialization + validation ---
(define (handler-model-validation rn)
    ($
"        "(handler-model-class rn)" model;
        try
        {
            model = content.Deserialize<"(handler-model-class rn)">()!;
        }
        catch (JsonException ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }

        var validationErrors = "(h-validate rn)"(model);
        if (validationErrors.Count > 0)
            return FhirResult.ValidationErrors(ctx, validationErrors);

"))

; ===================================================================
; Section: Read
; ===================================================================

(define (handler-read rn has-history)
    ($
"    public static async Task<""IResult> Read(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        var resource = await repo.ReadAsync(id);
        if (resource == null)
        {
" (if has-history "            if (await repo.IsDeletedAsync(id))
                return FhirResult.Error(ctx, \"error\", \"deleted\", \"Resource has been deleted\", 410);
" "") "            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found\", 404);
        }

"
    "        var ifNoneMatch = ctx.Request.Headers[\"If-None-Match\"].FirstOrDefault();
        if (ifNoneMatch != null)
        {
            var versionStr = ifNoneMatch.Replace(\"W/\\\"\", \"\").Replace(\"\\\"\", \"\");
            if (int.TryParse(versionStr, out var v) && v == resource.versionId)
                return Results.StatusCode(304);
        }

        var ifModifiedSince = ctx.Request.Headers[\"If-Modified-Since\"].FirstOrDefault();
        if (ifModifiedSince != null && DateTime.TryParseExact(ifModifiedSince, \"R\",
            System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None, out var since)
            && resource.lastUpdated.ToUniversalTime() <= since.ToUniversalTime())
            return Results.StatusCode(304);

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
"
    (if (or (string=? "Binary" rn) (h-generic? rn)) ($
(if (h-generic? rn) "        if (T.Name == \"Binary\")
        {
" "")
"        // Binary content negotiation: serve raw bytes unless FHIR format is requested
        var format = ctx.Request.Query[\"_format\"].FirstOrDefault() ?? \"\";
        var accept = ctx.Request.Headers[\"Accept\"].FirstOrDefault() ?? \"\";
        var wantsFhir = format.Contains(\"json\") || format.Contains(\"xml\")
            || accept.Contains(\"fhir\");
        if (!wantsFhir)
        {
            var parsed = JsonNode.Parse(resource.content);
            var binaryContentType = parsed?[\"contentType\"]?.GetValue<""string>();
            var data = parsed?[\"data\"]?.GetValue<""string>();
            if (binaryContentType != null && data != null)
            {
                var bytes = Convert.FromBase64String(data);
                ctx.Response.ContentType = binaryContentType;
                ctx.Response.StatusCode = 200;
                await ctx.Response.Body.WriteAsync(bytes);
                return Results.Empty;
            }
        }
"
(if (h-generic? rn) "        }
" "")
"
"
        )"")
"        return FhirResult.Resource(ctx, resource.content);
    }

"))

; ===================================================================
; Section: VersionRead
; ===================================================================

(define (handler-vread rn)
    ($
"    public static async Task<""IResult> VersionRead(string id, string vid, HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!int.TryParse(vid, out var versionId))
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid version id\", 400);
        }

        var resource = await repo.VersionReadAsync(id, versionId);
        if (resource == null)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id}/_history/{versionId} not found\", 404);
        }

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
"
    (if (or (string=? "Binary" rn) (h-generic? rn)) ($
(if (h-generic? rn) "        if (T.Name == \"Binary\")
        {
" "")
"        // Binary content negotiation: serve raw bytes unless FHIR format is requested
        var format = ctx.Request.Query[\"_format\"].FirstOrDefault() ?? \"\";
        var accept = ctx.Request.Headers[\"Accept\"].FirstOrDefault() ?? \"\";
        var wantsFhir = format.Contains(\"json\") || format.Contains(\"xml\")
            || accept.Contains(\"fhir\");
        if (!wantsFhir)
        {
            var parsed = JsonNode.Parse(resource.content);
            var binaryContentType = parsed?[\"contentType\"]?.GetValue<""string>();
            var data = parsed?[\"data\"]?.GetValue<""string>();
            if (binaryContentType != null && data != null)
            {
                var bytes = Convert.FromBase64String(data);
                ctx.Response.ContentType = binaryContentType;
                ctx.Response.StatusCode = 200;
                await ctx.Response.Body.WriteAsync(bytes);
                return Results.Empty;
            }
        }
"
(if (h-generic? rn) "        }
" "")
"
"
        )"")
"        return FhirResult.Resource(ctx, resource.content);
    }

"))

(define (handler-vread-non-versioned rn)
    ($
"    public static async Task<""IResult> VersionRead(string id, string vid, HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!int.TryParse(vid, out var versionId))
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid version id\", 400);
        }

        var resource = await repo.ReadAsync(id);
        if (resource == null || resource.versionId != versionId)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id}/_history/{versionId} not found\", 404);
        }

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
        return FhirResult.Resource(ctx, resource.content);
    }

"))

; ===================================================================
; Section: Update
; ===================================================================

(define (handler-update rn has-history)
    (if has-history
        (handler-update-versioned rn)
        (handler-update-non-versioned rn)))

(define (handler-update-versioned rn)
    ($
"    public static async Task<""IResult> Update(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        // Validate ID format
        if (id.Length > 64 || id.Any(c => !char.IsAsciiLetterOrDigit(c) && c != '-' && c != '.'))
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid resource ID format\", 400);
        }

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
        if (resourceType != "(h-name rn)")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Resource type mismatch: expected "(h-in rn)", got {resourceType}\", 400);
        }

        var bodyId = content[\"id\"]?.GetValue<""string>();
        if (bodyId != null && bodyId != id)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\",
                $\"Resource ID in body ({bodyId}) does not match URL ID ({id})\", 400);
        }

"
        (handler-model-validation rn)
"        int? ifMatchVersion = null;
        var ifMatch = ctx.Request.Headers[\"If-Match\"].FirstOrDefault();
        if (ifMatch != null)
        {
            // Format: W/\"<""versionId>\"
            var versionStr = ifMatch.Replace(\"W/\\\"\", \"\").Replace(\"\\\"\", \"\");
            if (int.TryParse(versionStr, out var v))
                ifMatchVersion = v;
        }

        try
        {
            var resource = await repo.UpdateAsync(id, content, ifMatchVersion);
            if (resource == null)
            {
                // Update-as-create: PUT to non-existent ID creates the resource
                resource = await repo.CreateAsync(content, id);
                var baseUrl = GetBaseUrl(ctx);
                ctx.Response.Headers[\"Location\"] = $\"{baseUrl}/"(h-in rn)"/{resource.id}/_history/{resource.versionId}\";
                ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
                ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
                return await HandlePrefer(ctx, resource.content, 201);
            }

            ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
            ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
            return await HandlePrefer(ctx, resource.content, 200);
        }
        catch (ConcurrencyException)
        {
            return FhirResult.Error(ctx, \"error\", \"conflict\", \"Version conflict\", 409);
        }
        // what the repository refuses of the content is the client's, as on a create
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }
    }

"))

(define (handler-update-non-versioned rn)
    ($
"    public static async Task<""IResult> Update(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        // Validate ID format
        if (id.Length > 64 || id.Any(c => !char.IsAsciiLetterOrDigit(c) && c != '-' && c != '.'))
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid resource ID format\", 400);
        }

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
        if (resourceType != "(h-name rn)")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Resource type mismatch: expected "(h-in rn)", got {resourceType}\", 400);
        }

        var bodyId = content[\"id\"]?.GetValue<""string>();
        if (bodyId != null && bodyId != id)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\",
                $\"Resource ID in body ({bodyId}) does not match URL ID ({id})\", 400);
        }

"
        (handler-model-validation rn)
"        try
        {
            var resource = await repo.UpdateAsync(id, content);
            if (resource == null)
            {
                // Update-as-create: PUT to non-existent ID creates the resource
                resource = await repo.CreateAsync(content, id);
                var baseUrl = GetBaseUrl(ctx);
                ctx.Response.Headers[\"Location\"] = $\"{baseUrl}/"(h-in rn)"/{resource.id}/_history/{resource.versionId}\";
                ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
                ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
                return await HandlePrefer(ctx, resource.content, 201);
            }

            ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
            ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
            return await HandlePrefer(ctx, resource.content, 200);
        }
        // what the repository refuses of the content is the client's, as on a create
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }
    }

"))

; ===================================================================
; Section: ConditionalUpdate
; ===================================================================

(define (handler-conditional-update rn has-history)
    (if has-history
        (handler-conditional-update-versioned rn)
        (handler-conditional-update-non-versioned rn)))

(define (handler-conditional-update-versioned rn)
    ($
"    public static async Task<""IResult> ConditionalUpdate(HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!ctx.Request.Query.Any())
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Conditional update requires search parameters\", 400);

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
        if (resourceType != "(h-name rn)")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Resource type mismatch: expected "(h-in rn)", got {resourceType}\", 400);
        }

"
        (handler-model-validation rn)
"        int? ifMatchVersion = null;
        var ifMatch = ctx.Request.Headers[\"If-Match\"].FirstOrDefault();
        if (ifMatch != null)
        {
            var versionStr = ifMatch.Replace(\"W/\\\"\", \"\").Replace(\"\\\"\", \"\");
            if (int.TryParse(versionStr, out var v))
                ifMatchVersion = v;
        }

        var (existing, total) = await repo.SearchAsync(ctx.Request.Query, 2, 0);

        if (total > 1)
            return FhirResult.Error(ctx, \"error\", \"multiple-matches\", \"Multiple matches found for conditional update\", 412);

        if (total == 1)
        {
            try
            {
                var resource = await repo.UpdateAsync(existing[0].id, content, ifMatchVersion);
                ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource!.versionId}\\\"\";
                ctx.Response.Headers[\"Last-Modified\"] = resource!.lastUpdated.ToUniversalTime().ToString(\"R\");
                return await HandlePrefer(ctx, resource.content, 200);
            }
            catch (ConcurrencyException)
            {
                return FhirResult.Error(ctx, \"error\", \"conflict\", \"Version conflict\", 409);
            }
        }

        // No match - create
        var id = content[\"id\"]?.GetValue<""string>();
        var created = id != null
            ? await repo.CreateAsync(content, id)
            : await repo.CreateAsync(content);
        var baseUrl = GetBaseUrl(ctx);
        ctx.Response.Headers[\"Location\"] = $\"{baseUrl}/"(h-in rn)"/{created.id}/_history/{created.versionId}\";
        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{created.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = created.lastUpdated.ToUniversalTime().ToString(\"R\");
        return await HandlePrefer(ctx, created.content, 201);
    }

"))

(define (handler-conditional-update-non-versioned rn)
    ($
"    public static async Task<""IResult> ConditionalUpdate(HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!ctx.Request.Query.Any())
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Conditional update requires search parameters\", 400);

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
        if (resourceType != "(h-name rn)")
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", $\"Resource type mismatch: expected "(h-in rn)", got {resourceType}\", 400);
        }

"
        (handler-model-validation rn)
"        var (existing, total) = await repo.SearchAsync(ctx.Request.Query, 2, 0);

        if (total > 1)
            return FhirResult.Error(ctx, \"error\", \"multiple-matches\", \"Multiple matches found for conditional update\", 412);

        if (total == 1)
        {
            var resource = await repo.UpdateAsync(existing[0].id, content);
            ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource!.versionId}\\\"\";
            ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
            return await HandlePrefer(ctx, resource.content, 200);
        }

        // No match - create
        var id = content[\"id\"]?.GetValue<""string>();
        var created = id != null
            ? await repo.CreateAsync(content, id)
            : await repo.CreateAsync(content);
        var baseUrl = GetBaseUrl(ctx);
        ctx.Response.Headers[\"Location\"] = $\"{baseUrl}/"(h-in rn)"/{created.id}/_history/{created.versionId}\";
        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{created.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = created.lastUpdated.ToUniversalTime().ToString(\"R\");
        return await HandlePrefer(ctx, created.content, 201);
    }

"))

; ===================================================================
; Section: Patch
; ===================================================================

(define (handler-patch rn has-history)
    (if has-history
        (handler-patch-versioned rn)
        (handler-patch-non-versioned rn)))

(define (handler-patch-versioned rn)
    ($
"    public static async Task<""IResult> Patch(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        JsonArray patchOps;
        try
        {
            using var doc = await JsonDocument.ParseAsync(ctx.Request.Body);
            patchOps = JsonNode.Parse(doc.RootElement.GetRawText())!.AsArray();
        }
        catch
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid JSON Patch\", 400);
        }

        int? ifMatchVersion = null;
        var ifMatch = ctx.Request.Headers[\"If-Match\"].FirstOrDefault();
        if (ifMatch != null)
        {
            var match = System.Text.RegularExpressions.Regex.Match(ifMatch, \"W/\\\"(\\\\d+)\\\"\");
            if (match.Success)
                ifMatchVersion = int.Parse(match.Groups[1].Value);
        }

        "(h-res rn)"? resource;
        try
        {
            resource = await repo.PatchAsync(id, patchOps, ifMatchVersion);
        }
        catch (ConcurrencyException ex)
        {
            return FhirResult.Error(ctx, \"error\", \"conflict\",
                $\"Version conflict: current version is {ex.CurrentVersion}\", 409);
        }
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }

        if (resource == null)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found\", 404);
        }

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
        return await HandlePrefer(ctx, resource.content, 200);
    }

"))

(define (handler-patch-non-versioned rn)
    ($
"    public static async Task<""IResult> Patch(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        JsonArray patchOps;
        try
        {
            using var doc = await JsonDocument.ParseAsync(ctx.Request.Body);
            patchOps = JsonNode.Parse(doc.RootElement.GetRawText())!.AsArray();
        }
        catch
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid JSON Patch\", 400);
        }

        "(h-res rn)"? resource;
        try
        {
            resource = await repo.PatchAsync(id, patchOps);
        }
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400);
        }

        if (resource == null)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found\", 404);
        }

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
        return await HandlePrefer(ctx, resource.content, 200);
    }

"))

; ===================================================================
; Section: Delete
; ===================================================================

(define (handler-delete rn)
    ($
"    public static async Task<""IResult> Delete(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        var deleted = await repo.DeleteAsync(id);
        if (!deleted)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found\", 404);
        }

        return Results.StatusCode(204);
    }

"))

; ===================================================================
; Section: ConditionalDelete
; ===================================================================

(define (handler-conditional-delete rn)
    ($
"    public static async Task<""IResult> ConditionalDelete(HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!ctx.Request.Query.Any())
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Conditional delete requires search parameters\", 400);

        var (existing, total) = await repo.SearchAsync(ctx.Request.Query, int.MaxValue, 0);

        foreach (var resource in existing)
            await repo.DeleteAsync(resource.id);

        return Results.StatusCode(204);
    }

"))

; ===================================================================
; Section: Rollback
; ===================================================================

(define (handler-rollback rn)
    ($
"    public static async Task<""IResult> Rollback(string id, string vid, HttpContext ctx, "(h-repo rn)" repo)
    {
        if (!int.TryParse(vid, out var versionId))
        {
            return FhirResult.Error(ctx, \"error\", \"invalid\", \"Invalid version id\", 400);
        }

        // The version that is about to be removed. Whatever the same write recorded next to
        // it - its provenance - carries a timestamp no older than this one.
        var rolledBack = await repo.ReadAsync(id);

        var resource = await repo.RollbackAsync(id, versionId);
        if (resource == null)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} rollback failed\", 404);
        }

        if (rolledBack != null)
            await RemoveProvenanceAsync(ctx, id, rolledBack.lastUpdated);

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
        return FhirResult.Resource(ctx, resource.content);
    }

    // A rollback undoes a version completely, so the provenance that the write recorded for it
    // goes as well - hard, without a tombstone of its own, otherwise the undone version would
    // leave a trace after all. Server-written provenance targets carry no version, so the time
    // of the write is what tells the provenance of that version from the older ones.
    private static async "(handler-plain-task)" RemoveProvenanceAsync(HttpContext ctx, string id, DateTime since)
    {
        var provenanceRepo = ctx.RequestServices.GetRequiredService<""IProvenanceRepository>();
        var query = new QueryCollection(new Dictionary<""string, StringValues>
        {
            [\"target\"] = $\""(h-in rn)"/{id}\"
        });
        var (provenances, _) = await provenanceRepo.SearchAsync(query, 1000, 0);
        foreach (var provenance in provenances)
        {
            if (provenance.lastUpdated < since) continue;
            var targets = JsonNode.Parse(provenance.content)?[\"target\"]?.AsArray();
            if (targets == null) continue;
            foreach (var target in targets)
            {
                if (target?[\"reference\"]?.GetValue<""string>() != $\""(h-in rn)"/{id}\") continue;
                await provenanceRepo.HardDeleteAsync(provenance.id);
                break;
            }
        }
    }

"))

; ===================================================================
; Section: Undelete
; ===================================================================

(define (handler-undelete rn)
    ($
"    public static async Task<""IResult> Undelete(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        var resource = await repo.UndeleteAsync(id);
        if (resource == null)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found or not deleted\", 404);
        }

        ctx.Response.Headers[\"ETag\"] = $\"W/\\\"{resource.versionId}\\\"\";
        ctx.Response.Headers[\"Last-Modified\"] = resource.lastUpdated.ToUniversalTime().ToString(\"R\");
        return FhirResult.Resource(ctx, resource.content);
    }

"))

; ===================================================================
; Section: Search
; ===================================================================

(define (handler-search rn)
    ($
"    public static async Task<""IResult> Search(HttpContext ctx, "(h-repo rn)" repo)
    {
"
        (handler-search-body rn "ctx.Request.Query")))

; ===================================================================
; Section: PostSearch
; ===================================================================

(define (handler-post-search rn)
    ($
"    public static async Task<""IResult> PostSearch(HttpContext ctx, "(h-repo rn)" repo)
    {
        var form = await ctx.Request.ReadFormAsync();
        var queryDict = new Dictionary<""string, Microsoft.Extensions.Primitives.StringValues>();
        foreach (var key in form.Keys)
            queryDict[key] = form[key];
        foreach (var q in ctx.Request.Query)
            if (!queryDict.ContainsKey(q.Key))
                queryDict[q.Key] = q.Value;
        var query = new QueryCollection(queryDict);

"
        (handler-search-body rn "query")))

; --- Shared search body ---

(define (handler-search-body rn query-source)
    ($
"        var count = 20;
        var offset = 0;

        if (" query-source ".TryGetValue(\"_count\", out var countStr) && int.TryParse(countStr, out var c))
            count = Math.Clamp(c, 0, 1000);
        if (" query-source ".TryGetValue(\"_offset\", out var offsetStr) && int.TryParse(offsetStr, out var o))
            offset = o;

        var summary = " query-source ".TryGetValue(\"_summary\", out var summaryStr) ? (string?)summaryStr : null;
        if (summary == \"count\")
            count = 0;

        var totalParam = " query-source ".TryGetValue(\"_total\", out var totalStr) ? (string?)totalStr : null;

        // Resolve forward chains, then reverse chains (_has); what they refuse is the request's fault
        IQueryCollection searchQuery;
        string[]? hasIds;
        try
        {
            searchQuery = await SearchHelper.ResolveForwardChainsAsync(ctx.RequestServices, "(h-name rn)", " query-source ");
            hasIds = await SearchHelper.ResolveReverseChainsAsync(ctx.RequestServices, " query-source ");
        }
        catch (ArgumentException ex) { return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400); }
        if (hasIds != null)
        {
            var modified = new Dictionary<""string, Microsoft.Extensions.Primitives.StringValues>();
            foreach (var p in searchQuery)
                modified[p.Key] = p.Value;
            if (hasIds.Length > 0)
                modified[\"_id\"] = new Microsoft.Extensions.Primitives.StringValues(string.Join(\",\", hasIds));
            else
                modified[\"_id\"] = new Microsoft.Extensions.Primitives.StringValues(\"__no_match__\");
            searchQuery = new QueryCollection(modified);
        }

        // Handle composite search params (code-value-quantity, etc.)
        var compositeParams = " query-source ".Where(p =>
            p.Key == \"code-value-quantity\" || p.Key == \"code-value-concept\" ||
            p.Key == \"component-code-value-quantity\" || p.Key == \"component-code-value-concept\").ToList();

        List<"(h-res rn)"> resources;
        int total;

        if (compositeParams.Count > 0)
        {
            // Extract code components into regular search params
            var compositeModified = new Dictionary<""string, Microsoft.Extensions.Primitives.StringValues>();
            foreach (var p in searchQuery)
            {
                if (p.Key == \"code-value-quantity\" || p.Key == \"code-value-concept\" ||
                    p.Key == \"component-code-value-quantity\" || p.Key == \"component-code-value-concept\")
                    continue;
                compositeModified[p.Key] = p.Value;
            }

            foreach (var cp in compositeParams)
            {
                foreach (var v in cp.Value)
                {
                    if (v == null) continue;
                    var (codeParamName, codeValue) = CompositeSearchHelper.ExtractCodeComponent(cp.Key, v);
                    if (codeParamName != null && codeValue != null)
                        compositeModified[codeParamName] = new Microsoft.Extensions.Primitives.StringValues(codeValue);
                }
            }

            var compositeQuery = new QueryCollection(compositeModified);
            try { var (allResources, _) = await repo.SearchAsync(compositeQuery, 10000, 0);

            // Filter in memory by composite conditions
            var filtered = allResources.Where(r =>
                compositeParams.All(cp =>
                    cp.Value.All(v => v != null && CompositeSearchHelper.MatchesComposite(JsonNode.Parse(r.content)!, cp.Key, v)))).ToList();

            total = filtered.Count;
            resources = filtered.Skip(offset).Take(count).ToList(); }
            catch (ArgumentException ex) { return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400); }
        }
        else
        {
            try { (resources, total) = await repo.SearchAsync(searchQuery, count, offset); }
            catch (ArgumentException ex) { return FhirResult.Error(ctx, \"error\", \"invalid\", ex.Message, 400); }
        }

        var baseUrl = GetBaseUrl(ctx);
        var entries = resources.Select(r =>
            FhirResponse.SearchEntry(JsonNode.Parse(r.content)!, $\"{baseUrl}/"(h-in rn)"/{r.id}\")).ToList();

        // Resolve includes
        var primaryContents = resources.Select(r => (JsonNode)JsonNode.Parse(r.content)!).ToList();
        var primaryIds = resources.Select(r => r.id).ToList();
        var includeEntries = await SearchHelper.ResolveIncludesAsync(
            ctx.RequestServices, "(h-name rn)", primaryContents, primaryIds, baseUrl, " query-source ");
        entries.AddRange(includeEntries);

        // Handle _summary: add SUBSETTED tag
        if (summary == \"true\" || summary == \"text\" || summary == \"data\")
        {
            foreach (var entry in entries)
            {
                var res = entry[\"resource\"]?.AsObject();
                if (res == null) continue;
                if (summary == \"text\")
                {
                    var keysToKeep = new HashSet<""string> { \"resourceType\", \"id\", \"meta\", \"text\" };
                    var keysToRemove = res.Select(p => p.Key).Where(k => !keysToKeep.Contains(k)).ToList();
                    foreach (var key in keysToRemove) res.Remove(key);
                }
                else if (summary == \"data\")
                {
                    res.Remove(\"text\");
                }
                res[\"meta\"] ??= new JsonObject();
                var meta = res[\"meta\"]!.AsObject();
                meta[\"tag\"] = new JsonArray(new JsonObject
                {
                    [\"system\"] = \"http://terminology.hl7.org/CodeSystem/v3-ObservationValue\",
                    [\"code\"] = \"SUBSETTED\"
                });
            }
        }

        // Handle _elements parameter: sparse fieldsets
        var elements = " query-source ".TryGetValue(\"_elements\", out var elementsStr) ? (string?)elementsStr : null;
        if (elements != null)
        {
            var allowed = new HashSet<""string>(elements.Split(',')) { \"resourceType\", \"id\", \"meta\" };
            foreach (var entry in entries)
            {
                var res = entry[\"resource\"]?.AsObject();
                if (res == null) continue;
                var keys = res.Select(p => p.Key).ToList();
                foreach (var key in keys)
                    if (!allowed.Contains(key))
                        res.Remove(key);
                res[\"meta\"] ??= new JsonObject();
                var meta = res[\"meta\"]!.AsObject();
                meta[\"tag\"] = new JsonArray(new JsonObject
                {
                    [\"system\"] = \"http://terminology.hl7.org/CodeSystem/v3-ObservationValue\",
                    [\"code\"] = \"SUBSETTED\"
                });
            }
        }

        var selfUrl = $\"{baseUrl}/"(h-in rn)"{ctx.Request.QueryString}\";
        var bundleTotal = totalParam == \"none\" ? -1 : total;
        var bundle = FhirResponse.SearchBundle(entries, bundleTotal, selfUrl, count, offset);

        return FhirResult.Bundle(ctx, bundle);
    }

"))

; ===================================================================
; Section: InstanceHistory
; ===================================================================

(define (handler-instance-history rn)
    ($
"    public static async Task<""IResult> InstanceHistory(string id, HttpContext ctx, "(h-repo rn)" repo)
    {
        var count = 20;
        var offset = 0;
        if (ctx.Request.Query.TryGetValue(\"_count\", out var countStr) && int.TryParse(countStr, out var c))
            count = Math.Min(c, 1000);
        if (ctx.Request.Query.TryGetValue(\"_offset\", out var offsetStr) && int.TryParse(offsetStr, out var o))
            offset = o;

        var (rows, total) = await repo.InstanceHistoryAsync(id, count, offset);
        if (rows.Count == 0)
        {
            return FhirResult.Error(ctx, \"error\", \"not-found\", $\""(h-in rn)"/{id} not found\", 404);
        }

        var baseUrl = GetBaseUrl(ctx);
        var entries = rows.Select(r =>
        {
            // A DELETE row stands for two entries: the tombstone the query synthesizes as
            // TOMBSTONE, and the row itself, which still carries the state the resource was
            // in when it was deleted and is rendered as the version it is.
            if (r.historyOperation == \"TOMBSTONE\")
            {
                return FhirResponse.HistoryDeleteEntry(
                    $\"{baseUrl}/"(h-in rn)"/{r.id}\",
                    \"DELETE\", $\""(h-in rn)"/{r.id}\",
                    \"204\",
                    r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
            }
            return FhirResponse.HistoryEntry(
                JsonNode.Parse(r.content)!,
                $\"{baseUrl}/"(h-in rn)"/{r.id}\",
                r.historyOperation == \"CURRENT\" ? \"PUT\" : \"PUT\",
                $\""(h-in rn)"/{r.id}\",
                \"200\",
                r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
        }).ToList();

        var selfUrl = $\"{baseUrl}/"(h-in rn)"/{id}/_history{ctx.Request.QueryString}\";
        var bundle = FhirResponse.HistoryBundle(entries, total, selfUrl, count, offset);

        return FhirResult.Bundle(ctx, bundle);
    }

"))

; ===================================================================
; Section: TypeHistory
; ===================================================================

(define (handler-type-history rn)
    ($
"    public static async Task<""IResult> TypeHistory(HttpContext ctx, "(h-repo rn)" repo)
    {
        var count = 20;
        var offset = 0;
        if (ctx.Request.Query.TryGetValue(\"_count\", out var countStr) && int.TryParse(countStr, out var c))
            count = Math.Min(c, 1000);
        if (ctx.Request.Query.TryGetValue(\"_offset\", out var offsetStr) && int.TryParse(offsetStr, out var o))
            offset = o;

        var (rows, total) = await repo.TypeHistoryAsync(count, offset);

        var baseUrl = GetBaseUrl(ctx);
        var entries = rows.Select(r =>
        {
            // A DELETE row stands for two entries: the tombstone the query synthesizes as
            // TOMBSTONE, and the row itself, which still carries the state the resource was
            // in when it was deleted and is rendered as the version it is.
            if (r.historyOperation == \"TOMBSTONE\")
            {
                return FhirResponse.HistoryDeleteEntry(
                    $\"{baseUrl}/"(h-in rn)"/{r.id}\",
                    \"DELETE\", $\""(h-in rn)"/{r.id}\",
                    \"204\",
                    r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
            }
            return FhirResponse.HistoryEntry(
                JsonNode.Parse(r.content)!,
                $\"{baseUrl}/"(h-in rn)"/{r.id}\",
                \"PUT\",
                $\""(h-in rn)"/{r.id}\",
                \"200\",
                r.lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\"));
        }).ToList();

        var selfUrl = $\"{baseUrl}/"(h-in rn)"/_history{ctx.Request.QueryString}\";
        var bundle = FhirResponse.HistoryBundle(entries, total, selfUrl, count, offset);

        return FhirResult.Bundle(ctx, bundle);
    }

"))

; ===================================================================
; Section: Autocomplete
; ===================================================================

(define (handler-autocomplete rn)
    ($
"    public static async Task<""IResult> Autocomplete(HttpContext ctx, "(h-repo rn)" repo)
    {
        var query = ctx.Request.Query;
        var property = query[\"_autocomplete\"].FirstOrDefault();
        int? count = null;
        int? offset = null;
        if (query.TryGetValue(\"_count\", out var countStr) && int.TryParse(countStr, out var c))
            count = Math.Max(c, 0);
        if (query.TryGetValue(\"_offset\", out var offsetStr) && int.TryParse(offsetStr, out var o))
            offset = o;

        string? param = null;
        string? value = null;
        foreach (var q in query)
        {
            if (q.Key is \"_autocomplete\" or \"_count\" or \"_offset\")
                continue;
            param = q.Key;
            value = q.Value.FirstOrDefault();
            break;
        }

        if (param == null || value == null)
            return FhirResult.Error(ctx, \"error\", \"required\", \"A search parameter is required\", 400);

        var results = await repo.AutocompleteAsync(param, value, property ?? param, count, offset);
        return Results.Json(results);
    }

"))

; ===================================================================
; Section: CreateSearch / DropSearch
; ===================================================================

(define (handler-create-search rn)
    ($
"    public static async Task<""IResult> CreateSearch(string parameter, HttpContext ctx, "(h-repo rn)" repo)
    {
        await repo.CreateSearchAsync(parameter);
        return FhirResult.Error(ctx, \"information\", \"informational\", $\"Search table '{parameter}' created for "(h-in rn)"\", 200);
    }

"))

(define (handler-drop-search rn)
    ($
"    public static async Task<""IResult> DropSearch(string parameter, HttpContext ctx, "(h-repo rn)" repo)
    {
        await repo.DropSearchAsync(parameter);
        return FhirResult.Error(ctx, \"information\", \"informational\", $\"Search table '{parameter}' dropped for "(h-in rn)"\", 200);
    }

"))

; ===================================================================
; Section: Purge
; ===================================================================

; 'before' keeps a grace period: only resources deleted before that instant are purged,
; everything younger stays recoverable via $undelete. Without it the whole set goes, which
; is what the admin UI's button does.
;
; The same handler answers GET and POST. POST is what a client uses when it relies on the
; bound - a server generated before this parameter existed has only the GET route, so a POST
; comes back 405 instead of silently ignoring 'before' and purging everything.
(define (handler-purge rn)
    ($
"    public static async Task<""IResult> Purge(HttpContext ctx, "(h-repo rn)" repo)
    {
        DateTime? before = null;
        if (ctx.Request.Query.TryGetValue(\"before\", out var beforeValue) && !string.IsNullOrWhiteSpace(beforeValue))
        {
            if (!DateTime.TryParse(beforeValue, System.Globalization.CultureInfo.InvariantCulture,
                    System.Globalization.DateTimeStyles.AdjustToUniversal | System.Globalization.DateTimeStyles.AssumeUniversal,
                    out var parsed))
                return FhirResult.Error(ctx, \"error\", \"invalid\", \"Query parameter 'before' must be an ISO 8601 instant\", 400);
            before = DateTime.SpecifyKind(parsed, DateTimeKind.Utc);
        }

        var count = await repo.PurgeAsync(before);
        var scope = before == null ? \"\" : $\" deleted before {before:o}\";
        return FhirResult.Error(ctx, \"information\", \"informational\",
            $\"Purge completed for "(h-in rn)": {count} resource(s){scope} removed\", 200);
    }

"))

; ===================================================================
; Section: ImportVersion
; ===================================================================

(define (handler-import rn)
    ($
"    public static async Task<""IResult> ImportVersion(string id, HttpContext ctx, "(h-repo rn)" repo)
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

        try
        {
            var existing = await repo.ReadAsync(id);
            if (existing == null)
                await repo.CreateAsync(content, id);
            else
                await repo.UpdateAsync(id, content);
        }
        catch (System.Data.Common.DbException) { throw; }
        catch (System.Net.Sockets.SocketException) { throw; }
        catch (TimeoutException) { throw; }
        catch (Exception ex)
        {
            return FhirResult.Error(ctx, \"error\", \"exception\", ex.Message, 500);
        }

        return Results.Ok();
    }

"))

; ===================================================================
; Section: GetBaseUrl helper + close class
; ===================================================================

(define (handler-get-base-url)
    ($
"    private static async Task<""IResult> HandlePrefer(HttpContext ctx, byte[] content, int statusCode)
    {
        // Handle X-Provenance header
        var xProv = ctx.Request.Headers[\"X-Provenance\"].FirstOrDefault();
        if (xProv != null)
        {
            try
            {
                var provContent = JsonNode.Parse(xProv);
                var resNode = JsonNode.Parse(content);
                if (provContent != null && resNode != null)
                {
                    var resType = resNode[\"resourceType\"]?.GetValue<""string>();
                    var resId = resNode[\"id\"]?.GetValue<""string>();
                    if (resType != null && resId != null)
                    {
                        provContent[\"resourceType\"] = \"Provenance\";
                        provContent[\"target\"] = new JsonArray(new JsonObject { [\"reference\"] = $\"{resType}/{resId}\" });
                        var provRepo = ctx.RequestServices.GetRequiredService<""IProvenanceRepository>();
                        await provRepo.CreateAsync(provContent);
                    }
                }
            }
            catch { /* ignore malformed provenance */ }
        }

        // Handle Prefer header
        var prefer = ctx.Request.Headers[\"Prefer\"].FirstOrDefault();
        if (prefer == \"return=minimal\")
        {
            ctx.Response.Headers[\"Content-Length\"] = \"0\";
            return Results.StatusCode(statusCode);
        }
        if (prefer == \"return=OperationOutcome\")
        {
            var oo = FhirResponse.OperationOutcome(\"information\", \"informational\", \"Operation successful\");
            return FhirResult.Resource(ctx, JsonSerializer.SerializeToUtf8Bytes(oo), statusCode);
        }
        return FhirResult.Resource(ctx, content, statusCode);
    }

    private static string GetBaseUrl(HttpContext ctx)
    {
        return $\"{ctx.Request.Scheme}://{ctx.Request.Host}/" (route-prefix) "\";
    }
}
"))

; ===================================================================
; Per-resource Program.cs route registrations (shared by all projects)
; ===================================================================
; Emits the MapXxx endpoint registrations for one resource. When `with-auth?`
; is #t, .RequireAuthorization() is chained on every route except the 405
; stub for POST /{id} (no business logic to gate); /admin/createsearch,
; /admin/dropsearch, /purgeschema additionally require the "admin" role.
; When `with-auth?` is #f (e.g. server sits behind a REST facade and is not
; reachable from the outside), no authorization is chained — including on
; the admin/purge routes.
; Rollback and $undelete are POST (state-changing); everything else follows
; FHIR REST verb conventions.
(define (resource-routes resource with-auth?)
    (let ((rn (name-of resource))
          (has-history (history? resource))
          (auth (if with-auth? ".RequireAuthorization()" ""))
          (admin-auth (if with-auth? ".RequireAuthorization(\"admin\")" "")))
        ($"// " rn " routes
app.MapPost(\"/" (route-prefix) "/" rn "/_search\", " rn "Handlers.PostSearch)" auth ";
app.MapPost(\"/" (route-prefix) "/" rn "\", " rn "Handlers.Create)" auth ";
app.MapGet(\"/" (route-prefix) "/" rn "/{id}\", " rn "Handlers.Read)" auth ";
app.MapPut(\"/" (route-prefix) "/" rn "/{id}\", " rn "Handlers.Update)" auth ";
app.MapPut(\"/" (route-prefix) "/" rn "\", " rn "Handlers.ConditionalUpdate)" auth ";
app.MapPatch(\"/" (route-prefix) "/" rn "/{id}\", " rn "Handlers.Patch)" auth ";
app.MapDelete(\"/" (route-prefix) "/" rn "/{id}\", " rn "Handlers.Delete)" auth ";
app.MapDelete(\"/" (route-prefix) "/" rn "\", " rn "Handlers.ConditionalDelete)" auth ";
app.MapGet(\"/" (route-prefix) "/" rn "\", " rn "Handlers.Search)" auth ";
app.MapGet(\"/" (route-prefix) "/" rn "/{id}/_history/{vid}\", " rn "Handlers.VersionRead)" auth ";
"
            (if has-history
                ($"app.MapGet(\"/" (route-prefix) "/" rn "/{id}/_history\", " rn "Handlers.InstanceHistory)" auth ";
app.MapGet(\"/" (route-prefix) "/" rn "/_history\", " rn "Handlers.TypeHistory)" auth ";
app.MapPost(\"/" (route-prefix) "/" rn "/{id}/_rollback/{vid}\", " rn "Handlers.Rollback)" auth ";
app.MapPost(\"/" (route-prefix) "/" rn "/{id}/$undelete\", " rn "Handlers.Undelete)" auth ";
")
                "")
"app.MapPut(\"/" (route-prefix) "/" rn "/_history/{id}\", " rn "Handlers.ImportVersion)" auth ";
app.MapPost(\"/" (route-prefix) "/" rn "/{id}\", (string id, HttpContext ctx) =>
    FhirResult.Error(ctx, \"error\", \"invalid\", \"POST to instance endpoint is not allowed\", 405));

// Autocomplete
app.MapGet(\"/" (route-prefix) "/" rn "/$autocomplete\", " rn "Handlers.Autocomplete)" auth ";

// Search table management
app.MapGet(\"/admin/createsearch/" rn "/{parameter}\", " rn "Handlers.CreateSearch)" admin-auth ";
app.MapGet(\"/admin/dropsearch/" rn "/{parameter}\", " rn "Handlers.DropSearch)" admin-auth ";

// Purge deleted resources from history. POST is the form that carries 'before'; see handler-purge.
app.MapGet(\"/purgeschema/" rn "\", " rn "Handlers.Purge)" admin-auth ";
app.MapPost(\"/purgeschema/" rn "\", " rn "Handlers.Purge)" admin-auth ";

")))

