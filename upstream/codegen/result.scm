; FhirResult.cs generator — content negotiation + per-format result helpers.
; Auth-agnostic (no JWT or role-specific code), so it lives in the shared layer
; and is imported by every project's map.dsl. Every project reuses this single
; definition; bug fixes here apply to all.

(define (result)
    (file "FhirResult.cs" ($<![CDATA[
"using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace "(project-name)";

public static class FhirResult
{
    public static string ResolveFormat(HttpContext ctx)
    {
        // ?_format=... wins over the Accept header (explicit override).
        if (ctx.Request.Query.TryGetValue(\"_format\", out var qFormat))
        {
            var f = qFormat.ToString().ToLowerInvariant();
            if (f is \"xml\" or \"text/xml\" or \"application/xml\" or \"application/fhir+xml\")
                return \"xml\";
            if (f is \"html\" or \"text/html\")
                return \"html\";
            // \"json\" / \"application/json\" / \"application/fhir+json\" / unknown → fall through to default.
        }
        else
        {
            var accept = ctx.Request.Headers[\"Accept\"].ToString();
            // Explicit JSON request beats the HTML substring heuristic, so a
            // fetch() with Accept: application/json gets JSON even though the
            // bundle list page that initiated it was HTML.
            if (accept.Contains(\"application/fhir+json\") || accept.Contains(\"application/json\"))
                return \"json\";
            if (accept.Contains(\"application/fhir+xml\"))
                return \"xml\";
            // Browsers send text/html,application/xhtml+xml,...,*/*;q=0.8 → html.
            if (accept.Contains(\"html\"))
                return \"html\";
        }
        return \"json\";
    }

    public static IResult Resource(HttpContext ctx, byte[] content, int statusCode = 200)
    {
        var format = ResolveFormat(ctx);
        // Vary: Accept tells caches (browser + intermediaries) to key on Accept
        // so the JSON body served to an Accept: application/json fetch isn't
        // replayed to a later browser navigation that wants HTML for the same URL.
        ctx.Response.Headers[\"Vary\"] = \"Accept\";
        return format switch
        {
            \"xml\" => Results.Text(FhirXmlSerializer.ToXml(JsonNode.Parse(content)!),
                \"application/fhir+xml\", statusCode: statusCode),
            \"html\" => RenderHtml(content, statusCode),
            _ => new FhirJsonResult(content, statusCode)
        };
    }

    public static IResult Bundle(HttpContext ctx, JsonObject bundle)
    {
        return Resource(ctx, JsonSerializer.SerializeToUtf8Bytes(bundle));
    }

    public static IResult Error(HttpContext ctx, string severity, string code,
        string diagnostics, int statusCode)
    {
        var outcome = FhirResponse.OperationOutcome(severity, code, diagnostics);
        return Resource(ctx, JsonSerializer.SerializeToUtf8Bytes(outcome), statusCode);
    }

    public static IResult ValidationErrors(HttpContext ctx, List<""string> errors)
    {
        var outcome = FhirResponse.OperationOutcome(errors);
        return Resource(ctx, JsonSerializer.SerializeToUtf8Bytes(outcome), 400);
    }

    private static IResult RenderHtml(byte[] content, int statusCode)
    {
        using var doc = JsonDocument.Parse(content);
        var resourceType = doc.RootElement.TryGetProperty(\"resourceType\", out var rt) ? rt.GetString() ?? \"Resource\" : \"Resource\";
        var json = Encoding.UTF8.GetString(content).Replace(\"-->\", \"-/->\");

        if (resourceType == \"Bundle\")
        {
            var html = $@\"<!DOCTYPE html>
<html>
  <head>
    <title>Bundle</title>
    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>
    <script src='/js/Elements.js'></script>
    <script src='/js/Bundle.js'></script>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'/>
    <link rel='stylesheet' href='/css/fhir.css'/>
    <link rel='stylesheet' href='/css/fhir-pub.css'/>
    <link rel='stylesheet' href='/css/bootstrap.css'/>
    <link rel='stylesheet' href='/css/bootstrap-fhir.css'/>
    <link rel='stylesheet' href='/css/project.css'/>
    <link rel='stylesheet' href='/css/jquery-ui.css'/>
  </head>
  <body>
    <div id='json'><!--{json}--></div>
    <script>buildBundle();</script>
  </body>
</html>\";
            return Results.Text(html, \"text/html\", statusCode: statusCode);
        }

        var instanceHtml = $@\"<!DOCTYPE html>
<html>
  <head>
    <title>{resourceType}</title>
    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>
    <script src='/js/Elements.js'></script>
    <script src='/js/{resourceType}.js'></script>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'/>
    <link rel='stylesheet' href='/css/fhir.css'/>
    <link rel='stylesheet' href='/css/fhir-pub.css'/>
    <link rel='stylesheet' href='/css/bootstrap.css'/>
    <link rel='stylesheet' href='/css/bootstrap-fhir.css'/>
    <link rel='stylesheet' href='/css/project.css'/>
    <link rel='stylesheet' href='/css/jquery-ui.css'/>
  </head>
  <body id='body'>
    <div id='json'><!--{json}--></div>
    <script>buildResource();</script>
  </body>
</html>\";
        return Results.Text(instanceHtml, \"text/html\", statusCode: statusCode);
    }

    private class FhirJsonResult : IResult
    {
        private readonly byte[] _content;
        private readonly int _statusCode;
        public FhirJsonResult(byte[] content, int statusCode)
        { _content = content; _statusCode = statusCode; }
        public async Task ExecuteAsync(HttpContext httpContext)
        {
            httpContext.Response.StatusCode = _statusCode;
            httpContext.Response.ContentType = \"application/fhir+json\";
            httpContext.Response.ContentLength = _content.Length;
            await httpContext.Response.Body.WriteAsync(_content);
        }
    }
}
"]]>)))
