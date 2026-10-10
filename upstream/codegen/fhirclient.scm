; fhirclient.scm — Type-safe FHIR client library generator
; Generates client/{ProjectName}.Client.csproj and supporting files
; Called from (element resources ...) context

; --- Search param type mapping ---

(define (search-param-class search-type)
    (case search-type
        (("date") "DateParam")
        (("token") "TokenParam")
        (("string") "StringParam")
        (("reference") "ReferenceParam")
        (("quantity") "QuantityParam")
        (("uri") "UriParam")
        (("composite") "CompositeParam")
        (else #f)))

; --- Per-resource search descriptor file ---

(define (fhir-client-search-descriptor resource)
    (let ((rn (name-of resource)))
        (file ($ rn "Search.cs") ($
"namespace " (project-name) ".Client;

public class "rn"Search
{
    static "rn"Search() { SearchMeta<"rn"Search>.ResourceType = \""rn"\"; }
"       (for (children (select-children "searches" resource))
            (lambda (search)
                (let ((type (% "type" search))
                      (query (search-query search))
                      (sname (% "name" search)))
                    (let ((cls (search-param-class type)))
                        (if cls
                            (case type
                                (("reference")
                                    ($"    public " cls " " query " => new(\"" sname "\", \""rn"\");
"))
                                (else
                                    ($"    public " cls " " query " => new(\"" sname "\");
")))
                            "")))))
"}"))))

; --- All search descriptors ---

(define (fhir-client-all-search-descriptors)
    (for-nodes (active-resources-with-searches) fhir-client-search-descriptor))

; --- Fire.Client.csproj ---

(define (fhir-client-csproj)
    (file ($ (project-name) ".Client.csproj") ($
<![CDATA["<Project Sdk=\"Microsoft.NET.Sdk\">

  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>

  <ItemGroup>
    <ProjectReference Include=\""(model-ref)"/"(project-name)".Model.csproj\" />
  </ItemGroup>

</Project>
"]]>)))

; --- SearchParams.cs ---

(define (fhir-client-search-params)
    (file "SearchParams.cs" ($
"namespace " (project-name) ".Client;

public readonly record struct SearchCriterion(string Name, string Value);

public static class SearchMeta<""T> where T : new()
{
    public static string ResourceType { get; internal set; } = \"\";
}

public class DateParam(string name)
{
    internal string Name { get; } = name;
    public SearchCriterion Eq(string value) => new(Name, value);
    public SearchCriterion Ne(string value) => new(Name, \"ne\" + value);
    public SearchCriterion Sa(string value) => new(Name, \"sa\" + value);
    public SearchCriterion Eb(string value) => new(Name, \"eb\" + value);
    public SearchCriterion Ap(string value) => new(Name, \"ap\" + value);
    public static SearchCriterion operator >(DateParam p, string v) => new(p.Name, \"gt\" + v);
    public static SearchCriterion operator <(DateParam p, string v) => new(p.Name, \"lt\" + v);
    public static SearchCriterion operator >=(DateParam p, string v) => new(p.Name, \"ge\" + v);
    public static SearchCriterion operator <=(DateParam p, string v) => new(p.Name, \"le\" + v);
    public static SearchCriterion operator ==(DateParam p, string v) => new(p.Name, v);
    public static SearchCriterion operator !=(DateParam p, string v) => new(p.Name, \"ne\" + v);
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}

public class TokenParam(string name)
{
    internal string Name { get; } = name;
    public SearchCriterion Not(string value) => new(Name + \":not\", value);
    public static SearchCriterion operator ==(TokenParam p, string v) => new(p.Name, v);
    public static SearchCriterion operator !=(TokenParam p, string v) => new(p.Name + \":not\", v);
    public static SearchCriterion operator ==(TokenParam p, (string system, string code) v) => new(p.Name, v.system + \"|\" + v.code);
    public static SearchCriterion operator !=(TokenParam p, (string system, string code) v) => new(p.Name + \":not\", v.system + \"|\" + v.code);
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}

public class StringParam(string name)
{
    internal string Name { get; } = name;
    public SearchCriterion Exact(string value) => new(Name + \":exact\", value);
    public SearchCriterion Contains(string value) => new(Name + \":contains\", value);
    public static SearchCriterion operator ==(StringParam p, string v) => new(p.Name, v);
    public static SearchCriterion operator !=(StringParam p, string v) => throw new NotSupportedException();
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}

public class ReferenceParam(string name, string resourceType)
{
    internal string ParamName { get; } = name;
    internal string ResourceType { get; } = resourceType;
    public static SearchCriterion operator ==(ReferenceParam p, string v) => new(p.ParamName, v);
    public static SearchCriterion operator !=(ReferenceParam p, string v) => throw new NotSupportedException();
    public SearchCriterion Chain(string param, string value) => new(ParamName + \".\" + param, value);
    public SearchCriterion Chain<""TSearch>(Func<""TSearch, SearchCriterion> predicate) where TSearch : new()
    {
        var criterion = predicate(new TSearch());
        var targetType = SearchMeta<""TSearch>.ResourceType;
        return new(ParamName + \":\" + targetType + \".\" + criterion.Name, criterion.Value);
    }
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => ParamName.GetHashCode();
}

public class QuantityParam(string name)
{
    internal string Name { get; } = name;
    public static SearchCriterion operator >(QuantityParam p, string v) => new(p.Name, \"gt\" + v);
    public static SearchCriterion operator <(QuantityParam p, string v) => new(p.Name, \"lt\" + v);
    public static SearchCriterion operator >=(QuantityParam p, string v) => new(p.Name, \"ge\" + v);
    public static SearchCriterion operator <=(QuantityParam p, string v) => new(p.Name, \"le\" + v);
    public static SearchCriterion operator ==(QuantityParam p, string v) => new(p.Name, v);
    public static SearchCriterion operator !=(QuantityParam p, string v) => new(p.Name, \"ne\" + v);
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}

public class UriParam(string name)
{
    internal string Name { get; } = name;
    public SearchCriterion Above(string value) => new(Name + \":above\", value);
    public SearchCriterion Below(string value) => new(Name + \":below\", value);
    public static SearchCriterion operator ==(UriParam p, string v) => new(p.Name, v);
    public static SearchCriterion operator !=(UriParam p, string v) => throw new NotSupportedException();
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}

public class CompositeParam(string name)
{
    internal string Name { get; } = name;
    public static SearchCriterion operator ==(CompositeParam p, (string, string) v) => new(p.Name, v.Item1 + \"$\" + v.Item2);
    public static SearchCriterion operator !=(CompositeParam p, (string, string) v) => throw new NotSupportedException();
    public override bool Equals(object? obj) => ReferenceEquals(this, obj);
    public override int GetHashCode() => Name.GetHashCode();
}
")))

; --- SearchQuery.cs ---

(define (fhir-client-search-query)
    (file "SearchQuery.cs" ($
"using System.Net.Http.Json;
using System.Text.Json;
using " (project-name) ".Model;

namespace " (project-name) ".Client;

public class SearchQuery<""TSearch> where TSearch : new()
{
    private readonly FhirClient _client;
    private readonly string _resourceType;
    private readonly List<""SearchCriterion> _criteria = new();
    private readonly List<""KeyValuePair<""string, string>> _params = new();

    internal SearchQuery(FhirClient client, string resourceType)
    {
        _client = client;
        _resourceType = resourceType;
    }

    public SearchQuery<""TSearch> Where(Func<""TSearch, SearchCriterion> predicate)
    {
        _criteria.Add(predicate(new TSearch()));
        return this;
    }

    public SearchQuery<""TSearch> Param(string name, string value)
    {
        _params.Add(new(name, value));
        return this;
    }

    public SearchQuery<""TSearch> Count(int count) => Param(\"_count\", count.ToString());
    public SearchQuery<""TSearch> Sort(string value) => Param(\"_sort\", value);
    public SearchQuery<""TSearch> Offset(int offset) => Param(\"_offset\", offset.ToString());

    public SearchQuery<""TSearch> Include(Func<""TSearch, ReferenceParam> selector)
    {
        var param = selector(new TSearch());
        return Param(\"_include\", _resourceType + \":\" + param.ParamName);
    }

    public SearchQuery<""TSearch> RevInclude<""TOther>(Func<""TOther, ReferenceParam> selector) where TOther : new()
    {
        var param = selector(new TOther());
        return Param(\"_revinclude\", param.ResourceType + \":\" + param.ParamName);
    }

    public SearchQuery<""TSearch> Include(string value) => Param(\"_include\", value);
    public SearchQuery<""TSearch> RevInclude(string value) => Param(\"_revinclude\", value);

    public async Task<""Bundle> ExecuteAsync()
    {
        var query = BuildQueryString();
        var url = string.IsNullOrEmpty(query) ? _resourceType : $\"{_resourceType}?{query}\";
        var response = await _client.Http.GetAsync(url);
        await _client.EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_client.JsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""List<""string>> AutocompleteAsync(string property)
    {
        var query = BuildQueryString();
        var prop = $\"_autocomplete={Uri.EscapeDataString(property)}\";
        var url = string.IsNullOrEmpty(query)
            ? $\"{_resourceType}/$autocomplete?{prop}\"
            : $\"{_resourceType}/$autocomplete?{query}&{prop}\";
        var response = await _client.Http.GetAsync(url);
        await _client.EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""List<""string>>()
            ?? new List<""string>();
    }

    internal string BuildQueryString()
    {
        var parts = new List<""string>();
        foreach (var c in _criteria)
            parts.Add($\"{Uri.EscapeDataString(c.Name)}={Uri.EscapeDataString(c.Value)}\");
        foreach (var p in _params)
            parts.Add($\"{Uri.EscapeDataString(p.Key)}={Uri.EscapeDataString(p.Value)}\");
        return string.Join(\"&\", parts);
    }
}
")))

; --- FhirClient.cs ---

(define (fhir-client-cs)
    (file "FhirClient.cs" ($
"using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text.Json;
using " (project-name) ".Model;

namespace " (project-name) ".Client;

public class FhirClient
{
    private readonly HttpClient _http;
    private readonly JwtRefreshHandler? _jwtHandler;
    private readonly JsonSerializerOptions _jsonOptions;

    public FhirClient(string baseUrl, bool retryOnTransient = false)
    {
        _jwtHandler = new JwtRefreshHandler(new HttpClientHandler());
        HttpMessageHandler handler = _jwtHandler;
        if (retryOnTransient)
            handler = new TransientRetryHandler(_jwtHandler);
        _http = new HttpClient(handler) { BaseAddress = new Uri(baseUrl.TrimEnd('/') + \"/\") };
        // The retry handler enforces a per-attempt timeout itself, so the client
        // must not impose its own (finite) ceiling across all retries.
        if (retryOnTransient)
            _http.Timeout = System.Threading.Timeout.InfiniteTimeSpan;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Converters = { new ResourceJsonConverter() }
        };
    }

    public FhirClient(HttpClient http)
    {
        _http = http;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Converters = { new ResourceJsonConverter() }
        };
    }

    public HttpClient Http => _http;
    internal JsonSerializerOptions JsonOptions => _jsonOptions;

    public async Task LoginAsync(string username, string password)
    {
        var baseUri = _http.BaseAddress
            ?? throw new InvalidOperationException(\"FhirClient has no BaseAddress; cannot resolve /auth/login\");
        var authUri = new Uri(baseUri, \"../auth/login\");
        if (_jwtHandler != null)
        {
            // The handler keeps the credentials and re-logins when the token expires,
            // so long-running workers don't go dark after Jwt.ExpirationHours.
            await _jwtHandler.LoginAsync(authUri, username, password);
            return;
        }
        var response = await _http.PostAsJsonAsync(authUri, new { username, password });
        if (!response.IsSuccessStatusCode)
            throw new HttpRequestException($\"Login failed: {(int)response.StatusCode} {response.ReasonPhrase}\");
        var result = await response.Content.ReadFromJsonAsync<""LoginResponse>()
            ?? throw new HttpRequestException(\"Login response was empty\");
        if (string.IsNullOrEmpty(result.token))
            throw new HttpRequestException(\"Login response had no token\");
        _http.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(\"Bearer\", result.token);
    }

    private sealed record LoginResponse(string token, string expiresAt);

    public async Task<""T> ReadAsync<""T>(string id) where T : Resource
    {
        var type = typeof(T).Name;
        var response = await _http.GetAsync($\"{type}/{id}\");
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""T> CreateAsync<""T>(T resource) where T : Resource
    {
        var type = typeof(T).Name;
        EnsureResourceType(resource);
        var response = await _http.PostAsJsonAsync(type, resource, _jsonOptions);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""T> UpdateAsync<""T>(T resource) where T : Resource
    {
        var type = typeof(T).Name;
        EnsureResourceType(resource);
        var response = await _http.PutAsJsonAsync($\"{type}/{resource.id}\", resource, _jsonOptions);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task DeleteAsync<""T>(string id) where T : Resource
    {
        var type = typeof(T).Name;
        var response = await _http.DeleteAsync($\"{type}/{id}\");
        await EnsureSuccess(response);
    }
"    (for (active-resources-with-searches)
        (lambda (resource)
            (let ((rn (name-of resource)))
                ($"
    public SearchQuery<"rn"Search> "rn"() => new(this, \""rn"\");"))))
"

    public async Task<""T> VReadAsync<""T>(string id, string versionId) where T : Resource
    {
        var type = typeof(T).Name;
        var response = await _http.GetAsync($\"{type}/{id}/_history/{versionId}\");
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    /// <""summary>
    /// Formats a version id as the weak ETag the server expects in the If-Match header: the letter W,
    /// a slash, and the version id in double quotes. A method so that generated callers need no
    /// embedded quotes of their own.
    /// <""/summary>
    public static string WeakETag(string versionId) => $\"W/\\\"{versionId}\\\"\";

    public async Task<""T> UpdateAsync<""T>(T resource, string ifMatch) where T : Resource
    {
        var type = typeof(T).Name;
        EnsureResourceType(resource);
        var request = new HttpRequestMessage(HttpMethod.Put, $\"{type}/{resource.id}\");
        request.Content = JsonContent.Create(resource, options: _jsonOptions);
        request.Headers.TryAddWithoutValidation(\"If-Match\", ifMatch);
        var response = await _http.SendAsync(request);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""(T Resource, bool Created)> ConditionalCreateAsync<""T>(T resource, string searchParams) where T : Resource
    {
        var type = typeof(T).Name;
        EnsureResourceType(resource);
        var request = new HttpRequestMessage(HttpMethod.Post, type);
        request.Content = JsonContent.Create(resource, options: _jsonOptions);
        request.Headers.TryAddWithoutValidation(\"If-None-Exist\", searchParams);
        var response = await _http.SendAsync(request);
        await EnsureSuccess(response);
        var result = await response.Content.ReadFromJsonAsync<""T>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
        return (result, response.StatusCode == System.Net.HttpStatusCode.Created);
    }

    public async Task<""Bundle> HistoryAsync<""T>(string id, string? queryString = null) where T : Resource
    {
        var type = typeof(T).Name;
        var url = $\"{type}/{id}/_history\";
        if (queryString != null) url += \"?\" + queryString;
        var response = await _http.GetAsync(url);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""Bundle> TypeHistoryAsync<""T>(string? queryString = null) where T : Resource
    {
        var type = typeof(T).Name;
        var url = $\"{type}/_history\";
        if (queryString != null) url += \"?\" + queryString;
        var response = await _http.GetAsync(url);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""Bundle> SystemHistoryAsync(string? queryString = null)
    {
        var url = \"_history\";
        if (queryString != null) url += \"?\" + queryString;
        var response = await _http.GetAsync(url);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""Bundle> SubmitBundleAsync(Bundle bundle)
    {
        EnsureResourceType(bundle);
        var response = await _http.PostAsJsonAsync(\"\", bundle, _jsonOptions);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""Bundle?> NextPageAsync(Bundle bundle)
    {
        var nextLink = bundle.link?.FirstOrDefault(l => l.relation == \"next\");
        if (nextLink?.url == null) return null;
        var response = await _http.GetAsync(nextLink.url);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public static string? GetSnapshotId(Bundle bundle)
    {
        var nextLink = bundle.link?.FirstOrDefault(l => l.relation == \"next\");
        if (nextLink?.url == null) return null;
        var queryPart = nextLink.url.Contains('?') ? nextLink.url.Split('?')[1] : null;
        if (queryPart == null) return null;
        foreach (var param in queryPart.Split('&'))
        {
            var kv = param.Split('=', 2);
            if (kv.Length == 2 && kv[0] == \"id\") return kv[1];
        }
        return null;
    }

    public async Task<""Bundle> GetSnapshotPageAsync(string snapshotId, int start, int? count = null)
    {
        var url = $\"_snapshot?id={snapshotId}&""start={start}\";
        if (count != null) url += $\"&""count={count}\";
        var response = await _http.GetAsync(url);
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    public async Task<""Bundle> SearchAsync(string resourceType, string queryString)
    {
        var response = await _http.GetAsync($\"{resourceType}?{queryString}\");
        await EnsureSuccess(response);
        return await response.Content.ReadFromJsonAsync<""Bundle>(_jsonOptions)
            ?? throw new InvalidOperationException(\"Null response\");
    }

    private static void EnsureResourceType(Resource resource)
    {
        resource.resourceType ??= resource.GetType().Name;
        if (resource is Bundle bundle && bundle.entry != null)
            foreach (var entry in bundle.entry)
                if (entry.resource != null)
                    EnsureResourceType(entry.resource);
    }

    public async Task EnsureSuccess(HttpResponseMessage response)
    {
        if (!response.IsSuccessStatusCode)
        {
            var body = await response.Content.ReadAsStringAsync();
            try
            {
                var outcome = System.Text.Json.JsonSerializer.Deserialize<"(project-name)".Model.OperationOutcome>(body, _jsonOptions);
                if (outcome != null)
                    throw new "(project-name)".Model.OperationOutcomeException(outcome);
            }
            catch (System.Text.Json.JsonException) { }
            throw new FhirException(response.StatusCode, body);
        }
    }
}
")))

; --- ResourceJsonConverter.cs ---

(define (fhir-client-json-converter)
    (file "ResourceJsonConverter.cs" ($
"using System.Text.Json;
using System.Text.Json.Serialization;
using " (project-name) ".Model;

namespace " (project-name) ".Client;

public class ResourceJsonConverter : JsonConverter<""Resource>
{
    private JsonSerializerOptions? _inner;

    public override Resource? Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        _inner ??= StripConverter(options);
        using var doc = JsonDocument.ParseValue(ref reader);
        var raw = doc.RootElement.GetRawText();
        if (doc.RootElement.TryGetProperty(\"resourceType\", out var rt))
        {
            return rt.GetString() switch
            {
"
    (for (node-list-filter
            (lambda (r) (not (string=? "Resource" (name-of r))))
            (model-resources))
        (lambda (resource)
            (let ((rn (name-of resource)))
                ($"                \""rn"\" => JsonSerializer.Deserialize<"(project-name)".Model."rn">(raw, _inner),
"   ))))
"                _ => JsonSerializer.Deserialize<""Resource>(raw, _inner)
            };
        }
        return JsonSerializer.Deserialize<""Resource>(raw, _inner);
    }

    public override void Write(Utf8JsonWriter writer, Resource value, JsonSerializerOptions options)
    {
        _inner ??= StripConverter(options);
        value.resourceType ??= value.GetType().Name;
        JsonSerializer.Serialize(writer, value, value.GetType(), _inner);
    }

    private static JsonSerializerOptions StripConverter(JsonSerializerOptions options)
    {
        var copy = new JsonSerializerOptions(options);
        for (int i = copy.Converters.Count - 1; i >= 0; i--)
            if (copy.Converters[i] is ResourceJsonConverter)
                copy.Converters.RemoveAt(i);
        return copy;
    }
}
")))

; --- FhirException.cs ---

(define (fhir-client-exception)
    (file "FhirException.cs" ($
"using System.Net;

namespace " (project-name) ".Client;

public class FhirException(HttpStatusCode statusCode, string body) : Exception($\"FHIR request failed with status {statusCode}: {body}\")
{
    public HttpStatusCode StatusCode => statusCode;
    public string Body => body;
}
")))

; --- TransientRetryHandler.cs (opt-in resilience for unattended clients) ---

(define (fhir-client-retry-handler)
    (file "TransientRetryHandler.cs" ($
"using System.Net;

namespace " (project-name) ".Client;

/// <""summary>
/// DelegatingHandler that retries transient failures (connection refused,
/// timeouts, HTTP 408/429/502/503/504) with capped exponential backoff.
/// Opt-in via the FhirClient(baseUrl, retryOnTransient: true) constructor —
/// intended for unattended clients (e.g. the processing worker) that must ride
/// out a briefly unavailable server instead of failing the operation. The
/// per-attempt timeout is enforced here, so the owning HttpClient runs with an
/// infinite Timeout. Interactive services should NOT enable this; they should
/// surface 503 immediately.
/// </""summary>
public sealed class TransientRetryHandler : DelegatingHandler
{
    private readonly int _maxRetries;
    private readonly TimeSpan _baseDelay;
    private readonly TimeSpan _maxDelay;
    private readonly TimeSpan _perAttemptTimeout;

    public TransientRetryHandler(
        HttpMessageHandler innerHandler,
        int maxRetries = 8,
        TimeSpan? baseDelay = null,
        TimeSpan? maxDelay = null,
        TimeSpan? perAttemptTimeout = null)
        : base(innerHandler)
    {
        _maxRetries = maxRetries;
        _baseDelay = baseDelay ?? TimeSpan.FromSeconds(2);
        _maxDelay = maxDelay ?? TimeSpan.FromSeconds(60);
        _perAttemptTimeout = perAttemptTimeout ?? TimeSpan.FromSeconds(100);
    }

    private static bool IsTransient(HttpStatusCode status) =>
        status == HttpStatusCode.RequestTimeout
        || (int)status == 429
        || status == HttpStatusCode.BadGateway
        || status == HttpStatusCode.ServiceUnavailable
        || status == HttpStatusCode.GatewayTimeout;

    protected override async Task<""HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        // Buffer the body once so the request can be replayed on each attempt
        // (an HttpRequestMessage and its content can only be sent a single time).
        byte[]? body = null;
        if (request.Content is not null)
            body = await request.Content.ReadAsByteArrayAsync(cancellationToken);

        for (var attempt = 0; ; attempt++)
        {
            using var clone = CloneRequest(request, body);
            using var attemptCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            attemptCts.CancelAfter(_perAttemptTimeout);

            HttpResponseMessage? response = null;
            try
            {
                response = await base.SendAsync(clone, attemptCts.Token);
                if (!IsTransient(response.StatusCode) || attempt >= _maxRetries)
                    return response;
            }
            catch (Exception ex) when (
                _maxRetries > attempt
                && !cancellationToken.IsCancellationRequested
                && (ex is HttpRequestException || ex is TimeoutException || ex is OperationCanceledException))
            {
                // transient failure — fall through to backoff and retry
            }

            response?.Dispose();
            cancellationToken.ThrowIfCancellationRequested();

            var delayTicks = Math.Min(_baseDelay.Ticks * (long)Math.Pow(2, attempt), _maxDelay.Ticks);
            await Task.Delay(TimeSpan.FromTicks(delayTicks), cancellationToken);
        }
    }

    private static HttpRequestMessage CloneRequest(HttpRequestMessage request, byte[]? body)
    {
        var clone = new HttpRequestMessage(request.Method, request.RequestUri) { Version = request.Version };
        foreach (var header in request.Headers)
            clone.Headers.TryAddWithoutValidation(header.Key, header.Value);
        if (body is not null)
        {
            clone.Content = new ByteArrayContent(body);
            if (request.Content is not null)
                foreach (var header in request.Content.Headers)
                    clone.Content.Headers.TryAddWithoutValidation(header.Key, header.Value);
        }
        return clone;
    }
}
")))

; --- Entry point ---

(define (fhir-client-boilerplate)
    (sosofo-append
        (fhir-client-csproj)
        (fhir-client-search-params)
        (fhir-client-search-query)
        (fhir-client-cs)
        (fhir-client-json-converter)
        (fhir-client-retry-handler)
        (fhir-client-exception)))
