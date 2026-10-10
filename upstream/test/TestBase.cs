using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public abstract class TestBase : IAsyncLifetime
{
    private static readonly string BaseUrl =
        Environment.GetEnvironmentVariable("FHIR_BASE_URL") ?? "http://localhost:5080/fhir";
    private const string PurgeKey = "c522382c-8656-463e-8277-b913e4466f53";

    protected readonly FhirClient Client = new(BaseUrl);
    protected readonly HttpClient Http;
    protected readonly JsonSerializerOptions Json;
    private readonly List<(string Type, string Id)> _tracked = new();

    protected TestBase()
    {
        Http = new HttpClient { BaseAddress = new Uri(BaseUrl.TrimEnd('/') + "/") };
        Http.DefaultRequestHeaders.Accept.Add(
            new MediaTypeWithQualityHeaderValue("application/fhir+json"));
        Json = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            Converters = { new ResourceJsonConverter() }
        };
    }

    protected void Track(string resourceType, string id) => _tracked.Add((resourceType, id));

    protected async Task<Patient> CreatePatientAsync(Patient patient)
    {
        var result = await Client.CreateAsync(patient);
        Track("Patient", result.id!);
        return result;
    }

    protected async Task<Observation> CreateObservationAsync(Observation observation)
    {
        var result = await Client.CreateAsync(observation);
        Track("Observation", result.id!);
        return result;
    }

    protected async Task<Practitioner> CreatePractitionerAsync(Practitioner practitioner)
    {
        var result = await Client.CreateAsync(practitioner);
        Track("Practitioner", result.id!);
        return result;
    }

    protected static List<string> MatchIds(Bundle bundle)
    {
        return bundle.entry?
            .Where(e => e.search?.mode is null or "match")
            .Select(e => e.resource?.id!)
            .Where(id => id != null)
            .ToList() ?? [];
    }

    protected static int CountByType(Bundle bundle, string resourceType)
    {
        return bundle.entry?.Count(e => e.resource?.resourceType == resourceType) ?? 0;
    }

    protected static HashSet<string> ResourceTypes(Bundle bundle)
    {
        return bundle.entry?.Select(e => e.resource?.resourceType!).Where(t => t != null).ToHashSet()
            ?? [];
    }

    protected async Task<HttpResponseMessage> PostBundleAsync(object bundle)
    {
        var content = JsonContent.Create(bundle, options: Json);
        return await Http.PostAsync("", content);
    }

    protected async Task<HttpResponseMessage> RawGetAsync(string url)
    {
        return await Http.GetAsync(url);
    }

    protected async Task<HttpResponseMessage> RawPostAsync(string url, object body,
        Dictionary<string, string>? headers = null)
    {
        var request = new HttpRequestMessage(HttpMethod.Post, url);
        request.Content = JsonContent.Create(body, options: Json);
        if (headers != null)
            foreach (var (key, value) in headers)
                request.Headers.TryAddWithoutValidation(key, value);
        return await Http.SendAsync(request);
    }

    protected async Task<HttpResponseMessage> RawPutAsync(string url, object body,
        Dictionary<string, string>? headers = null)
    {
        var request = new HttpRequestMessage(HttpMethod.Put, url);
        request.Content = JsonContent.Create(body, options: Json);
        request.Content.Headers.ContentType = new MediaTypeHeaderValue("application/fhir+json");
        if (headers != null)
            foreach (var (key, value) in headers)
                request.Headers.TryAddWithoutValidation(key, value);
        return await Http.SendAsync(request);
    }

    protected async Task<HttpResponseMessage> RawPostStringAsync(string url, string body,
        string contentType = "application/fhir+json")
    {
        var content = new StringContent(body, Encoding.UTF8, contentType);
        return await Http.PostAsync(url, content);
    }

    protected async Task<T> DeserializeAsync<T>(HttpResponseMessage response)
    {
        var json = await response.Content.ReadAsStringAsync();
        return JsonSerializer.Deserialize<T>(json, Json)!;
    }

    public virtual Task InitializeAsync() => Task.CompletedTask;

    public virtual async Task DisposeAsync()
    {
        foreach (var (type, id) in _tracked.AsEnumerable().Reverse())
        {
            try { await Http.DeleteAsync($"{type}/{id}"); } catch { }
        }
        foreach (var type in _tracked.Select(x => x.Type).Distinct())
        {
            try { await Http.GetAsync($"purgeschema/{type}/{PurgeKey}"); } catch { }
        }
    }
}
