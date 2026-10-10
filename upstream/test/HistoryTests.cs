using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestResourceHistory : TestBase
{
    private Patient _patient = null!;
    private string _id = null!;

    public override async Task InitializeAsync()
    {
        _patient = await CreatePatientAsync(Gen.Patient(family: "HistV1"));

        _patient.name = [new HumanName { family = "HistV2", given = ["Updated"] }];
        _patient = await Client.UpdateAsync(_patient);

        _patient.name = [new HumanName { family = "HistV3", given = ["Final"] }];
        _patient = await Client.UpdateAsync(_patient);

        _id = _patient.id!;
    }

    [Fact]
    public async Task test_instance_history()
    {
        var bundle = await Client.HistoryAsync<Patient>(_id);
        Assert.Equal("history", bundle.type);
        Assert.NotNull(bundle.entry);
        Assert.True(bundle.entry!.Count >= 3, $"Expected >= 3 entries, got {bundle.entry.Count}");

        // Versions should be in descending order (newest first)
        var versions = bundle.entry
            .Select(e => int.Parse(e.resource!.meta!.versionId!))
            .ToList();
        for (var i = 0; i < versions.Count - 1; i++)
            Assert.True(versions[i] > versions[i + 1],
                $"Expected descending order: version {versions[i]} should be > {versions[i + 1]}");
    }

    [Fact]
    public async Task test_type_history()
    {
        var bundle = await Client.TypeHistoryAsync<Patient>();
        Assert.Equal("history", bundle.type);
        Assert.NotNull(bundle.entry);
        Assert.True(bundle.entry!.Count > 0, "Expected at least one entry in type history");
    }

    [Fact]
    public async Task test_system_history()
    {
        var bundle = await Client.SystemHistoryAsync();
        Assert.Equal("Bundle", bundle.resourceType);
    }

    [Fact]
    public async Task test_history_with_count()
    {
        var bundle = await Client.HistoryAsync<Patient>(_id, "_count=2");
        Assert.NotNull(bundle.entry);
        Assert.True(bundle.entry!.Count <= 2,
            $"Expected at most 2 entries, got {bundle.entry.Count}");
    }

    [Fact]
    public async Task test_history_with_since()
    {
        var since = DateTime.UtcNow.AddMinutes(-5).ToString("yyyy-MM-ddTHH:mm:ssZ");
        var bundle = await Client.HistoryAsync<Patient>(_id, $"_since={since}");
        Assert.NotNull(bundle.entry);
        Assert.True(bundle.entry!.Count > 0,
            "Expected at least one entry for _since 5 minutes ago");
    }
}

public class TestVersionReading : TestBase
{
    private Patient _v1 = null!;
    private Patient _v2 = null!;

    public override async Task InitializeAsync()
    {
        _v1 = await CreatePatientAsync(Gen.Patient(family: "VreadV1", given: "First"));

        _v1.name = [new HumanName { family = "VreadV2", given = ["Second"] }];
        _v2 = await Client.UpdateAsync(_v1);
    }

    [Fact]
    public async Task test_vread_specific_version()
    {
        var versionId = _v1.meta!.versionId!;
        var patient = await Client.VReadAsync<Patient>(_v1.id!, versionId);
        Assert.Equal(versionId, patient.meta!.versionId);
        Assert.Equal("VreadV1", patient.name![0].family);
    }

    [Fact]
    public async Task test_vread_latest_version()
    {
        var versionId = _v2.meta!.versionId!;
        var patient = await Client.VReadAsync<Patient>(_v2.id!, versionId);
        Assert.Equal(versionId, patient.meta!.versionId);
        Assert.Equal("VreadV2", patient.name![0].family);
    }

    [Fact]
    public async Task test_vread_nonexistent_version()
    {
        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.VReadAsync<Patient>(_v1.id!, "999"));
        Assert.Equal(HttpStatusCode.NotFound, ex.StatusCode);
    }
}

public class TestETagSupport : TestBase
{
    [Fact]
    public async Task test_etag_on_read()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "ETagRead"));

        var response = await RawGetAsync($"Patient/{patient.id}");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var etag = response.Headers.ETag?.ToString();
        Assert.NotNull(etag);
        Assert.StartsWith("W/\"", etag!);
    }

    [Fact]
    public async Task test_etag_on_vread()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "ETagVread"));
        var versionId = patient.meta!.versionId!;

        var response = await RawGetAsync($"Patient/{patient.id}/_history/{versionId}");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var etag = response.Headers.ETag?.ToString();
        Assert.NotNull(etag);
        Assert.Contains(versionId, etag!);
    }

    [Fact]
    public async Task test_if_none_match()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "IfNoneMatch"));

        var response1 = await RawGetAsync($"Patient/{patient.id}");
        Assert.Equal(HttpStatusCode.OK, response1.StatusCode);
        var etag = response1.Headers.ETag?.ToString();
        Assert.NotNull(etag);

        var request = new HttpRequestMessage(HttpMethod.Get, $"Patient/{patient.id}");
        request.Headers.TryAddWithoutValidation("If-None-Match", etag!);
        var response2 = await Http.SendAsync(request);

        Assert.True(
            response2.StatusCode is HttpStatusCode.NotModified or HttpStatusCode.OK,
            $"Expected 304 or 200, got {(int)response2.StatusCode}");
    }

    [Fact]
    public async Task test_if_match_on_update()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "IfMatchOk"));
        var etag = $"W/\"{patient.meta!.versionId}\"";

        patient.name = [new HumanName { family = "IfMatchUpdated" }];
        var updated = await Client.UpdateAsync(patient, etag);
        Assert.Equal("IfMatchUpdated", updated.name![0].family);
    }

    [Fact]
    public async Task test_if_match_wrong_version()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "IfMatchWrong"));

        patient.name = [new HumanName { family = "ShouldFail" }];
        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.UpdateAsync(patient, "W/\"999\""));
        Assert.True(
            ex.StatusCode is HttpStatusCode.Conflict or HttpStatusCode.PreconditionFailed,
            $"Expected 409 or 412, got {(int)ex.StatusCode}");
    }
}

public class TestVersioning : TestBase
{
    [Fact]
    public async Task test_version_increments()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "VerInc1"));
        var v1 = int.Parse(patient.meta!.versionId!);

        patient.name = [new HumanName { family = "VerInc2" }];
        var updated = await Client.UpdateAsync(patient);
        var v2 = int.Parse(updated.meta!.versionId!);

        Assert.True(v2 > v1, $"Expected v2 ({v2}) > v1 ({v1})");
    }

    [Fact]
    public async Task test_last_updated_changes()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "LastUpd1"));
        var lastUpdated1 = patient.meta!.lastUpdated!;

        await Task.Delay(100);

        patient.name = [new HumanName { family = "LastUpd2" }];
        var updated = await Client.UpdateAsync(patient);
        var lastUpdated2 = updated.meta!.lastUpdated!;

        Assert.NotEqual(lastUpdated1, lastUpdated2);
    }

    [Fact]
    public async Task test_delete_creates_version()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "DelVer"));
        var id = patient.id!;

        await Client.DeleteAsync<Patient>(id);

        var bundle = await Client.HistoryAsync<Patient>(id);
        Assert.NotNull(bundle.entry);
        Assert.True(bundle.entry!.Count > 0, "Expected history entries after delete");
    }

    [Fact]
    public async Task test_version_aware_update()
    {
        var patient = await CreatePatientAsync(Gen.Patient(family: "VerAware"));
        var etag = $"W/\"{patient.meta!.versionId}\"";

        patient.name = [new HumanName { family = "VerAwareUpdated" }];
        var updated = await Client.UpdateAsync(patient, etag);
        Assert.Equal("VerAwareUpdated", updated.name![0].family);
    }
}
