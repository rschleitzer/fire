using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestPatientLifecycle : TestBase
{
    [Fact]
    public async Task test_create_patient()
    {
        var patient = Gen.Patient(family: "Smith", given: "Alice");

        var created = await CreatePatientAsync(patient);

        Assert.Equal("Patient", created.resourceType);
        Assert.NotNull(created.id);
        Assert.NotNull(created.meta);
        Assert.NotNull(created.meta!.versionId);
        Assert.NotNull(created.meta.lastUpdated);
        Assert.Equal("Smith", created.name![0].family);
    }

    [Fact]
    public async Task test_read_patient()
    {
        var patient = Gen.Patient(family: "Read", given: "Bob", gender: "male", birthDate: "1985-03-15");
        var created = await CreatePatientAsync(patient);

        var read = await Client.ReadAsync<Patient>(created.id!);

        Assert.Equal(created.id, read.id);
        Assert.Equal("Patient", read.resourceType);
        Assert.Equal("Read", read.name![0].family);
        Assert.Equal("Bob", read.name[0].given![0]);
        Assert.Equal("male", read.gender);
        Assert.Equal("1985-03-15", read.birthDate);
        Assert.Equal(created.meta!.versionId, read.meta!.versionId);
    }

    [Fact]
    public async Task test_update_patient()
    {
        var patient = Gen.Patient(family: "Before");
        var created = await CreatePatientAsync(patient);
        var version1 = created.meta!.versionId;

        created.name = [new HumanName { family = "After", given = ["Updated"] }];
        var updated = await Client.UpdateAsync(created);

        Assert.Equal("After", updated.name![0].family);
        Assert.NotEqual(version1, updated.meta!.versionId);
    }

    [Fact]
    public async Task test_delete_patient()
    {
        var patient = Gen.Patient(family: "ToDelete");
        var created = await CreatePatientAsync(patient);

        await Client.DeleteAsync<Patient>(created.id!);

        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.ReadAsync<Patient>(created.id!));
        Assert.True(
            ex.StatusCode is HttpStatusCode.NotFound or HttpStatusCode.Gone,
            $"Expected 404 or 410, got {(int)ex.StatusCode}");
    }

    [Fact]
    public async Task test_create_returns_location_header()
    {
        var patient = Gen.Patient(family: "Location");
        var response = await RawPostAsync("Patient", patient);

        Assert.Equal(HttpStatusCode.Created, response.StatusCode);
        var location = response.Headers.Location?.ToString();
        Assert.NotNull(location);
        Assert.Contains("Patient/", location);

        var created = await DeserializeAsync<Patient>(response);
        Track("Patient", created.id!);
        Assert.Contains(created.id!, location);
    }

    [Fact]
    public async Task test_create_patient_with_id()
    {
        var patient = Gen.Patient(family: "NoClientId");

        var created = await CreatePatientAsync(patient);

        Assert.NotNull(created.id);
        Assert.NotEmpty(created.id!);
    }

    [Fact]
    public async Task test_update_returns_200()
    {
        var patient = Gen.Patient(family: "UpdateStatus");
        var created = await CreatePatientAsync(patient);

        created.name = [new HumanName { family = "UpdatedStatus" }];
        var response = await RawPutAsync($"Patient/{created.id}", created);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
    }

    [Fact]
    public async Task test_delete_then_search()
    {
        var identifier = Gen.Id(system: "http://test.org/delete-search", value: Guid.NewGuid().ToString());
        var patient = Gen.Patient(family: "DeleteSearch", identifier: [identifier]);
        var created = await CreatePatientAsync(patient);

        await Client.DeleteAsync<Patient>(created.id!);

        var bundle = await Client.Patient()
            .Where(s => s.Identifier == $"http://test.org/delete-search|{identifier.value}")
            .ExecuteAsync();

        var matchIds = MatchIds(bundle);
        Assert.DoesNotContain(created.id!, matchIds);
    }
}

public class TestPatientConditionalCreate : TestBase
{
    [Fact]
    public async Task test_conditional_create_first_request()
    {
        var uniqueValue = Guid.NewGuid().ToString();
        var patient = Gen.Patient(
            family: "Conditional",
            identifier: [Gen.Id(system: "http://test.org/cond", value: uniqueValue)]);

        var result = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond|{uniqueValue}");

        Assert.True(result.Created);
        Track("Patient", result.Resource.id!);
    }

    [Fact]
    public async Task test_conditional_create_duplicate()
    {
        var uniqueValue = Guid.NewGuid().ToString();
        var patient1 = Gen.Patient(
            family: "CondDup",
            identifier: [Gen.Id(system: "http://test.org/cond-dup", value: uniqueValue)]);

        var result1 = await Client.ConditionalCreateAsync<Patient>(patient1,
            $"identifier=http://test.org/cond-dup|{uniqueValue}");
        Assert.True(result1.Created);
        Track("Patient", result1.Resource.id!);

        var patient2 = Gen.Patient(
            family: "CondDup2",
            identifier: [Gen.Id(system: "http://test.org/cond-dup", value: uniqueValue)]);

        var result2 = await Client.ConditionalCreateAsync<Patient>(patient2,
            $"identifier=http://test.org/cond-dup|{uniqueValue}");

        Assert.False(result2.Created);
        Assert.Equal(result1.Resource.id, result2.Resource.id);
    }

    [Fact]
    public async Task test_conditional_create_multiple_matches()
    {
        var sharedFamily = $"MultiMatch-{Guid.NewGuid():N}";
        var p1 = Gen.Patient(family: sharedFamily);
        var p2 = Gen.Patient(family: sharedFamily);
        var created1 = await CreatePatientAsync(p1);
        var created2 = await CreatePatientAsync(p2);

        var patient3 = Gen.Patient(family: sharedFamily);
        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.ConditionalCreateAsync<Patient>(patient3, $"family={sharedFamily}"));
        Assert.Equal(HttpStatusCode.PreconditionFailed, ex.StatusCode);
    }
}

public class TestPatientETag : TestBase
{
    [Fact]
    public async Task test_etag_on_read()
    {
        var patient = Gen.Patient(family: "ETag");
        var created = await CreatePatientAsync(patient);

        var response = await RawGetAsync($"Patient/{created.id}");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var etag = response.Headers.ETag?.ToString();
        Assert.NotNull(etag);
        Assert.StartsWith("W/\"", etag!);
    }

    [Fact]
    public async Task test_if_match_update()
    {
        var patient = Gen.Patient(family: "IfMatch");
        var created = await CreatePatientAsync(patient);
        var etag = $"W/\"{created.meta!.versionId}\"";

        created.name = [new HumanName { family = "IfMatchUpdated" }];
        var updated = await Client.UpdateAsync<Patient>(created, etag);

        Assert.Equal("IfMatchUpdated", updated.name![0].family);
    }

    [Fact]
    public async Task test_if_match_wrong_version()
    {
        var patient = Gen.Patient(family: "WrongETag");
        var created = await CreatePatientAsync(patient);

        created.name = [new HumanName { family = "ShouldFail" }];
        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.UpdateAsync<Patient>(created, "W/\"999\""));
        Assert.True(
            ex.StatusCode is HttpStatusCode.Conflict or HttpStatusCode.PreconditionFailed,
            $"Expected 409 or 412, got {(int)ex.StatusCode}");
    }
}
