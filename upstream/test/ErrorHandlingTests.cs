using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestInvalidJson : TestBase
{
    [Fact]
    public async Task test_invalid_json_body()
    {
        var response = await RawPostStringAsync("Patient", "{invalid json syntax}");

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for invalid JSON, got {(int)response.StatusCode}");

        var body = await response.Content.ReadAsStringAsync();
        if (!string.IsNullOrEmpty(body))
        {
            var result = await DeserializeAsync<Resource>(response);
            Assert.Equal("OperationOutcome", result.resourceType);
        }
    }

    [Fact]
    public async Task test_empty_body()
    {
        var response = await RawPostStringAsync("Patient", "");

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for empty body, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_wrong_content_type()
    {
        var patient = Gen.Patient(family: "ContentType");
        var json = System.Text.Json.JsonSerializer.Serialize(patient, Json);
        var response = await RawPostStringAsync("Patient", json, "text/plain");

        Assert.True(
            response.StatusCode is HttpStatusCode.Created
                or HttpStatusCode.BadRequest
                or HttpStatusCode.UnsupportedMediaType,
            $"Expected 201, 400, or 415, got {(int)response.StatusCode}");

        if (response.StatusCode == HttpStatusCode.Created)
        {
            var created = await DeserializeAsync<Patient>(response);
            Track("Patient", created.id!);
        }
    }
}

public class TestInvalidResource : TestBase
{
    [Fact]
    public async Task test_missing_resource_type()
    {
        var response = await RawPostStringAsync("Patient",
            "{\"name\": [{\"family\": \"Test\"}]}");

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for missing resourceType, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_wrong_resource_type()
    {
        var patient = Gen.Patient(family: "WrongEndpoint");
        var response = await RawPostAsync("Observation", patient);

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for resource type mismatch, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_invalid_gender_enum()
    {
        var response = await RawPostStringAsync("Patient",
            "{\"resourceType\": \"Patient\", \"gender\": \"invalid_value\", \"name\": [{\"family\": \"Test\"}]}");

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for invalid enum, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_invalid_active_type()
    {
        var response = await RawPostStringAsync("Patient",
            "{\"resourceType\": \"Patient\", \"active\": \"not-a-boolean\", \"name\": [{\"family\": \"Test\"}]}");

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for invalid field type, got {(int)response.StatusCode}");
    }
}

public class TestHttpMethodErrors : TestBase
{
    [Fact]
    public async Task test_post_to_instance_url()
    {
        var patient = Gen.Patient(family: "PostInstance");
        var created = await CreatePatientAsync(patient);

        var response = await RawPostAsync($"Patient/{created.id}", patient);

        Assert.True(
            response.StatusCode is HttpStatusCode.MethodNotAllowed or HttpStatusCode.BadRequest,
            $"Expected 405 or 400, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_put_without_id()
    {
        var patient = Gen.Patient(family: "PutNoId");
        var response = await RawPutAsync("Patient", patient);

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.MethodNotAllowed,
            $"Expected 400 or 405, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_delete_nonexistent()
    {
        var response = await Http.DeleteAsync("Patient/nonexistent-id-99999");

        Assert.True(
            response.StatusCode is HttpStatusCode.NotFound or HttpStatusCode.NoContent,
            $"Expected 404 or 204, got {(int)response.StatusCode}");
    }
}

public class TestSearchErrors : TestBase
{
    [Fact]
    public async Task test_invalid_search_param()
    {
        var response = await RawGetAsync("Patient?unknown_param=value");

        Assert.True(
            response.StatusCode is HttpStatusCode.OK or HttpStatusCode.BadRequest,
            $"Expected 200 or 400, got {(int)response.StatusCode}");

        if (response.StatusCode == HttpStatusCode.OK)
        {
            var bundle = await DeserializeAsync<Bundle>(response);
            Assert.Equal("Bundle", bundle.resourceType);
        }
    }

    [Fact]
    public async Task test_invalid_date_format()
    {
        var response = await RawGetAsync("Patient?birthdate=not-a-date");

        Assert.True(
            response.StatusCode is HttpStatusCode.OK or HttpStatusCode.BadRequest,
            $"Expected 200 or 400, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_search_invalid_resource_type()
    {
        var response = await RawGetAsync("InvalidType?name=test");

        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }
}

public class TestReadErrors : TestBase
{
    [Fact]
    public async Task test_read_nonexistent_resource()
    {
        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.ReadAsync<Patient>("nonexistent-id-12345"));

        Assert.Equal(HttpStatusCode.NotFound, ex.StatusCode);
    }

    [Fact]
    public async Task test_read_invalid_resource_type()
    {
        var response = await RawGetAsync("InvalidResourceType/123");

        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }

    [Fact]
    public async Task test_vread_nonexistent()
    {
        var response = await RawGetAsync("Patient/nonexistent-id-12345/_history/1");

        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }
}

public class TestUpdateErrors : TestBase
{
    [Fact]
    public async Task test_update_id_mismatch()
    {
        var patient = Gen.Patient(family: "Mismatch");
        var created = await CreatePatientAsync(patient);
        var urlId = created.id!;

        // Set a different id in the body than in the URL
        created.id = "completely-different-id";
        var response = await RawPutAsync($"Patient/{urlId}", created);

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest
                or HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for ID mismatch, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task test_update_missing_id()
    {
        var patient = Gen.Patient(family: "NoId");
        // Explicitly ensure no id is set
        patient.id = null;

        var response = await RawPutAsync("Patient/some-id", patient);

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest
                or HttpStatusCode.UnprocessableEntity
                or HttpStatusCode.OK
                or HttpStatusCode.Created,
            $"Unexpected status code {(int)response.StatusCode}");

        // If the server accepted it (update-as-create), clean up
        if (response.StatusCode is HttpStatusCode.OK or HttpStatusCode.Created)
        {
            var created = await DeserializeAsync<Patient>(response);
            Track("Patient", created.id!);
        }
    }

    [Fact]
    public async Task test_concurrent_update()
    {
        var patient = Gen.Patient(family: "Concurrent");
        var created = await CreatePatientAsync(patient);
        var originalVersion = created.meta!.versionId!;

        // First update succeeds
        created.name = [new HumanName { family = "Updated1" }];
        var updated = await Client.UpdateAsync(created);

        // Second update with stale version should fail
        var staleEtag = $"W/\"{originalVersion}\"";
        created.name = [new HumanName { family = "Updated2" }];
        var response = await RawPutAsync($"Patient/{created.id}", created,
            new Dictionary<string, string> { ["If-Match"] = staleEtag });

        Assert.True(
            response.StatusCode is HttpStatusCode.Conflict or HttpStatusCode.PreconditionFailed,
            $"Expected 409 or 412 for stale version, got {(int)response.StatusCode}");
    }
}

public class TestSpecialCharacters : TestBase
{
    [Fact]
    public async Task test_search_with_special_characters()
    {
        var patient = Gen.Patient(
            family: "O'Brien",
            name: [new HumanName { family = "O'Brien", given = ["Test"] }]);
        var created = await CreatePatientAsync(patient);

        var response = await RawGetAsync("Patient?family=O'Brien");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var bundle = await DeserializeAsync<Bundle>(response);
        Assert.Equal("Bundle", bundle.resourceType);
    }

    [Fact]
    public async Task test_create_with_unicode_name()
    {
        var patient = Gen.Patient(
            family: "Mueller",
            name: [new HumanName
            {
                family = "M\u00fcller",
                given = ["Fran\u00e7ois", "Jos\u00e9"]
            }]);

        var created = await CreatePatientAsync(patient);
        var read = await Client.ReadAsync<Patient>(created.id!);

        Assert.Equal("M\u00fcller", read.name![0].family);
        Assert.Equal("Fran\u00e7ois", read.name[0].given![0]);
        Assert.Equal("Jos\u00e9", read.name[0].given[1]);
    }
}
