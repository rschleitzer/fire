using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class ConditionalCreateTests : TestBase
{
    [Fact]
    public async Task test_conditional_create_first_request_creates()
    {
        var uniqueValue = $"COND-CREATE-{Guid.NewGuid():N}";
        var patient = Gen.Patient(
            family: "ConditionalTest",
            identifier: [Gen.Id(system: "http://test.org/cond-create", value: uniqueValue)]);

        var result = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond-create|{uniqueValue}");

        Assert.True(result.Created);
        Assert.NotNull(result.Resource.id);
        Track("Patient", result.Resource.id!);
        Assert.Equal("Patient", result.Resource.resourceType);

        var matching = result.Resource.identifier?.Where(i => i.value == uniqueValue).ToList();
        Assert.NotNull(matching);
        Assert.NotEmpty(matching!);
    }

    [Fact]
    public async Task test_conditional_create_duplicate_returns_existing()
    {
        var uniqueValue = $"COND-DUP-{Guid.NewGuid():N}";
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
        Assert.Equal("Patient", result2.Resource.resourceType);
    }

    [Fact]
    public async Task test_conditional_create_no_duplicate()
    {
        var uniqueValue = $"COND-NODUP-{Guid.NewGuid():N}";
        var patient = Gen.Patient(
            family: "ConditionalNoDup",
            identifier: [Gen.Id(system: "http://test.org/cond-nodup", value: uniqueValue)]);

        var result1 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond-nodup|{uniqueValue}");
        Assert.True(result1.Created);
        Track("Patient", result1.Resource.id!);

        var result2 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond-nodup|{uniqueValue}");
        Assert.False(result2.Created);

        var bundle = await Client.Patient()
            .Where(p => p.Identifier == $"http://test.org/cond-nodup|{uniqueValue}")
            .ExecuteAsync();

        Assert.Equal(1, bundle.total);
    }

    [Fact]
    public async Task test_conditional_create_with_system_and_value()
    {
        var system = "http://hospital.org/conditional-test";
        var value = $"VAL-{Guid.NewGuid():N}";
        var patient = Gen.Patient(
            family: "ConditionalSystem",
            identifier: [Gen.Id(system: system, value: value)]);

        var result1 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier={system}|{value}");
        Assert.True(result1.Created);
        Track("Patient", result1.Resource.id!);

        var result2 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier={system}|{value}");
        Assert.False(result2.Created);
        Assert.Equal(result1.Resource.id, result2.Resource.id);
    }

    [Fact]
    public async Task test_conditional_create_multiple_matches_precondition_failed()
    {
        var sharedFamily = $"ConditionalMulti-{Guid.NewGuid():N}";
        await CreatePatientAsync(Gen.Patient(family: sharedFamily));
        await CreatePatientAsync(Gen.Patient(family: sharedFamily));

        var patient3 = Gen.Patient(family: sharedFamily);
        var ex = await Assert.ThrowsAsync<FhirException>(() =>
            Client.ConditionalCreateAsync<Patient>(patient3, $"family={sharedFamily}"));

        Assert.Equal(HttpStatusCode.PreconditionFailed, ex.StatusCode);
    }

    [Fact]
    public async Task test_conditional_create_with_multiple_search_parameters()
    {
        var uniqueValue = $"COND-MULTI-{Guid.NewGuid():N}";
        var familyName = $"MultiParam-{Guid.NewGuid():N}";
        var patient = Gen.Patient(
            family: familyName,
            identifier: [Gen.Id(system: "http://test.org/cond-multi", value: uniqueValue)]);

        var result1 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond-multi|{uniqueValue}&family={familyName}");
        Assert.True(result1.Created);
        Track("Patient", result1.Resource.id!);

        var result2 = await Client.ConditionalCreateAsync<Patient>(patient,
            $"identifier=http://test.org/cond-multi|{uniqueValue}&family={familyName}");
        Assert.False(result2.Created);
        Assert.Equal(result1.Resource.id, result2.Resource.id);
    }
}
