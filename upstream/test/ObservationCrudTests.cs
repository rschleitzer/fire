using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestObservationCRUD : TestBase
{
    [Fact]
    public async Task CreateObservation()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            codeSystem: "http://loinc.org",
            code: "8867-4",
            codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min"));

        var created = await CreateObservationAsync(obs);

        Assert.Equal("Observation", created.resourceType);
        Assert.Equal("final", created.status);
        Assert.Equal("8867-4", created.code!.coding![0].code);
        Assert.Equal($"Patient/{patient.id}", created.subject!.reference);
        Assert.Equal(72, created.valueQuantity!.value);
    }

    [Fact]
    public async Task ReadObservation()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            valueQuantity: Gen.Qty(75, "bpm"));

        var created = await CreateObservationAsync(obs);
        var read = await Client.ReadAsync<Observation>(created.id!);

        Assert.Equal(created.id, read.id);
        Assert.Equal(75, read.valueQuantity!.value);
        Assert.Equal("8867-4", read.code!.coding![0].code);
    }

    [Fact]
    public async Task UpdateObservation()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            valueQuantity: Gen.Qty(72, "bpm"));

        var created = await CreateObservationAsync(obs);
        created.valueQuantity = Gen.Qty(78, "bpm");

        var updated = await Client.UpdateAsync(created);

        Assert.Equal(78, updated.valueQuantity!.value);
        Assert.True(int.Parse(updated.meta!.versionId!) > int.Parse(created.meta!.versionId!));
    }

    [Fact]
    public async Task DeleteObservation()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(subject: Gen.Ref("Patient", patient.id!));

        var created = await CreateObservationAsync(obs);
        await Client.DeleteAsync<Observation>(created.id!);

        var ex = await Assert.ThrowsAsync<FhirException>(
            () => Client.ReadAsync<Observation>(created.id!));
        Assert.True(
            ex.StatusCode is HttpStatusCode.NotFound or HttpStatusCode.Gone,
            $"Expected 404 or 410, got {(int)ex.StatusCode}");
    }

    [Fact]
    public async Task CreateObservationWithCodeableConceptValue()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            codeSystem: "http://loinc.org",
            code: "32451-7",
            codeDisplay: "Physical findings",
            valueCodeableConcept: Gen.Code("http://snomed.info/sct", "271594007", "Normal"));

        var created = await CreateObservationAsync(obs);

        Assert.NotNull(created.valueCodeableConcept);
        Assert.Equal("271594007", created.valueCodeableConcept!.coding![0].code);
    }

    [Fact]
    public async Task CreateObservationWithStringValue()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var obs = Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            codeSystem: "http://loinc.org",
            code: "11543-6",
            codeDisplay: "Notes",
            valueString: "Patient appears healthy");

        var created = await CreateObservationAsync(obs);

        Assert.Equal("Patient appears healthy", created.valueString);
    }
}

public class TestObservationSearch : TestBase
{
    [Fact]
    public async Task SearchByPatient()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "8867-4", codeDisplay: "Heart rate"));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "2339-0", codeDisplay: "Glucose"));

        var bundle = await Client.Observation()
            .Where(o => o.Patient == $"Patient/{patient.id}")
            .ExecuteAsync();

        Assert.True(bundle.total >= 2);
        foreach (var entry in bundle.entry!)
        {
            var obs = (Observation)entry.resource!;
            Assert.Contains(patient.id!, obs.subject!.reference!);
        }
    }

    [Fact]
    public async Task SearchByCode()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "8867-4", codeDisplay: "Heart rate"));

        var bundle = await Client.Observation()
            .Where(o => o.Code == "http://loinc.org|8867-4")
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        var ids = MatchIds(bundle);
        Assert.NotEmpty(ids);
        foreach (var entry in bundle.entry!.Where(e => e.search?.mode is null or "match"))
        {
            var obs = (Observation)entry.resource!;
            var codes = obs.code!.coding!.Select(c => c.code).ToList();
            Assert.Contains("8867-4", codes);
        }
    }

    [Fact]
    public async Task SearchByCategory()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var labCategory = new List<CodeableConcept>
        {
            Gen.Code(
                "http://terminology.hl7.org/CodeSystem/observation-category",
                "laboratory",
                "Laboratory")
        };
        var created = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            codeSystem: "http://loinc.org",
            code: "2339-0",
            codeDisplay: "Glucose",
            valueQuantity: Gen.Qty(95, "mg/dL", "http://unitsofmeasure.org", "mg/dL"),
            category: labCategory));

        var bundle = await Client.Observation()
            .Where(o => o.Category == "laboratory")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(created.id!, ids);
    }

    [Fact]
    public async Task SearchByStatus()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            status: "final"));

        var bundle = await Client.Observation()
            .Where(o => o.Status == "final")
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        foreach (var entry in bundle.entry!.Where(e => e.search?.mode is null or "match"))
        {
            var obs = (Observation)entry.resource!;
            Assert.Equal("final", obs.status);
        }
    }

    [Fact]
    public async Task SearchByDateRange()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var now = DateTime.UtcNow;
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            effectiveDateTime: now.ToString("yyyy-MM-ddTHH:mm:ssZ")));

        var dayBefore = now.AddDays(-1).ToString("yyyy-MM-dd");
        var dayAfter = now.AddDays(1).ToString("yyyy-MM-dd");

        var bundle = await Client.Observation()
            .Where(o => o.Date >= dayBefore)
            .Where(o => o.Date <= dayAfter)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
    }
}

public class TestObservationValidation : TestBase
{
    [Fact]
    public async Task CreateObservationInvalidStatus()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var body = new
        {
            resourceType = "Observation",
            status = "invalid-status",
            code = new
            {
                coding = new[]
                {
                    new { system = "http://loinc.org", code = "8867-4", display = "Heart rate" }
                }
            },
            subject = new { reference = $"Patient/{patient.id}" },
            valueQuantity = new { value = 72, unit = "bpm" }
        };

        var response = await RawPostAsync("Observation", body);

        Assert.True(
            response.StatusCode == HttpStatusCode.BadRequest ||
            response.StatusCode == HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for invalid status, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task CreateObservationMissingCode()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var body = new
        {
            resourceType = "Observation",
            status = "final",
            subject = new { reference = $"Patient/{patient.id}" }
        };

        var response = await RawPostAsync("Observation", body);

        Assert.True(
            response.StatusCode == HttpStatusCode.BadRequest ||
            response.StatusCode == HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for missing code, got {(int)response.StatusCode}");
    }

    [Fact]
    public async Task CreateObservationMissingStatus()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var body = new
        {
            resourceType = "Observation",
            code = new
            {
                coding = new[]
                {
                    new { system = "http://loinc.org", code = "8867-4", display = "Heart rate" }
                }
            },
            subject = new { reference = $"Patient/{patient.id}" }
        };

        var response = await RawPostAsync("Observation", body);

        Assert.True(
            response.StatusCode == HttpStatusCode.BadRequest ||
            response.StatusCode == HttpStatusCode.UnprocessableEntity,
            $"Expected 400 or 422 for missing status, got {(int)response.StatusCode}");
    }
}
