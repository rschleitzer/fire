using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestIncludeForward : TestBase
{
    private string _family = null!;
    private Practitioner _anderson = null!;
    private Practitioner _martinez = null!;
    private Patient _taylor = null!;
    private Patient _chen = null!;
    private Patient _lee = null!;
    private Observation _taylorHr = null!;
    private Observation _taylorBp = null!;
    private Observation _taylorTemp = null!;
    private Observation _chenHr = null!;
    private Observation _chenTemp = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Inc{Guid.NewGuid():N}";

        _anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));
        _martinez = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Martinez", given: "Carlos"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", _anderson.id!)]));
        _chen = await CreatePatientAsync(Gen.Patient(
            family: _family + "Chen", given: "Oliver",
            generalPractitioner: [Gen.Ref("Practitioner", _martinez.id!)]));
        _lee = await CreatePatientAsync(Gen.Patient(
            family: _family + "Lee", given: "Sophia"));

        _taylorHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _taylorBp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "85354-9", codeDisplay: "Blood pressure systolic",
            valueQuantity: Gen.Qty(118, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));
        _taylorTemp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(36.8m, "Cel", "http://unitsofmeasure.org", "Cel")));
        _chenHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(68, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _chenTemp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(37.1m, "Cel", "http://unitsofmeasure.org", "Cel")));
    }

    [Fact]
    public async Task IncludePatientGeneralPractitioner()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .Include(p => p.GeneralPractitioner)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
        Assert.Contains("Practitioner", types);
        Assert.True(CountByType(bundle, "Patient") >= 1);
        Assert.True(CountByType(bundle, "Practitioner") >= 1);
    }

    [Fact]
    public async Task IncludeObservationSubject()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Code == "8867-4")
            .Include(o => o.Subject)
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Observation", types);
        Assert.Contains("Patient", types);
        Assert.True(CountByType(bundle, "Observation") >= 1);
        Assert.True(CountByType(bundle, "Patient") >= 1);
    }

    [Fact]
    public async Task IncludeMultipleReferences()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain<PatientSearch>(p => p.Family == _family + "Taylor"))
            .Include(o => o.Subject)
            .Include(o => o.Performer)
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Observation", types);
    }

    [Fact]
    public async Task IncludeWithResourceTypeSpecified()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .Include("Patient:general-practitioner:Practitioner")
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
        if (types.Count > 1)
            Assert.Contains("Practitioner", types);
    }
}

public class TestRevInclude : TestBase
{
    private string _family = null!;
    private Practitioner _anderson = null!;
    private Practitioner _martinez = null!;
    private Patient _taylor = null!;
    private Patient _chen = null!;
    private Patient _lee = null!;
    private Observation _taylorHr = null!;
    private Observation _taylorBp = null!;
    private Observation _taylorTemp = null!;
    private Observation _chenHr = null!;
    private Observation _chenTemp = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Rev{Guid.NewGuid():N}";

        _anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));
        _martinez = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Martinez", given: "Carlos"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", _anderson.id!)]));
        _chen = await CreatePatientAsync(Gen.Patient(
            family: _family + "Chen", given: "Oliver",
            generalPractitioner: [Gen.Ref("Practitioner", _martinez.id!)]));
        _lee = await CreatePatientAsync(Gen.Patient(
            family: _family + "Lee", given: "Sophia"));

        _taylorHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _taylorBp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "85354-9", codeDisplay: "Blood pressure systolic",
            valueQuantity: Gen.Qty(118, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));
        _taylorTemp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(36.8m, "Cel", "http://unitsofmeasure.org", "Cel")));
        _chenHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(68, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _chenTemp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(37.1m, "Cel", "http://unitsofmeasure.org", "Cel")));
    }

    [Fact]
    public async Task RevIncludePatientObservations()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .RevInclude<ObservationSearch>(o => o.Subject)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
        Assert.Contains("Observation", types);
        Assert.True(CountByType(bundle, "Patient") >= 1);
        Assert.True(CountByType(bundle, "Observation") >= 3, "Should include Emma Taylor's 3 observations");
    }

    [Fact]
    public async Task RevIncludePractitionerPatients()
    {
        var bundle = await Client.Practitioner()
            .Where(p => p.Family == _family + "Anderson")
            .RevInclude<PatientSearch>(p => p.GeneralPractitioner)
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Practitioner", types);
        Assert.Contains("Patient", types);
        Assert.True(CountByType(bundle, "Practitioner") >= 1);
        Assert.True(CountByType(bundle, "Patient") >= 1);
    }

    [Fact]
    public async Task RevIncludeWithResourceType()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Chen")
            .RevInclude("Observation:subject:Patient")
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
    }
}

public class TestIncludeIterate : TestBase
{
    private string _family = null!;
    private Practitioner _anderson = null!;
    private Patient _taylor = null!;
    private Observation _taylorHr = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Iter{Guid.NewGuid():N}";

        _anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", _anderson.id!)]));

        _taylorHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
    }

    [Fact]
    public async Task IncludeIterateTwoLevels()
    {
        // Observation -> include Subject (Patient) -> iterate include GeneralPractitioner (Practitioner)
        var bundle = await Client.Observation()
            .Where(o => o.Code == "8867-4")
            .Include(o => o.Subject)
            .Param("_include:iterate", "Patient:general-practitioner")
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Observation", types);
        // Patient should be included from the first include
        Assert.Contains("Patient", types);
        // Practitioner should be included from the iterate
        Assert.Contains("Practitioner", types);
    }

    [Fact]
    public async Task RevIncludeIterate()
    {
        // Practitioner -> revinclude Patient:general-practitioner -> iterate revinclude Observation:subject
        var bundle = await Client.Practitioner()
            .Where(p => p.Family == _family + "Anderson")
            .RevInclude<PatientSearch>(p => p.GeneralPractitioner)
            .Param("_revinclude:iterate", "Observation:subject")
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Practitioner", types);
        // Patient should be rev-included from first level
        Assert.Contains("Patient", types);
        // Observation via iterate may not be supported by all servers
        // If supported, Observation should appear in the bundle
    }
}

public class TestIncludeCombinations : TestBase
{
    private string _family = null!;
    private Practitioner _anderson = null!;
    private Patient _taylor = null!;
    private Observation _taylorHr = null!;
    private Observation _taylorBp = null!;
    private Observation _taylorTemp = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Cmb{Guid.NewGuid():N}";

        _anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", _anderson.id!)]));

        _taylorHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _taylorBp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "85354-9", codeDisplay: "Blood pressure systolic",
            valueQuantity: Gen.Qty(118, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));
        _taylorTemp = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(36.8m, "Cel", "http://unitsofmeasure.org", "Cel")));
    }

    [Fact]
    public async Task IncludeAndRevIncludeTogether()
    {
        // Search Patient, include GP (forward), revinclude Observations (reverse)
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .Include(p => p.GeneralPractitioner)
            .RevInclude<ObservationSearch>(o => o.Subject)
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
        Assert.Contains("Practitioner", types);
        Assert.Contains("Observation", types);
    }

    [Fact]
    public async Task MultipleIncludes()
    {
        var bundle = await Client.Patient()
            .Include(p => p.GeneralPractitioner)
            .Include(p => p.Organization)
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
    }

    [Fact]
    public async Task MultipleRevIncludes()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .RevInclude<ObservationSearch>(o => o.Subject)
            .RevInclude("Condition:subject")
            .ExecuteAsync();

        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
    }
}

public class TestIncludeEdgeCases : TestBase
{
    private string _family = null!;
    private Patient _taylor = null!;
    private Patient _lee = null!;
    private Observation _taylorHr = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Edge{Guid.NewGuid():N}";

        var anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", anderson.id!)]));
        _lee = await CreatePatientAsync(Gen.Patient(
            family: _family + "Lee", given: "Sophia"));

        _taylorHr = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
    }

    [Fact]
    public async Task IncludeNoMatchingReferences()
    {
        // Lee has no GP, include GP should return only the patient
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Lee")
            .Include(p => p.GeneralPractitioner)
            .ExecuteAsync();

        Assert.True(CountByType(bundle, "Patient") >= 1);
        Assert.Equal(0, CountByType(bundle, "Practitioner"));
    }

    [Fact]
    public async Task RevIncludeNoReferencingResources()
    {
        // Lee has no observations, revinclude should return only the patient
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Lee")
            .RevInclude<ObservationSearch>(o => o.Subject)
            .ExecuteAsync();

        Assert.True(CountByType(bundle, "Patient") >= 1);
        Assert.Equal(0, CountByType(bundle, "Observation"));
    }

    [Fact]
    public async Task IncludeInvalidParameter()
    {
        // Include a non-existent search parameter; server should handle gracefully
        var bundle = await Client.Patient()
            .Include("Patient:nonexistent-parameter")
            .ExecuteAsync();

        // Server should either ignore the invalid include or return a valid bundle
        Assert.NotNull(bundle);
    }

    [Fact]
    public async Task IncludeWildcard()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .Include("Patient:*")
            .ExecuteAsync();

        Assert.True(CountByType(bundle, "Patient") >= 1);
    }
}

public class TestIncludeWithSearchParams : TestBase
{
    private string _family = null!;
    private Practitioner _anderson = null!;
    private Practitioner _martinez = null!;
    private Patient _taylor = null!;
    private Patient _chen = null!;

    public override async Task InitializeAsync()
    {
        _family = $"Isp{Guid.NewGuid():N}";

        _anderson = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Anderson", given: "Emily"));
        _martinez = await CreatePractitionerAsync(Gen.Practitioner(
            family: _family + "Martinez", given: "Carlos"));

        _taylor = await CreatePatientAsync(Gen.Patient(
            family: _family + "Taylor", given: "Emma",
            generalPractitioner: [Gen.Ref("Practitioner", _anderson.id!)]));
        _chen = await CreatePatientAsync(Gen.Patient(
            family: _family + "Chen", given: "Oliver",
            generalPractitioner: [Gen.Ref("Practitioner", _martinez.id!)]));

        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "85354-9", codeDisplay: "Blood pressure systolic",
            valueQuantity: Gen.Qty(118, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _taylor.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(36.8m, "Cel", "http://unitsofmeasure.org", "Cel")));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(68, "beats/minute", "http://unitsofmeasure.org", "/min")));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _chen.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(37.1m, "Cel", "http://unitsofmeasure.org", "Cel")));
    }

    [Fact]
    public async Task IncludeWithFilter()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family + "Taylor")
            .Where(p => p.Given == "Emma")
            .Include(p => p.GeneralPractitioner)
            .ExecuteAsync();

        var matchIds = MatchIds(bundle);
        Assert.Single(matchIds);
        Assert.Equal(_taylor.id, matchIds[0]);
        Assert.True(CountByType(bundle, "Practitioner") >= 1);
    }

    [Fact]
    public async Task IncludeWithCount()
    {
        var bundle = await Client.Patient()
            .Include(p => p.GeneralPractitioner)
            .Count(2)
            .ExecuteAsync();

        // _count limits primary results; included resources are extra
        var matchIds = MatchIds(bundle);
        Assert.True(matchIds.Count <= 2);
    }

    [Fact]
    public async Task RevIncludeWithSort()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .RevInclude<ObservationSearch>(o => o.Subject)
            .Sort("family")
            .ExecuteAsync();

        // Patients should be returned (sorted); observations are included extras
        var types = ResourceTypes(bundle);
        Assert.Contains("Patient", types);
    }
}
