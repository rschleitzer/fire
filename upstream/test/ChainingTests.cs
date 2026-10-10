using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestForwardChaining : TestBase
{
    private string _uid = null!;
    private Practitioner _pracAnderson = null!;
    private Practitioner _pracMartinez = null!;
    private Patient _patTaylor = null!;
    private Patient _patChen = null!;
    private Patient _patLee = null!;
    private Observation _obsTaylor1 = null!;
    private Observation _obsTaylor2 = null!;
    private Observation _obsTaylor3 = null!;
    private Observation _obsChen1 = null!;
    private Observation _obsChen2 = null!;

    public override async Task InitializeAsync()
    {
        _uid = Guid.NewGuid().ToString("N")[..8];

        // Create 2 practitioners
        _pracAnderson = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"Anderson{_uid}", given: "John"));
        _pracMartinez = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"Martinez{_uid}", given: "Sarah"));

        // Create 3 patients: Taylor with Anderson GP, Chen with Martinez GP, Lee with no GP
        _patTaylor = await CreatePatientAsync(Gen.Patient(
            family: $"Taylor{_uid}", gender: "female",
            generalPractitioner: [Gen.Ref("Practitioner", _pracAnderson.id!)]));
        _patChen = await CreatePatientAsync(Gen.Patient(
            family: $"Chen{_uid}", gender: "male",
            generalPractitioner: [Gen.Ref("Practitioner", _pracMartinez.id!)]));
        _patLee = await CreatePatientAsync(Gen.Patient(
            family: $"Lee{_uid}", gender: "female"));

        // Create 5 observations: 3 for Taylor, 2 for Chen
        _obsTaylor1 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patTaylor.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(72, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _obsTaylor2 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patTaylor.id!),
            code: "8310-5", codeDisplay: "Body temperature",
            valueQuantity: Gen.Qty(37, "Cel", "http://unitsofmeasure.org", "Cel")));
        _obsTaylor3 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patTaylor.id!),
            code: "9279-1", codeDisplay: "Respiratory rate",
            valueQuantity: Gen.Qty(16, "breaths/minute", "http://unitsofmeasure.org", "/min")));
        _obsChen1 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patChen.id!),
            code: "8867-4", codeDisplay: "Heart rate",
            valueQuantity: Gen.Qty(80, "beats/minute", "http://unitsofmeasure.org", "/min")));
        _obsChen2 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patChen.id!),
            code: "85354-9", codeDisplay: "Blood pressure",
            valueQuantity: Gen.Qty(120, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));
    }

    [Fact]
    public async Task ChainObservationSubjectFamily()
    {
        // Find observations where subject (Patient) has family "Taylor{uid}"
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain("family", $"Taylor{_uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Equal(3, ids.Count);
        Assert.Contains(_obsTaylor1.id, ids);
        Assert.Contains(_obsTaylor2.id, ids);
        Assert.Contains(_obsTaylor3.id, ids);
        Assert.DoesNotContain(_obsChen1.id, ids);
        Assert.DoesNotContain(_obsChen2.id, ids);
    }

    [Fact]
    public async Task ChainObservationSubjectGender()
    {
        // Find observations where subject (Patient) has gender "female"
        // Taylor is female (3 obs), Lee is female (0 obs), Chen is male (2 obs)
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain("gender", "female"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        // Should include Taylor's 3 observations but not Chen's 2
        Assert.Contains(_obsTaylor1.id, ids);
        Assert.Contains(_obsTaylor2.id, ids);
        Assert.Contains(_obsTaylor3.id, ids);
        Assert.DoesNotContain(_obsChen1.id, ids);
        Assert.DoesNotContain(_obsChen2.id, ids);
    }

    [Fact]
    public async Task TypedChainObservationSubjectFamily()
    {
        // Typed chain: includes :Patient modifier in the parameter name
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain<PatientSearch>(p => p.Family == $"Taylor{_uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Equal(3, ids.Count);
        Assert.Contains(_obsTaylor1.id, ids);
        Assert.Contains(_obsTaylor2.id, ids);
        Assert.Contains(_obsTaylor3.id, ids);
        Assert.DoesNotContain(_obsChen1.id, ids);
        Assert.DoesNotContain(_obsChen2.id, ids);
    }

    [Fact]
    public async Task ChainPatientGeneralPractitionerFamily()
    {
        // Find patients whose general-practitioner has family "Anderson{uid}"
        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner.Chain("family", $"Anderson{_uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Equal(1, ids.Count);
        Assert.Contains(_patTaylor.id, ids);
        Assert.DoesNotContain(_patChen.id, ids);
        Assert.DoesNotContain(_patLee.id, ids);
    }

    [Fact]
    public async Task ChainWithNoMatch()
    {
        // Chain that matches nothing
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain("family", $"NonExistent{_uid}"))
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);
        Assert.True(bundle.entry is null || bundle.entry.Count == 0);
    }

    [Fact]
    public async Task TypedChainIncludesModifier()
    {
        // Typed chain should generate "subject:Patient.family" (with :Patient modifier)
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain<PatientSearch>(p => p.Family == $"Chen{_uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Equal(2, ids.Count);
        Assert.Contains(_obsChen1.id, ids);
        Assert.Contains(_obsChen2.id, ids);
        Assert.DoesNotContain(_obsTaylor1.id, ids);
    }
}

public class TestReverseChaining : TestBase
{
    private string _uid = null!;
    private Practitioner _pracAnderson = null!;
    private Practitioner _pracMartinez = null!;
    private Patient _patTaylor = null!;
    private Patient _patChen = null!;
    private Patient _patLee = null!;

    public override async Task InitializeAsync()
    {
        _uid = Guid.NewGuid().ToString("N")[..8];

        _pracAnderson = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"Anderson{_uid}"));
        _pracMartinez = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"Martinez{_uid}"));

        _patTaylor = await CreatePatientAsync(Gen.Patient(
            family: $"Taylor{_uid}",
            generalPractitioner: [Gen.Ref("Practitioner", _pracAnderson.id!)]));
        _patChen = await CreatePatientAsync(Gen.Patient(
            family: $"Chen{_uid}",
            generalPractitioner: [Gen.Ref("Practitioner", _pracMartinez.id!)]));
        _patLee = await CreatePatientAsync(Gen.Patient(
            family: $"Lee{_uid}"));

        // Taylor has heart rate observation, Chen has blood pressure
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patTaylor.id!),
            code: "8867-4", codeDisplay: "Heart rate"));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patChen.id!),
            code: "85354-9", codeDisplay: "Blood pressure"));
    }

    [Fact]
    public async Task HasPatientWithObservationCode()
    {
        // Find patients who have an Observation with code 8867-4 (heart rate)
        var bundle = await Client.Patient()
            .Param("_has:Observation:patient:code", "http://loinc.org|8867-4")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patTaylor.id, ids);
        Assert.DoesNotContain(_patChen.id, ids);
        Assert.DoesNotContain(_patLee.id, ids);
    }

    [Fact]
    public async Task HasPatientWithObservationForSpecificPatient()
    {
        // Find patients who have an Observation with blood pressure code
        var bundle = await Client.Patient()
            .Param("_has:Observation:patient:code", "http://loinc.org|85354-9")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patChen.id, ids);
        Assert.DoesNotContain(_patTaylor.id, ids);
        Assert.DoesNotContain(_patLee.id, ids);
    }

    [Fact]
    public async Task HasNoMatch()
    {
        // _has with non-matching code returns 0
        var bundle = await Client.Patient()
            .Param("_has:Observation:patient:code", "http://loinc.org|99999-9")
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);
        Assert.True(bundle.entry is null || bundle.entry.Count == 0);
    }

    [Fact]
    public async Task HasChainedWithOtherParams()
    {
        // Combine _has with family filter
        var bundle = await Client.Patient()
            .Where(p => p.Family == $"Taylor{_uid}")
            .Param("_has:Observation:patient:code", "http://loinc.org|8867-4")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Single(ids);
        Assert.Contains(_patTaylor.id, ids);
    }
}

public class TestChainingEdgeCases : TestBase
{
    [Fact]
    public async Task ChainMultipleParameters()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];

        // Create practitioner with specific family and given
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"DoubleChain{uid}", given: $"Jane{uid}"));

        // Create patient linked to this practitioner
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"DcPatient{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));

        // Another practitioner with same family but different given
        var prac2 = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"DoubleChain{uid}", given: $"Other{uid}"));

        var patient2 = await CreatePatientAsync(Gen.Patient(
            family: $"DcPatient2{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac2.id!)]));

        // Search with TWO chained parameters — both must match (AND logic)
        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner.Chain("family", $"DoubleChain{uid}"))
            .Where(p => p.GeneralPractitioner.Chain("given", $"Jane{uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
        Assert.DoesNotContain(patient2.id, ids);
    }

    [Fact]
    public async Task ChainCombinedWithRegularSearch()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];

        var patient = await CreatePatientAsync(Gen.Patient(family: $"Combo{uid}"));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "8867-4", codeDisplay: "Heart rate"));
        await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "8310-5", codeDisplay: "Body temperature"));

        // Chain (subject family) + regular param (code)
        var bundle = await Client.Observation()
            .Where(o => o.Subject.Chain<PatientSearch>(p => p.Family == $"Combo{uid}"))
            .Where(o => o.Code == "http://loinc.org|8867-4")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Single(ids);
        foreach (var entry in bundle.entry!.Where(e => e.search?.mode is null or "match"))
        {
            var obs = (Observation)entry.resource!;
            Assert.Equal("8867-4", obs.code!.coding![0].code);
        }
    }

    [Fact]
    public async Task TwoLevelChain()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];

        // Create practitioner -> patient -> observation chain
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"TwoLevel{uid}"));
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"TlPatient{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));
        var obs = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient.id!),
            code: "8867-4", codeDisplay: "Heart rate"));

        // Another practitioner with different name
        var prac2 = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"OtherDoc{uid}"));
        var patient2 = await CreatePatientAsync(Gen.Patient(
            family: $"TlPatient2{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac2.id!)]));
        var obs2 = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", patient2.id!),
            code: "8867-4", codeDisplay: "Heart rate"));

        // Two-level chain: Observation -> Patient -> Practitioner
        // subject:Patient.general-practitioner.family
        var bundle = await Client.Observation()
            .Param("subject:Patient.general-practitioner.family", $"TwoLevel{uid}")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(obs.id, ids);
        Assert.DoesNotContain(obs2.id, ids);
    }
}
