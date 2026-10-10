using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestCodeValueQuantityComposite : TestBase
{
    private Observation _systolicHigh = null!;   // 8480-6, 180 mmHg (with system)
    private Observation _systolicNormal = null!; // 8480-6, 120 mmHg (with system)
    private Observation _diastolic = null!;      // 8462-4, 90 mmHg (with system)
    private Observation _systolicNoSystem = null!; // 8480-6, 150 mmHg (no code system)

    public override async Task InitializeAsync()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var patientRef = Gen.Ref("Patient", patient.id!);

        _systolicHigh = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8480-6",
            codeDisplay: "Systolic blood pressure",
            valueQuantity: Gen.Qty(180, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));

        _systolicNormal = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8480-6",
            codeDisplay: "Systolic blood pressure",
            valueQuantity: Gen.Qty(120, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));

        _diastolic = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8462-4",
            codeDisplay: "Diastolic blood pressure",
            valueQuantity: Gen.Qty(90, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")));

        _systolicNoSystem = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "",
            code: "8480-6",
            codeDisplay: "Systolic blood pressure",
            valueQuantity: Gen.Qty(150, "mmHg")));
    }

    [Fact]
    public async Task CodeValueQuantityGt()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|8480-6", "gt150"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityLt()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|8480-6", "lt150"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicNormal.id, ids);
        Assert.DoesNotContain(_systolicHigh.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityGe()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|8480-6", "ge150"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        // systolicNoSystem has empty code system, doesn't match http://loinc.org|8480-6
        Assert.DoesNotContain(_systolicNoSystem.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityEq()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|8480-6", "120"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicNormal.id, ids);
        Assert.DoesNotContain(_systolicHigh.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityWithoutSystem()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("8480-6", "gt140"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        Assert.Contains(_systolicNoSystem.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityNoMatchWrongCode()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|2339-0", "gt100"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
        Assert.DoesNotContain(_systolicNoSystem.id, ids);
    }

    [Fact]
    public async Task CodeValueQuantityNoMatchOutOfRange()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueQuantity == ("http://loinc.org|8480-6", "gt200"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolic.id, ids);
        Assert.DoesNotContain(_systolicNoSystem.id, ids);
    }
}

public class TestCodeValueConceptComposite : TestBase
{
    private Observation _systolicHigh = null!;   // 8480-6, value=H (High)
    private Observation _systolicNormal = null!; // 8480-6, value=N (Normal)
    private Observation _diastolicHigh = null!;  // 8462-4, value=H (High)

    public override async Task InitializeAsync()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var patientRef = Gen.Ref("Patient", patient.id!);

        _systolicHigh = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8480-6",
            codeDisplay: "Systolic blood pressure",
            valueCodeableConcept: Gen.Code(
                "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation", "H", "High")));

        _systolicNormal = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8480-6",
            codeDisplay: "Systolic blood pressure",
            valueCodeableConcept: Gen.Code(
                "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation", "N", "Normal")));

        _diastolicHigh = await CreateObservationAsync(Gen.Observation(
            subject: patientRef,
            codeSystem: "http://loinc.org",
            code: "8462-4",
            codeDisplay: "Diastolic blood pressure",
            valueCodeableConcept: Gen.Code(
                "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation", "H", "High")));
    }

    [Fact]
    public async Task CodeValueConceptWithSystems()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueConcept == (
                "http://loinc.org|8480-6",
                "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation|H"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolicHigh.id, ids);
    }

    [Fact]
    public async Task CodeValueConceptCodeOnly()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueConcept == ("http://loinc.org|8480-6", "H"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolicHigh.id, ids);
    }

    [Fact]
    public async Task CodeValueConceptNoSystems()
    {
        var bundle = await Client.Observation()
            .Where(o => o.CodeValueConcept == ("8480-6", "H"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_systolicHigh.id, ids);
        Assert.DoesNotContain(_systolicNormal.id, ids);
        Assert.DoesNotContain(_diastolicHigh.id, ids);
    }
}

public class TestComponentCodeValueQuantityComposite : TestBase
{
    private Observation _highBp = null!;   // systolic=180, diastolic=90 (with system)
    private Observation _normalBp = null!; // systolic=120, diastolic=80 (without system)

    public override async Task InitializeAsync()
    {
        var patient = await CreatePatientAsync(Gen.Patient());
        var patientRef = Gen.Ref("Patient", patient.id!);

        var highBp = new Observation
        {
            resourceType = "Observation",
            status = "final",
            code = Gen.Code("http://loinc.org", "85354-9", "Blood pressure panel"),
            subject = patientRef,
            category = [Gen.VitalSignsCategory()],
            effectiveDateTime = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ"),
            component =
            [
                new Observation.Component
                {
                    code = Gen.Code("http://loinc.org", "8480-6", "Systolic blood pressure"),
                    valueQuantity = Gen.Qty(180, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")
                },
                new Observation.Component
                {
                    code = Gen.Code("http://loinc.org", "8462-4", "Diastolic blood pressure"),
                    valueQuantity = Gen.Qty(90, "mmHg", "http://unitsofmeasure.org", "mm[Hg]")
                }
            ]
        };
        _highBp = await CreateObservationAsync(highBp);

        var normalBp = new Observation
        {
            resourceType = "Observation",
            status = "final",
            code = Gen.Code("http://loinc.org", "85354-9", "Blood pressure panel"),
            subject = patientRef,
            category = [Gen.VitalSignsCategory()],
            effectiveDateTime = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ"),
            component =
            [
                new Observation.Component
                {
                    code = Gen.Code("http://loinc.org", "8480-6", "Systolic blood pressure"),
                    valueQuantity = Gen.Qty(120, "mmHg")
                },
                new Observation.Component
                {
                    code = Gen.Code("http://loinc.org", "8462-4", "Diastolic blood pressure"),
                    valueQuantity = Gen.Qty(80, "mmHg")
                }
            ]
        };
        _normalBp = await CreateObservationAsync(normalBp);
    }

    [Fact]
    public async Task ComponentCodeValueQuantityGt()
    {
        var bundle = await Client.Observation()
            .Where(o => o.ComponentCodeValueQuantity == ("http://loinc.org|8480-6", "gt150"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_highBp.id, ids);
        Assert.DoesNotContain(_normalBp.id, ids);
    }

    [Fact]
    public async Task ComponentCodeValueQuantityLt()
    {
        var bundle = await Client.Observation()
            .Where(o => o.ComponentCodeValueQuantity == ("http://loinc.org|8480-6", "lt150"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_normalBp.id, ids);
        Assert.DoesNotContain(_highBp.id, ids);
    }

    [Fact]
    public async Task ComponentCodeValueQuantityDiastolic()
    {
        var bundle = await Client.Observation()
            .Where(o => o.ComponentCodeValueQuantity == ("http://loinc.org|8462-4", "gt85"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_highBp.id, ids);
        Assert.DoesNotContain(_normalBp.id, ids);
    }

    [Fact]
    public async Task ComponentCodeValueQuantityNoSystem()
    {
        var bundle = await Client.Observation()
            .Where(o => o.ComponentCodeValueQuantity == ("8480-6", "ge120"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_highBp.id, ids);
        Assert.Contains(_normalBp.id, ids);
    }
}
