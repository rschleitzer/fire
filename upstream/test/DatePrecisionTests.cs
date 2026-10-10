using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestDateBoundary : TestBase
{
    private const string KnownBirthdate = "1993-06-15";

    private string _unique = null!;
    private Patient _patient = null!;

    public override async Task InitializeAsync()
    {
        _unique = $"DateBoundaryExact{Guid.NewGuid():N}";
        _patient = await CreatePatientAsync(Gen.Patient(family: _unique, birthDate: KnownBirthdate));
    }

    [Fact]
    public async Task EqExactDate()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate == "1993-06-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task NeExactDate()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate.Ne("1993-06-15"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task GtExactDateNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1993-06-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task LtExactDateNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1993-06-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task GeExactDate()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate >= "1993-06-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task LeExactDate()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate <= "1993-06-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task GtDayBefore()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1993-06-14")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task LtDayAfter()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1993-06-16")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task GtDayAfterNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1993-06-16")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task LtDayBeforeNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1993-06-14")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }
}

public class TestInstantBoundary : TestBase
{
    private const string KnownInstant = "2025-06-15T10:30:00Z";

    private string _unique = null!;
    private Patient _patient = null!;
    private Observation _observation = null!;

    public override async Task InitializeAsync()
    {
        _unique = $"InstantBoundary{Guid.NewGuid():N}";
        _patient = await CreatePatientAsync(Gen.Patient(family: _unique));
        _observation = await CreateObservationAsync(Gen.Observation(
            subject: Gen.Ref("Patient", _patient.id!),
            effectiveDateTime: KnownInstant));
    }

    [Fact]
    public async Task EqExactInstant()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date == "2025-06-15T10:30:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_observation.id, ids);
    }

    [Fact]
    public async Task NeExactInstant()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date.Ne("2025-06-15T10:30:00Z"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_observation.id, ids);
    }

    [Fact]
    public async Task GtExactInstantNoMatch()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date > "2025-06-15T10:30:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_observation.id, ids);
    }

    [Fact]
    public async Task LtExactInstantNoMatch()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date < "2025-06-15T10:30:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_observation.id, ids);
    }

    [Fact]
    public async Task GeExactInstant()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date >= "2025-06-15T10:30:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_observation.id, ids);
    }

    [Fact]
    public async Task LeExactInstant()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date <= "2025-06-15T10:30:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_observation.id, ids);
    }

    [Fact]
    public async Task GtOneSecondBefore()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date > "2025-06-15T10:29:59Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_observation.id, ids);
    }

    [Fact]
    public async Task LtOneSecondAfter()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date < "2025-06-15T10:30:01Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_observation.id, ids);
    }

    [Fact]
    public async Task GtOneSecondAfterNoMatch()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date > "2025-06-15T10:30:01Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_observation.id, ids);
    }

    [Fact]
    public async Task LtOneSecondBeforeNoMatch()
    {
        var bundle = await Client.Observation()
            .Where(o => o.Subject == $"Patient/{_patient.id}")
            .Where(o => o.Date < "2025-06-15T10:29:59Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_observation.id, ids);
    }
}

public class TestCrossPrecisionBoundary : TestBase
{
    private const string KnownBirthdate = "1993-06-15";

    private string _unique = null!;
    private Patient _patient = null!;

    public override async Task InitializeAsync()
    {
        _unique = $"CrossPrecision{Guid.NewGuid():N}";
        _patient = await CreatePatientAsync(Gen.Patient(family: _unique, birthDate: KnownBirthdate));
    }

    [Fact]
    public async Task GeRangeLow()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate >= "1993-06-14T22:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task LeRangeHigh()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate <= "1993-06-15T22:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_patient.id, ids);
    }

    [Fact]
    public async Task GtRangeHighNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1993-06-15T22:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task LtRangeLowNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1993-06-14T22:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task GtMidpointNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1993-06-15T10:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }

    [Fact]
    public async Task LtMidpointNoMatch()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1993-06-15T10:00:00Z")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.DoesNotContain(_patient.id, ids);
    }
}
