using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestPatientSearchBasic : TestBase
{
    [Fact]
    public async Task SearchByFamily()
    {
        var unique = $"Fam{Guid.NewGuid():N}";
        await CreatePatientAsync(Gen.Patient(family: unique));
        await CreatePatientAsync(Gen.Patient(family: unique));
        await CreatePatientAsync(Gen.Patient(family: "Other" + unique));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Equal(2, ids.Count);
    }

    [Fact]
    public async Task SearchByGiven()
    {
        var unique = $"Giv{Guid.NewGuid():N}";
        var created = await CreatePatientAsync(Gen.Patient(given: unique));

        var bundle = await Client.Patient()
            .Where(p => p.Given == unique)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(created.id, ids);
    }

    [Fact]
    public async Task SearchByGender()
    {
        var unique = $"Gen{Guid.NewGuid():N}";
        var male = await CreatePatientAsync(Gen.Patient(family: unique, gender: "male"));
        var female = await CreatePatientAsync(Gen.Patient(family: unique, gender: "female"));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Where(p => p.Gender == "male")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(male.id, ids);
        Assert.DoesNotContain(female.id, ids);
    }

    [Fact]
    public async Task SearchByBirthdate()
    {
        var unique = $"Bdt{Guid.NewGuid():N}";
        var p1 = await CreatePatientAsync(Gen.Patient(family: unique, birthDate: "1980-05-15"));
        var p2 = await CreatePatientAsync(Gen.Patient(family: unique, birthDate: "1990-01-01"));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Where(p => p.Birthdate == "1980-05-15")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(p1.id, ids);
        Assert.DoesNotContain(p2.id, ids);
    }

    [Fact]
    public async Task SearchByIdentifierWithSystem()
    {
        var mrn = $"MRN-{Guid.NewGuid():N}";
        var created = await CreatePatientAsync(Gen.Patient(
            identifier: [Gen.Id(system: "http://hospital.org/mrn", value: mrn)]));

        var bundle = await Client.Patient()
            .Where(p => p.Identifier == ("http://hospital.org/mrn", mrn))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(created.id, ids);
    }

    [Fact]
    public async Task SearchByIdentifierValueOnly()
    {
        var mrn = $"MRN-{Guid.NewGuid():N}";
        var created = await CreatePatientAsync(Gen.Patient(
            identifier: [Gen.Id(system: "http://hospital.org/mrn", value: mrn)]));

        var bundle = await Client.Patient()
            .Where(p => p.Identifier == mrn)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(created.id, ids);
    }

    [Fact]
    public async Task SearchByActive()
    {
        var unique = $"Act{Guid.NewGuid():N}";
        var active = await CreatePatientAsync(Gen.Patient(family: unique, active: true));
        var inactive = await CreatePatientAsync(Gen.Patient(family: unique, active: false));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Where(p => p.Active == "true")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(active.id, ids);
        Assert.DoesNotContain(inactive.id, ids);
    }

    [Fact]
    public async Task SearchByName()
    {
        var unique = $"Nam{Guid.NewGuid():N}";
        var p1 = await CreatePatientAsync(Gen.Patient(family: unique, given: "John"));
        var p2 = await CreatePatientAsync(Gen.Patient(family: "Other", given: unique));

        // name search param matches both family and given
        var bundle = await Client.Patient()
            .Where(p => p.Name == unique)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(p1.id, ids);
        Assert.Contains(p2.id, ids);
    }
}

public class TestPatientSearchDatePrefixes : TestBase
{
    private string _unique = null!;
    private Patient _early = null!;   // 1975-03-10
    private Patient _middle = null!;  // 1985-08-20
    private Patient _late = null!;    // 1995-12-01

    public override async Task InitializeAsync()
    {
        _unique = $"Dp{Guid.NewGuid():N}";
        _early = await CreatePatientAsync(Gen.Patient(family: _unique, birthDate: "1975-03-10"));
        _middle = await CreatePatientAsync(Gen.Patient(family: _unique, birthDate: "1985-08-20"));
        _late = await CreatePatientAsync(Gen.Patient(family: _unique, birthDate: "1995-12-01"));
    }

    [Fact]
    public async Task SearchBirthdateGt()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate > "1990-01-01")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_late.id, ids);
        Assert.DoesNotContain(_middle.id, ids);
        Assert.DoesNotContain(_early.id, ids);
    }

    [Fact]
    public async Task SearchBirthdateLt()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate < "1980-01-01")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_early.id, ids);
        Assert.DoesNotContain(_middle.id, ids);
        Assert.DoesNotContain(_late.id, ids);
    }

    [Fact]
    public async Task SearchBirthdateGe()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate >= "1985-08-20")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_middle.id, ids);
        Assert.Contains(_late.id, ids);
        Assert.DoesNotContain(_early.id, ids);
    }

    [Fact]
    public async Task SearchBirthdateLe()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate <= "1985-08-20")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_early.id, ids);
        Assert.Contains(_middle.id, ids);
        Assert.DoesNotContain(_late.id, ids);
    }

    [Fact]
    public async Task SearchBirthdateNe()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _unique)
            .Where(p => p.Birthdate.Ne("1985-08-20"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(_early.id, ids);
        Assert.Contains(_late.id, ids);
        Assert.DoesNotContain(_middle.id, ids);
    }
}

public class TestPatientSearchModifiers : TestBase
{
    [Fact]
    public async Task SearchFamilyExact()
    {
        var unique = $"Exsmith{Guid.NewGuid():N}";
        var exact = await CreatePatientAsync(Gen.Patient(family: unique));
        var partial = await CreatePatientAsync(Gen.Patient(family: unique + "son"));

        // Default string search is starts-with, so both should match
        var bundleDefault = await Client.Patient()
            .Where(p => p.Family == unique)
            .ExecuteAsync();
        var idsDefault = MatchIds(bundleDefault);
        Assert.Contains(exact.id, idsDefault);
        Assert.Contains(partial.id, idsDefault);

        // Exact modifier should only match the exact value
        var bundleExact = await Client.Patient()
            .Where(p => p.Family.Exact(unique))
            .ExecuteAsync();
        var idsExact = MatchIds(bundleExact);
        Assert.Contains(exact.id, idsExact);
        Assert.DoesNotContain(partial.id, idsExact);
    }

    [Fact]
    public async Task SearchFamilyContains()
    {
        var unique = $"Cn{Guid.NewGuid():N}";
        var created = await CreatePatientAsync(Gen.Patient(family: "Pre" + unique + "Post"));

        var bundle = await Client.Patient()
            .Where(p => p.Family.Contains(unique))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(created.id, ids);
    }

    [Fact]
    public async Task SearchIdentifierNot()
    {
        var unique = $"Not{Guid.NewGuid():N}";
        var mrn1 = $"MRN-{Guid.NewGuid():N}";
        var mrn2 = $"MRN-{Guid.NewGuid():N}";
        var p1 = await CreatePatientAsync(Gen.Patient(family: unique,
            identifier: [Gen.Id(value: mrn1)]));
        var p2 = await CreatePatientAsync(Gen.Patient(family: unique,
            identifier: [Gen.Id(value: mrn2)]));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Where(p => p.Identifier.Not(mrn1))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(p2.id, ids);
        Assert.DoesNotContain(p1.id, ids);
    }

    [Fact]
    public async Task SearchMultipleCriteriaAnd()
    {
        var unique = $"And{Guid.NewGuid():N}";
        var match = await CreatePatientAsync(Gen.Patient(
            family: unique, gender: "female", active: true));
        var noMatch1 = await CreatePatientAsync(Gen.Patient(
            family: unique, gender: "male", active: true));
        var noMatch2 = await CreatePatientAsync(Gen.Patient(
            family: unique, gender: "female", active: false));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Where(p => p.Gender == "female")
            .Where(p => p.Active == "true")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(match.id, ids);
        Assert.DoesNotContain(noMatch1.id, ids);
        Assert.DoesNotContain(noMatch2.id, ids);
    }
}

public class TestPatientSearchResultControl : TestBase
{
    [Fact]
    public async Task SearchWithCount()
    {
        var unique = $"Cnt{Guid.NewGuid():N}";
        for (var i = 0; i < 5; i++)
            await CreatePatientAsync(Gen.Patient(family: unique));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Count(2)
            .ExecuteAsync();

        var entries = bundle.entry ?? [];
        Assert.True(entries.Count <= 2, "Should return at most 2 entries");
        Assert.True(bundle.total >= 5, "Total should reflect all matches");
    }

    [Fact]
    public async Task SearchWithSort()
    {
        var unique = $"Srt{Guid.NewGuid():N}";
        await CreatePatientAsync(Gen.Patient(family: unique + "C", birthDate: "1970-01-01"));
        await CreatePatientAsync(Gen.Patient(family: unique + "A", birthDate: "1990-01-01"));
        await CreatePatientAsync(Gen.Patient(family: unique + "B", birthDate: "1980-01-01"));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .Sort("family")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.True(ids.Count >= 3);

        var families = bundle.entry!
            .Where(e => e.search?.mode is null or "match")
            .Select(e => (e.resource as Patient)?.name?.FirstOrDefault()?.family)
            .Where(f => f != null)
            .ToList();

        Assert.Equal(families.OrderBy(f => f, StringComparer.Ordinal).ToList(), families);
    }

    [Fact]
    public async Task SearchWithTotal()
    {
        var unique = $"Tot{Guid.NewGuid():N}";
        await CreatePatientAsync(Gen.Patient(family: unique));
        await CreatePatientAsync(Gen.Patient(family: unique));
        await CreatePatientAsync(Gen.Patient(family: unique));

        var bundle = await Client.Patient()
            .Where(p => p.Family == unique)
            .ExecuteAsync();

        Assert.Equal(3, bundle.total);
        Assert.Equal(3, MatchIds(bundle).Count);
    }

    [Fact]
    public async Task SearchNoResults()
    {
        var nonexistent = $"NoExist{Guid.NewGuid():N}";

        var bundle = await Client.Patient()
            .Where(p => p.Family == nonexistent)
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);
        Assert.Empty(bundle.entry ?? []);
    }
}
