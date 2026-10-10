using Fire.Model;

namespace Fire.Test;

public class TestPractitionerSearchBasic : TestBase
{
    [Fact]
    public async Task SearchByFamily()
    {
        var family = $"Thornberry-{Guid.NewGuid():N}";
        await CreatePractitionerAsync(Gen.Practitioner(family: family));

        var bundle = await Client.Practitioner()
            .Where(p => p.Family == family)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        Assert.Contains(family, MatchIds(bundle).Select(id =>
            bundle.entry!.First(e => e.resource!.id == id).resource)
            .Cast<Practitioner>()
            .SelectMany(p => p.name!)
            .Select(n => n.family));
    }

    [Fact]
    public async Task SearchByGiven()
    {
        var given = $"Xiomara-{Guid.NewGuid():N}";
        await CreatePractitionerAsync(Gen.Practitioner(given: given));

        var bundle = await Client.Practitioner()
            .Where(p => p.Given == given)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        var matchedResources = bundle.entry!
            .Where(e => e.search?.mode is null or "match")
            .Select(e => (Practitioner)e.resource!)
            .ToList();
        Assert.Contains(matchedResources, pr =>
            pr.name!.Any(n => n.given != null && n.given.Contains(given)));
    }

    [Fact]
    public async Task SearchByName()
    {
        var family = $"Waverly-{Guid.NewGuid():N}";
        var given = $"Quinton-{Guid.NewGuid():N}";
        await CreatePractitionerAsync(Gen.Practitioner(family: family, given: given));

        // name param should match family
        var bundleByFamily = await Client.Practitioner()
            .Where(p => p.Name == family)
            .ExecuteAsync();
        Assert.True(bundleByFamily.total >= 1);

        // name param should match given
        var bundleByGiven = await Client.Practitioner()
            .Where(p => p.Name == given)
            .ExecuteAsync();
        Assert.True(bundleByGiven.total >= 1);
    }

    [Fact]
    public async Task SearchByIdentifier()
    {
        var system = "http://test.org/npi";
        var value = $"NPI-{Guid.NewGuid():N}";
        var practitioner = Gen.Practitioner(
            identifier: [new Identifier { system = system, value = value }]);
        var created = await CreatePractitionerAsync(practitioner);

        var bundle = await Client.Practitioner()
            .Where(p => p.Identifier == (system, value))
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        Assert.Contains(created.id, MatchIds(bundle));
    }

    [Fact]
    public async Task SearchFamilyPrefix()
    {
        var unique = Guid.NewGuid().ToString("N")[..8];
        var family = $"Anderson{unique}";
        var created = await CreatePractitionerAsync(Gen.Practitioner(family: family));

        // Default string search is starts-with: "Anderson" should match "Anderson{unique}"
        var bundle = await Client.Practitioner()
            .Where(p => p.Family == "Anderson")
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        Assert.Contains(created.id, MatchIds(bundle));
    }

    [Fact]
    public async Task SearchFamilyExact()
    {
        var unique = Guid.NewGuid().ToString("N")[..8];
        var family = $"Anderson{unique}";
        await CreatePractitionerAsync(Gen.Practitioner(family: family));

        // Exact search for a substring should NOT match the full name
        var bundle = await Client.Practitioner()
            .Where(p => p.Family.Exact($"Ander{unique}"))
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);

        // Exact search for the full name should match
        var bundleExact = await Client.Practitioner()
            .Where(p => p.Family.Exact(family))
            .ExecuteAsync();

        Assert.True(bundleExact.total >= 1);
        Assert.Contains(bundleExact.entry!
            .Where(e => e.search?.mode is null or "match")
            .Select(e => (Practitioner)e.resource!),
            pr => pr.name!.Any(n => n.family == family));
    }
}

public class TestPractitionerSearchAdvanced : TestBase
{
    [Fact]
    public async Task SearchWithMultipleNames()
    {
        var officialFamily = $"Westbrook-{Guid.NewGuid():N}";
        var oldFamily = $"Eastwood-{Guid.NewGuid():N}";
        var practitioner = Gen.Practitioner(
            name:
            [
                new HumanName { use = "official", family = officialFamily, given = ["Current"] },
                new HumanName { use = "old", family = oldFamily, given = ["Former"] }
            ]);
        var created = await CreatePractitionerAsync(practitioner);

        // Search by old/former name should find the practitioner
        var bundle = await Client.Practitioner()
            .Where(p => p.Family == oldFamily)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        Assert.Contains(created.id, MatchIds(bundle));
    }

    [Fact]
    public async Task SearchFamilyContains()
    {
        var unique = Guid.NewGuid().ToString("N")[..8];
        var family = $"Anderson{unique}";
        await CreatePractitionerAsync(Gen.Practitioner(family: family));

        // contains "derson" should match "Anderson{unique}"
        var bundle = await Client.Practitioner()
            .Where(p => p.Family.Contains($"derson{unique}"))
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        var matchedResources = bundle.entry!
            .Where(e => e.search?.mode is null or "match")
            .Select(e => (Practitioner)e.resource!)
            .ToList();
        Assert.Contains(matchedResources, pr =>
            pr.name!.Any(n => n.family == family));
    }

    [Fact]
    public async Task SearchMultipleCriteria()
    {
        var family = $"Pemberton-{Guid.NewGuid():N}";
        var given = $"Reginald-{Guid.NewGuid():N}";
        var created = await CreatePractitionerAsync(
            Gen.Practitioner(family: family, given: given));

        // Search with both family AND given
        var bundle = await Client.Practitioner()
            .Where(p => p.Family == family)
            .Where(p => p.Given == given)
            .ExecuteAsync();

        Assert.True(bundle.total >= 1);
        Assert.Contains(created.id, MatchIds(bundle));
    }

    [Fact]
    public async Task SearchNoResults()
    {
        var nonExistent = $"Zzzyxwv-{Guid.NewGuid():N}";

        var bundle = await Client.Practitioner()
            .Where(p => p.Family == nonExistent)
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);
        Assert.True(bundle.entry is null || bundle.entry.Count == 0);
    }
}
