using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestPaginationBasic : TestBase
{
    private string _family = null!;

    public override async Task InitializeAsync()
    {
        _family = $"PagBasic{Guid.NewGuid():N}";
        for (var i = 0; i < 10; i++)
            await CreatePatientAsync(Gen.Patient(family: _family));
    }

    [Fact]
    public async Task DefaultPagination()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .ExecuteAsync();

        Assert.NotNull(bundle.total);
        Assert.True(bundle.total >= 10, $"Expected total >= 10, got {bundle.total}");
    }

    [Fact]
    public async Task CountLimitsEntries()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(3)
            .ExecuteAsync();

        var entries = bundle.entry ?? [];
        Assert.True(entries.Count <= 3, $"Expected at most 3 entries, got {entries.Count}");
        Assert.NotNull(bundle.total);
        Assert.True(bundle.total >= 10, $"Expected total >= 10, got {bundle.total}");
    }

    [Fact]
    public async Task NextLinkPresent()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(3)
            .ExecuteAsync();

        var links = bundle.link ?? [];
        var nextLink = links.FirstOrDefault(l => l.relation == "next");
        Assert.NotNull(nextLink);
        Assert.Contains(_family, nextLink.url);
    }

    [Fact]
    public async Task SelfLinkPresent()
    {
        var bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(3)
            .ExecuteAsync();

        var links = bundle.link ?? [];
        var selfLink = links.FirstOrDefault(l => l.relation == "self");
        Assert.NotNull(selfLink);
    }

    [Fact]
    public async Task FollowNextLink()
    {
        var bundle1 = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(3)
            .ExecuteAsync();

        var bundle2 = await Client.NextPageAsync(bundle1);
        Assert.NotNull(bundle2);
        Assert.Equal("Bundle", bundle2.resourceType);
        Assert.NotNull(bundle2.entry);
        Assert.NotEmpty(bundle2.entry);
    }
}

public class TestPaginationEdgeCases : TestBase
{
    [Fact]
    public async Task CountZero()
    {
        var bundle = await Client.Patient()
            .Count(0)
            .ExecuteAsync();

        Assert.NotNull(bundle.total);
        var entries = bundle.entry ?? [];
        Assert.Empty(entries);
    }

    [Fact]
    public async Task CountOne()
    {
        var family = $"PagOne{Guid.NewGuid():N}";
        for (var i = 0; i < 3; i++)
            await CreatePatientAsync(Gen.Patient(family: family));

        var bundle = await Client.Patient()
            .Where(p => p.Family == family)
            .Count(1)
            .ExecuteAsync();

        var entries = bundle.entry ?? [];
        Assert.Single(entries);
    }

    [Fact]
    public async Task CountLargerThanResults()
    {
        var family = $"PagFew{Guid.NewGuid():N}";
        await CreatePatientAsync(Gen.Patient(family: family));
        await CreatePatientAsync(Gen.Patient(family: family));

        var bundle = await Client.Patient()
            .Where(p => p.Family == family)
            .Count(100)
            .ExecuteAsync();

        Assert.Equal(2, bundle.total);
        var links = bundle.link ?? [];
        var nextLink = links.FirstOrDefault(l => l.relation == "next");
        Assert.Null(nextLink);
    }

    [Fact]
    public async Task Offset()
    {
        var family = $"PagOff{Guid.NewGuid():N}";
        for (var i = 0; i < 6; i++)
            await CreatePatientAsync(Gen.Patient(family: family));

        var bundle1 = await Client.Patient()
            .Where(p => p.Family == family)
            .Count(3)
            .Sort("_id")
            .ExecuteAsync();

        var bundle2 = await Client.Patient()
            .Where(p => p.Family == family)
            .Count(3)
            .Sort("_id")
            .Offset(3)
            .ExecuteAsync();

        var ids1 = MatchIds(bundle1);
        var ids2 = MatchIds(bundle2);

        Assert.NotEmpty(ids1);
        Assert.NotEmpty(ids2);
        Assert.Empty(ids1.Intersect(ids2));
    }
}

public class TestPaginationLarge : TestBase
{
    private string _family = null!;
    private int _created;

    public override async Task InitializeAsync()
    {
        _family = $"PagLg{Guid.NewGuid():N}";
        _created = 25;
        for (var i = 0; i < _created; i++)
            await CreatePatientAsync(Gen.Patient(family: _family));
    }

    [Fact]
    public async Task PaginateAllResults()
    {
        var allIds = new List<string>();
        Bundle? bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(5)
            .ExecuteAsync();

        var expectedTotal = bundle.total;
        Assert.NotNull(expectedTotal);

        var maxPages = 20;
        var pages = 0;

        while (bundle != null && pages < maxPages)
        {
            pages++;
            allIds.AddRange(MatchIds(bundle));
            bundle = await Client.NextPageAsync(bundle);
        }

        Assert.Equal(expectedTotal, allIds.Count);
    }

    [Fact]
    public async Task NoDuplicateAcrossPages()
    {
        var allIds = new List<string>();
        Bundle? bundle = await Client.Patient()
            .Where(p => p.Family == _family)
            .Count(5)
            .ExecuteAsync();

        var maxPages = 20;
        var pages = 0;

        while (bundle != null && pages < maxPages)
        {
            pages++;
            allIds.AddRange(MatchIds(bundle));
            bundle = await Client.NextPageAsync(bundle);
        }

        var uniqueIds = new HashSet<string>(allIds);
        Assert.Equal(allIds.Count, uniqueIds.Count);
    }
}
