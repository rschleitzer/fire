using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestReferenceSearch : TestBase
{
    [Fact]
    public async Task SearchByGeneralPractitioner()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"RefTest{uid}", given: "Doctor"));
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"RefPatient{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));

        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner == $"Practitioner/{prac.id}")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task SearchByGeneralPractitionerIdOnly()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"RefIdOnly{uid}", given: "Doctor"));
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"RefIdOnlyPat{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));

        // Search with just the ID (no type prefix)
        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner == prac.id!)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task ReferenceSearchNoMatch()
    {
        var nonExistentId = Guid.NewGuid().ToString();

        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner == $"Practitioner/{nonExistentId}")
            .ExecuteAsync();

        Assert.Equal(0, bundle.total);
        Assert.Empty(bundle.entry ?? []);
    }

    [Fact]
    public async Task ReferenceChainPractitionerFamily()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"Anderson{uid}", given: "John"));
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"ChainPat{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));
        var other = await CreatePatientAsync(Gen.Patient(family: $"ChainOther{uid}"));

        var bundle = await Client.Patient()
            .Where(p => p.GeneralPractitioner.Chain("family", $"Anderson{uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
        Assert.DoesNotContain(other.id, ids);
    }
}

public class TestMissingModifier : TestBase
{
    [Fact]
    public async Task SearchMissingTrue()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"MissDoc{uid}"));
        var withGp = await CreatePatientAsync(Gen.Patient(
            family: $"HasGP{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));
        var withoutGp = await CreatePatientAsync(Gen.Patient(
            family: $"NoGP{uid}"));

        // :missing=true should find patients WITHOUT general-practitioner
        var bundle = await Client.Patient()
            .Where(p => p.Family == $"NoGP{uid}")
            .Param("general-practitioner:missing", "true")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(withoutGp.id, ids);
        Assert.DoesNotContain(withGp.id, ids);
    }

    [Fact]
    public async Task SearchMissingFalse()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var prac = await CreatePractitionerAsync(
            Gen.Practitioner(family: $"MissDoc{uid}"));
        var withGp = await CreatePatientAsync(Gen.Patient(
            family: $"HasGP{uid}",
            generalPractitioner: [Gen.Ref("Practitioner", prac.id!)]));
        var withoutGp = await CreatePatientAsync(Gen.Patient(
            family: $"NoGP{uid}"));

        // :missing=false should find patients WITH general-practitioner
        var bundle = await Client.Patient()
            .Where(p => p.Family == $"HasGP{uid}")
            .Param("general-practitioner:missing", "false")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(withGp.id, ids);
        Assert.DoesNotContain(withoutGp.id, ids);
    }

    [Fact]
    public async Task SearchMissingGender()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var withGender = await CreatePatientAsync(Gen.Patient(
            family: $"HasGen{uid}", gender: "male"));
        // Build patient without gender directly — Gen.Patient defaults gender to "male"
        var withoutGender = await CreatePatientAsync(new Patient
        {
            resourceType = "Patient",
            identifier = [Gen.Id()],
            name = [new HumanName { family = $"NoGen{uid}", given = ["Test"] }],
            active = true,
            birthDate = "1990-01-01"
        });

        // :missing=true should find patients without gender set
        var bundle = await Client.Patient()
            .Where(p => p.Family == $"NoGen{uid}")
            .Param("gender:missing", "true")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(withoutGender.id, ids);
        Assert.DoesNotContain(withGender.id, ids);
    }
}

public class TestNotModifier : TestBase
{
    [Fact]
    public async Task SearchGenderNot()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var male = await CreatePatientAsync(Gen.Patient(
            family: $"NotMod{uid}", gender: "male"));
        var female = await CreatePatientAsync(Gen.Patient(
            family: $"NotMod{uid}", gender: "female"));

        var bundle = await Client.Patient()
            .Where(p => p.Family == $"NotMod{uid}")
            .Where(p => p.Gender.Not("male"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(female.id, ids);
        Assert.DoesNotContain(male.id, ids);
    }

    [Fact]
    public async Task SearchIdentifierNot()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var mrn1 = $"MRN-{Guid.NewGuid():N}";
        var mrn2 = $"MRN-{Guid.NewGuid():N}";
        var p1 = await CreatePatientAsync(Gen.Patient(
            family: $"NotId{uid}",
            identifier: [Gen.Id(value: mrn1)]));
        var p2 = await CreatePatientAsync(Gen.Patient(
            family: $"NotId{uid}",
            identifier: [Gen.Id(value: mrn2)]));

        var bundle = await Client.Patient()
            .Where(p => p.Family == $"NotId{uid}")
            .Where(p => p.Identifier.Not(mrn1))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(p2.id, ids);
        Assert.DoesNotContain(p1.id, ids);
    }
}

public class TestAddressSearch : TestBase
{
    [Fact]
    public async Task SearchByAddress()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"Addr{uid}",
            address: [new Address
            {
                line = [$"123 Main St {uid}"],
                city = $"Springfield{uid}",
                state = "IL",
                postalCode = $"62701{uid}"
            }]));

        var bundle = await Client.Patient()
            .Where(p => p.Address.Contains($"Springfield{uid}"))
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task SearchByAddressCity()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"City{uid}",
            address: [new Address
            {
                city = $"Springfield{uid}",
                state = "IL",
                postalCode = "62701"
            }]));

        var bundle = await Client.Patient()
            .Where(p => p.AddressCity == $"Springfield{uid}")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task SearchByAddressPostalcode()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"Zip{uid}",
            address: [new Address
            {
                city = "Springfield",
                state = "IL",
                postalCode = $"ZIP{uid}"
            }]));

        var bundle = await Client.Patient()
            .Where(p => p.AddressPostalcode == $"ZIP{uid}")
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }
}

public class TestTelecomSearch : TestBase
{
    [Fact]
    public async Task SearchByPhone()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var phone = $"555-{uid}";
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"Phone{uid}",
            telecom: [new ContactPoint { system = "phone", value = phone, use = "home" }]));

        var bundle = await Client.Patient()
            .Where(p => p.Phone == phone)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task SearchByEmail()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var email = $"test{uid}@example.com";
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"Email{uid}",
            telecom: [new ContactPoint { system = "email", value = email, use = "work" }]));

        var bundle = await Client.Patient()
            .Where(p => p.Email == email)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }

    [Fact]
    public async Task SearchByTelecom()
    {
        var uid = Guid.NewGuid().ToString("N")[..8];
        var phone = $"555-tel-{uid}";
        var patient = await CreatePatientAsync(Gen.Patient(
            family: $"Telecom{uid}",
            telecom: [new ContactPoint { system = "phone", value = phone, use = "mobile" }]));

        // General telecom search should match any contact point
        var bundle = await Client.Patient()
            .Where(p => p.Telecom == phone)
            .ExecuteAsync();

        var ids = MatchIds(bundle);
        Assert.Contains(patient.id, ids);
    }
}
