using Fire.Model;

namespace Fire.Test;

public static class Gen
{
    private static int _counter;
    private static string UniqueId() => $"{Interlocked.Increment(ref _counter)}-{Guid.NewGuid():N}";

    public static Patient Patient(
        string? family = null,
        string? given = null,
        string? gender = null,
        string? birthDate = null,
        bool? active = null,
        List<Identifier>? identifier = null,
        List<HumanName>? name = null,
        List<Address>? address = null,
        List<ContactPoint>? telecom = null,
        List<Reference>? generalPractitioner = null,
        string? id = null)
    {
        return new Patient
        {
            resourceType = "Patient",
            id = id,
            identifier = identifier ?? [new Identifier { system = "http://hospital.org/mrn", value = UniqueId() }],
            name = name ?? [new HumanName { family = family ?? "Test", given = given != null ? [given] : ["John"] }],
            gender = gender ?? "male",
            birthDate = birthDate ?? "1990-01-01",
            active = active ?? true,
            address = address,
            telecom = telecom,
            generalPractitioner = generalPractitioner
        };
    }

    public static Practitioner Practitioner(
        string? family = null,
        string? given = null,
        List<Identifier>? identifier = null,
        List<HumanName>? name = null)
    {
        return new Practitioner
        {
            resourceType = "Practitioner",
            identifier = identifier ?? [new Identifier { system = "http://hl7.org/fhir/sid/us-npi", value = UniqueId() }],
            name = name ?? [new HumanName { family = family ?? "Doctor", given = given != null ? [given] : ["Jane"] }],
            active = true
        };
    }

    public static Observation Observation(
        Reference? subject = null,
        string codeSystem = "http://loinc.org",
        string code = "8867-4",
        string codeDisplay = "Heart rate",
        Quantity? valueQuantity = null,
        CodeableConcept? valueCodeableConcept = null,
        string? valueString = null,
        string? effectiveDateTime = null,
        string? status = "final",
        List<CodeableConcept>? category = null,
        List<Observation.Component>? component = null)
    {
        var obs = new Observation
        {
            resourceType = "Observation",
            status = status,
            category = category ?? [VitalSignsCategory()],
            code = new CodeableConcept
            {
                coding = [new Coding { system = codeSystem, code = code, display = codeDisplay }]
            },
            subject = subject,
            effectiveDateTime = effectiveDateTime ?? DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ"),
            component = component
        };

        if (valueCodeableConcept != null)
            obs.valueCodeableConcept = valueCodeableConcept;
        else if (valueString != null)
            obs.valueString = valueString;
        else
            obs.valueQuantity = valueQuantity ?? new Quantity
            {
                value = 72, unit = "beats/minute",
                system = "http://unitsofmeasure.org", code = "/min"
            };

        return obs;
    }

    public static Reference Ref(string resourceType, string id, string? display = null)
        => new() { reference = $"{resourceType}/{id}", display = display };

    public static Identifier Id(string? system = null, string? value = null)
        => new() { system = system ?? "http://hospital.org/mrn", value = value ?? UniqueId() };

    public static CodeableConcept VitalSignsCategory() => new()
    {
        coding = [new Coding
        {
            system = "http://terminology.hl7.org/CodeSystem/observation-category",
            code = "vital-signs",
            display = "Vital Signs"
        }]
    };

    public static CodeableConcept Code(string system, string code, string? display = null) => new()
    {
        coding = [new Coding { system = system, code = code, display = display }]
    };

    public static Quantity Qty(decimal value, string unit, string? system = null, string? code = null) => new()
    {
        value = value, unit = unit, system = system, code = code
    };
}
