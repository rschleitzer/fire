// This tool reads FHIR R5 JSON definitions and outputs an XML model

using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text;

// ============================================================================
// XML Data Structures (output model)
// ============================================================================

class FhirModel
{
    public string Name { get; set; } = "";
    public string Version { get; set; } = "";
    public List<Resource> Resources { get; set; } = new();
    public List<Element> Elements { get; set; } = new();
    public List<Codeset> Codesets { get; set; } = new();
}

class Resource
{
    public string Id { get; set; } = "";
    public string Name { get; set; } = "";
    public string? Active { get; set; }
    public string Description { get; set; } = "";
    public List<Property> Properties { get; set; } = new();
    public List<Element> Elements { get; set; } = new();
    public List<Codeset> Codesets { get; set; } = new();
    public List<Search> Searches { get; set; } = new();
    public List<Operation> Operations { get; set; } = new();
}

class Element
{
    public string Id { get; set; } = "";
    public string Name { get; set; } = "";
    public string? Explicit { get; set; }
    public string? Backbone { get; set; }
    public string Description { get; set; } = "";
    public List<Property> Properties { get; set; } = new();
    public List<Element> Elements { get; set; } = new();
    public List<Codeset> Codesets { get; set; } = new();
}

class Property
{
    public string Id { get; set; } = "";
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public string? TypeAttr { get; set; }
    public string? RefAttr { get; set; }
    public string? IsCollection { get; set; }
    public string? NotNull { get; set; }
    public string? Summary { get; set; }
    public string? Modifier { get; set; }
    public List<Target>? Targets { get; set; }
    public List<Variant> Variants { get; set; } = new();
}

class Variant
{
    public string? TypeAttr { get; set; }
    public string? RefAttr { get; set; }
    public List<Target>? Targets { get; set; }
}

class Target
{
    public string Resource { get; set; } = "";
}

class Codeset
{
    public string Id { get; set; } = "";
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public List<Code> Codes { get; set; } = new();
}

class Code
{
    public string Name { get; set; } = "";
    public string Value { get; set; } = "";
    public string Description { get; set; } = "";
}

class Search
{
    public string Name { get; set; } = "";
    public string Query { get; set; } = "";
    public string SearchType { get; set; } = "";
    public string? Reference { get; set; }
    public List<SearchPath> Paths { get; set; } = new();
}

class SearchPath
{
    public List<Part> Parts { get; set; } = new();
    public List<Cast>? Casts { get; set; }
    public List<SearchComponent>? Components { get; set; }
    public List<Target>? Targets { get; set; }
}

class Part
{
    public string RefAttr { get; set; } = "";
    public string? Resolve { get; set; }
    public string? Property { get; set; }
    public string? TypeAttr { get; set; }
    public string? First { get; set; }
    public string? NotNull { get; set; }
    public string? Variant { get; set; }
    public string? Op { get; set; }
    public string? Value { get; set; }
}

class Cast
{
    public string To { get; set; } = "";
}

class SearchComponent
{
    public string RefAttr { get; set; } = "";
    public List<Cast>? Casts { get; set; }
}

class Operation
{
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public List<Parameter> Input { get; set; } = new();
    public List<Parameter> Output { get; set; } = new();
}

class Parameter
{
    public string Name { get; set; } = "";
    public string Description { get; set; } = "";
    public string? TypeAttr { get; set; }
    public string? Reference { get; set; }
    public string? IsCollection { get; set; }
    public string? NotNull { get; set; }
}

// ============================================================================
// FHIR JSON Input Structures
// ============================================================================

class Bundle
{
    [JsonPropertyName("entry")]
    public List<BundleEntry> Entry { get; set; } = new();
}

class BundleEntry
{
    [JsonPropertyName("resource")]
    public JsonElement Resource { get; set; }
}

class StructureDefinition
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";
    [JsonPropertyName("snapshot")]
    public SnapshotComponent Snapshot { get; set; } = new();
    [JsonPropertyName("baseDefinition")]
    public string? BaseDefinition { get; set; }
}

class SnapshotComponent
{
    [JsonPropertyName("element")]
    public List<ElementDefinition> Element { get; set; } = new();
}

class ElementDefinition
{
    [JsonPropertyName("id")]
    public string Id { get; set; } = "";
    [JsonPropertyName("definition")]
    public string? Definition { get; set; }
    [JsonPropertyName("min")]
    public int? Min { get; set; }
    [JsonPropertyName("max")]
    public string? Max { get; set; }
    [JsonPropertyName("isSummary")]
    public bool? IsSummary { get; set; }
    [JsonPropertyName("isModifier")]
    public bool? IsModifier { get; set; }
    [JsonPropertyName("type")]
    public List<ElementType> Type { get; set; } = new();
    [JsonPropertyName("contentReference")]
    public string? ContentReference { get; set; }
    [JsonPropertyName("binding")]
    public ElementBinding? Binding { get; set; }
    [JsonPropertyName("extension")]
    public List<ElementExtension>? Extension { get; set; }
}

class ElementType
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
    [JsonPropertyName("targetProfile")]
    public List<string>? TargetProfile { get; set; }
}

class ElementBinding
{
    [JsonPropertyName("strength")]
    public string? Strength { get; set; }
    [JsonPropertyName("valueSet")]
    public string? ValueSet { get; set; }
}

class ElementExtension
{
    [JsonPropertyName("url")]
    public string? Url { get; set; }
    [JsonPropertyName("valueString")]
    public string? ValueString { get; set; }
}

class SearchParameterDef
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";
    [JsonPropertyName("type")]
    public string Type { get; set; } = "";
    [JsonPropertyName("expression")]
    public string? Expression { get; set; }
    [JsonPropertyName("base")]
    public List<string> Base { get; set; } = new();
    [JsonPropertyName("target")]
    public List<string>? Target { get; set; }
    [JsonPropertyName("component")]
    public List<SearchParameterComponent>? Component { get; set; }
    [JsonPropertyName("version")]
    public string? Version { get; set; }
}

class SearchParameterComponent
{
    [JsonPropertyName("expression")]
    public string Expression { get; set; } = "";
}

class OperationDefinitionDef
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    [JsonPropertyName("resource")]
    public List<string>? Resource { get; set; }
    [JsonPropertyName("parameter")]
    public List<OperationParameterDef>? Parameter { get; set; }
}

class OperationParameterDef
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";
    [JsonPropertyName("use")]
    public string Use { get; set; } = "";
    [JsonPropertyName("min")]
    public int Min { get; set; }
    [JsonPropertyName("max")]
    public string Max { get; set; } = "";
    [JsonPropertyName("documentation")]
    public string? Documentation { get; set; }
    [JsonPropertyName("type")]
    public string? Type { get; set; }
}

class ValueSetDef
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";
    [JsonPropertyName("url")]
    public string Url { get; set; } = "";
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    [JsonPropertyName("compose")]
    public ValueSetCompose Compose { get; set; } = new();
}

class ValueSetCompose
{
    [JsonPropertyName("include")]
    public List<ValueSetInclude> Include { get; set; } = new();
}

class ValueSetInclude
{
    [JsonPropertyName("system")]
    public string? System { get; set; }
    [JsonPropertyName("concept")]
    public List<ValueSetConcept>? Concept { get; set; }
}

class ValueSetConcept
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
    [JsonPropertyName("extension")]
    public List<ValueSetConceptExtension>? Extension { get; set; }
}

class ValueSetConceptExtension
{
    [JsonPropertyName("url")]
    public string Url { get; set; } = "";
    [JsonPropertyName("valueString")]
    public string? ValueString { get; set; }
}

class CodeSystemDef
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = "";
    [JsonPropertyName("url")]
    public string Url { get; set; } = "";
    [JsonPropertyName("concept")]
    public List<CodeSystemConcept>? Concept { get; set; }
}

class CodeSystemConcept
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
    [JsonPropertyName("definition")]
    public string? Definition { get; set; }
    [JsonPropertyName("property")]
    public List<CodeSystemConceptProperty>? Property { get; set; }
    [JsonPropertyName("concept")]
    public List<CodeSystemConcept>? Concept { get; set; }
}

class CodeSystemConceptProperty
{
    [JsonPropertyName("code")]
    public string Code { get; set; } = "";
}

// ============================================================================
// Main Program
// ============================================================================

class Program
{
    static void Main()
    {
        Console.WriteLine("Modeler - FHIR R5 to XML converter (C# port)");

        // Load FHIR R5 JSON files
        var basePath = FindBasePath();
        var profilesResourcesJson = File.ReadAllText(Path.Combine(basePath, "fhir-r5/profiles-resources.json"));
        var profilesTypesJson = File.ReadAllText(Path.Combine(basePath, "fhir-r5/profiles-types.json"));
        var searchParametersJson = File.ReadAllText(Path.Combine(basePath, "fhir-r5/search-parameters.json"));
        var valuesetsJson = File.ReadAllText(Path.Combine(basePath, "fhir-r5/valuesets.json"));

        Console.WriteLine("Parsing JSON files...");
        var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = false };
        var resourcesBundle = JsonSerializer.Deserialize<Bundle>(profilesResourcesJson, options)!;
        var typesBundle = JsonSerializer.Deserialize<Bundle>(profilesTypesJson, options)!;
        var searchBundle = JsonSerializer.Deserialize<Bundle>(searchParametersJson, options)!;
        var valuesetsBundle = JsonSerializer.Deserialize<Bundle>(valuesetsJson, options)!;

        // Extract specific resource types via discriminator
        var structureDefs = new List<StructureDefinition>();
        var operationDefs = new List<OperationDefinitionDef>();

        foreach (var entry in resourcesBundle.Entry)
        {
            var resourceType = entry.Resource.GetProperty("resourceType").GetString();
            if (resourceType == "StructureDefinition")
            {
                var sd = entry.Resource.Deserialize<StructureDefinition>(options)!;
                if (IsResource(sd.Snapshot.Element[0].Id))
                    structureDefs.Add(sd);
            }
            else if (resourceType == "OperationDefinition")
            {
                operationDefs.Add(entry.Resource.Deserialize<OperationDefinitionDef>(options)!);
            }
        }

        var elementDefs = new List<StructureDefinition>();
        foreach (var entry in typesBundle.Entry)
        {
            var resourceType = entry.Resource.GetProperty("resourceType").GetString();
            if (resourceType == "StructureDefinition")
            {
                var sd = entry.Resource.Deserialize<StructureDefinition>(options)!;
                if (IsDataType(sd.Snapshot.Element[0].Id))
                    elementDefs.Add(sd);
            }
        }

        var searchParams = new List<SearchParameterDef>();
        foreach (var entry in searchBundle.Entry)
        {
            var resourceType = entry.Resource.GetProperty("resourceType").GetString();
            if (resourceType == "SearchParameter")
                searchParams.Add(entry.Resource.Deserialize<SearchParameterDef>(options)!);
        }

        var valueSets = new List<ValueSetDef>();
        var codeSystems = new List<CodeSystemDef>();
        foreach (var entry in valuesetsBundle.Entry)
        {
            var resourceType = entry.Resource.GetProperty("resourceType").GetString();
            if (resourceType == "ValueSet")
                valueSets.Add(entry.Resource.Deserialize<ValueSetDef>(options)!);
            else if (resourceType == "CodeSystem")
                codeSystems.Add(entry.Resource.Deserialize<CodeSystemDef>(options)!);
        }

        var dictValueSets = valueSets.ToDictionary(vs => vs.Url);
        var dictCodeSystems = codeSystems.ToDictionary(cs => cs.Url);

        Console.WriteLine("Building model...");
        var model = new FhirModel { Name = "Fire", Version = "5.0.0" };

        var dictElements = new Dictionary<string, List<Element>>();
        var dictProperties = new Dictionary<string, List<Property>>();
        var dictProperty = new Dictionary<string, Property>();

        // Process resources
        foreach (var sd in structureDefs)
        {
            var snapshot = sd.Snapshot;
            var currentElementName = snapshot.Element[0].Id;

            string? active = null;
            if (IsActive(currentElementName)) active = "true";
            else if (IsModel(currentElementName)) active = "model";

            var resource = new Resource
            {
                Id = currentElementName.ToLower(),
                Active = active,
                Name = currentElementName,
                Description = CleanDescription(snapshot.Element[0].Definition),
            };

            dictElements[resource.Id] = new List<Element>();
            dictProperties[resource.Id] = new List<Property>();
            model.Resources.Add(resource);

            foreach (var elemDef in snapshot.Element)
                HandleElement(elemDef, dictValueSets, dictCodeSystems, dictElements, dictProperties, dictProperty, model);
        }

        // Process data types
        foreach (var sd in elementDefs)
        {
            var snapshot = sd.Snapshot;
            var currentElementName = snapshot.Element[0].Id;

            if (model.Elements.Any(e => e.Name == currentElementName))
                continue;

            var isBackbone = sd.BaseDefinition == "http://hl7.org/fhir/StructureDefinition/BackboneElement";

            var element = new Element
            {
                Id = currentElementName.ToLower(),
                Name = currentElementName,
                Backbone = isBackbone ? "true" : null,
                Description = CleanDescription(snapshot.Element[0].Definition)?.Replace("\u2026", "...") ?? "",
            };

            dictElements[element.Id] = new List<Element>();
            dictProperties[element.Id] = new List<Property>();
            model.Elements.Add(element);

            foreach (var elemDef in snapshot.Element)
                HandleElement(elemDef, dictValueSets, dictCodeSystems, dictElements, dictProperties, dictProperty, model);
        }

        Console.WriteLine("Copying properties and elements into model...");
        CopyDictToModel(dictElements, dictProperties, model);

        Console.WriteLine("Building searches and operations...");
        BuildSearches(searchParams, dictProperty, model);
        BuildOperations(operationDefs, model);

        Console.WriteLine("Normalizing model for DTD compliance...");
        NormalizeModel(model);

        Console.WriteLine("Writing XML output...");
        WriteXmlOutput(model, basePath);

        Console.WriteLine("Done!");
    }

    static string FindBasePath()
    {
        // Walk up from current directory looking for fire.dtd
        var dir = Directory.GetCurrentDirectory();
        while (dir != null)
        {
            if (File.Exists(Path.Combine(dir, "fire.dtd")))
                return dir;
            dir = Directory.GetParent(dir)?.FullName;
        }
        // Fallback: try two levels up from the tool location
        var toolDir = AppContext.BaseDirectory;
        var candidate = Path.GetFullPath(Path.Combine(toolDir, "..", "..", "..", "..", ".."));
        if (File.Exists(Path.Combine(candidate, "fire.dtd")))
            return candidate;
        throw new Exception("Cannot find project root (looking for fire.dtd)");
    }

    // ========================================================================
    // Helper predicates (matching Rust exactly)
    // ========================================================================

    static readonly HashSet<string> ResourceNames = new()
    {
        "Resource", "Account", "ActivityDefinition", "ActorDefinition",
        "AdministrableProductDefinition", "AdverseEvent", "AllergyIntolerance",
        "Appointment", "AppointmentResponse", "ArtifactAssessment", "AuditEvent",
        "Basic", "Binary", "BiologicallyDerivedProduct",
        "BiologicallyDerivedProductDispense", "BodyStructure", "Bundle",
        "CapabilityStatement", "CarePlan", "CareTeam", "CatalogEntry",
        "ChargeItem", "ChargeItemDefinition", "Citation", "Claim",
        "ClaimResponse", "ClinicalImpression", "ClinicalUseDefinition",
        "CodeSystem", "Communication", "CommunicationRequest",
        "CompartmentDefinition", "Composition", "ConceptMap", "Condition",
        "ConditionDefinition", "Consent", "Contract", "Coverage",
        "CoverageEligibilityRequest", "CoverageEligibilityResponse",
        "DetectedIssue", "Device", "DeviceAssociation", "DeviceDefinition",
        "DeviceDispense", "DeviceMetric", "DeviceRequest", "DeviceUsage",
        "DeviceUseStatement", "DiagnosticReport", "DocumentManifest",
        "DocumentReference", "Encounter", "EncounterHistory", "Endpoint",
        "EnrollmentRequest", "EnrollmentResponse", "EpisodeOfCare",
        "EventDefinition", "Evidence", "EvidenceReport", "EvidenceVariable",
        "ExampleScenario", "ExplanationOfBenefit", "FamilyMemberHistory",
        "Flag", "FormularyItem", "GenomicStudy", "Goal", "GraphDefinition",
        "Group", "GuidanceResponse", "HealthcareService", "ImagingSelection",
        "ImagingStudy", "Immunization", "ImmunizationEvaluation",
        "ImmunizationRecommendation", "ImplementationGuide", "Ingredient",
        "InsurancePlan", "InventoryItem", "InventoryReport", "Invoice",
        "Library", "Linkage", "List", "Location",
        "ManufacturedItemDefinition", "Measure", "MeasureReport", "Media",
        "Medication", "MedicationAdministration", "MedicationDispense",
        "MedicationKnowledge", "MedicationRequest", "MedicationStatement",
        "MedicinalProductDefinition", "MessageDefinition", "MessageHeader",
        "MolecularSequence", "NamingSystem", "NutritionIntake",
        "NutritionOrder", "NutritionProduct", "Observation",
        "ObservationDefinition", "OperationDefinition", "OperationOutcome",
        "Organization", "OrganizationAffiliation", "PackagedProductDefinition",
        "Parameters", "Patient", "PaymentNotice", "PaymentReconciliation",
        "Permission", "Person", "PlanDefinition", "Practitioner",
        "PractitionerRole", "Procedure", "Provenance", "Questionnaire",
        "QuestionnaireResponse", "RegulatedAuthorization", "RelatedPerson",
        "RequestGroup", "RequestOrchestration", "Requirements",
        "ResearchDefinition", "ResearchElementDefinition", "ResearchStudy",
        "ResearchSubject", "RiskAssessment", "Schedule", "SearchParameter",
        "ServiceRequest", "Slot", "Specimen", "SpecimenDefinition",
        "StructureDefinition", "StructureMap", "Subscription",
        "SubscriptionStatus", "SubscriptionTopic", "Substance",
        "SubstanceDefinition", "SupplyDelivery", "SupplyRequest", "Task",
        "TerminologyCapabilities", "TestPlan", "TestReport", "TestScript",
        "Transport", "ValueSet", "VerificationResult", "VisionPrescription"
    };

    static bool IsResource(string name) => ResourceNames.Contains(name);

    static bool IsActive(string name) => name is "Binary" or "Observation" or "Patient" or "Practitioner" or "Provenance";

    static bool IsModel(string name) => name is "Resource" or "Bundle" or "CapabilityStatement"
        or "CompartmentDefinition" or "ConceptMap" or "OperationDefinition"
        or "OperationOutcome" or "SearchParameter";

    static readonly HashSet<string> PrimitiveNames = new()
    {
        "date", "dateTime", "string", "integer", "integer64", "oid",
        "canonical", "uri", "uuid", "url", "instant", "boolean",
        "base64Binary", "unsignedInt", "markdown", "id", "time",
        "positiveInt", "decimal", "code", "xhtml"
    };

    static bool IsPrimitive(string name) => PrimitiveNames.Contains(name);

    static readonly HashSet<string> DataTypeNames = new()
    {
        "Meta", "Address", "Age", "Availability", "Attachment", "Contributor",
        "Count", "DataRequirement", "Distance", "Dosage", "Duration", "Element",
        "ExtendedContactDetail", "Money", "MonetaryComponent", "HumanName",
        "ContactPoint", "MarketingStatus", "Identifier", "SubstanceAmount",
        "Coding", "SampledData", "Population", "Ratio", "Range", "Reference",
        "TriggerDefinition", "Quantity", "Period", "RelatedArtifact",
        "Annotation", "ProductShelfLife", "ContactDetail", "UsageContext",
        "Expression", "Signature", "Timing", "RelativeTime",
        "ProdCharacteristic", "CodeableConcept", "ParameterDefinition",
        "ElementDefinition", "Narrative", "CodeableReference", "RatioRange",
        "VirtualServiceDetail"
    };

    static bool IsDataType(string name) => DataTypeNames.Contains(name);

    static readonly HashSet<string> SharedCodesetNames = new()
    {
        "administrativegender", "consentprovisiontype", "codesystemcontentmode",
        "consentdatameaning", "searchcomparator", "searchmodifiercode",
        "encounterstatus", "eventstatus", "evidencevariablehandling",
        "examplescenarioactortype", "devicenametype", "observationstatus",
        "publicationstatus", "daysofweek", "quantitycomparator",
        "participationstatus", "financialresourcestatuscodes", "use",
        "requestpriority", "requeststatus", "requestintent",
        "claimprocessingcodes", "documentreferencestatus",
        "documentrelationshiptype", "compositionstatus", "listmode",
        "notetype", "compartmenttype", "invoicecomponenttype",
        "invoicepricecomponenttype", "searchparamtype", "actionparticipanttype",
        "actionconditionkind", "actionrelationshiptype", "actiongroupingbehavior",
        "actionselectionbehavior", "actionrequiredbehavior",
        "actionprecheckbehavior", "actioncardinalitybehavior",
        "evidencevariabletype", "groupmeasure", "fhirversion", "fhirtypes",
        "capabilitystatementkind", "filteroperator", "bindingstrength",
        "operationparameteruse", "medicationstatuscodes", "remittanceoutcome",
        "subscriptionstatuscodes", "resourcetype", "mimetypes", "fhiralltypes",
        "alllanguages", "commonlanguages", "versionindependentresourcetypesall",
    };

    static readonly HashSet<string> UnsupportedProperties = new()
    {
        "contained", "extension", "modifierExtension"
    };

    static string CleanDescription(string? s)
    {
        if (s == null) return "";
        return s.Replace("\n", "").Replace("\r", "").Replace("'", "");
    }

    static string StripNonAscii(string s)
    {
        var chars = new char[s.Length];
        int j = 0;
        foreach (var c in s)
            if (c < 128) chars[j++] = c;
        return new string(chars, 0, j);
    }

    // ========================================================================
    // HandleElement — process each ElementDefinition into properties/elements
    // ========================================================================

    static void HandleElement(
        ElementDefinition elemDef,
        Dictionary<string, ValueSetDef> dictValueSets,
        Dictionary<string, CodeSystemDef> dictCodeSystems,
        Dictionary<string, List<Element>> dictElements,
        Dictionary<string, List<Property>> dictProperties,
        Dictionary<string, Property> dictProperty,
        FhirModel model)
    {
        var pathParts = elemDef.Id.Split('.');
        if (pathParts.Length == 1) return;

        var name = pathParts[^1];
        if (UnsupportedProperties.Contains(name)) return;

        var parentId = string.Join("", pathParts[..^1]).ToLower();

        // Handle BackboneElement or Element types
        if (elemDef.Type.Count > 0
            && (elemDef.Type[0].Code == "BackboneElement" || elemDef.Type[0].Code == "Element"))
        {
            var explicitTypeName = elemDef.Extension?
                .FirstOrDefault(e => e.ValueString != null)?.ValueString;

            var elementId = string.Join("", pathParts).ToLower();
            var elementName = char.ToUpper(pathParts[^1][0]) + pathParts[^1][1..];

            var element = new Element
            {
                Id = elementId,
                Name = elementName,
                Explicit = explicitTypeName,
                Description = $"Backbone element for {pathParts[0]}.",
            };

            if (dictElements.TryGetValue(parentId, out var parentElements))
                parentElements.Add(element);
            dictElements[elementId] = new List<Element>();
            dictProperties[elementId] = new List<Property>();
        }

        // Create property
        var propertyId = elemDef.Id.ToLower();
        var propertyName = name;

        // Handle choice types (name[x])
        if (elemDef.Type.Count > 1)
            propertyName = propertyName.TrimEnd("[x]".ToCharArray());

        // Fix: TrimEnd removes individual chars. Use proper suffix removal.
        if (name.EndsWith("[x]") && elemDef.Type.Count > 1)
            propertyName = name[..^3];

        var property = new Property
        {
            Id = propertyId,
            Name = propertyName,
            Description = (elemDef.Definition ?? "")
                .Replace("\n", "").Replace("\r", "").Replace("'", "")
                .Replace(">=", "greater or equal ")
                .Replace("<=", "lower or equal ")
                .Replace("<", "lower than"),
        };

        // Handle contentReference (no type specified)
        if (elemDef.Type.Count == 0 && elemDef.ContentReference != null)
        {
            var linkedRef = elemDef.ContentReference[1..]; // Remove leading #
            var linkedRefLower = linkedRef.ToLower();
            var linkedParts = linkedRefLower.Split('.');
            var linkedClass = linkedParts[0] + string.Join("", linkedParts[1..]);

            property.Variants.Add(new Variant
            {
                TypeAttr = "element",
                RefAttr = linkedClass,
            });
        }

        // Handle typed properties
        if (elemDef.Type.Count > 0)
        {
            foreach (var elemType in elemDef.Type)
            {
                var typeCode = elemType.Code;
                var variant = new Variant();

                if (IsPrimitive(typeCode))
                {
                    variant.TypeAttr = typeCode;
                    if (typeCode == "string")
                        variant.TypeAttr = null; // Default

                    // Handle code types with bindings
                    if (typeCode == "code" && elemDef.Binding is { Strength: "required", ValueSet: not null })
                    {
                        var valueSetUri = elemDef.Binding.ValueSet.Split('|')[0];
                        if (dictValueSets.TryGetValue(valueSetUri, out var valueSet))
                        {
                            var valueSetId = valueSet.Name.ToLower().Replace(" ", "");
                            if (valueSetId != "mimetypes")
                            {
                                if (valueSetId is "biologicallyderivedproductdispense" or "subscriptionstatus")
                                    valueSetId += "code";

                                bool codesetAdded;
                                if (SharedCodesetNames.Contains(valueSetId))
                                {
                                    if (!model.Codesets.Any(cs => cs.Id == valueSetId))
                                        codesetAdded = AddCodeset(model.Codesets, valueSet, dictCodeSystems);
                                    else
                                        codesetAdded = true;
                                }
                                else
                                {
                                    codesetAdded = false;
                                    var res = model.Resources.FirstOrDefault(r => r.Name == pathParts[0]);
                                    if (res != null)
                                        codesetAdded = AddCodeset(res.Codesets, valueSet, dictCodeSystems);
                                    var elem = model.Elements.FirstOrDefault(e => e.Name == pathParts[0]);
                                    if (elem != null)
                                        codesetAdded = AddCodeset(elem.Codesets, valueSet, dictCodeSystems);
                                }

                                if (codesetAdded)
                                    variant.RefAttr = valueSetId;
                            }
                        }

                        // Handle special URIs
                        switch (valueSetUri)
                        {
                            case "http://hl7.org/fhir/ValueSet/resource-types":
                                variant.RefAttr = "resourcetype";
                                break;
                            case "http://hl7.org/fhir/ValueSet/units-of-time":
                                variant.RefAttr = "unitsoftime";
                                break;
                            case "http://hl7.org/fhir/ValueSet/days-of-week":
                                variant.RefAttr = "daysofweek";
                                break;
                        }
                    }
                }
                else if (typeCode == "http://hl7.org/fhirpath/System.String" || typeCode == "")
                {
                    variant.TypeAttr = null;
                }
                else
                {
                    // Complex type
                    variant.TypeAttr = "element";
                    if (typeCode is "BackboneElement" or "Element")
                        variant.RefAttr = elemDef.Id.Replace(".", "").ToLower();
                    else
                        variant.RefAttr = typeCode.ToLower();

                    // Handle Reference targetProfile
                    if (typeCode == "Reference" && elemType.TargetProfile != null)
                    {
                        var targets = new List<Target>();
                        foreach (var tp in elemType.TargetProfile)
                        {
                            var parts = tp.Split('/');
                            if (parts.Length >= 6)
                                targets.Add(new Target { Resource = parts[5] });
                        }
                        if (targets.Count > 0)
                            variant.Targets = targets;
                    }
                }

                // Special case: id property
                if (propertyName == "id")
                    variant.TypeAttr = "id";

                property.Variants.Add(variant);
            }
        }

        // Handle cardinality
        if (elemDef.Max != null)
        {
            switch (elemDef.Max)
            {
                case "0": return;
                case "*":
                    property.IsCollection = "true";
                    break;
                case "1":
                    if (elemDef.Min == 1)
                        property.NotNull = "true";
                    break;
            }
        }

        if (elemDef.IsSummary == true) property.Summary = "true";
        if (elemDef.IsModifier == true) property.Modifier = "true";

        // Skip properties with no variants
        if (property.Variants.Count == 0) return;

        if (dictProperties.TryGetValue(parentId, out var propList))
            propList.Add(Clone(property));

        // Normalize key — remove [x] suffix for choice types
        var dictKey = propertyId.EndsWith("[x]") ? propertyId[..^3] : propertyId;
        dictProperty[dictKey] = property;
    }

    static Property Clone(Property p) => new()
    {
        Id = p.Id, Name = p.Name, Description = p.Description,
        TypeAttr = p.TypeAttr, RefAttr = p.RefAttr,
        IsCollection = p.IsCollection, NotNull = p.NotNull,
        Summary = p.Summary, Modifier = p.Modifier,
        Targets = p.Targets?.Select(t => new Target { Resource = t.Resource }).ToList(),
        Variants = p.Variants.Select(v => new Variant
        {
            TypeAttr = v.TypeAttr, RefAttr = v.RefAttr,
            Targets = v.Targets?.Select(t => new Target { Resource = t.Resource }).ToList(),
        }).ToList(),
    };

    // ========================================================================
    // AddCodeset
    // ========================================================================

    static bool AddCodeset(List<Codeset> codesets, ValueSetDef valueSet, Dictionary<string, CodeSystemDef> dictCodeSystems)
    {
        var valueSetName = valueSet.Name.Replace(" ", "");
        if (codesets.Any(cs => cs.Name == valueSetName))
            return true;

        var codesetId = valueSet.Name.ToLower().Replace(" ", "");
        if (codesetId is "biologicallyderivedproductdispense" or "subscriptionstatus")
            codesetId += "code";

        var codeset = new Codeset
        {
            Id = codesetId,
            Name = valueSetName,
            Description = (valueSet.Description ?? "")
                .Replace("\n", "").Replace("\r", "").Replace("'", "").Replace("--", "-"),
        };

        foreach (var include in valueSet.Compose.Include)
        {
            if (include.Concept != null)
            {
                foreach (var concept in include.Concept)
                    AddCodesetCodesFromValueSet(codeset, concept);
            }
            else if (include.System != null && dictCodeSystems.TryGetValue(include.System, out var codeSystem))
            {
                if (codeSystem.Concept != null)
                    foreach (var concept in codeSystem.Concept)
                        AddCodesetCodesFromCodeSystem(codeset, concept);
            }
        }

        if (codeset.Codes.Count > 0)
        {
            codesets.Add(codeset);
            return true;
        }
        return false;
    }

    static void AddCodesetCodesFromValueSet(Codeset codeset, ValueSetConcept concept)
    {
        var name = string.Join("", concept.Code.Split('-')
            .Select(part => part.Length > 0 ? char.ToUpper(part[0]) + part[1..] : ""));
        name = name.Replace(".", "_");

        if (name.Length > 0 && char.IsAsciiDigit(name[0]))
            name = "N" + name;

        var description = "";
        var ext = concept.Extension?.FirstOrDefault(e =>
            e.Url == "http://hl7.org/fhir/StructureDefinition/valueset-concept-definition");
        if (ext?.ValueString != null)
            description = ext.ValueString.Replace("\n", "").Replace("\r", "").Replace("'", "").Replace("&", "and");

        name = name switch
        {
            "=" => "Equal", "!=" => "NotEqual",
            ">" => "GreaterThan", "<" => "LessThan",
            ">=" => "GreaterOrEqual", "<=" => "LessOrEqual",
            _ => name
        };

        codeset.Codes.Add(new Code { Name = name, Value = concept.Code, Description = description });
    }

    static void AddCodesetCodesFromCodeSystem(Codeset codeset, CodeSystemConcept concept)
    {
        var name = string.Join("", concept.Code.Split('-')
            .Select(part => part.Length > 0 ? char.ToUpper(part[0]) + part[1..] : ""));
        name = name.Replace(".", "_");

        if (name.Length > 0 && char.IsAsciiDigit(name[0]))
            name = "N" + name;

        var description = (concept.Definition ?? "")
            .Replace("\n", "").Replace("\r", "").Replace("'", "").Replace("&", "and");

        name = name switch
        {
            "=" => "Equal", "!=" => "NotEqual",
            ">" => "GreaterThan", "<" => "LessThan",
            ">=" => "GreaterOrEqual", "<=" => "LessOrEqual",
            _ => name
        };

        var notSelectable = concept.Property?.Any(p => p.Code == "notSelectable") ?? false;
        if (!notSelectable)
            codeset.Codes.Add(new Code { Name = name, Value = concept.Code, Description = description });

        if (concept.Concept != null)
            foreach (var child in concept.Concept)
                AddCodesetCodesFromCodeSystem(codeset, child);
    }

    // ========================================================================
    // CopyDictToModel
    // ========================================================================

    static void CopyDictToModel(
        Dictionary<string, List<Element>> dictElements,
        Dictionary<string, List<Property>> dictProperties,
        FhirModel model)
    {
        foreach (var resource in model.Resources)
        {
            if (dictProperties.TryGetValue(resource.Id, out var props))
                resource.Properties = new List<Property>(props);
            if (dictElements.TryGetValue(resource.Id, out var elems))
            {
                resource.Elements = new List<Element>(elems);
                CopyNestedElements(resource.Elements, dictElements, dictProperties);
            }
        }

        foreach (var element in model.Elements)
        {
            if (dictProperties.TryGetValue(element.Id, out var props))
                element.Properties = new List<Property>(props);
            if (dictElements.TryGetValue(element.Id, out var elems))
            {
                element.Elements = new List<Element>(elems);
                CopyNestedElements(element.Elements, dictElements, dictProperties);
            }
        }
    }

    static void CopyNestedElements(
        List<Element> elements,
        Dictionary<string, List<Element>> dictElements,
        Dictionary<string, List<Property>> dictProperties)
    {
        foreach (var element in elements)
        {
            if (dictProperties.TryGetValue(element.Id, out var props))
                element.Properties = new List<Property>(props);
            if (dictElements.TryGetValue(element.Id, out var nested))
            {
                element.Elements = new List<Element>(nested);
                CopyNestedElements(element.Elements, dictElements, dictProperties);
            }
        }
    }

    // ========================================================================
    // BuildSearches
    // ========================================================================

    static void BuildSearches(
        List<SearchParameterDef> searchParams,
        Dictionary<string, Property> dictProperty,
        FhirModel model)
    {
        // Add _lastUpdated search to all resources
        foreach (var resource in model.Resources)
        {
            resource.Searches.Add(new Search
            {
                Name = "_lastUpdated",
                Query = "LastUpdated",
                SearchType = "date",
                Paths = new List<SearchPath>
                {
                    new()
                    {
                        Parts = new List<Part>
                        {
                            new() { RefAttr = $"{resource.Name.ToLower()}.meta" },
                            new() { RefAttr = "meta.lastupdated" },
                        }
                    }
                }
            });
        }

        foreach (var sp in searchParams)
        {
            if (sp.Code.StartsWith('_')) continue;
            if (sp.Expression == null) continue;

            var expression = sp.Expression;
            if (expression.Contains(".extension(")) continue;
            if (expression.Contains("resolve()") && !expression.Contains(".where(resolve() is ")) continue;

            // Handle special deceased cases
            if (expression == "Patient.deceased.exists() and Patient.deceased != false"
                || expression == "Practitioner.deceased.exists() and Practitioner.deceased != false"
                || expression == "Person.deceased.exists() and Person.deceased != false")
            {
                foreach (var baseResource in sp.Base)
                {
                    var resource = model.Resources.FirstOrDefault(r => r.Name == baseResource);
                    if (resource != null)
                    {
                        resource.Searches.Add(new Search
                        {
                            Name = sp.Code,
                            Query = ToPascalCase(sp.Code),
                            SearchType = sp.Type.ToLower(),
                            Paths = new List<SearchPath>
                            {
                                new()
                                {
                                    Parts = new List<Part>
                                    {
                                        new()
                                        {
                                            RefAttr = $"{baseResource.ToLower()}.deceased",
                                            NotNull = "true",
                                            Variant = "boolean",
                                            Op = "ne",
                                            Value = "true",
                                        }
                                    }
                                }
                            }
                        });
                    }
                }
                continue;
            }

            // Parse FHIRPath expressions
            foreach (var baseResource in sp.Base)
            {
                var resource = model.Resources.FirstOrDefault(r => r.Name == baseResource);
                if (resource == null) continue;

                var paths = ParseFhirPathExpression(expression, baseResource, sp, dictProperty);

                // For composite searches with resource-only expression
                if (sp.Type == "composite" && paths.Count == 0 && expression == baseResource)
                {
                    paths.Add(new SearchPath
                    {
                        Parts = new List<Part>
                        {
                            new() { RefAttr = baseResource.ToLower() }
                        }
                    });
                }

                // Process composite search components
                if (sp.Type == "composite" && sp.Component != null)
                {
                    foreach (var path in paths)
                        path.Components = BuildComponents(sp.Component, path, dictProperty);
                }

                if (paths.Count > 0)
                {
                    resource.Searches.Add(new Search
                    {
                        Name = sp.Code,
                        Query = ToPascalCase(sp.Code),
                        SearchType = sp.Type.ToLower(),
                        Paths = paths,
                    });
                }
            }
        }
    }

    static string ToPascalCase(string s) =>
        string.Join("", s.Split('-').Select(w => w.Length > 0 ? char.ToUpper(w[0]) + w[1..] : ""));

    static List<SearchPath> ParseFhirPathExpression(
        string expression, string resourceName,
        SearchParameterDef sp, Dictionary<string, Property> dictProperty)
    {
        var paths = new List<SearchPath>();
        foreach (var pathExpr in expression.Split(" | "))
        {
            var trimmed = pathExpr.Trim();
            if (!trimmed.ToLower().StartsWith(resourceName.ToLower())) continue;
            var path = ParseSinglePath(trimmed, resourceName, sp, dictProperty);
            if (path != null) paths.Add(path);
        }
        return paths;
    }

    static SearchPath? ParseSinglePath(
        string expr, string resourceName,
        SearchParameterDef sp, Dictionary<string, Property> dictProperty)
    {
        var parts = new List<Part>();
        var castVec = new List<Cast>();
        string? targetResourceType = null;
        var currentPath = resourceName.ToLower();

        // Handle parentheses
        expr = expr.Trim('(', ')');

        // Strip .where(resolve() is ResourceType) pattern
        var whereResolveIdx = expr.IndexOf(".where(resolve() is ");
        if (whereResolveIdx >= 0)
        {
            var baseExpr = expr[..whereResolveIdx];
            var typeStart = whereResolveIdx + 20;
            var parenEnd = expr.IndexOf(')', typeStart);
            if (parenEnd >= 0)
            {
                targetResourceType = expr[typeStart..parenEnd].Trim();
                expr = baseExpr;
            }
        }

        // Check for .ofType() or .as() cast
        string? castType = null;
        var ofTypeIdx = expr.IndexOf(".ofType(");
        var asMethodIdx = expr.IndexOf(".as(");
        var asSpaceIdx = expr.IndexOf(" as ");

        if (ofTypeIdx >= 0)
        {
            var baseExpr2 = expr[..ofTypeIdx];
            var castStart = ofTypeIdx + 8;
            var castEnd = expr.IndexOf(')', castStart);
            if (castEnd >= 0) castType = expr[castStart..castEnd];
            expr = baseExpr2;
        }
        else if (asMethodIdx >= 0)
        {
            var baseExpr2 = expr[..asMethodIdx];
            var castStart = asMethodIdx + 4;
            var castEnd = expr.IndexOf(')', castStart);
            if (castEnd >= 0) castType = expr[castStart..castEnd];
            expr = baseExpr2;
        }
        else if (asSpaceIdx >= 0)
        {
            castType = expr[(asSpaceIdx + 4)..].Trim();
            expr = expr[..asSpaceIdx];
        }

        // Check for .where() function
        string? whereClause = null;
        var whereIdx = expr.IndexOf(".where(");
        if (whereIdx >= 0)
        {
            var baseExpr2 = expr[..whereIdx];
            var whereStart = whereIdx + 7;
            var whereEnd = expr.IndexOf(')', whereStart);
            if (whereEnd >= 0) whereClause = expr[whereStart..whereEnd];
            expr = baseExpr2;
        }

        var segments = expr.Split('.');
        for (int i = 0; i < segments.Length; i++)
        {
            if (i == 0)
            {
                if (!segments[i].Equals(resourceName, StringComparison.OrdinalIgnoreCase))
                    return null;
                continue;
            }

            var segment = segments[i];
            bool isFirst = false;

            // Check for [0]
            if (segment.EndsWith("[0]"))
            {
                segment = segment[..^3];
                isFirst = true;
            }
            else if (segment == "first()")
            {
                if (parts.Count > 0)
                    parts[^1].First = "true";
                continue;
            }

            var propertyId = $"{currentPath}.{segment.ToLower()}";
            var part = new Part
            {
                RefAttr = propertyId,
                First = isFirst ? "true" : null,
            };

            // Handle where clause on last segment
            if (whereClause != null && i == segments.Length - 1)
                ParseWhereClause(whereClause, part);

            parts.Add(part);

            // Update currentPath for next iteration
            if (dictProperty.TryGetValue(propertyId, out var prop))
            {
                if (prop.Variants.Count > 0 && prop.Variants[0] is { } variant)
                {
                    if (variant.RefAttr != null)
                    {
                        var propertyIdNoDots = propertyId.Replace(".", "");
                        if (variant.RefAttr == propertyIdNoDots)
                        {
                            currentPath = propertyId;
                        }
                        else
                        {
                            if (dictProperty.ContainsKey(variant.RefAttr))
                            {
                                currentPath = variant.RefAttr;
                            }
                            else
                            {
                                var foundDotted = dictProperty.Keys
                                    .FirstOrDefault(k => k.Replace(".", "") == variant.RefAttr && k.Count(c => c == '.') >= 1);
                                currentPath = foundDotted ?? variant.RefAttr;
                            }
                        }
                    }
                    else
                    {
                        currentPath = propertyId;
                    }
                }
                else
                {
                    currentPath = propertyId;
                }
            }
            else
            {
                currentPath = propertyId;
            }
        }

        if (castType != null)
            castVec.Add(new Cast { To = castType });

        var casts = castVec.Count > 0 ? castVec : null;

        List<Target>? targets = null;
        if (sp.Type == "reference")
        {
            if (targetResourceType != null)
                targets = new List<Target> { new() { Resource = targetResourceType } };
            else if (sp.Target != null)
                targets = sp.Target.Select(t => new Target { Resource = t }).ToList();
        }

        return parts.Count == 0 ? null : new SearchPath { Parts = parts, Casts = casts, Targets = targets };
    }

    static void ParseWhereClause(string whereExpr, Part part)
    {
        var eqIdx = whereExpr.IndexOf('=');
        if (eqIdx >= 0)
        {
            part.Property = whereExpr[..eqIdx].Trim();
            part.TypeAttr = whereExpr[(eqIdx + 1)..].Trim().Trim('\'', '"');
        }
    }

    static List<SearchComponent>? BuildComponents(
        List<SearchParameterComponent> componentsDef,
        SearchPath path,
        Dictionary<string, Property> dictProperty)
    {
        var componentVec = new List<SearchComponent>();

        // Determine context element from last part
        string contextElement;
        var lastPart = path.Parts.LastOrDefault();
        if (lastPart == null) return null;

        if (dictProperty.TryGetValue(lastPart.RefAttr, out var prop))
        {
            string? refAttrFromVariant = prop.Variants.Count == 1 ? prop.Variants[0].RefAttr : null;

            if (refAttrFromVariant != null)
            {
                if (!refAttrFromVariant.Contains('.') && lastPart.RefAttr.Contains('.'))
                {
                    var concat = lastPart.RefAttr.Replace(".", "");
                    contextElement = refAttrFromVariant == concat ? lastPart.RefAttr : refAttrFromVariant;
                }
                else
                {
                    contextElement = refAttrFromVariant;
                }
            }
            else
            {
                var partsCount = lastPart.RefAttr.Count(c => c == '.') + 1;
                contextElement = partsCount >= 3
                    ? lastPart.RefAttr
                    : lastPart.RefAttr.Split('.').Last().ToLower();
            }
        }
        else
        {
            var partsCount = lastPart.RefAttr.Count(c => c == '.') + 1;
            contextElement = partsCount >= 3
                ? lastPart.RefAttr
                : lastPart.RefAttr.Split('.').Last().ToLower();
        }

        foreach (var compDef in componentsDef)
        {
            foreach (var exprPart in compDef.Expression.Split(" | "))
            {
                var trimmedExpr = exprPart.Trim();

                // Strip parentheses
                while (trimmedExpr.StartsWith('(') && trimmedExpr.EndsWith(')'))
                    trimmedExpr = trimmedExpr[1..^1].Trim();

                // Build full expression
                string fullExpr;
                if (trimmedExpr.Contains('.'))
                    fullExpr = trimmedExpr.ToLower().StartsWith(contextElement.ToLower())
                        ? trimmedExpr : $"{contextElement}.{trimmedExpr}";
                else
                    fullExpr = $"{contextElement}.{trimmedExpr}";

                // Check for .ofType() cast
                string? compCastType = null;
                var ofTypeIdx2 = fullExpr.IndexOf(".ofType(");
                string baseExpr3;
                if (ofTypeIdx2 >= 0)
                {
                    baseExpr3 = fullExpr[..ofTypeIdx2];
                    var castStart = ofTypeIdx2 + 8;
                    var castEnd = fullExpr.IndexOf(')', castStart);
                    if (castEnd >= 0) compCastType = fullExpr[castStart..castEnd];
                }
                else
                {
                    baseExpr3 = fullExpr;
                }

                var propRef = baseExpr3.ToLower();

                // Skip known problematic refs
                string[] skipRefs =
                {
                    "ingredient.substance.strength.presentation.denominator",
                    "ingredient.substance.strength.presentation.numerator",
                    "ingredient.substance.strength.concentration.denominator",
                    "ingredient.substance.strength.concentration.numerator",
                    "composition.section.section.code",
                    "composition.section.section.text",
                    "encounter.location.location.reference",
                    "device.specification",
                };
                if (skipRefs.Contains(propRef)) continue;

                var casts = compCastType != null ? new List<Cast> { new() { To = compCastType } } : null;
                componentVec.Add(new SearchComponent { RefAttr = propRef, Casts = casts });
            }
        }

        return componentVec.Count > 0 ? componentVec : null;
    }

    // ========================================================================
    // BuildOperations
    // ========================================================================

    static void BuildOperations(List<OperationDefinitionDef> operationDefs, FhirModel model)
    {
        foreach (var opDef in operationDefs)
        {
            if (opDef.Resource == null) continue;
            foreach (var resourceType in opDef.Resource)
            {
                var resource = model.Resources.FirstOrDefault(r => r.Name == resourceType);
                if (resource == null) continue;

                var operation = new Operation
                {
                    Name = opDef.Code,
                    Description = (opDef.Description ?? "").Replace("\r", "").Replace("\n", ""),
                };

                resource.Operations.Add(operation);

                if (opDef.Parameter == null) continue;
                foreach (var pc in opDef.Parameter)
                {
                    var pcType = pc.Type ?? "";
                    string paramType;
                    if (IsPrimitive(pcType))
                        paramType = char.ToLower(pcType[0]) + pcType[1..];
                    else
                        paramType = "resource";

                    string? reference;
                    if (IsPrimitive(pcType) || pcType is "Any" or "Bundle" or "Resource")
                        reference = null;
                    else
                        reference = pcType.ToLower();

                    var parameter = new Parameter
                    {
                        Name = pc.Name,
                        Description = (pc.Documentation ?? "").Replace("\n", "").Replace("\r", "").Replace("'", ""),
                        TypeAttr = paramType,
                        Reference = reference,
                        IsCollection = pc.Max == "*" ? "true" : null,
                        NotNull = pc.Min == 1 ? "true" : null,
                    };

                    if (parameter.TypeAttr is "string" or ""
                        || (parameter.TypeAttr == "resource" && parameter.Reference == null))
                        parameter.TypeAttr = null;

                    if (pc.Use == "in")
                        operation.Input.Add(parameter);
                    else
                        operation.Output.Add(parameter);
                }
            }
        }
    }

    // ========================================================================
    // NormalizeModel
    // ========================================================================

    static void NormalizeModel(FhirModel model)
    {
        foreach (var resource in model.Resources)
        {
            resource.Description = StripNonAscii(resource.Description);
            NormalizeProperties(resource.Properties);
            NormalizeElements(resource.Elements);
            NormalizeCodes(resource.Codesets);
            NormalizeOperations(resource.Operations);
        }

        foreach (var element in model.Elements)
        {
            element.Description = StripNonAscii(element.Description);
            NormalizeProperties(element.Properties);
            NormalizeElements(element.Elements);
            NormalizeCodes(element.Codesets);
        }

        NormalizeCodes(model.Codesets);
    }

    static void NormalizeProperties(List<Property> properties)
    {
        foreach (var p in properties)
        {
            if (p.Id.EndsWith("[x]"))
                p.Id = p.Id[..^3];

            p.Description = StripNonAscii(p.Description);

            if (p.Variants.Count == 1)
            {
                var v = p.Variants[0];
                p.TypeAttr = v.TypeAttr;
                p.RefAttr = v.RefAttr;
                p.Targets = v.Targets;
                p.Variants.Clear();
            }
            else if (p.Variants.Count > 1)
            {
                p.TypeAttr = "variant";
            }
        }
    }

    static void NormalizeElements(List<Element> elements)
    {
        foreach (var e in elements)
        {
            e.Description = StripNonAscii(e.Description);
            NormalizeProperties(e.Properties);
            NormalizeElements(e.Elements);
        }
    }

    static void NormalizeCodes(List<Codeset> codesets)
    {
        foreach (var cs in codesets)
        {
            cs.Description = StripNonAscii(cs.Description);
            foreach (var code in cs.Codes)
                code.Description = StripNonAscii(code.Description);
        }
    }

    static void NormalizeOperations(List<Operation> operations)
    {
        foreach (var op in operations)
        {
            op.Description = StripNonAscii(op.Description);
            foreach (var p in op.Input) p.Description = StripNonAscii(p.Description);
            foreach (var p in op.Output) p.Description = StripNonAscii(p.Description);
        }
    }

    // ========================================================================
    // WriteXmlOutput — custom writer matching Rust quick_xml format exactly
    // ========================================================================

    static void WriteXmlOutput(FhirModel model, string basePath)
    {
        var sb = new System.Text.StringBuilder(8 * 1024 * 1024); // 8MB initial
        sb.AppendLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
        sb.AppendLine("<!DOCTYPE fhir SYSTEM \"fire.dtd\">");
        sb.Append($"<fhir name=\"{Esc(model.Name)}\" version=\"{Esc(model.Version)}\">");
        sb.AppendLine();

        // Resources
        Indent(sb, 1); sb.AppendLine("<resources>");
        foreach (var r in model.Resources) WriteResource(sb, r, 2);
        Indent(sb, 1); sb.AppendLine("</resources>");

        // Elements
        Indent(sb, 1); sb.AppendLine("<elements>");
        foreach (var e in model.Elements) WriteElement(sb, e, 2);
        Indent(sb, 1); sb.AppendLine("</elements>");

        // Codesets
        Indent(sb, 1); sb.AppendLine("<codesets>");
        foreach (var cs in model.Codesets) WriteCodeset(sb, cs, 2);
        Indent(sb, 1); sb.AppendLine("</codesets>");

        sb.Append("</fhir>");
        File.WriteAllText(Path.Combine(basePath, "fhir.xml"), sb.ToString());
    }

    static void Indent(System.Text.StringBuilder sb, int level)
    {
        for (int i = 0; i < level; i++) sb.Append("  ");
    }

    static string Esc(string s) => s
        .Replace("&", "&amp;")
        .Replace("<", "&lt;")
        .Replace(">", "&gt;")
        .Replace("\"", "&quot;")
        .Replace("'", "&apos;");

    static string EscContent(string s) => s
        .Replace("&", "&amp;")
        .Replace("<", "&lt;")
        .Replace(">", "&gt;")
        .Replace("\"", "&quot;")
        .Replace("'", "&apos;");

    static string Attr(string name, string value) => $" {name}=\"{Esc(value)}\"";

    static string AttrIf(string name, string? value) => value != null ? Attr(name, value) : "";

    static void WriteResource(System.Text.StringBuilder sb, Resource r, int d)
    {
        Indent(sb, d);
        sb.Append($"<resource{Attr("name", r.Name)}{AttrIf("active", r.Active)}{Attr("id", r.Id)}>");
        sb.AppendLine();
        WriteDescriptionElement(sb, r.Description, d + 1);
        WriteProperties(sb, r.Properties, d + 1);
        WriteElements(sb, r.Elements, d + 1);
        WriteCodesets(sb, r.Codesets, d + 1);
        WriteSearches(sb, r.Searches, d + 1);
        WriteOperations(sb, r.Operations, d + 1);
        Indent(sb, d); sb.AppendLine("</resource>");
    }

    static void WriteElement(System.Text.StringBuilder sb, Element e, int d)
    {
        Indent(sb, d);
        sb.Append($"<element{Attr("id", e.Id)}{Attr("name", e.Name)}{AttrIf("explicit", e.Explicit)}{AttrIf("backbone", e.Backbone)}>");
        sb.AppendLine();
        WriteDescriptionElement(sb, e.Description, d + 1);
        WriteProperties(sb, e.Properties, d + 1);
        WriteElements(sb, e.Elements, d + 1);
        WriteCodesets(sb, e.Codesets, d + 1);
        Indent(sb, d); sb.AppendLine("</element>");
    }

    static void WriteDescriptionElement(System.Text.StringBuilder sb, string desc, int d)
    {
        Indent(sb, d);
        var trimmed = desc.Trim();
        if (trimmed.Length == 0)
            sb.AppendLine("<description/>");
        else
            sb.AppendLine($"<description>{EscContent(trimmed)}</description>");
    }

    static void WriteProperties(System.Text.StringBuilder sb, List<Property> props, int d)
    {
        if (props.Count == 0)
        {
            Indent(sb, d); sb.AppendLine("<properties/>");
            return;
        }
        Indent(sb, d); sb.AppendLine("<properties>");
        foreach (var p in props)
        {
            Indent(sb, d + 1);
            sb.Append($"<property{Attr("id", p.Id)}{Attr("name", p.Name)}{AttrIf("type", p.TypeAttr)}{AttrIf("ref", p.RefAttr)}{AttrIf("iscollection", p.IsCollection)}{AttrIf("notnull", p.NotNull)}{AttrIf("summary", p.Summary)}{AttrIf("modifier", p.Modifier)}>");
            sb.AppendLine();
            WriteDescriptionElement(sb, p.Description, d + 2);
            if (p.Variants.Count > 0)
            {
                Indent(sb, d + 2); sb.AppendLine("<variants>");
                foreach (var v in p.Variants) WriteVariant(sb, v, d + 3);
                Indent(sb, d + 2); sb.AppendLine("</variants>");
            }
            else if (p.Targets != null && p.Targets.Count > 0)
            {
                Indent(sb, d + 2); sb.AppendLine("<targets>");
                foreach (var t in p.Targets) WriteTarget(sb, t, d + 3);
                Indent(sb, d + 2); sb.AppendLine("</targets>");
            }
            Indent(sb, d + 1); sb.AppendLine("</property>");
        }
        Indent(sb, d); sb.AppendLine("</properties>");
    }

    static void WriteVariant(System.Text.StringBuilder sb, Variant v, int d)
    {
        Indent(sb, d);
        if (v.Targets != null && v.Targets.Count > 0)
        {
            sb.AppendLine($"<variant{AttrIf("type", v.TypeAttr)}{AttrIf("ref", v.RefAttr)}>");
            Indent(sb, d + 1); sb.AppendLine("<targets>");
            foreach (var t in v.Targets) WriteTarget(sb, t, d + 2);
            Indent(sb, d + 1); sb.AppendLine("</targets>");
            Indent(sb, d); sb.AppendLine("</variant>");
        }
        else
        {
            sb.AppendLine($"<variant{AttrIf("type", v.TypeAttr)}{AttrIf("ref", v.RefAttr)}/>");
        }
    }

    static void WriteTarget(System.Text.StringBuilder sb, Target t, int d)
    {
        Indent(sb, d);
        sb.AppendLine($"<target{Attr("resource", t.Resource)}/>");
    }

    static void WriteElements(System.Text.StringBuilder sb, List<Element> elements, int d)
    {
        if (elements.Count == 0)
        {
            Indent(sb, d); sb.AppendLine("<elements/>");
            return;
        }
        Indent(sb, d); sb.AppendLine("<elements>");
        foreach (var e in elements) WriteElement(sb, e, d + 1);
        Indent(sb, d); sb.AppendLine("</elements>");
    }

    static void WriteCodesets(System.Text.StringBuilder sb, List<Codeset> codesets, int d)
    {
        if (codesets.Count == 0)
        {
            Indent(sb, d); sb.AppendLine("<codesets/>");
            return;
        }
        Indent(sb, d); sb.AppendLine("<codesets>");
        foreach (var cs in codesets) WriteCodeset(sb, cs, d + 1);
        Indent(sb, d); sb.AppendLine("</codesets>");
    }

    static void WriteCodeset(System.Text.StringBuilder sb, Codeset cs, int d)
    {
        Indent(sb, d);
        sb.AppendLine($"<codeset{Attr("name", cs.Name)}{Attr("id", cs.Id)}>");
        WriteDescriptionElement(sb, cs.Description, d + 1);
        Indent(sb, d + 1); sb.AppendLine("<codes>");
        foreach (var c in cs.Codes)
        {
            Indent(sb, d + 2);
            sb.AppendLine($"<code{Attr("name", c.Name)}{Attr("value", c.Value)}>");
            WriteDescriptionElement(sb, c.Description, d + 3);
            Indent(sb, d + 2); sb.AppendLine("</code>");
        }
        Indent(sb, d + 1); sb.AppendLine("</codes>");
        Indent(sb, d); sb.AppendLine("</codeset>");
    }

    static void WriteSearches(System.Text.StringBuilder sb, List<Search> searches, int d)
    {
        if (searches.Count == 0)
        {
            Indent(sb, d); sb.AppendLine("<searches/>");
            return;
        }
        Indent(sb, d); sb.AppendLine("<searches>");
        foreach (var s in searches)
        {
            Indent(sb, d + 1);
            sb.AppendLine($"<search{Attr("name", s.Name)}{Attr("query", s.Query)}{Attr("type", s.SearchType)}>");
            Indent(sb, d + 2); sb.AppendLine("<paths>");
            foreach (var p in s.Paths) WriteSearchPath(sb, p, d + 3);
            Indent(sb, d + 2); sb.AppendLine("</paths>");
            Indent(sb, d + 1); sb.AppendLine("</search>");
        }
        Indent(sb, d); sb.AppendLine("</searches>");
    }

    static void WriteSearchPath(System.Text.StringBuilder sb, SearchPath p, int d)
    {
        Indent(sb, d); sb.AppendLine("<path>");
        Indent(sb, d + 1); sb.AppendLine("<parts>");
        foreach (var part in p.Parts)
        {
            Indent(sb, d + 2);
            sb.AppendLine($"<part{Attr("ref", part.RefAttr)}{AttrIf("resolve", part.Resolve)}{AttrIf("property", part.Property)}{AttrIf("type", part.TypeAttr)}{AttrIf("first", part.First)}{AttrIf("notnull", part.NotNull)}{AttrIf("variant", part.Variant)}{AttrIf("op", part.Op)}{AttrIf("value", part.Value)}/>");
        }
        Indent(sb, d + 1); sb.AppendLine("</parts>");
        if (p.Casts != null && p.Casts.Count > 0)
        {
            Indent(sb, d + 1); sb.AppendLine("<casts>");
            foreach (var c in p.Casts)
            {
                Indent(sb, d + 2); sb.AppendLine($"<cast{Attr("to", c.To)}/>");
            }
            Indent(sb, d + 1); sb.AppendLine("</casts>");
        }
        if (p.Targets != null && p.Targets.Count > 0)
        {
            Indent(sb, d + 1); sb.AppendLine("<targets>");
            foreach (var t in p.Targets) WriteTarget(sb, t, d + 2);
            Indent(sb, d + 1); sb.AppendLine("</targets>");
        }
        if (p.Components != null && p.Components.Count > 0)
        {
            Indent(sb, d + 1); sb.AppendLine("<components>");
            foreach (var c in p.Components)
            {
                Indent(sb, d + 2);
                if (c.Casts != null && c.Casts.Count > 0)
                {
                    sb.AppendLine($"<component{Attr("ref", c.RefAttr)}>");
                    Indent(sb, d + 3); sb.AppendLine("<casts>");
                    foreach (var ca in c.Casts)
                    {
                        Indent(sb, d + 4); sb.AppendLine($"<cast{Attr("to", ca.To)}/>");
                    }
                    Indent(sb, d + 3); sb.AppendLine("</casts>");
                    Indent(sb, d + 2); sb.AppendLine("</component>");
                }
                else
                {
                    sb.AppendLine($"<component{Attr("ref", c.RefAttr)}/>");
                }
            }
            Indent(sb, d + 1); sb.AppendLine("</components>");
        }
        Indent(sb, d); sb.AppendLine("</path>");
    }

    static void WriteOperations(System.Text.StringBuilder sb, List<Operation> ops, int d)
    {
        if (ops.Count == 0)
        {
            Indent(sb, d); sb.AppendLine("<operations/>");
            return;
        }
        Indent(sb, d); sb.AppendLine("<operations>");
        foreach (var o in ops)
        {
            Indent(sb, d + 1); sb.AppendLine($"<operation{Attr("name", o.Name)}>");
            WriteDescriptionElement(sb, o.Description, d + 2);
            if (o.Input.Count > 0)
            {
                Indent(sb, d + 2); sb.AppendLine("<input>");
                foreach (var p in o.Input) WriteParameter(sb, p, d + 3);
                Indent(sb, d + 2); sb.AppendLine("</input>");
            }
            else
            {
                Indent(sb, d + 2); sb.AppendLine("<input/>");
            }
            if (o.Output.Count > 0)
            {
                Indent(sb, d + 2); sb.AppendLine("<output>");
                foreach (var p in o.Output) WriteParameter(sb, p, d + 3);
                Indent(sb, d + 2); sb.AppendLine("</output>");
            }
            else
            {
                Indent(sb, d + 2); sb.AppendLine("<output/>");
            }
            Indent(sb, d + 1); sb.AppendLine("</operation>");
        }
        Indent(sb, d); sb.AppendLine("</operations>");
    }

    static void WriteParameter(System.Text.StringBuilder sb, Parameter p, int d)
    {
        Indent(sb, d);
        var refAttr = string.IsNullOrEmpty(p.Reference) ? "" : Attr("ref", p.Reference);
        sb.AppendLine($"<parameter{Attr("name", p.Name)}{AttrIf("type", p.TypeAttr)}{AttrIf("iscollection", p.IsCollection)}{refAttr}{AttrIf("notnull", p.NotNull)}>");
        WriteDescriptionElement(sb, p.Description, d + 1);
        Indent(sb, d); sb.AppendLine("</parameter>");
    }
}
