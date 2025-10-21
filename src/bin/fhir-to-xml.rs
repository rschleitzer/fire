// This tool reads FHIR R5 JSON definitions and outputs an XML model

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;

// ============================================================================
// XML Data Structures
// ============================================================================

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "element")]
pub struct Element {
    #[serde(rename = "@id")]
    pub id: String,
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@explicit", skip_serializing_if = "Option::is_none")]
    pub explicit: Option<String>,
    #[serde(rename = "@backbone", skip_serializing_if = "Option::is_none")]
    pub backbone: Option<String>,
    pub description: String,
    pub properties: Properties,
    pub elements: Elements,
    pub codesets: Codesets,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "resource")]
pub struct Resource {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@active", skip_serializing_if = "Option::is_none")]
    pub active: Option<String>,
    #[serde(rename = "@id")]
    pub id: String,
    pub description: String,
    pub properties: Properties,
    pub elements: Elements,
    pub codesets: Codesets,
    pub searches: Searches,
    pub operations: Operations,
}

#[derive(Debug, Clone, Serialize)]
pub struct Properties {
    #[serde(default, rename = "property")]
    pub property: Vec<Property>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Searches {
    #[serde(default, rename = "search")]
    pub search: Vec<Search>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Operations {
    #[serde(default, rename = "operation")]
    pub operation: Vec<Operation>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "fhir")]
pub struct Fhir {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@version")]
    pub version: String,
    pub resources: Resources,
    pub elements: Elements,
    pub codesets: Codesets,
}

#[derive(Debug, Clone, Serialize)]
pub struct Resources {
    #[serde(default, rename = "resource")]
    pub resource: Vec<Resource>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Elements {
    #[serde(default, rename = "element")]
    pub element: Vec<Element>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Codesets {
    #[serde(default, rename = "codeset")]
    pub codeset: Vec<Codeset>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Variants {
    #[serde(default, rename = "variant")]
    pub variant: Vec<Variant>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "property")]
pub struct Property {
    #[serde(rename = "@id")]
    pub id: String,
    #[serde(rename = "@name")]
    pub name: String,
    pub description: String,
    #[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
    pub type_attr: Option<String>,
    #[serde(rename = "@ref", skip_serializing_if = "Option::is_none")]
    pub ref_attr: Option<String>,
    #[serde(rename = "@iscollection", skip_serializing_if = "Option::is_none")]
    pub is_collection: Option<String>,
    #[serde(rename = "@notnull", skip_serializing_if = "Option::is_none")]
    pub not_null: Option<String>,
    #[serde(rename = "@summary", skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(rename = "@modifier", skip_serializing_if = "Option::is_none")]
    pub modifier: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub targets: Option<Targets>,
    #[serde(skip_serializing_if = "is_variants_empty")]
    pub variants: Variants,
}

fn is_variants_empty(variants: &Variants) -> bool {
    variants.variant.is_empty()
}

#[derive(Debug, Clone, Serialize)]
pub struct Targets {
    #[serde(default, rename = "target")]
    pub target: Vec<Target>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "variant")]
pub struct Variant {
    #[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
    pub type_attr: Option<String>,
    #[serde(rename = "@ref", skip_serializing_if = "Option::is_none")]
    pub ref_attr: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub targets: Option<Targets>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Codes {
    #[serde(default, rename = "code")]
    pub code: Vec<Code>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "codeset")]
pub struct Codeset {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@id")]
    pub id: String,
    pub description: String,
    pub codes: Codes,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "code")]
pub struct Code {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@value")]
    pub value: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "target")]
pub struct Target {
    #[serde(rename = "@resource")]
    pub resource: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Paths {
    #[serde(default, rename = "path")]
    pub path: Vec<Path>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Parts {
    #[serde(default, rename = "part")]
    pub part: Vec<Part>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Input {
    #[serde(default, rename = "parameter")]
    pub parameter: Vec<Parameter>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Output {
    #[serde(default, rename = "parameter")]
    pub parameter: Vec<Parameter>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "search")]
pub struct Search {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "@query")]
    pub query: String,
    #[serde(rename = "@type")]
    pub search_type: String,
    #[serde(rename = "@ref", skip_serializing_if = "Option::is_none")]
    pub reference: Option<String>,
    pub paths: Paths,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "path")]
pub struct Path {
    pub parts: Parts,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub casts: Option<Casts>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Components>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub targets: Option<Targets>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Components {
    #[serde(default, rename = "component")]
    pub component: Vec<Component>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "component")]
pub struct Component {
    #[serde(rename = "@ref")]
    pub ref_attr: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub casts: Option<Casts>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Casts {
    #[serde(default, rename = "cast")]
    pub cast: Vec<Cast>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "cast")]
pub struct Cast {
    #[serde(rename = "@to")]
    pub to: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "part")]
pub struct Part {
    #[serde(rename = "@ref")]
    pub ref_attr: String,
    #[serde(rename = "@resolve", skip_serializing_if = "Option::is_none")]
    pub resolve: Option<String>,
    #[serde(rename = "@property", skip_serializing_if = "Option::is_none")]
    pub property: Option<String>,
    #[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
    pub type_attr: Option<String>,
    #[serde(rename = "@first", skip_serializing_if = "Option::is_none")]
    pub first: Option<String>,
    #[serde(rename = "@notnull", skip_serializing_if = "Option::is_none")]
    pub not_null: Option<String>,
    #[serde(rename = "@variant", skip_serializing_if = "Option::is_none")]
    pub variant: Option<String>,
    #[serde(rename = "@op", skip_serializing_if = "Option::is_none")]
    pub op: Option<String>,
    #[serde(rename = "@value", skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "operation")]
pub struct Operation {
    #[serde(rename = "@name")]
    pub name: String,
    pub description: String,
    pub input: Input,
    pub output: Output,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename = "parameter")]
pub struct Parameter {
    #[serde(rename = "@name")]
    pub name: String,
    pub description: String,
    #[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
    pub type_attr: Option<String>,
    #[serde(rename = "@iscollection", skip_serializing_if = "Option::is_none")]
    pub is_collection: Option<String>,
    #[serde(rename = "@ref", skip_serializing_if = "is_none_or_empty")]
    pub reference: Option<String>,
    #[serde(rename = "@notnull", skip_serializing_if = "Option::is_none")]
    pub not_null: Option<String>,
}

fn is_none_or_empty(opt: &Option<String>) -> bool {
    opt.as_ref().map_or(true, |s| s.is_empty())
}

// ============================================================================
// FHIR JSON Input Structures (minimal required for parsing)
// ============================================================================

#[derive(Debug, Clone, Deserialize)]
pub struct Bundle {
    pub entry: Vec<BundleEntry>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "resourceType")]
pub enum BundleResource {
    StructureDefinition(Box<StructureDefinition>),
    SearchParameter(Box<SearchParameter>),
    OperationDefinition(Box<OperationDefinition>),
    ValueSet(Box<ValueSet>),
    CodeSystem(Box<CodeSystem>),
    #[serde(other)]
    Other,
}

#[derive(Debug, Clone, Deserialize)]
pub struct BundleEntry {
    pub resource: BundleResource,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StructureDefinition {
    pub name: String,
    pub snapshot: SnapshotComponent,
    pub base_definition: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SnapshotComponent {
    pub element: Vec<ElementDefinition>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElementDefinition {
    pub id: String,
    pub definition: Option<String>,
    pub min: Option<i32>,
    pub max: Option<String>,
    pub is_summary: Option<bool>,
    pub is_modifier: Option<bool>,
    #[serde(default)]
    pub r#type: Vec<ElementType>,
    pub content_reference: Option<String>,
    pub binding: Option<ElementBinding>,
    pub extension: Option<Vec<ElementExtension>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElementType {
    pub code: String,
    pub target_profile: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElementBinding {
    pub strength: Option<String>,
    pub value_set: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElementExtension {
    pub url: Option<String>,
    pub value_string: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchParameter {
    pub code: String,
    pub name: String,
    pub r#type: String,
    pub expression: Option<String>,
    pub base: Vec<String>,
    pub target: Option<Vec<String>>,
    pub component: Option<Vec<SearchParameterComponent>>,
    pub version: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SearchParameterComponent {
    pub expression: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct OperationDefinition {
    pub code: String,
    pub description: Option<String>,
    pub resource: Option<Vec<String>>,
    pub parameter: Option<Vec<OperationParameter>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OperationParameter {
    pub name: String,
    pub r#use: String,
    pub min: i32,
    pub max: String,
    pub documentation: Option<String>,
    pub r#type: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ValueSet {
    pub name: String,
    pub url: String,
    pub description: Option<String>,
    pub compose: ValueSetCompose,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ValueSetCompose {
    pub include: Vec<ValueSetInclude>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ValueSetInclude {
    pub system: Option<String>,
    pub concept: Option<Vec<ValueSetConcept>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ValueSetConcept {
    pub code: String,
    pub extension: Option<Vec<ValueSetConceptExtension>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ValueSetConceptExtension {
    pub url: String,
    pub value_string: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CodeSystem {
    pub name: String,
    pub url: String,
    pub concept: Option<Vec<CodeSystemConcept>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CodeSystemConcept {
    pub code: String,
    pub definition: Option<String>,
    pub property: Option<Vec<CodeSystemConceptProperty>>,
    pub concept: Option<Vec<CodeSystemConcept>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CodeSystemConceptProperty {
    pub code: String,
}

// ============================================================================
// Helper Functions
// ============================================================================

fn is_resource(type_name: &str) -> bool {
    matches!(
        type_name,
        "Resource" | "Account" | "ActivityDefinition" | "ActorDefinition"
        | "AdministrableProductDefinition" | "AdverseEvent" | "AllergyIntolerance"
        | "Appointment" | "AppointmentResponse" | "ArtifactAssessment" | "AuditEvent"
        | "Basic" | "Binary" | "BiologicallyDerivedProduct"
        | "BiologicallyDerivedProductDispense" | "BodyStructure" | "Bundle"
        | "CapabilityStatement" | "CarePlan" | "CareTeam" | "CatalogEntry"
        | "ChargeItem" | "ChargeItemDefinition" | "Citation" | "Claim"
        | "ClaimResponse" | "ClinicalImpression" | "ClinicalUseDefinition"
        | "CodeSystem" | "Communication" | "CommunicationRequest"
        | "CompartmentDefinition" | "Composition" | "ConceptMap" | "Condition"
        | "ConditionDefinition" | "Consent" | "Contract" | "Coverage"
        | "CoverageEligibilityRequest" | "CoverageEligibilityResponse"
        | "DetectedIssue" | "Device" | "DeviceAssociation" | "DeviceDefinition"
        | "DeviceDispense" | "DeviceMetric" | "DeviceRequest" | "DeviceUsage"
        | "DeviceUseStatement" | "DiagnosticReport" | "DocumentManifest"
        | "DocumentReference" | "Encounter" | "EncounterHistory" | "Endpoint"
        | "EnrollmentRequest" | "EnrollmentResponse" | "EpisodeOfCare"
        | "EventDefinition" | "Evidence" | "EvidenceReport" | "EvidenceVariable"
        | "ExampleScenario" | "ExplanationOfBenefit" | "FamilyMemberHistory"
        | "Flag" | "FormularyItem" | "GenomicStudy" | "Goal" | "GraphDefinition"
        | "Group" | "GuidanceResponse" | "HealthcareService" | "ImagingSelection"
        | "ImagingStudy" | "Immunization" | "ImmunizationEvaluation"
        | "ImmunizationRecommendation" | "ImplementationGuide" | "Ingredient"
        | "InsurancePlan" | "InventoryItem" | "InventoryReport" | "Invoice"
        | "Library" | "Linkage" | "List" | "Location"
        | "ManufacturedItemDefinition" | "Measure" | "MeasureReport" | "Media"
        | "Medication" | "MedicationAdministration" | "MedicationDispense"
        | "MedicationKnowledge" | "MedicationRequest" | "MedicationStatement"
        | "MedicinalProductDefinition" | "MessageDefinition" | "MessageHeader"
        | "MolecularSequence" | "NamingSystem" | "NutritionIntake"
        | "NutritionOrder" | "NutritionProduct" | "Observation"
        | "ObservationDefinition" | "OperationDefinition" | "OperationOutcome"
        | "Organization" | "OrganizationAffiliation" | "PackagedProductDefinition"
        | "Parameters" | "Patient" | "PaymentNotice" | "PaymentReconciliation"
        | "Permission" | "Person" | "PlanDefinition" | "Practitioner"
        | "PractitionerRole" | "Procedure" | "Provenance" | "Questionnaire"
        | "QuestionnaireResponse" | "RegulatedAuthorization" | "RelatedPerson"
        | "RequestGroup" | "RequestOrchestration" | "Requirements"
        | "ResearchDefinition" | "ResearchElementDefinition" | "ResearchStudy"
        | "ResearchSubject" | "RiskAssessment" | "Schedule" | "SearchParameter"
        | "ServiceRequest" | "Slot" | "Specimen" | "SpecimenDefinition"
        | "StructureDefinition" | "StructureMap" | "Subscription"
        | "SubscriptionStatus" | "SubscriptionTopic" | "Substance"
        | "SubstanceDefinition" | "SupplyDelivery" | "SupplyRequest" | "Task"
        | "TerminologyCapabilities" | "TestPlan" | "TestReport" | "TestScript"
        | "Transport" | "ValueSet" | "VerificationResult" | "VisionPrescription"
    )
}

fn is_active(type_name: &str) -> bool {
    matches!(
        type_name,
        "Observation" | "Patient" | "Practitioner"
    )
}

fn is_model(type_name: &str) -> bool {
    matches!(
        type_name,
        "Resource" | "Bundle" | "CapabilityStatement" | "CompartmentDefinition"
        | "ConceptMap" | "OperationDefinition" | "OperationOutcome" | "SearchParameter"
    )
}

fn is_primitive(type_name: &str) -> bool {
    matches!(
        type_name,
        "date" | "dateTime" | "string" | "integer" | "integer64" | "oid"
        | "canonical" | "uri" | "uuid" | "url" | "instant" | "boolean"
        | "base64Binary" | "unsignedInt" | "markdown" | "id" | "time"
        | "positiveInt" | "decimal" | "code" | "xhtml"
    )
}

fn is_data_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Meta" | "Address" | "Age" | "Availability" | "Attachment" | "Contributor"
        | "Count" | "DataRequirement" | "Distance" | "Dosage" | "Duration" | "Element"
        | "ExtendedContactDetail" | "Money" | "MonetaryComponent" | "HumanName"
        | "ContactPoint" | "MarketingStatus" | "Identifier" | "SubstanceAmount"
        | "Coding" | "SampledData" | "Population" | "Ratio" | "Range" | "Reference"
        | "TriggerDefinition" | "Quantity" | "Period" | "RelatedArtifact"
        | "Annotation" | "ProductShelfLife" | "ContactDetail" | "UsageContext"
        | "Expression" | "Signature" | "Timing" | "RelativeTime"
        | "ProdCharacteristic" | "CodeableConcept" | "ParameterDefinition"
        | "ElementDefinition" | "Narrative" | "CodeableReference" | "RatioRange"
        | "VirtualServiceDetail"
    )
}

// Shared codesets that go in the root <fhir> element
fn get_shared_codesets() -> HashSet<&'static str> {
    [
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
    ]
    .iter()
    .copied()
    .collect()
}

// Unsupported properties (extensions, etc.)
fn get_unsupported_properties() -> HashSet<&'static str> {
    ["contained", "extension", "modifierExtension"]
        .iter()
        .copied()
        .collect()
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("Modeler - FHIR R5 to XML converter (Rust port)");

    // Load FHIR R5 JSON files
    let profiles_resources_json = fs::read_to_string("fhir-r5/profiles-resources.json")?;
    let profiles_types_json = fs::read_to_string("fhir-r5/profiles-types.json")?;
    let search_parameters_json = fs::read_to_string("fhir-r5/search-parameters.json")?;
    let valuesets_json = fs::read_to_string("fhir-r5/valuesets.json")?;

    println!("Parsing JSON files...");
    let resources_bundle: Bundle = serde_json::from_str(&profiles_resources_json)?;
    let types_bundle: Bundle = serde_json::from_str(&profiles_types_json)?;
    let search_bundle: Bundle = serde_json::from_str(&search_parameters_json)?;
    let valuesets_bundle: Bundle = serde_json::from_str(&valuesets_json)?;

    // Extract specific resource types
    let mut structure_defs = Vec::new();
    let mut operation_defs = Vec::new();

    for entry in &resources_bundle.entry {
        match &entry.resource {
            BundleResource::StructureDefinition(sd) => {
                if is_resource(&sd.snapshot.element[0].id) {
                    structure_defs.push(sd.as_ref().clone());
                }
            }
            BundleResource::OperationDefinition(od) => {
                operation_defs.push(od.as_ref().clone());
            }
            _ => {}
        }
    }

    let mut element_defs = Vec::new();
    for entry in &types_bundle.entry {
        if let BundleResource::StructureDefinition(sd) = &entry.resource {
            if is_data_type(&sd.snapshot.element[0].id) {
                element_defs.push(sd.as_ref().clone());
            }
        }
    }

    let mut search_params = Vec::new();
    for entry in &search_bundle.entry {
        if let BundleResource::SearchParameter(sp) = &entry.resource {
            search_params.push(sp.as_ref().clone());
        }
    }

    // Build ValueSet and CodeSystem dictionaries
    let mut value_sets = Vec::new();
    let mut code_systems = Vec::new();

    for entry in &valuesets_bundle.entry {
        match &entry.resource {
            BundleResource::ValueSet(vs) => value_sets.push(vs.as_ref().clone()),
            BundleResource::CodeSystem(cs) => code_systems.push(cs.as_ref().clone()),
            _ => {}
        }
    }

    let dict_value_sets: HashMap<String, ValueSet> =
        value_sets.iter().map(|vs| (vs.url.clone(), vs.clone())).collect();
    let dict_code_systems: HashMap<String, CodeSystem> =
        code_systems.iter().map(|cs| (cs.url.clone(), cs.clone())).collect();

    println!("Building model...");
    let mut model = Fhir {
        name: "FHIR".to_string(),
        version: "5.0.0".to_string(),
        resources: Resources {
            resource: Vec::new(),
        },
        elements: Elements {
            element: Vec::new(),
        },
        codesets: Codesets {
            codeset: Vec::new(),
        },
    };

    // Global dictionaries for tracking elements and properties
    let mut dict_elements: HashMap<String, Vec<Element>> = HashMap::new();
    let mut dict_properties: HashMap<String, Vec<Property>> = HashMap::new();
    let mut dict_property: HashMap<String, Property> = HashMap::new();

    // Process resources
    for structure_def in &structure_defs {
        let snapshot = &structure_def.snapshot;
        let current_element_name = &snapshot.element[0].id;

        let active = if is_active(current_element_name) {
            Some("true".to_string())
        } else if is_model(current_element_name) {
            Some("model".to_string())
        } else {
            None
        };

        let resource = Resource {
            id: current_element_name.to_lowercase(),
            active,
            name: current_element_name.clone(),
            description: snapshot.element[0]
                .definition
                .as_ref()
                .unwrap_or(&String::new())
                .replace('\n', "")
                .replace('\r', "")
                .replace('\'', ""),
            properties: Properties {
                property: Vec::new(),
            },
            elements: Elements {
                element: Vec::new(),
            },
            codesets: Codesets {
                codeset: Vec::new(),
            },
            searches: Searches {
                search: Vec::new(),
            },
            operations: Operations {
                operation: Vec::new(),
            },
        };

        dict_elements.insert(resource.id.clone(), Vec::new());
        dict_properties.insert(resource.id.clone(), Vec::new());

        model.resources.resource.push(resource);

        // Process all element definitions for this resource
        for element_def in &snapshot.element {
            handle_element(
                element_def,
                &dict_value_sets,
                &dict_code_systems,
                &mut dict_elements,
                &mut dict_properties,
                &mut dict_property,
                &mut model,
            );
        }
    }

    // Process data types
    for structure_def in &element_defs {
        let snapshot = &structure_def.snapshot;
        let current_element_name = &snapshot.element[0].id;

        // Skip if already processed
        if model.elements.element.iter().any(|e| e.name == *current_element_name) {
            continue;
        }

        let is_backbone = structure_def
            .base_definition
            .as_ref()
            .map(|bd| bd == "http://hl7.org/fhir/StructureDefinition/BackboneElement")
            .unwrap_or(false);

        let element = Element {
            id: current_element_name.to_lowercase(),
            name: current_element_name.clone(),
            explicit: None,
            backbone: if is_backbone { Some("true".to_string()) } else { None },
            description: snapshot.element[0]
                .definition
                .as_ref()
                .unwrap_or(&String::new())
                .replace('\n', "")
                .replace('\r', "")
                .replace('\'', "")
                .replace("…", "..."),
            properties: Properties {
                property: Vec::new(),
            },
            elements: Elements {
                element: Vec::new(),
            },
            codesets: Codesets {
                codeset: Vec::new(),
            },
        };

        dict_elements.insert(element.id.clone(), Vec::new());
        dict_properties.insert(element.id.clone(), Vec::new());

        model.elements.element.push(element);

        // Process all element definitions
        for element_def in &snapshot.element {
            handle_element(
                element_def,
                &dict_value_sets,
                &dict_code_systems,
                &mut dict_elements,
                &mut dict_properties,
                &mut dict_property,
                &mut model,
            );
        }
    }

    // Copy properties and elements from dictionaries into model
    println!("Copying properties and elements into model...");
    copy_dict_to_model(&dict_elements, &dict_properties, &mut model);

    println!("Building searches and operations...");

    // Build searches with FHIRPath parsing
    build_searches(&search_params, &dict_property, &mut model);

    // Build operations
    build_operations(&operation_defs, &mut model);

    // Normalize model for DTD compliance
    println!("Normalizing model for DTD compliance...");
    normalize_model(&mut model);

    // Output XML
    println!("Writing XML output...");
    write_xml_output(&model)?;

    println!("Done!");
    Ok(())
}

fn handle_element(
    element_def: &ElementDefinition,
    dict_value_sets: &HashMap<String, ValueSet>,
    dict_code_systems: &HashMap<String, CodeSystem>,
    dict_elements: &mut HashMap<String, Vec<Element>>,
    dict_properties: &mut HashMap<String, Vec<Property>>,
    dict_property: &mut HashMap<String, Property>,
    model: &mut Fhir,
) {
    let unsupported = get_unsupported_properties();

    let path: Vec<&str> = element_def.id.split('.').collect();
    if path.len() == 1 {
        return;
    }

    let name = path[path.len() - 1];
    if unsupported.contains(name) {
        return;
    }

    let parent_id: String = path[..path.len() - 1].join("").replace('.', "").to_lowercase();

    // Handle BackboneElement or Element types
    if !element_def.r#type.is_empty()
        && (element_def.r#type[0].code == "BackboneElement" || element_def.r#type[0].code == "Element")
    {
        let explicit_type_name = element_def
            .extension
            .as_ref()
            .and_then(|exts| exts.iter().find(|e| e.value_string.is_some()))
            .and_then(|e| e.value_string.clone());

        let element_id: String = path.join("").replace('.', "").to_lowercase();
        let element_name = format!(
            "{}{}",
            path[path.len() - 1].chars().next().unwrap().to_uppercase(),
            &path[path.len() - 1][1..]
        );

        let element = Element {
            id: element_id.clone(),
            name: element_name,
            explicit: explicit_type_name,
            backbone: None,
            description: format!("Backbone element for {}.", path[0]),
            properties: Properties {
                property: Vec::new(),
            },
            elements: Elements {
                element: Vec::new(),
            },
            codesets: Codesets {
                codeset: Vec::new(),
            },
        };

        if let Some(elements_vec) = dict_elements.get_mut(&parent_id) {
            elements_vec.push(element);
        }
        dict_elements.insert(element_id.clone(), Vec::new());
        dict_properties.insert(element_id.clone(), Vec::new());
    }

    // Create property
    let property_id = element_def.id.to_lowercase();
    let mut property_name = name.to_string();

    // Handle choice types (name[x])
    if element_def.r#type.len() > 1 {
        property_name = property_name.trim_end_matches("[x]").to_string();
    }

    let mut property = Property {
        id: property_id.clone(),
        name: property_name.clone(),
        description: element_def
            .definition
            .as_ref()
            .unwrap_or(&String::new())
            .replace('\n', "")
            .replace('\r', "")
            .replace('\'', "")
            .replace(">=", "greater or equal ")
            .replace("<=", "lower or equal ")
            .replace('<', "lower than"),
        type_attr: None,
        ref_attr: None,
        is_collection: None,
        not_null: None,
        summary: None,
        modifier: None,
        targets: None,
        variants: Variants {
            variant: Vec::new(),
        },
    };

    // Handle contentReference (no type specified)
    if element_def.r#type.is_empty() {
        if let Some(content_ref) = &element_def.content_reference {
            let linked_ref = &content_ref[1..]; // Remove leading #
            let linked_ref_lower = linked_ref.to_lowercase();
            let linked_path: Vec<&str> = linked_ref_lower.split('.').collect();
            let mut linked_class = linked_path[0].to_string();
            for class_part in &linked_path[1..] {
                linked_class.push_str(class_part);
            }

            let variant = Variant {
                type_attr: Some("element".to_string()),
                ref_attr: Some(linked_class),
                targets: None,
            };
            property.variants.variant.push(variant);
        }
    }

    // Handle typed properties
    if !element_def.r#type.is_empty() {
        for element_type in &element_def.r#type {
            let type_code = &element_type.code;
            let mut variant = Variant {
                type_attr: None,
                ref_attr: None,
                targets: None,
            };

            if is_primitive(type_code) {
                variant.type_attr = Some(type_code.clone());
                if type_code == "string" {
                    variant.type_attr = None; // Default
                }

                // Handle code types with bindings
                if type_code == "code" {
                    if let Some(binding) = &element_def.binding {
                        if binding.strength.as_deref() == Some("required") {
                            if let Some(value_set_uri) = &binding.value_set {
                                let value_set_uri_clean = value_set_uri.split('|').next().unwrap();
                                if let Some(value_set) = dict_value_sets.get(value_set_uri_clean) {
                                    let mut value_set_id = value_set.name.to_lowercase().replace(' ', "");
                                    if value_set_id != "mimetypes" {
                                        if value_set_id == "biologicallyderivedproductdispense"
                                            || value_set_id == "subscriptionstatus"
                                        {
                                            value_set_id.push_str("code");
                                        }
                                        // Add codeset
                                        let shared_codesets = get_shared_codesets();
                                        let codeset_added = if shared_codesets.contains(value_set_id.as_str()) {
                                            // Add to global codesets if not already present
                                            if !model.codesets.codeset.iter().any(|cs| cs.id == value_set_id) {
                                                add_codeset(&mut model.codesets.codeset, value_set, dict_code_systems)
                                            } else {
                                                true // Already exists
                                            }
                                        } else {
                                            // Add to resource or element codesets
                                            let mut added = false;
                                            if let Some(resource) =
                                                model.resources.resource.iter_mut().find(|r| r.name == path[0])
                                            {
                                                added = add_codeset(&mut resource.codesets.codeset, value_set, dict_code_systems);
                                            }
                                            if let Some(element) =
                                                model.elements.element.iter_mut().find(|e| e.name == path[0])
                                            {
                                                added = add_codeset(&mut element.codesets.codeset, value_set, dict_code_systems);
                                            }
                                            added
                                        };

                                        // Only set ref_attr if codeset was actually added (has codes)
                                        if codeset_added {
                                            variant.ref_attr = Some(value_set_id.clone());
                                        }
                                    }
                                }

                                // Handle special URIs
                                match value_set_uri_clean {
                                    "http://hl7.org/fhir/ValueSet/resource-types" => {
                                        variant.ref_attr = Some("resourcetype".to_string());
                                    }
                                    "http://hl7.org/fhir/ValueSet/units-of-time" => {
                                        variant.ref_attr = Some("unitsoftime".to_string());
                                    }
                                    "http://hl7.org/fhir/ValueSet/days-of-week" => {
                                        variant.ref_attr = Some("daysofweek".to_string());
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            } else if type_code == "http://hl7.org/fhirpath/System.String" || type_code.is_empty() {
                variant.type_attr = None;
            } else {
                // Complex type
                variant.type_attr = Some("element".to_string());
                if type_code == "BackboneElement" || type_code == "Element" {
                    variant.ref_attr = Some(element_def.id.replace('.', "").to_lowercase());
                } else {
                    variant.ref_attr = Some(type_code.to_lowercase());
                }

                // Handle Reference targetProfile
                if type_code == "Reference" {
                    if let Some(target_profiles) = &element_type.target_profile {
                        let mut targets_vec = Vec::new();
                        for target_profile in target_profiles {
                            let parts: Vec<&str> = target_profile.split('/').collect();
                            if parts.len() >= 6 {
                                targets_vec.push(Target {
                                    resource: parts[5].to_string(),
                                });
                            }
                        }
                        if !targets_vec.is_empty() {
                            variant.targets = Some(Targets {
                                target: targets_vec,
                            });
                        }
                    }
                }
            }

            // Special case: id property
            if property_name == "id" {
                variant.type_attr = Some("id".to_string());
            }

            property.variants.variant.push(variant);
        }
    }

    // Handle cardinality (min/max)
    if let Some(max) = &element_def.max {
        match max.as_str() {
            "0" => {
                // Skip this property
                return;
            }
            "*" => {
                property.is_collection = Some("true".to_string());
            }
            "1" => {
                if let Some(min) = element_def.min {
                    if min == 1 {
                        property.not_null = Some("true".to_string());
                    }
                }
            }
            _ => {
                // Unexpected max value - could log warning
            }
        }
    }

    // Handle summary and modifier flags
    if element_def.is_summary == Some(true) {
        property.summary = Some("true".to_string());
    }
    if element_def.is_modifier == Some(true) {
        property.modifier = Some("true".to_string());
    }

    // Skip properties with no variants (DTD requires at least one variant)
    if property.variants.variant.is_empty() {
        return;
    }

    // Add property to dictionaries
    if let Some(properties_vec) = dict_properties.get_mut(&parent_id) {
        properties_vec.push(property.clone());
    }

    // Normalize the key for dict_property - remove [x] suffix for choice types
    let dict_key = if property_id.ends_with("[x]") {
        property_id[..property_id.len() - 3].to_string()
    } else {
        property_id
    };

    dict_property.insert(dict_key, property);
}

// Helper function to add codesets
// Returns true if the codeset was added, false if it has no codes
fn add_codeset(
    codesets: &mut Vec<Codeset>,
    value_set: &ValueSet,
    dict_code_systems: &HashMap<String, CodeSystem>,
) -> bool {
    // Check if already exists
    let value_set_name = value_set.name.replace(' ', "");
    if codesets.iter().any(|cs| cs.name == value_set_name) {
        return true; // Already exists, so it's valid
    }

    let mut codeset_id = value_set.name.to_lowercase().replace(' ', "");
    if codeset_id == "biologicallyderivedproductdispense" || codeset_id == "subscriptionstatus" {
        codeset_id.push_str("code");
    }

    let codeset = Codeset {
        id: codeset_id,
        name: value_set_name,
        description: value_set
            .description
            .as_ref()
            .unwrap_or(&String::new())
            .replace('\n', "")
            .replace('\r', "")
            .replace('\'', "")
            .replace("--", "-"),
        codes: Codes {
            code: Vec::new(),
        },
    };

    // Temporary codeset to collect codes
    let mut temp_codeset = codeset;

    // Add codes from ValueSet
    for include in &value_set.compose.include {
        if let Some(concepts) = &include.concept {
            for concept in concepts {
                add_codeset_codes_from_valueset(&mut temp_codeset, concept);
            }
        } else if let Some(system) = &include.system {
            if let Some(code_system) = dict_code_systems.get(system) {
                if let Some(concepts) = &code_system.concept {
                    for concept in concepts {
                        add_codeset_codes_from_codesystem(&mut temp_codeset, concept);
                    }
                }
            }
        }
    }

    // Only add codeset if it has at least one code (DTD requirement)
    if !temp_codeset.codes.code.is_empty() {
        codesets.push(temp_codeset);
        true
    } else {
        false
    }
}

fn add_codeset_codes_from_valueset(codeset: &mut Codeset, concept: &ValueSetConcept) {
    let mut name: String = concept
        .code
        .split('-')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(c) => c.to_uppercase().chain(chars).collect(),
                None => String::new(),
            }
        })
        .collect();
    name = name.replace('.', "_");

    if name.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
        name = format!("N{}", name);
    }

    let mut description = String::new();
    if let Some(extensions) = &concept.extension {
        if let Some(ext) = extensions
            .iter()
            .find(|e| e.url == "http://hl7.org/fhir/StructureDefinition/valueset-concept-definition")
        {
            if let Some(val_str) = &ext.value_string {
                description = val_str
                    .replace('\n', "")
                    .replace('\r', "")
                    .replace('\'', "")
                    .replace('&', "and");
            }
        }
    }

    let mut code = Code {
        name: name.clone(),
        value: concept.code.clone(),
        description,
    };

    // Handle special operator names
    code.name = match code.name.as_str() {
        "=" => "Equal".to_string(),
        "!=" => "NotEqual".to_string(),
        ">" => "GreaterThan".to_string(),
        "<" => "LessThan".to_string(),
        ">=" => "GreaterOrEqual".to_string(),
        "<=" => "LessOrEqual".to_string(),
        _ => code.name,
    };

    codeset.codes.code.push(code);
}

fn add_codeset_codes_from_codesystem(codeset: &mut Codeset, concept: &CodeSystemConcept) {
    let mut name: String = concept
        .code
        .split('-')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(c) => c.to_uppercase().chain(chars).collect(),
                None => String::new(),
            }
        })
        .collect();
    name = name.replace('.', "_");

    if name.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
        name = format!("N{}", name);
    }

    let description = concept
        .definition
        .as_ref()
        .unwrap_or(&String::new())
        .replace('\n', "")
        .replace('\r', "")
        .replace('\'', "")
        .replace('&', "and");

    let mut code = Code {
        name: name.clone(),
        value: concept.code.clone(),
        description,
    };

    // Handle special operator names
    code.name = match code.name.as_str() {
        "=" => "Equal".to_string(),
        "!=" => "NotEqual".to_string(),
        ">" => "GreaterThan".to_string(),
        "<" => "LessThan".to_string(),
        ">=" => "GreaterOrEqual".to_string(),
        "<=" => "LessOrEqual".to_string(),
        _ => code.name,
    };

    // Check if notSelectable
    let not_selectable = concept
        .property
        .as_ref()
        .map(|props| props.iter().any(|p| p.code == "notSelectable"))
        .unwrap_or(false);

    if !not_selectable {
        codeset.codes.code.push(code);
    }

    // Recursively add child concepts
    if let Some(child_concepts) = &concept.concept {
        for child_concept in child_concepts {
            add_codeset_codes_from_codesystem(codeset, child_concept);
        }
    }
}

fn build_searches(
    search_params: &[SearchParameter],
    dict_property: &HashMap<String, Property>,
    model: &mut Fhir,
) {
    // Add _lastUpdated search to all resources
    for resource in &mut model.resources.resource {
        let last_updated_search = Search {
            name: "_lastUpdated".to_string(),
            query: "LastUpdated".to_string(),
            search_type: "date".to_string(),
            reference: None,
            paths: Paths {
                path: vec![Path {
                    parts: Parts {
                        part: vec![
                            Part {
                                ref_attr: format!("{}.meta", resource.name.to_lowercase()),
                                resolve: None,
                                property: None,
                                type_attr: None,
                                first: None,
                                not_null: None,
                                variant: None,
                                op: None,
                                value: None,
                            },
                            Part {
                                ref_attr: "meta.lastupdated".to_string(),
                                resolve: None,
                                property: None,
                                type_attr: None,
                                first: None,
                                not_null: None,
                                variant: None,
                                op: None,
                                value: None,
                            },
                        ],
                    },
                    casts: None,
                    components: None,
                    targets: None,
                }],
            },
        };
        resource.searches.search.push(last_updated_search);
    }

    // Process search parameters from FHIR specification
    for search_param in search_params {
        // Skip internal search parameters
        if search_param.code.starts_with('_') {
            continue;
        }

        // Skip parameters without expressions
        if search_param.expression.is_none() {
            continue;
        }

        let expression = search_param.expression.as_ref().unwrap();

        // Skip complex expressions with extension() or other function calls we can't parse
        if expression.contains(".extension(") || expression.contains("resolve()") {
            continue;
        }

        // Handle special cases that require custom logic
        if expression == "Patient.deceased.exists() and Patient.deceased != false"
            || expression == "Practitioner.deceased.exists() and Practitioner.deceased != false"
            || expression == "Person.deceased.exists() and Person.deceased != false"
        {
            for base_resource in &search_param.base {
                if let Some(resource) = model.resources.resource.iter_mut().find(|r| &r.name == base_resource) {
                    let search = Search {
                        name: search_param.code.clone(),
                        query: to_pascal_case(&search_param.code),
                        search_type: search_param.r#type.to_lowercase(),
                        reference: None,
                        paths: Paths {
                            path: vec![Path {
                                parts: Parts {
                                    part: vec![Part {
                                        ref_attr: format!("{}.deceased", base_resource.to_lowercase()),
                                        resolve: None,
                                        property: None,
                                        type_attr: None,
                                        first: None,
                                        not_null: Some("true".to_string()),
                                        variant: Some("boolean".to_string()),
                                        op: Some("ne".to_string()),
                                        value: Some("true".to_string()),
                                    }],
                                },
                                casts: None,
                                components: None,
                                targets: None,
                            }],
                        },
                    };
                    resource.searches.search.push(search);
                }
            }
            continue;
        }

        // Parse FHIRPath expressions
        for base_resource in &search_param.base {
            if let Some(resource) = model.resources.resource.iter_mut().find(|r| &r.name == base_resource) {
                let mut paths = parse_fhirpath_expression(
                    expression,
                    base_resource,
                    search_param,
                    dict_property,
                );

                // For composite searches with resource-only expression, create a simple path
                if search_param.r#type == "composite" && paths.is_empty() && expression == base_resource {
                    // Create a path that just points to the resource
                    paths.push(Path {
                        parts: Parts {
                            part: vec![Part {
                                ref_attr: base_resource.to_lowercase(),
                                resolve: None,
                                property: None,
                                type_attr: None,
                                first: None,
                                not_null: None,
                                variant: None,
                                op: None,
                                value: None,
                            }],
                        },
                        casts: None,
                        components: None,
                        targets: None,
                    });
                }

                // Process composite search components
                if search_param.r#type == "composite" {
                    if let Some(components_def) = &search_param.component {
                        for path in &mut paths {
                            path.components = build_components(
                                components_def,
                                base_resource,
                                dict_property,
                            );
                        }
                    }
                }

                if !paths.is_empty() {
                    let search = Search {
                        name: search_param.code.clone(),
                        query: to_pascal_case(&search_param.code),
                        search_type: search_param.r#type.to_lowercase(),
                        reference: None,
                        paths: Paths { path: paths },
                    };
                    resource.searches.search.push(search);
                }
            }
        }
    }
}

// Convert kebab-case or snake_case to PascalCase
fn to_pascal_case(s: &str) -> String {
    s.split('-')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(c) => c.to_uppercase().chain(chars).collect::<String>(),
                None => String::new(),
            }
        })
        .collect()
}

// Parse FHIRPath expression into Path structures
fn parse_fhirpath_expression(
    expression: &str,
    resource_name: &str,
    search_param: &SearchParameter,
    dict_property: &HashMap<String, Property>,
) -> Vec<Path> {
    let mut paths = Vec::new();

    // Split on | for union paths
    let path_expressions: Vec<&str> = expression.split(" | ").collect();

    for path_expr in path_expressions {
        let path_expr = path_expr.trim();

        // Skip paths that don't start with our resource (shared search params)
        if !path_expr.to_lowercase().starts_with(&resource_name.to_lowercase()) {
            continue;
        }

        // Parse the path
        if let Some(path) = parse_single_path(path_expr, resource_name, search_param, dict_property) {
            paths.push(path);
        }
    }

    paths
}

// Parse a single FHIRPath expression
fn parse_single_path(
    expr: &str,
    resource_name: &str,
    search_param: &SearchParameter,
    dict_property: &HashMap<String, Property>,
) -> Option<Path> {
    let mut parts: Vec<Part> = Vec::new();
    let mut cast_vec: Vec<Cast> = Vec::new();

    // Track the current property path (with dots preserved from original FHIRPath)
    let mut current_path = resource_name.to_lowercase();

    // Handle parentheses
    let expr = expr.trim_matches(|c| c == '(' || c == ')');

    // Check for .ofType() or .as() cast
    let (base_expr, cast_type) = if let Some(idx) = expr.find(".ofType(") {
        let base = &expr[..idx];
        let cast_start = idx + 8; // ".ofType(".len()
        let cast_end = expr[cast_start..].find(')').map(|i| cast_start + i);
        let cast = cast_end.map(|end| expr[cast_start..end].to_string());
        (base, cast)
    } else if let Some(idx) = expr.find(".as(") {
        let base = &expr[..idx];
        let cast_start = idx + 4; // ".as(".len()
        let cast_end = expr[cast_start..].find(')').map(|i| cast_start + i);
        let cast = cast_end.map(|end| expr[cast_start..end].to_string());
        (base, cast)
    } else if let Some(idx) = expr.find(" as ") {
        let base = &expr[..idx];
        let cast = Some(expr[idx + 4..].trim().to_string());
        (base, cast)
    } else {
        (expr, None)
    };

    // Check for .where() function
    let (base_expr, where_clause) = if let Some(idx) = base_expr.find(".where(") {
        let base = &base_expr[..idx];
        let where_start = idx + 7; // ".where(".len()
        let where_end = base_expr[where_start..].find(')').map(|i| where_start + i);
        let where_expr = where_end.map(|end| base_expr[where_start..end].to_string());
        (base, where_expr)
    } else {
        (base_expr, None)
    };

    // Split path into segments
    let segments: Vec<&str> = base_expr.split('.').collect();

    for (i, segment) in segments.iter().enumerate() {
        if i == 0 {
            // First segment should be resource name
            if !segment.to_lowercase().eq_ignore_ascii_case(resource_name) {
                return None;
            }
            continue;
        }

        // Check for [0] or .first()
        let (clean_segment, is_first) = if segment.ends_with("[0]") {
            (&segment[..segment.len() - 3], true)
        } else if segment == &"first()" {
            // Skip .first() in path, will be added to previous part
            if let Some(last_part) = parts.last_mut() {
                last_part.first = Some("true".to_string());
            }
            continue;
        } else {
            (*segment, false)
        };

        // Build property ID using the accumulated path
        let property_id = format!("{}.{}", current_path, clean_segment.to_lowercase());

        let mut part = Part {
            ref_attr: property_id.clone(),
            resolve: None,
            property: None,
            type_attr: None,
            first: if is_first { Some("true".to_string()) } else { None },
            not_null: None,
            variant: None,
            op: None,
            value: None,
        };

        // Handle where clause
        if where_clause.is_some() && i == segments.len() - 1 {
            if let Some(ref where_expr) = where_clause {
                parse_where_clause(where_expr, &mut part);
            }
        }

        parts.push(part);

        // Update current_path for next iteration
        // Check if this property exists and what type it references
        if let Some(property) = dict_property.get(&property_id) {
            if let Some(variant) = property.variants.variant.first() {
                if let Some(ref_attr) = &variant.ref_attr {
                    // Check if the reference starts with current_path (without dots) AND is a direct child
                    // This indicates it's a backbone element under the current path (not a contentReference)
                    let _current_path_no_dots = current_path.replace('.', "");
                    let property_id_no_dots = property_id.replace('.', "");
                    if ref_attr == &property_id_no_dots {
                        // It's a backbone element defined by this property - keep the full dotted path
                        current_path = property_id.clone();
                    } else {
                        // It's a reference to a data type or contentReference
                        // For contentReference, we need to find the dotted form of the backbone element
                        // Check if the ref is a property that exists (indicates it's a contentReference)
                        if let Some(_backbone_property) = dict_property.get(ref_attr) {
                            // The ref itself is a dotted property ID (this shouldn't happen normally)
                            current_path = ref_attr.clone();
                        } else {
                            // ref_attr is likely a backbone element ID without dots
                            // Try to find its dotted form by looking for a property whose ID without dots matches
                            let found_dotted = dict_property.keys()
                                .find(|k| k.replace('.', "") == *ref_attr && k.matches('.').count() >= 1)
                                .cloned();

                            if let Some(dotted_form) = found_dotted {
                                // Use the dotted form as the current path
                                current_path = dotted_form;
                            } else {
                                // It's a data type reference (not a backbone element)
                                current_path = ref_attr.clone();
                            }
                        }
                    }
                } else {
                    // No reference, keep the property path
                    current_path = property_id.clone();
                }
            } else {
                // No variants, keep the property path
                current_path = property_id.clone();
            }
        } else {
            // Property not found in dict, keep building the path
            current_path = property_id.clone();
        }
    }

    // Add cast if present
    if let Some(cast_type) = cast_type {
        cast_vec.push(Cast { to: cast_type });
    }

    // Wrap casts in Casts struct if non-empty
    let casts = if cast_vec.is_empty() {
        None
    } else {
        Some(Casts { cast: cast_vec })
    };

    // Add targets for reference search parameters
    let targets = if search_param.r#type == "reference" {
        search_param.target.as_ref().map(|target_vec| Targets {
            target: target_vec
                .iter()
                .map(|t| Target {
                    resource: t.clone(),
                })
                .collect(),
        })
    } else {
        None
    };

    if parts.is_empty() {
        None
    } else {
        Some(Path {
            parts: Parts { part: parts },
            casts,
            components: None,
            targets,
        })
    }
}

// Build components for composite searches
fn build_components(
    components_def: &[SearchParameterComponent],
    resource_name: &str,
    _dict_property: &HashMap<String, Property>,
) -> Option<Components> {
    let mut component_vec = Vec::new();

    for comp_def in components_def {
        // Parse the component expression
        // Split on | for union expressions (some components have alternates)
        let expressions: Vec<&str> = comp_def.expression.split(" | ").collect();

        for expr in expressions {
            let expr = expr.trim();

            // Handle simple property references (e.g., "code")
            // For composite searches, the expression is relative to the resource
            let full_expr = if expr.contains('.') {
                // Already has dots, prepend resource name if not present
                if expr.to_lowercase().starts_with(&resource_name.to_lowercase()) {
                    expr.to_string()
                } else {
                    format!("{}.{}", resource_name, expr)
                }
            } else {
                // Simple property name
                format!("{}.{}", resource_name, expr)
            };

            // Check for .ofType() cast
            let (base_expr, cast_type) = if let Some(idx) = full_expr.find(".ofType(") {
                let base = &full_expr[..idx];
                let cast_start = idx + 8; // ".ofType(".len()
                let cast_end = full_expr[cast_start..].find(')').map(|i| cast_start + i);
                let cast = cast_end.map(|end| full_expr[cast_start..end].to_string());
                (base.to_string(), cast)
            } else {
                (full_expr, None)
            };

            // Build the property reference
            let prop_ref = base_expr.to_lowercase();

            // Create cast if needed
            let casts = cast_type.map(|cast| Casts {
                cast: vec![Cast { to: cast }],
            });

            component_vec.push(Component {
                ref_attr: prop_ref,
                casts,
            });
        }
    }

    if component_vec.is_empty() {
        None
    } else {
        Some(Components {
            component: component_vec,
        })
    }
}

// Parse .where() clause
fn parse_where_clause(where_expr: &str, part: &mut Part) {
    // Handle system='email' pattern
    if let Some(idx) = where_expr.find('=') {
        let property = where_expr[..idx].trim();
        let value = where_expr[idx + 1..]
            .trim()
            .trim_matches(|c| c == '\'' || c == '"');

        part.property = Some(property.to_string());
        part.type_attr = Some(value.to_string());
    }
}

fn build_operations(operation_defs: &[OperationDefinition], model: &mut Fhir) {
    for operation_def in operation_defs {
        if let Some(resource_types) = &operation_def.resource {
            for resource_type in resource_types {
                // Find the resource in the model
                if let Some(resource) = model.resources.resource.iter_mut().find(|r| r.name == *resource_type) {
                    let operation = Operation {
                        name: operation_def.code.clone(),
                        description: operation_def
                            .description
                            .as_ref()
                            .unwrap_or(&String::new())
                            .replace('\r', "")
                            .replace('\n', ""),
                        input: Input {
                            parameter: Vec::new(),
                        },
                        output: Output {
                            parameter: Vec::new(),
                        },
                    };

                    resource.operations.operation.push(operation);

                    // Add parameters
                    if let Some(parameters) = &operation_def.parameter {
                        let resource_operation = resource.operations.operation.last_mut().unwrap();

                        for param_component in parameters {
                            let pc_type_string = param_component.r#type.as_deref().unwrap_or("");

                            let param_type = if is_primitive(pc_type_string) {
                                // Make first char lowercase
                                let mut chars = pc_type_string.chars();
                                match chars.next() {
                                    Some(c) => format!("{}{}", c.to_lowercase(), chars.as_str()),
                                    None => String::new(),
                                }
                            } else {
                                "resource".to_string()
                            };

                            let reference = if is_primitive(pc_type_string)
                                || pc_type_string == "Any"
                                || pc_type_string == "Bundle"
                                || pc_type_string == "Resource"
                            {
                                None
                            } else {
                                Some(pc_type_string.to_lowercase())
                            };

                            let mut parameter = Parameter {
                                name: param_component.name.clone(),
                                description: param_component
                                    .documentation
                                    .as_ref()
                                    .unwrap_or(&String::new())
                                    .replace('\n', "")
                                    .replace('\r', "")
                                    .replace('\'', ""),
                                type_attr: Some(param_type),
                                reference,
                                is_collection: if param_component.max == "*" {
                                    Some("true".to_string())
                                } else {
                                    None
                                },
                                not_null: if param_component.min == 1 {
                                    Some("true".to_string())
                                } else {
                                    None
                                },
                            };

                            // Clean up type attribute
                            if parameter.type_attr.as_deref() == Some("string")
                                || parameter.type_attr.as_deref() == Some("")
                                || (parameter.type_attr.as_deref() == Some("resource")
                                    && parameter.reference.is_none())
                            {
                                parameter.type_attr = None;
                            }

                            // Add to input or output
                            if param_component.r#use == "in" {
                                resource_operation.input.parameter.push(parameter);
                            } else {
                                resource_operation.output.parameter.push(parameter);
                            }
                        }
                    }
                }
            }
        }
    }
}

fn normalize_model(model: &mut Fhir) {
    // Normalize resources
    for resource in &mut model.resources.resource {
        resource.description = strip_non_ascii(&resource.description);
        normalize_properties(&mut resource.properties.property);
        normalize_elements(&mut resource.elements.element);
        normalize_codesets(&mut resource.codesets.codeset);
        normalize_operations(&mut resource.operations.operation);
    }

    // Normalize data type elements
    for element in &mut model.elements.element {
        element.description = strip_non_ascii(&element.description);
        normalize_properties(&mut element.properties.property);
        normalize_elements(&mut element.elements.element);
        normalize_codesets(&mut element.codesets.codeset);
    }

    // Normalize global codesets
    normalize_codesets(&mut model.codesets.codeset);
}

fn normalize_operations(operations: &mut Vec<Operation>) {
    for operation in operations {
        operation.description = strip_non_ascii(&operation.description);
        for param in &mut operation.input.parameter {
            param.description = strip_non_ascii(&param.description);
        }
        for param in &mut operation.output.parameter {
            param.description = strip_non_ascii(&param.description);
        }
    }
}

fn normalize_properties(properties: &mut Vec<Property>) {
    for property in properties {
        // Fix property IDs containing [x] for choice types - remove [x] suffix entirely
        if property.id.ends_with("[x]") {
            property.id = property.id[..property.id.len() - 3].to_string();
        }

        // Strip non-ASCII characters from descriptions
        property.description = strip_non_ascii(&property.description);

        // Handle variants based on count
        if property.variants.variant.len() == 1 {
            // Single variant: flatten - move type and ref to property level
            let variant = &property.variants.variant[0];
            property.type_attr = variant.type_attr.clone();
            property.ref_attr = variant.ref_attr.clone();
            property.targets = variant.targets.clone();
            // Clear variants since we've moved the data to property level
            property.variants.variant.clear();
        } else if property.variants.variant.len() > 1 {
            // Multiple variants: set type="variant" on property
            property.type_attr = Some("variant".to_string());
        }
    }
}

fn normalize_elements(elements: &mut Vec<Element>) {
    for element in elements {
        // Strip non-ASCII characters from descriptions
        element.description = strip_non_ascii(&element.description);

        normalize_properties(&mut element.properties.property);
        normalize_elements(&mut element.elements.element);
    }
}

fn normalize_codesets(codesets: &mut Vec<Codeset>) {
    for codeset in codesets {
        // Strip non-ASCII characters from codeset descriptions
        codeset.description = strip_non_ascii(&codeset.description);

        // Strip non-ASCII characters from code descriptions
        for code in &mut codeset.codes.code {
            code.description = strip_non_ascii(&code.description);
        }
    }
}

// Helper to strip non-ASCII and normalize descriptions
fn strip_non_ascii(s: &str) -> String {
    s.chars()
        .filter(|c| c.is_ascii())
        .collect()
}

fn copy_dict_to_model(
    dict_elements: &HashMap<String, Vec<Element>>,
    dict_properties: &HashMap<String, Vec<Property>>,
    model: &mut Fhir,
) {
    // Copy properties and elements into resources
    for resource in &mut model.resources.resource {
        if let Some(properties) = dict_properties.get(&resource.id) {
            resource.properties.property = properties.clone();
        }
        if let Some(elements) = dict_elements.get(&resource.id) {
            resource.elements.element = elements.clone();
            // Recursively copy properties and elements for nested backbone elements
            copy_nested_elements(&mut resource.elements.element, dict_elements, dict_properties);
        }
    }

    // Copy properties and elements into data type elements
    for element in &mut model.elements.element {
        if let Some(properties) = dict_properties.get(&element.id) {
            element.properties.property = properties.clone();
        }
        if let Some(elements) = dict_elements.get(&element.id) {
            element.elements.element = elements.clone();
            // Recursively copy properties and elements for nested backbone elements
            copy_nested_elements(&mut element.elements.element, dict_elements, dict_properties);
        }
    }
}

fn copy_nested_elements(
    elements: &mut Vec<Element>,
    dict_elements: &HashMap<String, Vec<Element>>,
    dict_properties: &HashMap<String, Vec<Property>>,
) {
    for element in elements {
        if let Some(properties) = dict_properties.get(&element.id) {
            element.properties.property = properties.clone();
        }
        if let Some(nested_elements) = dict_elements.get(&element.id) {
            element.elements.element = nested_elements.clone();
            // Recursively handle deeper nesting
            copy_nested_elements(&mut element.elements.element, dict_elements, dict_properties);
        }
    }
}

fn write_xml_output(model: &Fhir) -> Result<(), Box<dyn Error>> {
    use quick_xml::se::to_string_with_root;
    use quick_xml::{Reader, Writer, events::Event};
    use std::io::Cursor;

    let xml = to_string_with_root("fhir", model)?;

    // Pretty-print the XML
    let mut reader = Reader::from_str(&xml);
    reader.trim_text(true);

    let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 2);

    loop {
        match reader.read_event() {
            Ok(Event::Eof) => break,
            Ok(event) => writer.write_event(event)?,
            Err(e) => return Err(Box::new(e)),
        }
    }

    let pretty_xml = String::from_utf8(writer.into_inner().into_inner())?;

    // Add DOCTYPE and XML declaration
    let output = format!(
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE fhir SYSTEM \"fhirspec.dtd\">\n{}",
        pretty_xml
    );

    fs::write("fhir.xml", output)?;
    Ok(())
}
