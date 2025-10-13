use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use clap::Parser;

/// FHIR R5 JSON to XML converter - generates curated XML model for Fire
#[derive(Parser, Debug)]
#[command(name = "fhir-to-xml")]
#[command(about = "Convert FHIR R5 JSON definitions to curated XML model", long_about = None)]
struct Args {
    /// Input directory containing FHIR R5 JSON files
    #[arg(short, long, default_value = "fhir-r5")]
    input: PathBuf,

    /// Output XML file path
    #[arg(short, long, default_value = "model/fhir.xml")]
    output: PathBuf,

    /// Resources to process (comma-separated, default: all)
    #[arg(short, long)]
    resources: Option<String>,
}

#[derive(Debug, Deserialize)]
struct FhirBundle {
    #[serde(rename = "resourceType")]
    resource_type: String,
    entry: Vec<BundleEntry>,
}

#[derive(Debug, Deserialize)]
struct BundleEntry {
    resource: Value,
}

#[derive(Debug, Deserialize)]
struct StructureDefinition {
    #[serde(rename = "resourceType")]
    resource_type: String,
    id: String,
    name: String,
    kind: String,
    #[serde(rename = "type")]
    type_name: String,
    description: Option<String>,
    snapshot: Option<Snapshot>,
}

#[derive(Debug, Deserialize)]
struct Snapshot {
    element: Vec<ElementDefinition>,
}

#[derive(Debug, Deserialize)]
struct ElementDefinition {
    id: String,
    path: String,
    #[serde(rename = "type")]
    types: Option<Vec<ElementType>>,
    #[serde(rename = "short")]
    short_description: Option<String>,
    definition: Option<String>,
    min: Option<u32>,
    max: Option<String>,
    binding: Option<ElementBinding>,
    #[serde(rename = "isSummary")]
    is_summary: Option<bool>,
    #[serde(rename = "isModifier")]
    is_modifier: Option<bool>,
}

#[derive(Debug, Deserialize)]
struct ElementBinding {
    strength: Option<String>,
    #[serde(rename = "valueSet")]
    value_set: Option<String>,
    extension: Option<Vec<Value>>,
}

#[derive(Debug, Deserialize)]
struct ElementType {
    code: String,
    #[serde(rename = "targetProfile")]
    target_profile: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize)]
struct SearchParameter {
    #[serde(rename = "resourceType")]
    resource_type: String,
    name: String,
    code: String,
    base: Vec<String>,
    #[serde(rename = "type")]
    param_type: String,
    expression: Option<String>,
    #[serde(rename = "multipleOr")]
    multiple_or: Option<bool>,
    #[serde(rename = "multipleAnd")]
    multiple_and: Option<bool>,
    component: Option<Vec<SearchComponent>>,
    target: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize)]
struct SearchComponent {
    definition: String,
    expression: String,
}

#[derive(Debug, Deserialize)]
struct CodeSystem {
    #[serde(rename = "resourceType")]
    resource_type: String,
    id: String,
    name: String,
    description: Option<String>,
    concept: Option<Vec<CodeConcept>>,
}

#[derive(Debug, Deserialize)]
struct CodeConcept {
    code: String,
    display: String,
    definition: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    println!("🔧 fhir-to-xml - Converting FHIR R5 definitions to XML model");
    println!("   Input:  {}", args.input.display());
    println!("   Output: {}", args.output.display());

    // Step 1: Load FHIR R5 JSON files
    println!("\n📖 Loading FHIR R5 definitions...");
    let structures = load_structure_definitions(&args.input)?;
    let complex_types = load_complex_types(&args.input)?;
    let search_params = load_search_parameters(&args.input)?;
    let code_systems = load_code_systems(&args.input)?;

    println!("   Found {} structure definitions", structures.len());
    println!("   Found {} complex type definitions", complex_types.len());
    println!("   Found {} search parameters", search_params.len());
    println!("   Found {} code systems", code_systems.len());

    // Step 2: Filter resources if specified
    let resource_filter: Option<Vec<String>> = args.resources.map(|r| {
        r.split(',').map(|s| s.trim().to_string()).collect()
    });

    // Step 3: Extract and organize data
    println!("\n🔨 Extracting resource definitions...");
    let resources = extract_resources(&structures, &search_params, &resource_filter)?;
    println!("   Extracted {} resources", resources.len());

    // Step 4: Generate XML
    println!("\n📝 Generating XML model...");
    let xml = generate_xml(&resources, &complex_types, &code_systems)?;

    // Step 5: Write output
    if let Some(parent) = args.output.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&args.output, xml)?;

    println!("\n✅ XML model written to {}", args.output.display());
    println!("   Next steps:");
    println!("   1. Review and curate model/fhir.xml");
    println!("   2. Add <note> elements for deviations from spec");
    println!("   3. Run DSSSL generators to create Rust code");

    Ok(())
}

fn load_structure_definitions(input_dir: &PathBuf) -> Result<Vec<StructureDefinition>, Box<dyn std::error::Error>> {
    let profiles_path = input_dir.join("profiles-resources.json");
    let content = fs::read_to_string(&profiles_path)?;
    let bundle: FhirBundle = serde_json::from_str(&content)?;

    let mut structures = Vec::new();
    for entry in bundle.entry {
        if let Ok(def) = serde_json::from_value::<StructureDefinition>(entry.resource) {
            // Only include resource types, not complex types or primitives
            if def.kind == "resource" {
                structures.push(def);
            }
        }
    }

    Ok(structures)
}

fn load_complex_types(input_dir: &PathBuf) -> Result<Vec<StructureDefinition>, Box<dyn std::error::Error>> {
    let types_path = input_dir.join("profiles-types.json");
    let content = fs::read_to_string(&types_path)?;
    let bundle: FhirBundle = serde_json::from_str(&content)?;

    let mut types = Vec::new();
    for entry in bundle.entry {
        if let Ok(def) = serde_json::from_value::<StructureDefinition>(entry.resource) {
            // Only include complex types (data types like Address, HumanName, etc.)
            if def.kind == "complex-type" {
                types.push(def);
            }
        }
    }

    Ok(types)
}

fn load_search_parameters(input_dir: &PathBuf) -> Result<HashMap<String, Vec<SearchParameter>>, Box<dyn std::error::Error>> {
    let search_path = input_dir.join("search-parameters.json");
    let content = fs::read_to_string(&search_path)?;
    let bundle: FhirBundle = serde_json::from_str(&content)?;

    let mut by_resource: HashMap<String, Vec<SearchParameter>> = HashMap::new();

    for entry in bundle.entry {
        if let Ok(param) = serde_json::from_value::<SearchParameter>(entry.resource) {
            for base in &param.base {
                by_resource.entry(base.clone()).or_insert_with(Vec::new).push(param.clone());
                break; // Only add once per parameter
            }
        }
    }

    Ok(by_resource)
}

fn load_code_systems(input_dir: &PathBuf) -> Result<Vec<CodeSystem>, Box<dyn std::error::Error>> {
    let valuesets_path = input_dir.join("valuesets.json");
    let content = fs::read_to_string(&valuesets_path)?;
    let bundle: FhirBundle = serde_json::from_str(&content)?;

    let mut code_systems = Vec::new();
    for entry in bundle.entry {
        if let Ok(cs) = serde_json::from_value::<CodeSystem>(entry.resource) {
            if cs.resource_type == "CodeSystem" && cs.concept.is_some() {
                code_systems.push(cs);
            }
        }
    }

    Ok(code_systems)
}

#[derive(Debug)]
struct ResourceDefinition {
    name: String,
    description: String,
    elements: Vec<ElementInfo>,
    backbone_elements: Vec<BackboneElementInfo>,
    search_params: Vec<SearchParamInfo>,
}

#[derive(Debug)]
struct BackboneElementInfo {
    name: String,
    path: String,
    description: String,
    properties: Vec<ElementInfo>,
}

#[derive(Debug, Clone)]
struct TypeInfo {
    code: String,
    target_resources: Vec<String>,  // For Reference types
}

#[derive(Debug, Clone)]
struct ElementInfo {
    name: String,
    path: String,
    types: Vec<TypeInfo>,
    description: String,
    cardinality: String,
    binding_name: Option<String>, // ValueSet binding name (e.g., "AdministrativeGender")
    is_summary: bool,
    is_modifier: bool,
}

#[derive(Debug)]
struct SearchParamInfo {
    name: String,
    code: String,
    param_type: String,
    expression: String,
    is_composite: bool,
    components: Vec<ComponentInfo>,
    targets: Vec<String>,
}

#[derive(Debug)]
struct ComponentInfo {
    expression: String,
}

fn extract_resources(
    structures: &[StructureDefinition],
    search_params: &HashMap<String, Vec<SearchParameter>>,
    filter: &Option<Vec<String>>,
) -> Result<Vec<ResourceDefinition>, Box<dyn std::error::Error>> {
    let mut resources = Vec::new();

    for structure in structures {
        // Apply filter if specified
        if let Some(ref filter_list) = filter {
            if !filter_list.contains(&structure.name) {
                continue;
            }
        }

        let elements = extract_elements(structure);
        let backbone_elements = extract_backbone_elements(structure);
        let search = extract_search_params(&structure.name, search_params);

        resources.push(ResourceDefinition {
            name: structure.name.clone(),
            description: structure.description.clone().unwrap_or_default(),
            elements,
            backbone_elements,
            search_params: search,
        });
    }

    Ok(resources)
}

fn extract_elements(structure: &StructureDefinition) -> Vec<ElementInfo> {
    let mut elements = Vec::new();

    if let Some(snapshot) = &structure.snapshot {
        for elem in &snapshot.element {
            // Skip root element and certain infrastructure field children (but include id, meta, implicitRules, language, text!)
            // Use exact match or match with trailing dot to avoid false positives
            // e.g., "Patient.id" should not match "Patient.identifier"
            if elem.path == structure.type_name
                || elem.path.starts_with(&format!("{}.id.", structure.type_name))  // Skip id children (e.g., Patient.id.extension)
                || elem.path.starts_with(&format!("{}.meta.", structure.type_name))  // Skip meta children
                || elem.path.starts_with(&format!("{}.implicitRules.", structure.type_name))  // Skip implicitRules children, but include implicitRules itself
                || elem.path.starts_with(&format!("{}.language.", structure.type_name))  // Skip language children, but include language itself
                || elem.path.starts_with(&format!("{}.text.", structure.type_name))  // Skip text children, but include text itself
            {
                continue;
            }

            // Only include direct properties of the resource
            if elem.path.matches('.').count() == 1 {
                let name = elem.path.split('.').last().unwrap_or("").to_string();
                let types = elem.types.as_ref()
                    .map(|t| t.iter().map(|et| TypeInfo {
                        code: et.code.clone(),
                        target_resources: et.target_profile.as_ref()
                            .map(|profiles| profiles.iter()
                                .filter_map(|url| url.split('/').last().map(String::from))
                                .collect())
                            .unwrap_or_default()
                    }).collect())
                    .unwrap_or_default();

                let cardinality = format!("{}..{}",
                    elem.min.unwrap_or(0),
                    elem.max.as_ref().map(|s| s.as_str()).unwrap_or("*")
                );

                // Extract binding name from elementdefinition-bindingName extension
                let binding_name = elem.binding.as_ref().and_then(|binding| {
                    binding.extension.as_ref().and_then(|extensions| {
                        for ext in extensions {
                            if let Some(url) = ext.get("url").and_then(|v| v.as_str()) {
                                if url == "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName" {
                                    if let Some(value) = ext.get("valueString").and_then(|v| v.as_str()) {
                                        return Some(value.to_string());
                                    }
                                }
                            }
                        }
                        None
                    })
                });

                elements.push(ElementInfo {
                    name,
                    path: elem.path.clone(),
                    types,
                    description: elem.definition.clone()
                        .or_else(|| elem.short_description.clone())
                        .unwrap_or_default(),
                    cardinality,
                    binding_name,
                    is_summary: elem.is_summary.unwrap_or(false),
                    is_modifier: elem.is_modifier.unwrap_or(false),
                });
            }
        }
    }

    elements
}

fn extract_backbone_elements(structure: &StructureDefinition) -> Vec<BackboneElementInfo> {
    let mut backbone_elements = Vec::new();

    if let Some(snapshot) = &structure.snapshot {
        // First pass: identify backbone elements (direct children with BackboneElement type)
        let mut backbone_paths = Vec::new();
        for elem in &snapshot.element {
            // Check if this is a direct child (e.g., Observation.component)
            if elem.path.matches('.').count() == 1 && elem.path != structure.type_name {
                // Check if it's a BackboneElement
                if let Some(types) = &elem.types {
                    if types.len() == 1 && types[0].code == "BackboneElement" {
                        backbone_paths.push(elem.path.clone());
                    }
                }
            }
        }

        // Second pass: extract properties for each backbone element
        for backbone_path in backbone_paths {
            let backbone_name = backbone_path.split('.').last().unwrap_or("").to_string();
            let prefix = format!("{}.", backbone_path);
            let mut properties = Vec::new();

            // Find description
            let description = snapshot.element.iter()
                .find(|e| e.path == backbone_path)
                .and_then(|e| e.definition.clone().or_else(|| e.short_description.clone()))
                .unwrap_or_default();

            // Extract properties (children of this backbone element)
            for elem in &snapshot.element {
                if elem.path.starts_with(&prefix) && elem.path.matches('.').count() == 2 {
                    // Skip extension fields
                    if elem.path.ends_with(".id")
                        || elem.path.ends_with(".extension")
                        || elem.path.ends_with(".modifierExtension") {
                        continue;
                    }

                    let name = elem.path.split('.').last().unwrap_or("").to_string();
                    let types = elem.types.as_ref()
                        .map(|t| t.iter().map(|et| TypeInfo {
                            code: et.code.clone(),
                            target_resources: et.target_profile.as_ref()
                                .map(|profiles| profiles.iter()
                                    .filter_map(|url| url.split('/').last().map(String::from))
                                    .collect())
                                .unwrap_or_default()
                        }).collect())
                        .unwrap_or_default();

                    let cardinality = format!("{}..{}",
                        elem.min.unwrap_or(0),
                        elem.max.as_ref().map(|s| s.as_str()).unwrap_or("*")
                    );

                    // Extract binding name
                    let binding_name = elem.binding.as_ref().and_then(|binding| {
                        binding.extension.as_ref().and_then(|extensions| {
                            for ext in extensions {
                                if let Some(url) = ext.get("url").and_then(|v| v.as_str()) {
                                    if url == "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName" {
                                        if let Some(value) = ext.get("valueString").and_then(|v| v.as_str()) {
                                            return Some(value.to_string());
                                        }
                                    }
                                }
                            }
                            None
                        })
                    });

                    properties.push(ElementInfo {
                        name,
                        path: elem.path.clone(),
                        types,
                        description: elem.definition.clone()
                            .or_else(|| elem.short_description.clone())
                            .unwrap_or_default(),
                        cardinality,
                        binding_name,
                        is_summary: elem.is_summary.unwrap_or(false),
                        is_modifier: elem.is_modifier.unwrap_or(false),
                    });
                }
            }

            backbone_elements.push(BackboneElementInfo {
                name: backbone_name,
                path: backbone_path,
                description,
                properties,
            });
        }
    }

    backbone_elements
}

fn extract_search_params(
    resource_name: &str,
    all_search_params: &HashMap<String, Vec<SearchParameter>>,
) -> Vec<SearchParamInfo> {
    let mut params = Vec::new();

    if let Some(resource_params) = all_search_params.get(resource_name) {
        for param in resource_params {
            let is_composite = param.component.is_some();
            let components = param.component.as_ref()
                .map(|comps| comps.iter().map(|c| ComponentInfo {
                    expression: c.expression.clone(),
                }).collect())
                .unwrap_or_default();

            let targets = param.target.clone().unwrap_or_default();

            params.push(SearchParamInfo {
                name: param.name.clone(),
                code: param.code.clone(),
                param_type: param.param_type.clone(),
                expression: param.expression.clone().unwrap_or_default(),
                is_composite,
                components,
                targets,
            });
        }
    }

    params
}

fn generate_xml(resources: &[ResourceDefinition], all_complex_types: &[StructureDefinition], all_code_systems: &[CodeSystem]) -> Result<String, Box<dyn std::error::Error>> {
    let mut xml = String::new();

    // XML header and DTD reference
    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<!DOCTYPE fhir SYSTEM \"fhirspec.dtd\">\n\n");

    // Root element
    xml.push_str("<fhir name=\"Fire\" version=\"5.0.0\">\n");
    xml.push_str("  <resources>\n");

    for resource in resources {
        generate_resource_xml(&mut xml, resource)?;
    }

    xml.push_str("  </resources>\n");

    // Collect all referenced element types recursively
    let mut referenced_types = HashSet::new();
    let mut to_process = Vec::new();

    // Start with types referenced by resources
    for resource in resources {
        for element in &resource.elements {
            for type_info in &element.types {
                let dtd_type = map_fhir_type_to_dtd(&type_info.code);
                if dtd_type == "element" {
                    if referenced_types.insert(type_info.code.as_str()) {
                        to_process.push(type_info.code.as_str());
                    }
                }
            }
        }
    }

    // Create a map for quick lookup
    let type_map: HashMap<&str, &StructureDefinition> = all_complex_types
        .iter()
        .map(|t| (t.type_name.as_str(), t))
        .collect();

    // Recursively follow references
    while let Some(type_name) = to_process.pop() {
        if let Some(complex_type) = type_map.get(type_name) {
            if let Some(snapshot) = &complex_type.snapshot {
                for elem in &snapshot.element {
                    if let Some(types) = &elem.types {
                        for elem_type in types {
                            let dtd_type = map_fhir_type_to_dtd(&elem_type.code);
                            if dtd_type == "element" {
                                if referenced_types.insert(elem_type.code.as_str()) {
                                    to_process.push(elem_type.code.as_str());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Generate element definitions for all referenced types
    xml.push_str("  <elements>\n");

    // Add hardcoded Resource element (abstract base type)
    xml.push_str("    <element id=\"resource\" name=\"Resource\">\n");
    xml.push_str("      <description>Base Resource (abstract)</description>\n");
    xml.push_str("      <properties>\n");
    // Suppress defaults: type="string" becomes type="id", iscollection="false", notnull="false"
    // Output on one line with id last
    xml.push_str("        <property name=\"id\" type=\"id\" id=\"resource.id\"><description>Logical id of this artifact</description></property>\n");
    // type="element" is not default, ref is required, suppress iscollection and notnull defaults
    xml.push_str("        <property name=\"meta\" type=\"element\" ref=\"meta\" id=\"resource.meta\"><description>Metadata about the resource</description></property>\n");
    xml.push_str("      </properties>\n");
    xml.push_str("      <elements/>\n");
    xml.push_str("      <codesets/>\n");
    xml.push_str("    </element>\n");

    // Track generated IDs to avoid duplicates
    let mut generated_ids = HashSet::new();

    for complex_type in all_complex_types {
        let type_name = complex_type.type_name.as_str();
        if referenced_types.contains(type_name) {
            let element_id = type_name.to_lowercase();

            // Skip if already generated (e.g., MoneyQuantity and SimpleQuantity duplicate Quantity)
            if generated_ids.insert(element_id.clone()) {
                generate_element_xml(&mut xml, complex_type)?;
            }
        }
    }
    xml.push_str("  </elements>\n");

    // Collect all referenced codesets
    let mut referenced_codesets = HashSet::new();

    // From resources
    for resource in resources {
        for element in &resource.elements {
            if let Some(binding) = &element.binding_name {
                referenced_codesets.insert(to_lowercase_preserve_digits(binding));
            }
        }

        // From backbone elements
        for backbone in &resource.backbone_elements {
            for property in &backbone.properties {
                if let Some(binding) = &property.binding_name {
                    referenced_codesets.insert(to_lowercase_preserve_digits(binding));
                }
            }
        }
    }

    // From elements
    for complex_type in all_complex_types {
        if let Some(snapshot) = &complex_type.snapshot {
            for elem in &snapshot.element {
                if let Some(binding) = &elem.binding {
                    if let Some(extensions) = &binding.extension {
                        for ext in extensions {
                            if let Some(url) = ext.get("url").and_then(|v| v.as_str()) {
                                if url == "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName" {
                                    if let Some(value) = ext.get("valueString").and_then(|v| v.as_str()) {
                                        referenced_codesets.insert(to_lowercase_preserve_digits(value));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Generate codesets
    xml.push_str("  <codesets>\n");

    // Create multiple maps for different lookup strategies
    // 1. By normalized ID (e.g., "observation-triggeredbytype" -> "observationtriggeredbytype")
    let codeset_map_by_id: HashMap<String, &CodeSystem> = all_code_systems
        .iter()
        .map(|cs| (cs.id.replace("-", "").to_lowercase(), cs))
        .collect();

    // 2. By normalized name (e.g., "TriggeredBytype" -> "triggeredbytype")
    let codeset_map_by_name: HashMap<String, &CodeSystem> = all_code_systems
        .iter()
        .map(|cs| (to_lowercase_preserve_digits(&cs.name), cs))
        .collect();

    for codeset_id in referenced_codesets.iter() {
        // Try lookup by name first (binding name), then by ID
        let code_system = codeset_map_by_name.get(codeset_id.as_str())
            .or_else(|| codeset_map_by_id.get(codeset_id.as_str()));

        if let Some(code_system) = code_system {
            generate_codeset_xml(&mut xml, code_system)?;
        }
    }

    // Add hardcoded stub codesets for external references
    add_stub_codesets(&mut xml);

    xml.push_str("  </codesets>\n");
    xml.push_str("</fhir>\n");

    Ok(xml)
}

fn add_stub_codesets(xml: &mut String) {
    // MimeType - IANA MIME types
    xml.push_str("    <codeset name=\"MimeType\" id=\"mimetype\">\n");
    xml.push_str("      <description>IANA MIME types (placeholder - use IANA registry)</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"JSON\" value=\"application/json\">\n");
    xml.push_str("          <description>JSON format</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"XML\" value=\"application/xml\">\n");
    xml.push_str("          <description>XML format</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // Language - BCP 47
    xml.push_str("    <codeset name=\"Language\" id=\"language\">\n");
    xml.push_str("      <description>BCP 47 language codes (placeholder)</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"English\" value=\"en\">\n");
    xml.push_str("          <description>English</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"English (US)\" value=\"en-US\">\n");
    xml.push_str("          <description>English (United States)</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // CurrencyCode - ISO 4217
    xml.push_str("    <codeset name=\"CurrencyCode\" id=\"currencycode\">\n");
    xml.push_str("      <description>ISO 4217 currency codes (placeholder)</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"US Dollar\" value=\"USD\">\n");
    xml.push_str("          <description>US Dollar</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Euro\" value=\"EUR\">\n");
    xml.push_str("          <description>Euro</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // Units - UCUM
    xml.push_str("    <codeset name=\"Units\" id=\"units\">\n");
    xml.push_str("      <description>UCUM units (placeholder)</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"Seconds\" value=\"s\">\n");
    xml.push_str("          <description>Seconds</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Minutes\" value=\"min\">\n");
    xml.push_str("          <description>Minutes</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // ParameterUse
    xml.push_str("    <codeset name=\"ParameterUse\" id=\"parameteruse\">\n");
    xml.push_str("      <description>Operation parameter use</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"In\" value=\"in\">\n");
    xml.push_str("          <description>Input parameter</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Out\" value=\"out\">\n");
    xml.push_str("          <description>Output parameter</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // ExpressionLanguage
    xml.push_str("    <codeset name=\"ExpressionLanguage\" id=\"expressionlanguage\">\n");
    xml.push_str("      <description>Expression languages</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"FHIRPath\" value=\"text/fhirpath\">\n");
    xml.push_str("          <description>FHIRPath expression language</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"CQL\" value=\"text/cql\">\n");
    xml.push_str("          <description>Clinical Quality Language</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    // RelatedArtifactPublicationStatus
    xml.push_str("    <codeset name=\"RelatedArtifactPublicationStatus\" id=\"relatedartifactpublicationstatus\">\n");
    xml.push_str("      <description>Publication status of related artifacts</description>\n");
    xml.push_str("      <codes>\n");
    xml.push_str("        <code name=\"Draft\" value=\"draft\">\n");
    xml.push_str("          <description>Draft</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Active\" value=\"active\">\n");
    xml.push_str("          <description>Active</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Retired\" value=\"retired\">\n");
    xml.push_str("          <description>Retired</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("        <code name=\"Unknown\" value=\"unknown\">\n");
    xml.push_str("          <description>Unknown</description>\n");
    xml.push_str("        </code>\n");
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");
}

fn generate_codeset_xml(xml: &mut String, code_system: &CodeSystem) -> Result<(), Box<dyn std::error::Error>> {
    // Use the CodeSystem.name (binding name) as the ID, not the CodeSystem.id
    // This matches how properties reference codesets via binding names
    // e.g., binding name "TriggeredByType" -> id "triggeredbytype"
    let codeset_id = to_lowercase_preserve_digits(&code_system.name);

    xml.push_str(&format!("    <codeset name=\"{}\" id=\"{}\">\n", code_system.name, codeset_id));
    xml.push_str(&format!("      <description>{}</description>\n",
        escape_xml(&code_system.description.as_ref().unwrap_or(&String::new()))));

    xml.push_str("      <codes>\n");
    if let Some(concepts) = &code_system.concept {
        for concept in concepts {
            xml.push_str(&format!("        <code name=\"{}\" value=\"{}\">\n",
                escape_xml(&concept.display),
                escape_xml(&concept.code)));
            xml.push_str(&format!("          <description>{}</description>\n",
                escape_xml(&concept.definition.as_ref().unwrap_or(&concept.display))));
            xml.push_str("        </code>\n");
        }
    }
    xml.push_str("      </codes>\n");
    xml.push_str("    </codeset>\n");

    Ok(())
}

fn generate_resource_xml(xml: &mut String, resource: &ResourceDefinition) -> Result<(), Box<dyn std::error::Error>> {
    let resource_id = resource.name.to_lowercase();

    // Resources with reference implementations
    let implemented_resources = ["Patient", "Observation", "Practitioner"];
    let is_active = implemented_resources.contains(&resource.name.as_str());

    // Only output active="true" if resource has a reference implementation (default is "false")
    let active_attr = if is_active {
        " active=\"true\" "
    } else {
        " "  // Just a space before id
    };

    xml.push_str(&format!("    <resource name=\"{}\"{}id=\"{}\">\n", resource.name, active_attr, resource_id));
    xml.push_str(&format!("      <description>{}</description>\n", escape_xml(&resource.description)));

    // Properties (resource-level attributes)
    xml.push_str("      <properties>\n");
    for element in &resource.elements {
        generate_property_xml_with_backbone(xml, element, &resource.name, &resource.backbone_elements)?;
    }
    xml.push_str("      </properties>\n");

    // Elements (backbone elements - nested complex types)
    if resource.backbone_elements.is_empty() {
        xml.push_str("      <elements/>\n");
    } else {
        xml.push_str("      <elements>\n");
        for backbone in &resource.backbone_elements {
            generate_backbone_element_xml(xml, backbone, &resource.name)?;
        }
        xml.push_str("      </elements>\n");
    }

    // Codesets (local ValueSets)
    xml.push_str("      <codesets/>\n");

    // Search parameters
    xml.push_str("      <searches>\n");
    for param in &resource.search_params {
        generate_search_xml(xml, param, &resource.name)?;
    }
    xml.push_str("      </searches>\n");

    xml.push_str("    </resource>\n");

    Ok(())
}

fn generate_backbone_element_xml(xml: &mut String, backbone: &BackboneElementInfo, _resource_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Generate element ID following Telemed5000 convention
    // e.g., "Observation.component" -> ID "observationcomponent"
    let element_id = backbone.path.replace(".", "").to_lowercase();
    let element_name = capitalize_first(&backbone.name);

    // Only output backbone="true" since default is "false"
    xml.push_str(&format!("        <element id=\"{}\" name=\"{}\" backbone=\"true\">\n", element_id, element_name));
    xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&backbone.description)));

    // Properties
    xml.push_str("          <properties>\n");
    for property in &backbone.properties {
        // Generate property XML with the element_id as the parent
        // e.g., "observationcomponent.code"
        generate_property_xml_with_parent(xml, property, &element_id)?;
    }
    xml.push_str("          </properties>\n");

    xml.push_str("          <elements/>\n");
    xml.push_str("          <codesets/>\n");
    xml.push_str("        </element>\n");

    Ok(())
}

fn generate_element_xml(xml: &mut String, complex_type: &StructureDefinition) -> Result<(), Box<dyn std::error::Error>> {
    let element_id = complex_type.type_name.to_lowercase();

    xml.push_str(&format!("    <element id=\"{}\" name=\"{}\">\n", element_id, complex_type.name));
    xml.push_str(&format!("      <description>{}</description>\n", escape_xml(&complex_type.description.as_ref().unwrap_or(&String::new()))));

    // Properties
    xml.push_str("      <properties>\n");
    if let Some(snapshot) = &complex_type.snapshot {
        for elem in &snapshot.element {
            // Only include direct properties of the complex type (not nested or root)
            if elem.path.matches('.').count() == 1 && elem.path != complex_type.type_name {
                let name = elem.path.split('.').last().unwrap_or("").to_string();
                let types = elem.types.as_ref()
                    .map(|t| t.iter().map(|et| TypeInfo {
                        code: et.code.clone(),
                        target_resources: et.target_profile.as_ref()
                            .map(|profiles| profiles.iter()
                                .filter_map(|url| url.split('/').last().map(String::from))
                                .collect())
                            .unwrap_or_default()
                    }).collect())
                    .unwrap_or_default();

                let cardinality = format!("{}..{}",
                    elem.min.unwrap_or(0),
                    elem.max.as_ref().map(|s| s.as_str()).unwrap_or("*")
                );

                // Extract binding name
                let binding_name = elem.binding.as_ref().and_then(|binding| {
                    binding.extension.as_ref().and_then(|extensions| {
                        for ext in extensions {
                            if let Some(url) = ext.get("url").and_then(|v| v.as_str()) {
                                if url == "http://hl7.org/fhir/StructureDefinition/elementdefinition-bindingName" {
                                    if let Some(value) = ext.get("valueString").and_then(|v| v.as_str()) {
                                        return Some(value.to_string());
                                    }
                                }
                            }
                        }
                        None
                    })
                });

                let element_info = ElementInfo {
                    name,
                    path: elem.path.clone(),
                    types,
                    description: elem.definition.clone()
                        .or_else(|| elem.short_description.clone())
                        .unwrap_or_default(),
                    cardinality,
                    binding_name,
                    is_summary: elem.is_summary.unwrap_or(false),
                    is_modifier: elem.is_modifier.unwrap_or(false),
                };

                generate_property_xml(xml, &element_info, &complex_type.name)?;
            }
        }
    }
    xml.push_str("      </properties>\n");

    xml.push_str("      <elements/>\n");
    xml.push_str("      <codesets/>\n");
    xml.push_str("    </element>\n");

    Ok(())
}

fn generate_property_xml(xml: &mut String, element: &ElementInfo, resource_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Follow Telemed5000 naming convention: {resource_lowercase}.{property_lowercase}
    let parent_id = resource_name.to_lowercase();
    generate_property_xml_with_parent(xml, element, &parent_id)
}

fn generate_property_xml_with_backbone(xml: &mut String, element: &ElementInfo, resource_name: &str, backbone_elements: &[BackboneElementInfo]) -> Result<(), Box<dyn std::error::Error>> {
    // Check if this property refers to a BackboneElement
    // If so, replace "BackboneElement" with the specific backbone element ID
    if element.types.len() == 1 && element.types[0].code == "BackboneElement" {
        // Find the matching backbone element
        if let Some(backbone) = backbone_elements.iter().find(|b| b.name == element.name) {
            // Create a modified element with the specific backbone element ref
            let backbone_element_id = backbone.path.replace(".", "").to_lowercase();
            let mut modified_element = element.clone();
            modified_element.types = vec![TypeInfo {
                code: backbone_element_id,
                target_resources: vec![],
            }];
            return generate_property_xml(xml, &modified_element, resource_name);
        }
    }

    // Not a backbone element, generate normally
    generate_property_xml(xml, element, resource_name)
}

fn generate_property_xml_with_parent(xml: &mut String, element: &ElementInfo, parent_id: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Remove [x] suffix from choice types (e.g., deceased[x] -> deceased)
    let property_name_clean = element.name.replace("[x]", "");
    let property_id = format!("{}.{}",
        parent_id,
        to_lowercase_preserve_digits(&property_name_clean)
    );
    let is_collection = element.cardinality.ends_with("*");

    // Check if this is a true variant type (choice type with multiple types)
    let is_variant = element.types.len() > 1;

    if is_variant {
        // Choice type (e.g., value[x], deceased[x]) - use type="variant" and <variants>
        // Suppress default values: type="string" (but variant is not default), iscollection="false", notnull="false", summary="false", modifier="false"
        // Output id last, and use cleaned name (without [x])
        xml.push_str(&format!(
            "        <property name=\"{}\" type=\"variant\"{}{}{}{} id=\"{}\">\n",
            property_name_clean,
            attr_bool_if_not_default("iscollection", is_collection, false),
            attr_bool_if_not_default("notnull", false, false),
            attr_bool_if_not_default("summary", element.is_summary, false),
            attr_bool_if_not_default("modifier", element.is_modifier, false),
            property_id
        ));
        xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&element.description)));

        xml.push_str("          <variants>\n");
        for type_info in &element.types {
            let dtd_type = map_fhir_type_to_dtd(&type_info.code);
            if dtd_type == "element" {
                // Complex type - add ref attribute
                let element_ref = fhir_type_to_element_ref(&type_info.code);

                // Check if this is a Reference with target resources
                if type_info.code == "Reference" && !type_info.target_resources.is_empty() {
                    xml.push_str(&format!("            <variant type=\"{}\" ref=\"{}\">\n", dtd_type, element_ref));
                    xml.push_str("              <targets>\n");
                    for target in &type_info.target_resources {
                        xml.push_str(&format!("                <target resource=\"{}\"/>\n", target));
                    }
                    xml.push_str("              </targets>\n");
                    xml.push_str("            </variant>\n");
                } else {
                    xml.push_str(&format!("            <variant type=\"{}\" ref=\"{}\"/>\n", dtd_type, element_ref));
                }
            } else if dtd_type == "code" && element.binding_name.is_some() {
                // Code with binding - add ref to codeset
                let binding_ref = to_lowercase_preserve_digits(element.binding_name.as_ref().unwrap());
                xml.push_str(&format!("            <variant type=\"{}\" ref=\"{}\"/>\n", dtd_type, binding_ref));
            } else {
                // Primitive type - no ref
                xml.push_str(&format!("            <variant type=\"{}\"/>\n", dtd_type));
            }
        }
        xml.push_str("          </variants>\n");
        xml.push_str("        </property>\n");
    } else if element.types.len() == 1 {
        // Single type - flatten by putting type directly on property
        // Output on one line: <property ...><description>...</description></property>
        let fhir_type = map_fhir_type_to_dtd(&element.types[0].code);

        if fhir_type == "element" {
            // Complex type - add ref attribute
            let element_ref = fhir_type_to_element_ref(&element.types[0].code);
            xml.push_str(&format!(
                "        <property name=\"{}\"{}{}{}{}{}{} id=\"{}\"><description>{}</description></property>\n",
                element.name,
                attr_if_not_default("type", fhir_type, "string"),
                format!(" ref=\"{}\"", element_ref),  // ref is always required when type="element"
                attr_bool_if_not_default("iscollection", is_collection, false),
                attr_bool_if_not_default("notnull", false, false),
                attr_bool_if_not_default("summary", element.is_summary, false),
                attr_bool_if_not_default("modifier", element.is_modifier, false),
                property_id,
                escape_xml(&element.description)
            ));
        } else if fhir_type == "code" && element.binding_name.is_some() {
            // Code type with ValueSet binding - add ref to codeset
            let binding_ref = to_lowercase_preserve_digits(element.binding_name.as_ref().unwrap());
            xml.push_str(&format!(
                "        <property name=\"{}\"{}{}{}{}{}{} id=\"{}\"><description>{}</description></property>\n",
                element.name,
                attr_if_not_default("type", fhir_type, "string"),
                format!(" ref=\"{}\"", binding_ref),  // ref is always required when type="code" with binding
                attr_bool_if_not_default("iscollection", is_collection, false),
                attr_bool_if_not_default("notnull", false, false),
                attr_bool_if_not_default("summary", element.is_summary, false),
                attr_bool_if_not_default("modifier", element.is_modifier, false),
                property_id,
                escape_xml(&element.description)
            ));
        } else {
            // Primitive type - no ref
            xml.push_str(&format!(
                "        <property name=\"{}\"{}{}{}{}{} id=\"{}\"><description>{}</description></property>\n",
                element.name,
                attr_if_not_default("type", fhir_type, "string"),
                attr_bool_if_not_default("iscollection", is_collection, false),
                attr_bool_if_not_default("notnull", false, false),
                attr_bool_if_not_default("summary", element.is_summary, false),
                attr_bool_if_not_default("modifier", element.is_modifier, false),
                property_id,
                escape_xml(&element.description)
            ));
        }
    } else {
        // No type info - default to element without ref (will need manual curation)
        xml.push_str(&format!(
            "        <property name=\"{}\"{}{}{}{}{} id=\"{}\"><description>{}</description></property>\n",
            element.name,
            attr_if_not_default("type", "element", "string"),
            attr_bool_if_not_default("iscollection", is_collection, false),
            attr_bool_if_not_default("notnull", false, false),
            attr_bool_if_not_default("summary", element.is_summary, false),
            attr_bool_if_not_default("modifier", element.is_modifier, false),
            property_id,
            escape_xml(&element.description)
        ));
    }

    Ok(())
}

fn generate_search_xml(xml: &mut String, param: &SearchParamInfo, resource_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Convert parameter code to PascalCase for Rust-friendly query attribute
    // e.g., "address-city" -> "AddressCity", "email" -> "Email"
    let query_name = param.code
        .split('-')
        .map(|part| capitalize_first(part))
        .collect::<Vec<_>>()
        .join("");

    // Suppress default type="string"
    xml.push_str(&format!(
        "        <search name=\"{}\" query=\"{}\"{}>\n",
        param.name,
        query_name,
        attr_if_not_default("type", &param.param_type, "string")
    ));

    xml.push_str("          <paths>\n");

    // Parse the expression to generate paths
    // Handle OR expressions by splitting on "|"
    let expression_branches = param.expression.split('|').map(|s| s.trim()).collect::<Vec<_>>();

    // Filter branches to only include paths starting with the current resource
    // This prevents multi-resource search parameters (e.g., "email" on Patient, Person, Practitioner)
    // from generating paths for other resources in the current resource's definition
    let resource_prefix = resource_name.to_lowercase();

    for expr in expression_branches {
        if expr.is_empty() {
            continue;
        }

        // Check if this expression starts with the current resource
        // Handle parentheses: "(Patient.deceased.ofType(dateTime))" -> "Patient"
        let expr_trimmed = expr.trim_start_matches('(').trim();
        let expr_resource = expr_trimmed.split('.').next().unwrap_or("").to_lowercase();

        if expr_resource != resource_prefix {
            // Skip this branch - it's for a different resource
            continue;
        }

        xml.push_str("            <path>\n");
        xml.push_str("              <parts>\n");

        // Parse the expression and generate parts
        let parts = parse_fhirpath_expression(expr);
        for part_ref in parts {
            xml.push_str(&format!("                <part ref=\"{}\"/>\n", part_ref));
        }

        xml.push_str("              </parts>\n");

        // Generate targets for reference search parameters
        if param.param_type == "reference" && !param.targets.is_empty() {
            xml.push_str("              <targets>\n");
            for target in &param.targets {
                xml.push_str(&format!("                <target resource=\"{}\"/>\n", target));
            }
            xml.push_str("              </targets>\n");
        }

        // Generate components for composite search parameters
        if param.is_composite && !param.components.is_empty() {
            xml.push_str("              <components>\n");

            // The base expression (e.g., "Observation.component") gives us the starting path
            // Each component expression (e.g., "code" or "value.ofType(CodeableConcept)") extends it
            let base_path = parse_base_path(&param.expression);

            for component in &param.components {
                // Parse component expression to build property ref
                // Examples:
                //   "code" -> "observationcomponent.code"
                //   "value.ofType(CodeableConcept)" -> "observationcomponent.value" (strip ofType)
                let component_path = parse_component_path(&component.expression);
                let component_ref = if base_path.is_empty() {
                    component_path
                } else {
                    // Convert path to element ID format by removing dots
                    // e.g., "observation.component" -> "observationcomponent"
                    let element_id = base_path.replace(".", "");
                    format!("{}.{}", element_id, component_path)
                };

                xml.push_str(&format!("                <component ref=\"{}\"/>\n", component_ref));
            }
            xml.push_str("              </components>\n");
        }

        xml.push_str("            </path>\n");
    }

    xml.push_str("          </paths>\n");
    xml.push_str("        </search>\n");

    Ok(())
}

fn map_fhir_type_to_dtd(fhir_type: &str) -> &str {
    match fhir_type {
        "string" | "code" | "uri" | "url" | "canonical" | "oid" | "uuid" | "id" | "markdown" | "xhtml" => fhir_type,
        "boolean" => "boolean",
        "integer" | "positiveInt" | "unsignedInt" => "integer",
        "integer64" => "integer64",
        "decimal" => "decimal",
        "date" => "date",
        "dateTime" | "instant" => "dateTime",
        "time" => "time",
        "base64Binary" => "base64Binary",
        // FHIR internal types - map to primitive
        "http://hl7.org/fhirpath/System.String" => "string",
        t if t.starts_with("http://hl7.org/fhirpath/") => "string", // Other FHIRPath primitives
        _ => "element", // All complex types (including Reference) are elements
    }
}

/// Convert CamelCase to lowercase, preserving digits
/// Examples: birthDate -> birthdate, multipleBirth -> multiplebirth
fn to_lowercase_preserve_digits(s: &str) -> String {
    s.chars().map(|c| c.to_ascii_lowercase()).collect()
}

/// Capitalize first letter of a string
/// Examples: component -> Component, name -> Name
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().chain(chars).collect(),
    }
}

/// Map FHIR complex type names to their lowercase element ref
/// Examples: HumanName -> humanname, ContactPoint -> contactpoint, Reference -> reference
fn fhir_type_to_element_ref(fhir_type: &str) -> String {
    to_lowercase_preserve_digits(fhir_type)
}

/// Parse base path from search parameter expression
/// Examples:
///   "Observation.component" -> "observation.component"
///   "Patient" -> "patient"
///   "Observation | Observation.component" -> "observation.component" (takes last OR branch)
fn parse_base_path(expression: &str) -> String {
    // Simple implementation: convert to lowercase and return
    // More complex expressions would need proper FHIRPath parsing
    if expression.is_empty() {
        return String::new();
    }

    // For OR expressions (e.g., "Observation | Observation.component"), take the more specific path (last one)
    // TODO: Properly handle OR expressions by generating multiple paths
    let or_parts: Vec<&str> = expression.split('|').map(|s| s.trim()).collect();
    let selected = or_parts.last().unwrap_or(&"");

    // Strip any parentheses or filters (e.g., "Observation.component.where(...)")
    let clean = selected.split(".where(").next().unwrap_or(selected);

    clean.to_lowercase()
}

/// Parse component expression to property path
/// Examples:
///   "code" -> "code"
///   "value.ofType(CodeableConcept)" -> "value"
///   "value.ofType(Quantity)" -> "value"
fn parse_component_path(expression: &str) -> String {
    if expression.is_empty() {
        return String::new();
    }

    // Strip ofType() casts
    let without_oftype = expression.split(".ofType(").next().unwrap_or(expression);

    // Strip as() casts
    let without_as = without_oftype.split(" as ").next().unwrap_or(without_oftype);

    without_as.trim().to_lowercase()
}

/// Check if a property name refers to a complex type (vs backbone element)
/// Complex types are reusable across resources (Address, HumanName, etc.)
/// Backbone elements are resource-specific (component, link, qualification, etc.)
fn is_complex_type(property_name: &str) -> bool {
    matches!(
        property_name,
        "name" | "address" | "telecom" | "identifier" | "code" | "value" |
        "meta" | "extension" | "period" | "range" | "ratio" | "reference" |
        "annotation" | "attachment" | "coding" | "contactpoint" | "humanname" |
        "quantity" | "duration" | "distance" | "count" | "age" | "money" |
        "sampleddata" | "signature" | "timing" | "dosage" | "codeableconcept" |
        "codeablereference"
    )
}

/// Map FHIR property names to their type names
/// This is used when a property is identified as a complex type
/// Examples: "name" -> "humanname", "address" -> "address", "telecom" -> "contactpoint"
fn map_property_to_type(property_name: &str) -> String {
    match property_name {
        "name" => "humanname".to_string(),
        "address" => "address".to_string(),
        "telecom" => "contactpoint".to_string(),
        "identifier" => "identifier".to_string(),
        "code" => "codeableconcept".to_string(),
        "value" => "value".to_string(),  // Value is choice type, may need refinement
        "meta" => "meta".to_string(),
        "period" => "period".to_string(),
        "range" => "range".to_string(),
        "ratio" => "ratio".to_string(),
        "reference" => "reference".to_string(),
        "annotation" => "annotation".to_string(),
        "attachment" => "attachment".to_string(),
        "coding" => "coding".to_string(),
        "quantity" => "quantity".to_string(),
        "duration" => "duration".to_string(),
        "distance" => "distance".to_string(),
        "count" => "count".to_string(),
        "age" => "age".to_string(),
        "money" => "money".to_string(),
        "sampleddata" => "sampleddata".to_string(),
        "signature" => "signature".to_string(),
        "timing" => "timing".to_string(),
        "dosage" => "dosage".to_string(),
        _ => property_name.to_string(),  // Default: use property name as type name
    }
}

/// Parse a FHIRPath expression and convert it to property references
/// Examples:
///   "Patient.birthDate" -> ["patient.birthdate"]
///   "Patient.name.family" -> ["patient.name", "humanname.family"]
///   "Observation.code" -> ["observation.code"]
///   "Observation.value.ofType(Quantity)" -> ["observation.value"]
///   "Observation.subject.where(resolve() is Patient)" -> ["observation.subject"]
fn parse_fhirpath_expression(expression: &str) -> Vec<String> {
    let mut parts = Vec::new();

    if expression.is_empty() {
        return parts;
    }

    // Clean up the expression
    // Remove boolean logic - e.g., "Patient.deceased.exists() and Patient.deceased != false" -> "Patient.deceased"
    let clean_expr = expression
        .split(" and ")
        .next()
        .unwrap_or(expression)
        .split(" or ")
        .next()
        .unwrap_or(expression)
        .trim();

    // Remove .exists() calls
    let clean_expr = clean_expr
        .replace(".exists()", "");

    // Remove leading/trailing parentheses - e.g., "(Patient.deceased.ofType(dateTime))" -> "Patient.deceased.ofType(dateTime)"
    let clean_expr = clean_expr.trim_matches(|c| c == '(' || c == ')');

    // Remove .where() filters - e.g., "Patient.subject.where(resolve() is Patient)" -> "Patient.subject"
    let clean_expr = clean_expr
        .split(".where(")
        .next()
        .unwrap_or(&clean_expr);

    // Remove .first() calls
    let clean_expr = clean_expr
        .split(".first(")
        .next()
        .unwrap_or(clean_expr);

    // Split by dots to get path segments
    let segments: Vec<&str> = clean_expr.split('.').collect();

    if segments.is_empty() {
        return parts;
    }

    // First segment is the resource name (e.g., "Patient", "Observation")
    let resource_name = segments[0].to_lowercase();

    // Build paths following FHIR model rules:
    // 1. First level is always resource.property: ["patient.address"]
    // 2. For subsequent levels, check if previous property is a complex type or backbone element:
    //    - Complex types (Address, HumanName, etc.) start a new ID path: ["address.city"]
    //    - Backbone elements continue the resource path: ["observation.component.code"]
    //
    // Complex types are reusable across resources and defined in <elements>
    // Backbone elements are resource-specific and we generate flattened properties for them

    let mut current_prefix = resource_name.clone();
    let mut prev_property = String::new();

    for i in 1..segments.len() {
        let segment = segments[i];

        // Handle .ofType() casts - extract the type and use it as the prefix for subsequent properties
        // e.g., "Observation.value.ofType(CodeableConcept).text" -> use CodeableConcept as prefix for .text
        if segment.starts_with("ofType(") {
            // Extract type from ofType(TypeName)
            let type_name = segment
                .trim_start_matches("ofType(")
                .trim_end_matches(")")
                .trim()
                .to_lowercase();

            // Update current_prefix to this type for the next iteration
            current_prefix = type_name;
            continue;
        }

        // Strip parentheses and other noise
        let property_name = segment
            .split("(")
            .next()
            .unwrap_or(segment)
            .trim()
            .replace("[x]", "");

        if property_name.is_empty() {
            continue;
        }

        let property_name_lower = property_name.to_lowercase();

        if i == 1 {
            // First level: always resource.property
            parts.push(format!("{}.{}", current_prefix, property_name_lower));
            prev_property = property_name_lower.clone();
            // For next iteration, determine if this property is a complex type or backbone
            current_prefix = if is_complex_type(&property_name_lower) {
                // Complex type: next level uses type name
                map_property_to_type(&property_name_lower)
            } else {
                // Backbone element or continuation: next level extends resource path
                format!("{}.{}", current_prefix, property_name_lower)
            };
        } else {
            // Subsequent levels: use the determined prefix
            parts.push(format!("{}.{}", current_prefix, property_name_lower));
            prev_property = property_name_lower.clone();
            // Update prefix for next iteration
            current_prefix = if is_complex_type(&property_name_lower) {
                map_property_to_type(&property_name_lower)
            } else {
                format!("{}.{}", current_prefix, property_name_lower)
            };
        }
    }

    parts
}

fn escape_xml(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
        // Replace line breaks with spaces
        .replace('\n', " ")
        .replace('\r', " ")
        // Replace smart quotes with regular quotes
        .replace('\u{201C}', "\"")  // Left double quote
        .replace('\u{201D}', "\"")  // Right double quote
        .replace('\u{2018}', "'")   // Left single quote
        .replace('\u{2019}', "'")   // Right single quote
        // Replace other special characters
        .replace('\u{2013}', "-")   // En dash
        .replace('\u{2014}', "-")   // Em dash
        .replace('\u{2026}', "...")  // Ellipsis
}

/// Helper to conditionally output an attribute only if it differs from the default value
fn attr_if_not_default(name: &str, value: &str, default: &str) -> String {
    if value == default {
        String::new()
    } else {
        format!(" {}=\"{}\"", name, value)
    }
}

/// Helper to conditionally output a boolean attribute only if it differs from the default
fn attr_bool_if_not_default(name: &str, value: bool, default: bool) -> String {
    if value == default {
        String::new()
    } else {
        format!(" {}=\"{}\"", name, if value { "true" } else { "false" })
    }
}
