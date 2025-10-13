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
    #[arg(short, long, default_value = "fhir.xml")]
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
    #[serde(rename = "contentReference")]
    content_reference: Option<String>,
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
    let valueset_to_codesystem = build_valueset_to_codesystem_map(&args.input)?;

    println!("   Found {} structure definitions", structures.len());
    println!("   Found {} complex type definitions", complex_types.len());
    println!("   Found {} search parameters", search_params.len());
    println!("   Found {} code systems", code_systems.len());
    println!("   Found {} valueset mappings", valueset_to_codesystem.len());

    // Step 2: Filter resources if specified
    let resource_filter: Option<Vec<String>> = args.resources.map(|r| {
        r.split(',').map(|s| s.trim().to_string()).collect()
    });

    // Step 3: Extract and organize data
    println!("\n🔨 Extracting resource definitions...");
    let resources = extract_resources(&structures, &search_params, &resource_filter, &code_systems)?;
    println!("   Extracted {} resources", resources.len());

    // Step 4: Generate XML
    println!("\n📝 Generating XML model...");
    let xml = generate_xml(&resources, &complex_types, &code_systems, &valueset_to_codesystem)?;

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

/// Build a mapping from ValueSet ID to CodeSystem ID
/// This is needed because ValueSets often have different IDs than the CodeSystems they reference
/// Example: ValueSet "request-resource-types" → CodeSystem URL "http://hl7.org/fhir/fhir-types" → CodeSystem ID "fhir-types"
fn build_valueset_to_codesystem_map(input_dir: &PathBuf) -> Result<HashMap<String, String>, Box<dyn std::error::Error>> {
    let valuesets_path = input_dir.join("valuesets.json");
    let content = fs::read_to_string(&valuesets_path)?;
    let bundle: Value = serde_json::from_str(&content)?;

    let mut valueset_to_codesystem: HashMap<String, String> = HashMap::new();

    if let Some(entries) = bundle.get("entry").and_then(|e| e.as_array()) {
        for entry in entries {
            if let Some(resource) = entry.get("resource") {
                // Check if this is a ValueSet
                if resource.get("resourceType").and_then(|rt| rt.as_str()) == Some("ValueSet") {
                    let valueset_id = resource.get("id").and_then(|id| id.as_str());

                    // Extract the CodeSystem URL from compose.include[0].system
                    let codesystem_url = resource
                        .get("compose")
                        .and_then(|compose| compose.get("include"))
                        .and_then(|include| include.as_array())
                        .and_then(|arr| arr.get(0))
                        .and_then(|first| first.get("system"))
                        .and_then(|sys| sys.as_str());

                    if let (Some(vs_id), Some(cs_url)) = (valueset_id, codesystem_url) {
                        // Extract CodeSystem ID from URL
                        // Examples:
                        //   "http://hl7.org/fhir/fhir-types" → "fhir-types"
                        //   "http://hl7.org/fhir/fm-status" → "fm-status"
                        //   "http://terminology.hl7.org/CodeSystem/v3-ActCode" → "v3-ActCode"
                        let cs_id = cs_url.split('/').last().unwrap_or("");
                        if !cs_id.is_empty() {
                            valueset_to_codesystem.insert(vs_id.to_string(), cs_id.to_string());
                        }
                    }
                }
            }
        }
    }

    Ok(valueset_to_codesystem)
}

#[derive(Debug)]
struct ResourceDefinition {
    name: String,
    description: String,
    elements: Vec<ElementInfo>,
    backbone_elements: Vec<BackboneElementInfo>,
    search_params: Vec<SearchParamInfo>,
    local_codesets: Vec<LocalCodeSet>,
}

#[derive(Debug)]
struct LocalCodeSet {
    name: String,
    id: String,
    description: String,
    codes: Vec<(String, String, String)>, // (code, display, definition)
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
    value_set_url: Option<String>, // ValueSet canonical URL (e.g., "http://hl7.org/fhir/ValueSet/administrative-gender")
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
    all_code_systems: &[CodeSystem],
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
        let local_codesets = extract_local_codesets(&structure.name, &elements, &backbone_elements, all_code_systems);

        resources.push(ResourceDefinition {
            name: structure.name.clone(),
            description: structure.description.clone().unwrap_or_default(),
            elements,
            backbone_elements,
            search_params: search,
            local_codesets,
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

                // Handle contentReference for recursive structures
                let types = if let Some(content_ref) = &elem.content_reference {
                    // Parse contentReference: "#ResourceName.path.to.element" -> "resourcenamepath..."
                    let referenced_path = content_ref.trim_start_matches('#');
                    let element_id = referenced_path.replace(".", "").to_lowercase();

                    // Create synthetic type pointing to the referenced element
                    vec![TypeInfo {
                        code: element_id,
                        target_resources: vec![],
                    }]
                } else {
                    // Normal type extraction
                    elem.types.as_ref()
                        .map(|t| t.iter().map(|et| TypeInfo {
                            code: et.code.clone(),
                            target_resources: et.target_profile.as_ref()
                                .map(|profiles| profiles.iter()
                                    .filter_map(|url| url.split('/').last().map(String::from))
                                    .collect())
                                .unwrap_or_default()
                        }).collect())
                        .unwrap_or_default()
                };

                let cardinality = format!("{}..{}",
                    elem.min.unwrap_or(0),
                    elem.max.as_ref().map(|s| s.as_str()).unwrap_or("*")
                );

                // Extract binding name and ValueSet URL
                let (binding_name, value_set_url) = elem.binding.as_ref()
                    .map(|binding| {
                        let name = binding.extension.as_ref().and_then(|extensions| {
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
                        });
                        let url = binding.value_set.clone();
                        (name, url)
                    })
                    .unwrap_or((None, None));

                elements.push(ElementInfo {
                    name,
                    path: elem.path.clone(),
                    types,
                    description: elem.definition.clone()
                        .or_else(|| elem.short_description.clone())
                        .unwrap_or_default(),
                    cardinality,
                    binding_name,
                    value_set_url,
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
        // First pass: identify ALL backbone elements at any depth (not just direct children)
        let mut backbone_paths = Vec::new();
        for elem in &snapshot.element {
            // Check if this is a BackboneElement at any depth (excluding root)
            if elem.path != structure.type_name {
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
            let depth = backbone_path.matches('.').count();
            let mut properties = Vec::new();

            // Find description
            let description = snapshot.element.iter()
                .find(|e| e.path == backbone_path)
                .and_then(|e| e.definition.clone().or_else(|| e.short_description.clone()))
                .unwrap_or_default();

            // Extract properties (direct children of this backbone element)
            for elem in &snapshot.element {
                // Only include direct children (one level deeper than the backbone element)
                if elem.path.starts_with(&prefix) && elem.path.matches('.').count() == depth + 1 {
                    // Skip extension fields
                    if elem.path.ends_with(".id")
                        || elem.path.ends_with(".extension")
                        || elem.path.ends_with(".modifierExtension") {
                        continue;
                    }

                    let name = elem.path.split('.').last().unwrap_or("").to_string();

                    // Handle contentReference for recursive structures
                    let types = if let Some(content_ref) = &elem.content_reference {
                        // Parse contentReference: "#ResourceName.path.to.element" -> "resourcenamepath..."
                        let referenced_path = content_ref.trim_start_matches('#');
                        let element_id = referenced_path.replace(".", "").to_lowercase();

                        // Create synthetic type pointing to the referenced element
                        vec![TypeInfo {
                            code: element_id,
                            target_resources: vec![],
                        }]
                    } else {
                        // Normal type extraction
                        elem.types.as_ref()
                            .map(|t| t.iter().map(|et| TypeInfo {
                                code: et.code.clone(),
                                target_resources: et.target_profile.as_ref()
                                    .map(|profiles| profiles.iter()
                                        .filter_map(|url| url.split('/').last().map(String::from))
                                        .collect())
                                    .unwrap_or_default()
                            }).collect())
                            .unwrap_or_default()
                    };

                    let cardinality = format!("{}..{}",
                        elem.min.unwrap_or(0),
                        elem.max.as_ref().map(|s| s.as_str()).unwrap_or("*")
                    );

                    // Extract binding name and ValueSet URL
                    let (binding_name, value_set_url) = elem.binding.as_ref()
                        .map(|binding| {
                            let name = binding.extension.as_ref().and_then(|extensions| {
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
                            });
                            let url = binding.value_set.clone();
                            (name, url)
                        })
                        .unwrap_or((None, None));

                    properties.push(ElementInfo {
                        name,
                        path: elem.path.clone(),
                        types,
                        description: elem.definition.clone()
                            .or_else(|| elem.short_description.clone())
                            .unwrap_or_default(),
                        cardinality,
                        binding_name,
                        value_set_url,
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

fn extract_local_codesets(
    resource_name: &str,
    elements: &[ElementInfo],
    backbone_elements: &[BackboneElementInfo],
    all_code_systems: &[CodeSystem],
) -> Vec<LocalCodeSet> {
    let mut local_codesets = Vec::new();
    let resource_prefix = resource_name.to_lowercase();

    // Collect all binding names from elements and backbone elements
    let mut binding_names = HashSet::new();
    for element in elements {
        if let Some(binding) = &element.binding_name {
            binding_names.insert(binding.clone());
        }
    }
    for backbone in backbone_elements {
        for property in &backbone.properties {
            if let Some(binding) = &property.binding_name {
                binding_names.insert(binding.clone());
            }
        }
    }

    // Find CodeSystems whose IDs start with the resource name (resource-local codesets)
    // Examples:
    //   - "observation-triggeredbytype" for Observation resource
    //   - "observationworkflowstatus" for Observation resource
    //   - "location-mode" for Location resource
    for code_system in all_code_systems {
        // Check if the CodeSystem ID starts with the resource name (case-insensitive)
        let cs_id_lower = code_system.id.to_lowercase();
        let cs_id_normalized = cs_id_lower.replace("-", "");

        // Match if ID starts with resource name (with or without dash separator)
        // e.g., "observation-triggeredbytype" or "observationworkflowstatus"
        if cs_id_lower.starts_with(&format!("{}-", resource_prefix))
            || cs_id_normalized.starts_with(&resource_prefix) {

            // Check if this codeset is actually referenced by properties in this resource
            let binding_id = to_lowercase_preserve_digits(&code_system.name);
            if binding_names.contains(&code_system.name) || binding_names.iter().any(|b| to_lowercase_preserve_digits(b) == binding_id) {
                let codes = code_system.concept.as_ref()
                    .map(|concepts| concepts.iter().map(|c| {
                        (
                            c.code.clone(),
                            c.display.clone(),
                            c.definition.clone().unwrap_or_else(|| c.display.clone())
                        )
                    }).collect())
                    .unwrap_or_default();

                local_codesets.push(LocalCodeSet {
                    name: code_system.name.clone(),
                    id: to_lowercase_preserve_digits(&code_system.name),
                    description: code_system.description.clone().unwrap_or_default(),
                    codes,
                });
            }
        }
    }

    local_codesets
}

fn generate_xml(resources: &[ResourceDefinition], all_complex_types: &[StructureDefinition], all_code_systems: &[CodeSystem], valueset_to_codesystem: &HashMap<String, String>) -> Result<String, Box<dyn std::error::Error>> {
    let mut xml = String::new();

    // XML header and DTD reference
    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<!DOCTYPE fhir SYSTEM \"fhirspec.dtd\">\n\n");

    // Build CodeSystem ID → CodeSystem name mapping for resolving binding references
    // This is needed because FHIR R5 binding names often don't match CodeSystem names
    // We resolve via: ValueSet URL → ValueSet ID → CodeSystem ID → CodeSystem name
    // Example: ValueSet URL "http://hl7.org/fhir/ValueSet/request-resource-types|5.0.0"
    //          → ValueSet ID "request-resource-types"
    //          → CodeSystem ID "fhir-types" (via valueset_to_codesystem map)
    //          → CodeSystem name "FHIRTypes"
    let mut codeset_id_to_name: HashMap<String, String> = HashMap::new();
    for code_system in all_code_systems {
        codeset_id_to_name.insert(code_system.id.clone(), code_system.name.clone());
    }

    // Build type lookup dictionaries (following Telemed5000's approach)
    // Maps property ID (e.g., "appointment.reason") to its type reference (e.g., "codeablereference")
    let mut property_type_map: HashMap<String, String> = HashMap::new();

    // Add properties from resources
    for resource in resources {
        let resource_id = resource.name.to_lowercase();

        // Build a map of backbone element names to their IDs for this resource
        let mut backbone_map: HashMap<String, String> = HashMap::new();
        for backbone in &resource.backbone_elements {
            let element_id = backbone.path.replace(".", "").to_lowercase();
            backbone_map.insert(backbone.name.clone(), element_id);
        }

        for element in &resource.elements {
            let property_name_clean = element.name.replace("[x]", "");
            let property_id = format!("{}.{}", resource_id, to_lowercase_preserve_digits(&property_name_clean));

            // Store the type reference (use first type if multiple variants)
            if !element.types.is_empty() {
                let mut type_ref = fhir_type_to_element_ref(&element.types[0].code);

                // Special case: if this is a BackboneElement, look up the specific backbone element ID
                if element.types[0].code == "BackboneElement" {
                    if let Some(backbone_id) = backbone_map.get(&element.name) {
                        type_ref = backbone_id.clone();
                    }
                }

                property_type_map.insert(property_id, type_ref);
            }
        }

        // Add properties from backbone elements
        // Use the full dotted path as property ID (e.g., "conceptmap.group.element")
        // not the element ID (e.g., "conceptmapgroup.element")
        for backbone in &resource.backbone_elements {
            let backbone_path_lower = backbone.path.to_lowercase();
            for property in &backbone.properties {
                let property_name_clean = property.name.replace("[x]", "");
                let property_id = format!("{}.{}", backbone_path_lower, to_lowercase_preserve_digits(&property_name_clean));

                if !property.types.is_empty() {
                    let mut type_ref = fhir_type_to_element_ref(&property.types[0].code);

                    // Special case: if this is a nested BackboneElement, the type ref should be
                    // the property path with dots removed (e.g., "ConceptMap.group.element" -> "conceptmapgroupelement")
                    if property.types[0].code == "BackboneElement" {
                        type_ref = property.path.replace(".", "").to_lowercase();
                    }

                    property_type_map.insert(property_id, type_ref);
                }
            }
        }
    }

    // Add properties from complex types
    for complex_type in all_complex_types {
        let element_id = complex_type.type_name.to_lowercase();
        if let Some(snapshot) = &complex_type.snapshot {
            for elem in &snapshot.element {
                // Only include direct properties (not nested or root)
                if elem.path.matches('.').count() == 1 && elem.path != complex_type.type_name {
                    let name = elem.path.split('.').last().unwrap_or("").to_string();
                    let property_id = format!("{}.{}", element_id, to_lowercase_preserve_digits(&name));

                    if let Some(types) = &elem.types {
                        if !types.is_empty() {
                            let type_ref = fhir_type_to_element_ref(&types[0].code);
                            property_type_map.insert(property_id, type_ref);
                        }
                    }
                }
            }
        }
    }

    // Root element
    xml.push_str("<fhir name=\"Fire\" version=\"5.0.0\">\n");
    xml.push_str("  <resources>\n");

    for resource in resources {
        generate_resource_xml(&mut xml, resource, &property_type_map, valueset_to_codesystem, &codeset_id_to_name)?;
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

    // Track generated IDs to avoid duplicates
    let mut generated_ids = HashSet::new();

    for complex_type in all_complex_types {
        let type_name = complex_type.type_name.as_str();
        if referenced_types.contains(type_name) {
            let element_id = type_name.to_lowercase();

            // Skip if already generated (e.g., MoneyQuantity and SimpleQuantity duplicate Quantity)
            if generated_ids.insert(element_id.clone()) {
                generate_element_xml(&mut xml, complex_type, valueset_to_codesystem, &codeset_id_to_name)?;
            }
        }
    }
    xml.push_str("  </elements>\n");

    // Collect all referenced codesets
    let mut referenced_codesets = HashSet::new();

    // From resources
    for resource in resources {
        for element in &resource.elements {
            // Resolve binding name via ValueSet URL to get actual CodeSystem name
            let resolved = resolve_binding_name(
                element.binding_name.as_ref(),
                element.value_set_url.as_ref(),
                valueset_to_codesystem,
                &codeset_id_to_name
            );
            if let Some(binding) = resolved {
                referenced_codesets.insert(to_lowercase_preserve_digits(&binding));
            }
        }

        // From backbone elements
        for backbone in &resource.backbone_elements {
            for property in &backbone.properties {
                // Resolve binding name via ValueSet URL to get actual CodeSystem name
                let resolved = resolve_binding_name(
                    property.binding_name.as_ref(),
                    property.value_set_url.as_ref(),
                    valueset_to_codesystem,
                    &codeset_id_to_name
                );
                if let Some(binding) = resolved {
                    referenced_codesets.insert(to_lowercase_preserve_digits(&binding));
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

    // Track all IDs already used in the document to prevent collisions
    // Start with resource IDs and element IDs
    let mut all_used_ids: HashSet<String> = resources.iter()
        .map(|r| r.name.to_lowercase())
        .collect();

    // Add element IDs (from complex types)
    all_used_ids.extend(generated_ids.iter().cloned());

    // Collect IDs of all resource-local codesets to exclude them from global section
    let mut local_codeset_ids: HashSet<String> = HashSet::new();
    for resource in resources {
        for codeset in &resource.local_codesets {
            local_codeset_ids.insert(codeset.id.clone());
        }
    }

    for codeset_id in referenced_codesets.iter() {
        // Skip if this codeset is resource-local (already in resource's <codesets> section)
        if local_codeset_ids.contains(codeset_id) {
            continue;
        }

        // Try lookup by name first (binding name), then by ID
        let code_system = codeset_map_by_name.get(codeset_id.as_str())
            .or_else(|| codeset_map_by_id.get(codeset_id.as_str()));

        if let Some(code_system) = code_system {
            // Generate codeset with collision detection
            // The function will add "codes" suffix if needed and return the actual ID used
            let actual_id = generate_codeset_xml(&mut xml, code_system, &all_used_ids)?;

            // Track the ID to prevent future collisions
            all_used_ids.insert(actual_id);
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

fn generate_codeset_xml(xml: &mut String, code_system: &CodeSystem, all_used_ids: &HashSet<String>) -> Result<String, Box<dyn std::error::Error>> {
    // Use the CodeSystem.name (binding name) as the ID, not the CodeSystem.id
    // This matches how properties reference codesets via binding names
    // e.g., binding name "TriggeredByType" -> id "triggeredbytype"
    let mut codeset_id = to_lowercase_preserve_digits(&code_system.name);

    // Following Telemed5000's approach (lines 1565-1570, 1669-1670 in Program.cs):
    // Add "codes" suffix if ID collides with a resource or element ID
    // Examples:
    //   - "SubscriptionStatus" resource + "SubscriptionStatus" codeset → rename codeset to "subscriptionstatuscodes"
    //   - "BiologicallyDerivedProductDispense" codeset → "biologicallyderivedproductdispensecodes" (avoids resource collision)
    if all_used_ids.contains(&codeset_id) {
        codeset_id = format!("{}codes", codeset_id);
    }

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

    Ok(codeset_id)
}

fn generate_resource_xml(xml: &mut String, resource: &ResourceDefinition, property_type_map: &HashMap<String, String>, valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
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
        generate_property_xml_with_backbone(xml, element, &resource.name, &resource.backbone_elements, valueset_to_codesystem, codeset_id_to_name)?;
    }
    xml.push_str("      </properties>\n");

    // Elements (backbone elements - nested complex types)
    if resource.backbone_elements.is_empty() {
        xml.push_str("      <elements/>\n");
    } else {
        xml.push_str("      <elements>\n");
        for backbone in &resource.backbone_elements {
            generate_backbone_element_xml(xml, backbone, &resource.name, valueset_to_codesystem, codeset_id_to_name)?;
        }
        xml.push_str("      </elements>\n");
    }

    // Codesets (local ValueSets)
    if resource.local_codesets.is_empty() {
        xml.push_str("      <codesets/>\n");
    } else {
        xml.push_str("      <codesets>\n");
        for codeset in &resource.local_codesets {
            xml.push_str(&format!("        <codeset name=\"{}\" id=\"{}\">\n", codeset.name, codeset.id));
            xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&codeset.description)));
            xml.push_str("          <codes>\n");
            for (code, display, definition) in &codeset.codes {
                xml.push_str(&format!("            <code name=\"{}\" value=\"{}\">\n",
                    escape_xml(display),
                    escape_xml(code)));
                xml.push_str(&format!("              <description>{}</description>\n", escape_xml(definition)));
                xml.push_str("            </code>\n");
            }
            xml.push_str("          </codes>\n");
            xml.push_str("        </codeset>\n");
        }
        xml.push_str("      </codesets>\n");
    }

    // Search parameters
    // Skip search parameters for Bundle - bundles are never stored in the database
    if resource.name != "Bundle" {
        xml.push_str("      <searches>\n");
        for param in &resource.search_params {
            generate_search_xml(xml, param, &resource.name, property_type_map)?;
        }
        xml.push_str("      </searches>\n");
    } else {
        xml.push_str("      <searches/>\n");
    }

    xml.push_str("    </resource>\n");

    Ok(())
}

fn generate_backbone_element_xml(xml: &mut String, backbone: &BackboneElementInfo, _resource_name: &str, valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
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
        // Generate property XML with the full dotted path as the parent
        // Following Telemed5000 convention: property IDs use full dotted path from resource root
        // e.g., "ConceptMap.group.element" property has ID "conceptmap.group.element"
        let parent_path_lower = backbone.path.to_lowercase();
        generate_property_xml_with_parent(xml, property, &parent_path_lower, valueset_to_codesystem, codeset_id_to_name)?;
    }
    xml.push_str("          </properties>\n");

    xml.push_str("          <elements/>\n");
    xml.push_str("          <codesets/>\n");
    xml.push_str("        </element>\n");

    Ok(())
}

fn generate_element_xml(xml: &mut String, complex_type: &StructureDefinition, valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
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

                // Extract binding name and ValueSet URL
                let (binding_name, value_set_url) = elem.binding.as_ref()
                    .map(|binding| {
                        let name = binding.extension.as_ref().and_then(|extensions| {
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
                        });
                        let url = binding.value_set.clone();
                        (name, url)
                    })
                    .unwrap_or((None, None));

                let element_info = ElementInfo {
                    name,
                    path: elem.path.clone(),
                    types,
                    description: elem.definition.clone()
                        .or_else(|| elem.short_description.clone())
                        .unwrap_or_default(),
                    cardinality,
                    binding_name,
                    value_set_url,
                    is_summary: elem.is_summary.unwrap_or(false),
                    is_modifier: elem.is_modifier.unwrap_or(false),
                };

                generate_property_xml(xml, &element_info, &complex_type.name, valueset_to_codesystem, codeset_id_to_name)?;
            }
        }
    }
    xml.push_str("      </properties>\n");

    xml.push_str("      <elements/>\n");
    xml.push_str("      <codesets/>\n");
    xml.push_str("    </element>\n");

    Ok(())
}

fn generate_property_xml(xml: &mut String, element: &ElementInfo, resource_name: &str, valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
    // Follow Telemed5000 naming convention: {resource_lowercase}.{property_lowercase}
    let parent_id = resource_name.to_lowercase();
    generate_property_xml_with_parent(xml, element, &parent_id, valueset_to_codesystem, codeset_id_to_name)
}

fn generate_property_xml_with_backbone(xml: &mut String, element: &ElementInfo, resource_name: &str, backbone_elements: &[BackboneElementInfo], valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
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
            return generate_property_xml(xml, &modified_element, resource_name, valueset_to_codesystem, codeset_id_to_name);
        }
    }

    // Not a backbone element, generate normally
    generate_property_xml(xml, element, resource_name, valueset_to_codesystem, codeset_id_to_name)
}

fn generate_property_xml_with_parent(xml: &mut String, element: &ElementInfo, parent_id: &str, valueset_to_codesystem: &HashMap<String, String>, codeset_id_to_name: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
    // Remove [x] suffix from choice types (e.g., deceased[x] -> deceased)
    let property_name_clean = element.name.replace("[x]", "");
    let property_id = format!("{}.{}",
        parent_id,
        to_lowercase_preserve_digits(&property_name_clean)
    );
    let is_collection = element.cardinality.ends_with("*");

    // Resolve binding name using ValueSet URL if available
    let resolved_binding = resolve_binding_name(
        element.binding_name.as_ref(),
        element.value_set_url.as_ref(),
        valueset_to_codesystem,
        codeset_id_to_name
    );

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
            } else if dtd_type == "code" && resolved_binding.is_some() {
                // Code with binding - add ref to codeset using resolved binding
                let binding_name = resolved_binding.as_ref().unwrap();
                if binding_name != "??" {
                    let binding_ref = to_lowercase_preserve_digits(binding_name);
                    xml.push_str(&format!("            <variant type=\"{}\" ref=\"{}\"/>\n", dtd_type, binding_ref));
                } else {
                    // No ref attribute for invalid binding
                    xml.push_str(&format!("            <variant type=\"{}\"/>\n", dtd_type));
                }
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
        } else if fhir_type == "code" && resolved_binding.is_some() {
            // Code type with ValueSet binding - add ref to codeset using resolved binding
            let binding_name = resolved_binding.as_ref().unwrap();
            if binding_name == "??" {
                // Treat as code without binding (no ref attribute)
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
            } else {
                let binding_ref = to_lowercase_preserve_digits(binding_name);
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
            }
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

fn generate_search_xml(xml: &mut String, param: &SearchParamInfo, resource_name: &str, property_type_map: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
    // Parse the expression to generate paths
    // Handle OR expressions by splitting on "|"
    let expression_branches = param.expression.split('|').map(|s| s.trim()).collect::<Vec<_>>();

    // Filter branches to only include paths starting with the current resource
    // This prevents multi-resource search parameters (e.g., "email" on Patient, Person, Practitioner)
    // from generating paths for other resources in the current resource's definition
    let resource_prefix = resource_name.to_lowercase();

    // Collect valid paths first to check if we have any
    let mut valid_paths = Vec::new();

    for expr in expression_branches {
        if expr.is_empty() {
            continue;
        }

        // Skip expressions with .extension() calls - too complex to parse (URLs with dots/slashes)
        // Following Telemed5000's approach: omit extension paths, keep only primary paths
        // Examples skipped:
        //   - CareTeam.extension('http://hl7.org/fhir/StructureDefinition/careteam-alias').value
        //   - Location.extension('http://hl7.org/fhir/StructureDefinition/location-boundary-geojson').value
        if expr.contains(".extension(") {
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

        valid_paths.push(expr);
    }

    // Skip this search parameter entirely if there are no valid paths
    if valid_paths.is_empty() {
        return Ok(());
    }

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

    for expr in valid_paths {
        xml.push_str("            <path>\n");
        xml.push_str("              <parts>\n");

        // Parse the expression and generate parts and casts
        let (parts, casts) = parse_fhirpath_expression(expr, property_type_map);
        for part_ref in &parts {
            xml.push_str(&format!("                <part ref=\"{}\"/>\n", part_ref));
        }

        xml.push_str("              </parts>\n");

        // Generate casts if any were found
        if !casts.is_empty() {
            xml.push_str("              <casts>\n");
            for cast_type in &casts {
                xml.push_str(&format!("                <cast to=\"{}\"/>\n", cast_type));
            }
            xml.push_str("              </casts>\n");
        }

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

            // Determine the prefix for component refs based on cast types
            // If the base expression has a cast (e.g., ".ofType(Ratio)"), the component refs
            // are relative to the cast type, not the base path.
            // Examples:
            //   - "Observation.component" → components are "observation.component.code", "observation.component.value"
            //   - "Ingredient.substance.strength.presentation.ofType(Ratio)" → components are "ratio.numerator", "ratio.denominator"
            let component_prefix = if !casts.is_empty() {
                // Use the last cast type as the prefix (lowercased)
                casts.last().unwrap().to_lowercase()
            } else {
                // No casts, use the base path
                parse_base_path(&param.expression)
            };

            for component in &param.components {
                // Parse component expression to build property ref
                let component_path = parse_component_path(&component.expression);
                let component_ref = if component_prefix.is_empty() {
                    component_path
                } else {
                    format!("{}.{}", component_prefix, component_path)
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
///   "Ingredient.substance.strength.concentration.ofType(Ratio)" -> "ingredient.substance.strength.concentration"
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

    // Strip .ofType() casts - e.g., "Ingredient.substance.strength.concentration.ofType(Ratio)" -> "Ingredient.substance.strength.concentration"
    let clean = clean.split(".ofType(").next().unwrap_or(clean);

    clean.to_lowercase()
}

/// Parse component expression to property path
/// Examples:
///   "code" -> "code"
///   "value.ofType(CodeableConcept)" -> "value"
///   "(value.ofType(Quantity))" -> "value"
///   "value.ofType(boolean)" -> "value"
fn parse_component_path(expression: &str) -> String {
    if expression.is_empty() {
        return String::new();
    }

    // Strip leading/trailing parentheses - e.g., "(value.ofType(CodeableConcept))" -> "value.ofType(CodeableConcept)"
    let clean = expression.trim().trim_matches(|c| c == '(' || c == ')');

    // Strip ofType() casts
    let without_oftype = clean.split(".ofType(").next().unwrap_or(clean);

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
        "codeablereference" => "codeablereference".to_string(),
        "codeableconcept" => "codeableconcept".to_string(),
        "humanname" => "humanname".to_string(),
        "contactpoint" => "contactpoint".to_string(),
        "extension" => "extension".to_string(),
        _ => property_name.to_string(),  // Default: use property name as type name
    }
}

/// Parse a FHIRPath expression and convert it to property references
/// Returns (parts, casts) tuple where:
///   - parts: Vec of property references (e.g., ["patient.birthdate"])
///   - casts: Vec of cast type names (e.g., ["Reference", "CodeableConcept"])
/// Examples:
///   "Patient.birthDate" -> (["patient.birthdate"], [])
///   "Patient.name.family" -> (["patient.name", "humanname.family"], [])
///   "Observation.code" -> (["observation.code"], [])
///   "Observation.value.ofType(Quantity)" -> (["observation.value"], ["Quantity"])
///   "instance as Reference" -> (["adverseevent.suspectentity.instance"], ["Reference"])
fn parse_fhirpath_expression(expression: &str, property_type_map: &HashMap<String, String>) -> (Vec<String>, Vec<String>) {
    let mut parts = Vec::new();
    let mut casts = Vec::new();

    if expression.is_empty() {
        return (parts, casts);
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
        return (parts, casts);
    }

    // First segment is the resource name (e.g., "Patient", "Observation")
    let resource_name = segments[0].to_lowercase();

    // Build paths following FHIR model rules, using Telemed5000's approach:
    // 1. First level is always resource.property: ["patient.address"]
    // 2. For subsequent levels, look up the property type from the type map
    // 3. If the type is a complex type (not a backbone element), use the type as the prefix
    // 4. If the type starts with the current element ID, it's a backbone element, use property ID

    let mut current_prefix = resource_name.clone();

    for i in 1..segments.len() {
        let segment = segments[i];

        // Handle .ofType() casts - extract the type and use it as the prefix for subsequent properties
        // e.g., "Observation.value.ofType(CodeableConcept).text" -> use CodeableConcept as prefix for .text
        if segment.starts_with("ofType(") {
            // Extract type from ofType(TypeName)
            let type_name = segment
                .trim_start_matches("ofType(")
                .trim_end_matches(")")
                .trim();

            // Store the cast (preserve original case for type name)
            casts.push(type_name.to_string());

            // Update current_prefix to this type for the next iteration
            current_prefix = type_name.to_lowercase();
            continue;
        }

        // Strip parentheses and other noise
        let property_name = segment
            .split("(")
            .next()
            .unwrap_or(segment)
            .trim();

        // Handle " as TypeName" casts - e.g., "instance as Reference"
        // Extract the cast type and store it, then strip from property name
        let property_name = if let Some(as_pos) = property_name.find(" as ") {
            let parts: Vec<&str> = property_name.split(" as ").collect();
            if parts.len() >= 2 {
                // Store the cast (preserve original case for type name)
                casts.push(parts[1].trim().to_string());
                parts[0].trim()
            } else {
                property_name
            }
        } else {
            property_name
        };

        // Remove [x] suffix from choice types
        let property_name = property_name.replace("[x]", "");

        if property_name.is_empty() {
            continue;
        }

        let property_name_lower = property_name.to_lowercase();

        // Build the property ID for this level
        let property_id = format!("{}.{}", current_prefix, property_name_lower);

        // Add this part to the result
        parts.push(property_id.clone());

        // Look up the property type from the map to determine the next prefix
        // Following Telemed5000's approach (lines 1258-1270 in Program.cs):
        if let Some(type_ref) = property_type_map.get(&property_id) {
            // Check if this type reference is a backbone element or a complex type
            // Backbone elements: type ref starts with the resource name (e.g., "conceptmapgroup", "observationcomponent")
            // Complex types: type ref is independent of resource (e.g., "humanname", "codeablereference")

            if type_ref.starts_with(&resource_name) {
                // Backbone element: continue building the dotted path
                // e.g., "conceptmap.group" → next is "conceptmap.group.element"
                current_prefix = property_id;
            } else {
                // Complex type: use the type reference as the next prefix
                // e.g., "patient.name" (type="humanname") → next is "humanname.family"
                current_prefix = type_ref.clone();
            }
        } else {
            // No type information available - assume it's a continuation of the current path
            // This fallback ensures we don't break on missing type information
            current_prefix = property_id;
        }
    }

    (parts, casts)
}

fn escape_xml(text: &str) -> String {
    // First fix double-encoded UTF-8 (common FHIR R5 data quality issue)
    // "â€"" should be em-dash (these appear in source FHIR R5 JSON as corrupted UTF-8)
    let text = text.replace("\u{00E2}\u{20AC}\u{201D}", "\u{2014}");  // â€" -> em dash
    let text = text.replace("\u{00E2}\u{20AC}\u{201C}", "\u{2013}");  // â€œ -> en dash (if present)

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
        .replace('\u{2011}', "-")   // Non-breaking hyphen
        .replace('\u{2013}', "-")   // En dash
        .replace('\u{2014}', "-")   // Em dash
        .replace('\u{2026}', "...")  // Ellipsis
        .replace('\u{2265}', "&gt;=")  // Greater than or equal (≥)
        .replace('\u{2264}', "&lt;=")  // Less than or equal (≤)
        // Replace Windows-1252 control characters that sometimes appear in FHIR data
        .replace('\u{0091}', "'")   // Windows-1252 0x91 (left single quote)
        .replace('\u{0082}', ",")   // Windows-1252 0x82 (low double quote, use comma)
        .replace('\u{0089}', "per-mille")  // Windows-1252 0x89 (per-mille sign)
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

/// Extract CodeSystem ID from ValueSet canonical URL
/// Examples:
///   "http://hl7.org/fhir/ValueSet/fm-status|5.0.0" -> Some("fm-status")
///   "http://hl7.org/fhir/ValueSet/administrative-gender" -> Some("administrative-gender")
///   "http://terminology.hl7.org/ValueSet/v3-ActCode" -> Some("v3-ActCode")
fn extract_codeset_id_from_valueset_url(url: &str) -> Option<String> {
    // ValueSet URLs typically follow this pattern:
    // http://hl7.org/fhir/ValueSet/{id}|{version}
    // or
    // http://hl7.org/fhir/ValueSet/{id}

    // Split off version if present
    let url_without_version = url.split('|').next().unwrap_or(url);

    // Extract the last path segment after "ValueSet/"
    if let Some(valueset_pos) = url_without_version.rfind("/ValueSet/") {
        let id_start = valueset_pos + "/ValueSet/".len();
        let id = &url_without_version[id_start..];
        if !id.is_empty() {
            return Some(id.to_string());
        }
    }

    None
}

/// Resolve binding name to correct CodeSystem name using ValueSet URL
/// Following Telemed5000's approach: Use ValueSet URL to look up CodeSystem ID, then use ID to get name
/// This fixes FHIR R5 data quality issues where binding names don't match CodeSystem names
/// Examples:
///   binding_name="ActivityDefinitionKind", value_set_url="http://hl7.org/fhir/ValueSet/request-resource-types|5.0.0"
///     → ValueSet ID "request-resource-types"
///     → CodeSystem ID "fhir-types" (via valueset_to_codesystem map)
///     → CodeSystem name "FHIRTypes"
fn resolve_binding_name(
    binding_name: Option<&String>,
    value_set_url: Option<&String>,
    valueset_to_codesystem: &HashMap<String, String>,
    codeset_id_to_name: &HashMap<String, String>,
) -> Option<String> {
    // Strategy 1: If we have a ValueSet URL, use it to resolve the CodeSystem name
    if let Some(url) = value_set_url {
        if let Some(valueset_id) = extract_codeset_id_from_valueset_url(url) {
            // First try direct CodeSystem lookup (for ValueSets where ID == CodeSystem ID)
            if let Some(codeset_name) = codeset_id_to_name.get(&valueset_id) {
                return Some(codeset_name.clone());
            }

            // Then try via ValueSet → CodeSystem mapping (for ValueSets with different IDs)
            if let Some(codesystem_id) = valueset_to_codesystem.get(&valueset_id) {
                if let Some(codeset_name) = codeset_id_to_name.get(codesystem_id) {
                    return Some(codeset_name.clone());
                }
            }
        }
    }

    // Strategy 2: Fall back to binding name if no ValueSet URL or lookup failed
    binding_name.cloned()
}
