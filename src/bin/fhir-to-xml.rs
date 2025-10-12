use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
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
    min: Option<u32>,
    max: Option<String>,
    binding: Option<ElementBinding>,
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
}

#[derive(Debug, Clone, Deserialize)]
struct SearchComponent {
    definition: String,
    expression: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    println!("🔧 fhir-to-xml - Converting FHIR R5 definitions to XML model");
    println!("   Input:  {}", args.input.display());
    println!("   Output: {}", args.output.display());

    // Step 1: Load FHIR R5 JSON files
    println!("\n📖 Loading FHIR R5 definitions...");
    let structures = load_structure_definitions(&args.input)?;
    let search_params = load_search_parameters(&args.input)?;

    println!("   Found {} structure definitions", structures.len());
    println!("   Found {} search parameters", search_params.len());

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
    let xml = generate_xml(&resources)?;

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

#[derive(Debug)]
struct ResourceDefinition {
    name: String,
    description: String,
    elements: Vec<ElementInfo>,
    search_params: Vec<SearchParamInfo>,
}

#[derive(Debug)]
struct ElementInfo {
    name: String,
    path: String,
    types: Vec<String>,
    description: String,
    cardinality: String,
    binding_name: Option<String>, // ValueSet binding name (e.g., "AdministrativeGender")
}

#[derive(Debug)]
struct SearchParamInfo {
    name: String,
    code: String,
    param_type: String,
    expression: String,
    is_composite: bool,
    components: Vec<String>,
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
        let search = extract_search_params(&structure.name, search_params);

        resources.push(ResourceDefinition {
            name: structure.name.clone(),
            description: structure.description.clone().unwrap_or_default(),
            elements,
            search_params: search,
        });
    }

    Ok(resources)
}

fn extract_elements(structure: &StructureDefinition) -> Vec<ElementInfo> {
    let mut elements = Vec::new();

    if let Some(snapshot) = &structure.snapshot {
        for elem in &snapshot.element {
            // Skip root element and meta fields
            if elem.path == structure.type_name
                || elem.path.starts_with(&format!("{}.id", structure.type_name))
                || elem.path.starts_with(&format!("{}.meta", structure.type_name))
                || elem.path.starts_with(&format!("{}.implicitRules", structure.type_name))
                || elem.path.starts_with(&format!("{}.language", structure.type_name))
                || elem.path.starts_with(&format!("{}.text", structure.type_name))
            {
                continue;
            }

            // Only include direct properties of the resource
            if elem.path.matches('.').count() == 1 {
                let name = elem.path.split('.').last().unwrap_or("").to_string();
                let types = elem.types.as_ref()
                    .map(|t| t.iter().map(|et| et.code.clone()).collect())
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
                    description: elem.short_description.clone().unwrap_or_default(),
                    cardinality,
                    binding_name,
                });
            }
        }
    }

    elements
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
                .map(|comps| comps.iter().map(|c| c.definition.clone()).collect())
                .unwrap_or_default();

            params.push(SearchParamInfo {
                name: param.name.clone(),
                code: param.code.clone(),
                param_type: param.param_type.clone(),
                expression: param.expression.clone().unwrap_or_default(),
                is_composite,
                components,
            });
        }
    }

    params
}

fn generate_xml(resources: &[ResourceDefinition]) -> Result<String, Box<dyn std::error::Error>> {
    let mut xml = String::new();

    // XML header and DTD reference
    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<!DOCTYPE fhir SYSTEM \"fhirspec.dtd\">\n\n");

    // Root element
    xml.push_str("<fhir name=\"Fire\" version=\"5.0.0\" reasoning=\"true\" simplified=\"false\">\n");
    xml.push_str("  <resources>\n");

    for resource in resources {
        generate_resource_xml(&mut xml, resource)?;
    }

    xml.push_str("  </resources>\n");
    xml.push_str("  <elements/>\n"); // Global elements (complex types) - TODO
    xml.push_str("  <codesets/>\n"); // ValueSets - TODO
    xml.push_str("</fhir>\n");

    Ok(xml)
}

fn generate_resource_xml(xml: &mut String, resource: &ResourceDefinition) -> Result<(), Box<dyn std::error::Error>> {
    let resource_id = resource.name.to_lowercase();

    xml.push_str(&format!("    <resource id=\"{}\" name=\"{}\" active=\"true\">\n", resource_id, resource.name));
    xml.push_str(&format!("      <description>{}</description>\n", escape_xml(&resource.description)));

    // Properties (resource-level attributes)
    xml.push_str("      <properties>\n");
    for element in &resource.elements {
        generate_property_xml(xml, element, &resource.name)?;
    }
    xml.push_str("      </properties>\n");

    // Elements (nested complex types)
    xml.push_str("      <elements/>\n"); // TODO: Handle backbone elements

    // Codesets (local ValueSets)
    xml.push_str("      <codesets/>\n");

    // Search parameters
    xml.push_str("      <searches>\n");
    for param in &resource.search_params {
        generate_search_xml(xml, param)?;
    }
    xml.push_str("      </searches>\n");

    xml.push_str("    </resource>\n");

    Ok(())
}

fn generate_property_xml(xml: &mut String, element: &ElementInfo, resource_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Follow Telemed5000 naming convention: {resource_lowercase}.{property_lowercase}
    // Remove [x] suffix from choice types (e.g., deceased[x] -> deceased)
    let property_name_clean = element.name.replace("[x]", "");
    let property_id = format!("{}.{}",
        resource_name.to_lowercase(),
        to_lowercase_preserve_digits(&property_name_clean)
    );
    let is_collection = element.cardinality.ends_with("*");

    // Check if this is a true variant type (choice type with multiple types)
    let is_variant = element.types.len() > 1;

    if is_variant {
        // Choice type (e.g., value[x], deceased[x]) - use type="variant" and <variants>
        xml.push_str(&format!(
            "        <property id=\"{}\" name=\"{}\" type=\"variant\" iscollection=\"{}\" notnull=\"false\">\n",
            property_id,
            element.name,
            is_collection
        ));
        xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&element.description)));

        xml.push_str("          <variants>\n");
        for type_name in &element.types {
            let dtd_type = map_fhir_type_to_dtd(type_name);
            if dtd_type == "element" {
                // Complex type - add ref attribute
                let element_ref = fhir_type_to_element_ref(type_name);
                xml.push_str(&format!("            <variant type=\"{}\" ref=\"{}\"/>\n", dtd_type, element_ref));
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
    } else if element.types.len() == 1 {
        // Single type - flatten by putting type directly on property
        let fhir_type = map_fhir_type_to_dtd(&element.types[0]);

        if fhir_type == "element" {
            // Complex type - add ref attribute
            let element_ref = fhir_type_to_element_ref(&element.types[0]);
            xml.push_str(&format!(
                "        <property id=\"{}\" name=\"{}\" type=\"{}\" ref=\"{}\" iscollection=\"{}\" notnull=\"false\">\n",
                property_id,
                element.name,
                fhir_type,
                element_ref,
                is_collection
            ));
        } else if fhir_type == "code" && element.binding_name.is_some() {
            // Code type with ValueSet binding - add ref to codeset
            let binding_ref = to_lowercase_preserve_digits(element.binding_name.as_ref().unwrap());
            xml.push_str(&format!(
                "        <property id=\"{}\" name=\"{}\" type=\"{}\" ref=\"{}\" iscollection=\"{}\" notnull=\"false\">\n",
                property_id,
                element.name,
                fhir_type,
                binding_ref,
                is_collection
            ));
        } else {
            // Primitive type - no ref
            xml.push_str(&format!(
                "        <property id=\"{}\" name=\"{}\" type=\"{}\" iscollection=\"{}\" notnull=\"false\">\n",
                property_id,
                element.name,
                fhir_type,
                is_collection
            ));
        }
        xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&element.description)));
    } else {
        // No type info - default to element without ref (will need manual curation)
        xml.push_str(&format!(
            "        <property id=\"{}\" name=\"{}\" type=\"element\" iscollection=\"{}\" notnull=\"false\">\n",
            property_id,
            element.name,
            is_collection
        ));
        xml.push_str(&format!("          <description>{}</description>\n", escape_xml(&element.description)));
    }

    xml.push_str("        </property>\n");

    Ok(())
}

fn generate_search_xml(xml: &mut String, param: &SearchParamInfo) -> Result<(), Box<dyn std::error::Error>> {
    xml.push_str(&format!(
        "        <search name=\"{}\" query=\"{}\" type=\"{}\">\n",
        param.name,
        param.code,
        param.param_type
    ));

    xml.push_str("          <paths>\n");
    xml.push_str("            <path>\n");
    xml.push_str("              <parts>\n");
    xml.push_str("                <!-- TODO: Parse expression and generate parts -->\n");
    xml.push_str("              </parts>\n");

    if param.is_composite {
        xml.push_str("              <components>\n");
        for component in &param.components {
            xml.push_str(&format!("                <!-- Component: {} -->\n", component));
        }
        xml.push_str("              </components>\n");
    }

    xml.push_str("            </path>\n");
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
        "Reference" => "variant", // References are complex types
        _ => "element", // All complex types are elements
    }
}

/// Convert CamelCase to lowercase, preserving digits
/// Examples: birthDate -> birthdate, multipleBirth -> multiplebirth
fn to_lowercase_preserve_digits(s: &str) -> String {
    s.chars().map(|c| c.to_ascii_lowercase()).collect()
}

/// Map FHIR complex type names to their lowercase element ref
/// Examples: HumanName -> humanname, ContactPoint -> contactpoint, Reference -> reference
fn fhir_type_to_element_ref(fhir_type: &str) -> String {
    to_lowercase_preserve_digits(fhir_type)
}

fn escape_xml(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}
