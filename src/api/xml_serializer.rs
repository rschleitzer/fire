use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, Event};
use quick_xml::Writer;
use serde_json::Value;
use std::io::Cursor;

/// Convert FHIR JSON to XML
pub fn json_to_xml(json: &Value) -> Result<String, String> {
    let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 2);

    // Write XML declaration
    writer
        .write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))
        .map_err(|e| format!("Failed to write XML declaration: {}", e))?;

    // Write the root element
    write_element(&mut writer, json)?;

    let result = String::from_utf8(writer.into_inner().into_inner())
        .map_err(|e| format!("Failed to convert XML to UTF-8: {}", e))?;

    Ok(result)
}

fn write_element<W: std::io::Write>(writer: &mut Writer<W>, value: &Value) -> Result<(), String> {
    match value {
        Value::Object(map) => {
            // For FHIR resources, the root element is the resourceType
            if let Some(resource_type) = map.get("resourceType").and_then(|v| v.as_str()) {
                let mut elem = BytesStart::new(resource_type);
                elem.push_attribute(("xmlns", "http://hl7.org/fhir"));

                writer
                    .write_event(Event::Start(elem))
                    .map_err(|e| format!("Failed to write start tag: {}", e))?;

                // Write all other fields
                for (key, val) in map.iter() {
                    if key != "resourceType" {
                        write_field(writer, key, val)?;
                    }
                }

                writer
                    .write_event(Event::End(BytesEnd::new(resource_type)))
                    .map_err(|e| format!("Failed to write end tag: {}", e))?;
            }
        }
        _ => return Err("Expected JSON object at root".to_string()),
    }

    Ok(())
}

fn write_field<W: std::io::Write>(
    writer: &mut Writer<W>,
    key: &str,
    value: &Value,
) -> Result<(), String> {
    match value {
        Value::Null => {
            // Skip null values
        }
        Value::Bool(b) => {
            let mut elem = BytesStart::new(key);
            elem.push_attribute(("value", if *b { "true" } else { "false" }));

            writer
                .write_event(Event::Empty(elem))
                .map_err(|e| format!("Failed to write boolean element: {}", e))?;
        }
        Value::Number(n) => {
            let mut elem = BytesStart::new(key);
            elem.push_attribute(("value", n.to_string().as_str()));

            writer
                .write_event(Event::Empty(elem))
                .map_err(|e| format!("Failed to write number element: {}", e))?;
        }
        Value::String(s) => {
            if s.is_empty() {
                // Skip empty strings
            } else {
                let mut elem = BytesStart::new(key);
                elem.push_attribute(("value", s.as_str()));

                writer
                    .write_event(Event::Empty(elem))
                    .map_err(|e| format!("Failed to write string element: {}", e))?;
            }
        }
        Value::Array(arr) => {
            // Write each array element as a separate element with the same name
            for item in arr {
                write_field(writer, key, item)?;
            }
        }
        Value::Object(map) => {
            // Complex types - write as nested elements
            writer
                .write_event(Event::Start(BytesStart::new(key)))
                .map_err(|e| format!("Failed to write complex start tag: {}", e))?;

            for (nested_key, nested_val) in map.iter() {
                write_field(writer, nested_key, nested_val)?;
            }

            writer
                .write_event(Event::End(BytesEnd::new(key)))
                .map_err(|e| format!("Failed to write complex end tag: {}", e))?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_simple_patient() {
        let patient = json!({
            "resourceType": "Patient",
            "id": "example",
            "active": true,
            "name": [{
                "family": "Doe",
                "given": ["John"]
            }],
            "gender": "male"
        });

        let xml = json_to_xml(&patient).unwrap();
        assert!(xml.contains("<Patient"));
        assert!(xml.contains("xmlns=\"http://hl7.org/fhir\""));
        assert!(xml.contains("<id value=\"example\""));
        assert!(xml.contains("<active value=\"true\""));
        assert!(xml.contains("<gender value=\"male\""));
    }
}
