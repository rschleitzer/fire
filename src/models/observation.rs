use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::FromRow;

use super::traits::VersionedResource;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Observation {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is, returned without deserialization
}

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct ObservationHistory {
    pub id: String,
    pub version_id: i32,
    pub last_updated: DateTime<Utc>,
    #[serde(flatten)]
    pub content: Value, // Raw JSON stored as-is

    // History metadata
    pub history_operation: String,
    pub history_timestamp: DateTime<Utc>,
}

impl VersionedResource for Observation {
    type History = ObservationHistory;

    const RESOURCE_TYPE: &'static str = "Observation";
    const TABLE_NAME: &'static str = "observation";
    const HISTORY_TABLE_NAME: &'static str = "observation_history";

    fn get_id(&self) -> &String {
        &self.id
    }

    fn get_version_id(&self) -> i32 {
        self.version_id
    }

    fn get_last_updated(&self) -> &DateTime<Utc> {
        &self.last_updated
    }

    fn get_content(&self) -> &Value {
        &self.content
    }
}

/// Inject id and meta fields into a FHIR Observation resource
/// This is used at storage time to ensure all stored resources have complete id/meta
pub fn inject_id_meta(content: &Value, id: &str, version_id: i32, last_updated: &DateTime<Utc>) -> Value {
    if let Some(content_obj) = content.as_object() {
        // Create new object with fields in FHIR-standard order
        let mut resource = serde_json::Map::new();

        // 1. resourceType (if present in content)
        if let Some(resource_type) = content_obj.get("resourceType") {
            resource.insert("resourceType".to_string(), resource_type.clone());
        }

        // 2. id (always inject)
        resource.insert("id".to_string(), serde_json::json!(id));

        // 3. meta (always inject)
        resource.insert("meta".to_string(), serde_json::json!({
            "versionId": version_id.to_string(),
            "lastUpdated": last_updated.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        }));

        // 4. All other fields from content (skip resourceType, id, meta if client sent them)
        for (key, value) in content_obj {
            if key != "resourceType" && key != "id" && key != "meta" {
                resource.insert(key.clone(), value.clone());
            }
        }

        Value::Object(resource)
    } else {
        // Fallback if content is not an object - just return as-is
        content.clone()
    }
}
/// Extract search parameters from FHIR Observation JSON
pub fn extract_observation_search_params(content: &Value) -> ObservationSearchParams {
    let mut params = ObservationSearchParams::default();

    // Extract _lastUpdated
    if let Some(_last_updated) = content.get("meta").and_then(|b| b.as_str()) {
        if let Ok(date) = NaiveDate::parse_from_str(_last_updated, "%Y-%m-%d") {
            params._last_updated = Some(date);
        }
    }

    // Extract identifiers
    if let Some(identifiers) = content.get("identifier").and_then(|i| i.as_array()) {
        for identifier in identifiers {
            if let Some(system) = identifier.get("system").and_then(|s| s.as_str()) {
                params.identifier_system.push(system.to_string());
            }
            if let Some(value) = identifier.get("value").and_then(|v| v.as_str()) {
                params.identifier_value.push(value.to_string());
            }
        }
    }

    // Extract patient reference
    if let Some(reference) = content
        .get("subject")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("Patient/") {
            params.patient_reference = Some(reference.to_string());
        }
    }

    // Extract code
    if let Some(code_obj) = content.get("code") {
        if let Some(codings) = code_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.code_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.code_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract effectiveDateTime (maps to date_datetime in DB)
    if let Some(effective) = content.get("effectiveDateTime").and_then(|e| e.as_str()) {
        if let Ok(dt) = DateTime::parse_from_rfc3339(effective) {
            params.date_datetime = Some(dt.with_timezone(&Utc));
        }
    }

    // Extract effectivePeriod (maps to date_period_start/end in DB)
    if let Some(period) = content.get("effectivePeriod") {
        if let Some(start) = period.get("start").and_then(|s| s.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(start) {
                params.date_period_start = Some(dt.with_timezone(&Utc));
            }
        }
        if let Some(end) = period.get("end").and_then(|e| e.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(end) {
                params.date_period_end = Some(dt.with_timezone(&Utc));
            }
        }
    }

    // Extract encounter reference
    if let Some(reference) = content
        .get("encounter")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("Encounter/") {
            params.encounter_reference = Some(reference.to_string());
        }
    }

    // Extract based-on references
    if let Some(refs) = content.get("basedOn").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("DeviceRequest/") || reference.starts_with("ServiceRequest/") || reference.starts_with("CarePlan/") || reference.starts_with("MedicationRequest/") || reference.starts_with("ImmunizationRecommendation/") || reference.starts_with("NutritionOrder/") {
                    params.based_on_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract category
    if let Some(categorys) = content.get("category").and_then(|c| c.as_array()) {
        for category in categorys {
            if let Some(codings) = category.get("coding").and_then(|c| c.as_array()) {
                for coding in codings {
                    if let Some(system) = coding.get("system").and_then(|s| s.as_str()) {
                        params.category_system.push(system.to_string());
                    }
                    if let Some(code) = coding.get("code").and_then(|c| c.as_str()) {
                        params.category_code.push(code.to_string());
                    }
                }
            }
        }
    }

    // Extract combo-code
    if let Some(combo_code_obj) = content.get("code") {
        if let Some(codings) = combo_code_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.combo_code_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.combo_code_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract combo-data-absent-reason
    if let Some(combo_data_absent_reason_obj) = content.get("dataAbsentReason") {
        if let Some(codings) = combo_data_absent_reason_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.combo_data_absent_reason_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.combo_data_absent_reason_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract combo-value-concept
    if let Some(combo_value_concept_concept) = content.get("valueCodeableConcept") {
        if let Some(codings) = combo_value_concept_concept.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.combo_value_concept_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract combo-value-quantity
    if let Some(combo_value_quantity_qty) = content.get("valueQuantity") {
        if let Some(value) = combo_value_quantity_qty.get("value").and_then(|v| v.as_f64()) {
            params.combo_value_quantity_value = sqlx::types::BigDecimal::try_from(value)
                .ok();
        }
        if let Some(unit) = combo_value_quantity_qty.get("unit").and_then(|u| u.as_str()) {
            params.combo_value_quantity_unit = Some(unit.to_string());
        }
        if let Some(system) = combo_value_quantity_qty.get("system").and_then(|s| s.as_str()) {
            params.combo_value_quantity_system = Some(system.to_string());
        }
    }

    // Extract component-value-reference references
    if let Some(refs) = content.get("component").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("MolecularSequence/") {
                    params.component_value_reference_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract data-absent-reason
    if let Some(data_absent_reason_obj) = content.get("dataAbsentReason") {
        if let Some(codings) = data_absent_reason_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.data_absent_reason_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.data_absent_reason_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract derived-from references
    if let Some(refs) = content.get("derivedFrom").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("ImagingStudy/") || reference.starts_with("DocumentReference/") || reference.starts_with("Observation/") || reference.starts_with("MolecularSequence/") || reference.starts_with("GenomicStudy/") || reference.starts_with("ImagingSelection/") || reference.starts_with("QuestionnaireResponse/") {
                    params.derived_from_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract device reference
    if let Some(reference) = content
        .get("device")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("Device/") || reference.starts_with("DeviceMetric/") {
            params.device_reference = Some(reference.to_string());
        }
    }

    // Extract focus references
    if let Some(refs) = content.get("focus").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("Account/") || reference.starts_with("ActivityDefinition/") || reference.starts_with("ActorDefinition/") || reference.starts_with("AdministrableProductDefinition/") || reference.starts_with("AdverseEvent/") || reference.starts_with("AllergyIntolerance/") || reference.starts_with("Appointment/") || reference.starts_with("AppointmentResponse/") || reference.starts_with("ArtifactAssessment/") || reference.starts_with("AuditEvent/") || reference.starts_with("Basic/") || reference.starts_with("Binary/") || reference.starts_with("BiologicallyDerivedProduct/") || reference.starts_with("BiologicallyDerivedProductDispense/") || reference.starts_with("BodyStructure/") || reference.starts_with("Bundle/") || reference.starts_with("CapabilityStatement/") || reference.starts_with("CarePlan/") || reference.starts_with("CareTeam/") || reference.starts_with("ChargeItem/") || reference.starts_with("ChargeItemDefinition/") || reference.starts_with("Citation/") || reference.starts_with("Claim/") || reference.starts_with("ClaimResponse/") || reference.starts_with("ClinicalImpression/") || reference.starts_with("ClinicalUseDefinition/") || reference.starts_with("CodeSystem/") || reference.starts_with("Communication/") || reference.starts_with("CommunicationRequest/") || reference.starts_with("CompartmentDefinition/") || reference.starts_with("Composition/") || reference.starts_with("ConceptMap/") || reference.starts_with("Condition/") || reference.starts_with("ConditionDefinition/") || reference.starts_with("Consent/") || reference.starts_with("Contract/") || reference.starts_with("Coverage/") || reference.starts_with("CoverageEligibilityRequest/") || reference.starts_with("CoverageEligibilityResponse/") || reference.starts_with("DetectedIssue/") || reference.starts_with("Device/") || reference.starts_with("DeviceAssociation/") || reference.starts_with("DeviceDefinition/") || reference.starts_with("DeviceDispense/") || reference.starts_with("DeviceMetric/") || reference.starts_with("DeviceRequest/") || reference.starts_with("DeviceUsage/") || reference.starts_with("DiagnosticReport/") || reference.starts_with("DocumentReference/") || reference.starts_with("Encounter/") || reference.starts_with("EncounterHistory/") || reference.starts_with("Endpoint/") || reference.starts_with("EnrollmentRequest/") || reference.starts_with("EnrollmentResponse/") || reference.starts_with("EpisodeOfCare/") || reference.starts_with("EventDefinition/") || reference.starts_with("Evidence/") || reference.starts_with("EvidenceReport/") || reference.starts_with("EvidenceVariable/") || reference.starts_with("ExampleScenario/") || reference.starts_with("ExplanationOfBenefit/") || reference.starts_with("FamilyMemberHistory/") || reference.starts_with("Flag/") || reference.starts_with("FormularyItem/") || reference.starts_with("GenomicStudy/") || reference.starts_with("Goal/") || reference.starts_with("GraphDefinition/") || reference.starts_with("Group/") || reference.starts_with("GuidanceResponse/") || reference.starts_with("HealthcareService/") || reference.starts_with("ImagingSelection/") || reference.starts_with("ImagingStudy/") || reference.starts_with("Immunization/") || reference.starts_with("ImmunizationEvaluation/") || reference.starts_with("ImmunizationRecommendation/") || reference.starts_with("ImplementationGuide/") || reference.starts_with("Ingredient/") || reference.starts_with("InsurancePlan/") || reference.starts_with("InventoryItem/") || reference.starts_with("InventoryReport/") || reference.starts_with("Invoice/") || reference.starts_with("Library/") || reference.starts_with("Linkage/") || reference.starts_with("List/") || reference.starts_with("Location/") || reference.starts_with("ManufacturedItemDefinition/") || reference.starts_with("Measure/") || reference.starts_with("MeasureReport/") || reference.starts_with("Medication/") || reference.starts_with("MedicationAdministration/") || reference.starts_with("MedicationDispense/") || reference.starts_with("MedicationKnowledge/") || reference.starts_with("MedicationRequest/") || reference.starts_with("MedicationStatement/") || reference.starts_with("MedicinalProductDefinition/") || reference.starts_with("MessageDefinition/") || reference.starts_with("MessageHeader/") || reference.starts_with("MolecularSequence/") || reference.starts_with("NamingSystem/") || reference.starts_with("NutritionIntake/") || reference.starts_with("NutritionOrder/") || reference.starts_with("NutritionProduct/") || reference.starts_with("Observation/") || reference.starts_with("ObservationDefinition/") || reference.starts_with("OperationDefinition/") || reference.starts_with("OperationOutcome/") || reference.starts_with("Organization/") || reference.starts_with("OrganizationAffiliation/") || reference.starts_with("PackagedProductDefinition/") || reference.starts_with("Parameters/") || reference.starts_with("Patient/") || reference.starts_with("PaymentNotice/") || reference.starts_with("PaymentReconciliation/") || reference.starts_with("Permission/") || reference.starts_with("Person/") || reference.starts_with("PlanDefinition/") || reference.starts_with("Practitioner/") || reference.starts_with("PractitionerRole/") || reference.starts_with("Procedure/") || reference.starts_with("Provenance/") || reference.starts_with("Questionnaire/") || reference.starts_with("QuestionnaireResponse/") || reference.starts_with("RegulatedAuthorization/") || reference.starts_with("RelatedPerson/") || reference.starts_with("RequestOrchestration/") || reference.starts_with("Requirements/") || reference.starts_with("ResearchStudy/") || reference.starts_with("ResearchSubject/") || reference.starts_with("RiskAssessment/") || reference.starts_with("Schedule/") || reference.starts_with("SearchParameter/") || reference.starts_with("ServiceRequest/") || reference.starts_with("Slot/") || reference.starts_with("Specimen/") || reference.starts_with("SpecimenDefinition/") || reference.starts_with("StructureDefinition/") || reference.starts_with("StructureMap/") || reference.starts_with("Subscription/") || reference.starts_with("SubscriptionStatus/") || reference.starts_with("SubscriptionTopic/") || reference.starts_with("Substance/") || reference.starts_with("SubstanceDefinition/") || reference.starts_with("SubstanceNucleicAcid/") || reference.starts_with("SubstancePolymer/") || reference.starts_with("SubstanceProtein/") || reference.starts_with("SubstanceReferenceInformation/") || reference.starts_with("SubstanceSourceMaterial/") || reference.starts_with("SupplyDelivery/") || reference.starts_with("SupplyRequest/") || reference.starts_with("Task/") || reference.starts_with("TerminologyCapabilities/") || reference.starts_with("TestPlan/") || reference.starts_with("TestReport/") || reference.starts_with("TestScript/") || reference.starts_with("Transport/") || reference.starts_with("ValueSet/") || reference.starts_with("VerificationResult/") || reference.starts_with("VisionPrescription/") {
                    params.focus_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract has-member references
    if let Some(refs) = content.get("hasMember").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("Observation/") || reference.starts_with("MolecularSequence/") || reference.starts_with("QuestionnaireResponse/") {
                    params.has_member_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract method
    if let Some(method_obj) = content.get("method") {
        if let Some(codings) = method_obj.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(system) = first.get("system").and_then(|s| s.as_str()) {
                    params.method_system = Some(system.to_string());
                }
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.method_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract part-of references
    if let Some(refs) = content.get("partOf").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("ImagingStudy/") || reference.starts_with("Procedure/") || reference.starts_with("MedicationStatement/") || reference.starts_with("MedicationAdministration/") || reference.starts_with("GenomicStudy/") || reference.starts_with("Immunization/") || reference.starts_with("MedicationDispense/") {
                    params.part_of_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract performer references
    if let Some(refs) = content.get("performer").and_then(|g| g.as_array()) {
        for ref_item in refs {
            if let Some(reference) = ref_item.get("reference").and_then(|r| r.as_str()) {
                if reference.starts_with("Organization/") || reference.starts_with("CareTeam/") || reference.starts_with("RelatedPerson/") || reference.starts_with("PractitionerRole/") || reference.starts_with("Practitioner/") || reference.starts_with("Patient/") {
                    params.performer_reference.push(reference.to_string());
                }
            }
        }
    }

    // Extract specimen reference
    if let Some(reference) = content
        .get("specimen")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("Group/") || reference.starts_with("Specimen/") {
            params.specimen_reference = Some(reference.to_string());
        }
    }

    // Extract status
    if let Some(status) = content.get("status").and_then(|g| g.as_str()) {
        params.status = Some(status.to_string());
    }

    // Extract subject reference
    if let Some(reference) = content
        .get("subject")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("Device/") || reference.starts_with("Organization/") || reference.starts_with("Procedure/") || reference.starts_with("NutritionProduct/") || reference.starts_with("Group/") || reference.starts_with("Practitioner/") || reference.starts_with("BiologicallyDerivedProduct/") || reference.starts_with("Substance/") || reference.starts_with("Location/") || reference.starts_with("Patient/") || reference.starts_with("Medication/") {
            params.subject_reference = Some(reference.to_string());
        }
    }

    // Extract value-concept
    if let Some(value_concept_concept) = content.get("valueCodeableConcept") {
        if let Some(codings) = value_concept_concept.get("coding").and_then(|c| c.as_array()) {
            if let Some(first) = codings.first() {
                if let Some(code) = first.get("code").and_then(|c| c.as_str()) {
                    params.value_concept_code = Some(code.to_string());
                }
            }
        }
    }

    // Extract valueDateTime (maps to value_date_datetime in DB)
    if let Some(effective) = content.get("valueDateTime").and_then(|e| e.as_str()) {
        if let Ok(dt) = DateTime::parse_from_rfc3339(effective) {
            params.value_date_datetime = Some(dt.with_timezone(&Utc));
        }
    }

    // Extract valuePeriod (maps to value_date_period_start/end in DB)
    if let Some(period) = content.get("valuePeriod") {
        if let Some(start) = period.get("start").and_then(|s| s.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(start) {
                params.value_date_period_start = Some(dt.with_timezone(&Utc));
            }
        }
        if let Some(end) = period.get("end").and_then(|e| e.as_str()) {
            if let Ok(dt) = DateTime::parse_from_rfc3339(end) {
                params.value_date_period_end = Some(dt.with_timezone(&Utc));
            }
        }
    }

    // Extract value-quantity
    if let Some(value_quantity_qty) = content.get("valueQuantity") {
        if let Some(value) = value_quantity_qty.get("value").and_then(|v| v.as_f64()) {
            params.value_quantity_value = sqlx::types::BigDecimal::try_from(value)
                .ok();
        }
        if let Some(unit) = value_quantity_qty.get("unit").and_then(|u| u.as_str()) {
            params.value_quantity_unit = Some(unit.to_string());
        }
        if let Some(system) = value_quantity_qty.get("system").and_then(|s| s.as_str()) {
            params.value_quantity_system = Some(system.to_string());
        }
    }

    // Extract value-reference reference
    if let Some(reference) = content
        .get("value")
        .and_then(|s| s.get("reference"))
        .and_then(|r| r.as_str())
    {
        if reference.starts_with("MolecularSequence/") {
            params.value_reference_reference = Some(reference.to_string());
        }
    }

    params
}

#[derive(Debug, Default)]
pub struct ObservationSearchParams {
    pub _last_updated: Option<NaiveDate>,
    pub identifier_system: Vec<String>,
    pub identifier_value: Vec<String>,
    pub patient_reference: Option<String>,
    pub code_system: Option<String>,
    pub code_code: Option<String>,
    pub date_datetime: Option<DateTime<Utc>>,
    pub date_period_start: Option<DateTime<Utc>>,
    pub date_period_end: Option<DateTime<Utc>>,
    pub encounter_reference: Option<String>,
    pub based_on_reference: Vec<String>,
    pub category_system: Vec<String>,
    pub category_code: Vec<String>,
    pub combo_code_system: Option<String>,
    pub combo_code_code: Option<String>,
    pub combo_data_absent_reason_system: Option<String>,
    pub combo_data_absent_reason_code: Option<String>,
    pub combo_value_concept_code: Option<String>,
    pub combo_value_quantity_value: Option<sqlx::types::BigDecimal>,
    pub combo_value_quantity_unit: Option<String>,
    pub combo_value_quantity_system: Option<String>,
    pub component_value_quantity_value: Option<sqlx::types::BigDecimal>,
    pub component_value_quantity_unit: Option<String>,
    pub component_value_quantity_system: Option<String>,
    pub component_value_reference_reference: Vec<String>,
    pub data_absent_reason_system: Option<String>,
    pub data_absent_reason_code: Option<String>,
    pub derived_from_reference: Vec<String>,
    pub device_reference: Option<String>,
    pub focus_reference: Vec<String>,
    pub has_member_reference: Vec<String>,
    pub method_system: Option<String>,
    pub method_code: Option<String>,
    pub part_of_reference: Vec<String>,
    pub performer_reference: Vec<String>,
    pub specimen_reference: Option<String>,
    pub status: Option<String>,
    pub subject_reference: Option<String>,
    pub value_concept_code: Option<String>,
    pub value_date_datetime: Option<DateTime<Utc>>,
    pub value_date_period_start: Option<DateTime<Utc>>,
    pub value_date_period_end: Option<DateTime<Utc>>,
    pub value_quantity_value: Option<sqlx::types::BigDecimal>,
    pub value_quantity_unit: Option<String>,
    pub value_quantity_system: Option<String>,
    pub value_reference_reference: Option<String>,
}
