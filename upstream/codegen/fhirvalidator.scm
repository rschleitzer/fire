; fhirvalidator.scm — Per-resource/element validator generator
; Generates {project-dir}/Model/Validator/{Name}Validator.cs
; and {project-dir}/Model/Validator/ModelValidator.cs (dispatch)

; --- Validator name helpers ---

; Build concatenated name for nested elements: PatientContact, ObservationComponent
(define (fhir-validator-name el)
    (let ((parent-el (parent (parent el))))
        (if (string=? "element" (gi parent-el))
            ($ (fhir-validator-name parent-el) (fhir-model-explicit-name el))
            (if (string=? "resource" (gi parent-el))
                ($ (name-of parent-el) (fhir-model-explicit-name el))
                (name-of el)))))

; Fully-qualified C# type name for validator method parameter
(define (fhir-validator-type-name el)
    (fhir-model-type-name el))

; --- Per-resource validator file ---

(define (fhir-validator-resource-file resource)
    (file ($ (name-of resource) "Validator.cs") ($
"using "(project-name)".Model;

namespace "(project-name)".Model.Validator;

public static partial class ModelValidator
{"
        (fhir-validate-element resource)
        (for-children-of (select-children "elements" resource)
            (lambda (el) (fhir-validate-element-recursive el)))
"}"
    )))

; --- Per-global-element validator file ---

(define (fhir-validator-global-element-file el)
    (file ($ (name-of el) "Validator.cs") ($
"using "(project-name)".Model;

namespace "(project-name)".Model.Validator;

public static partial class ModelValidator
{"
        (fhir-validate-element el)
        (for-children-of (select-children "elements" el)
            (lambda (child) (fhir-validate-element-recursive child)))
"}"
    )))

; Recursively generate validation methods for nested elements
(define (fhir-validate-element-recursive el) ($
    (fhir-validate-element el)
    (for-children-of (select-children "elements" el)
        (lambda (child) (fhir-validate-element-recursive child)))))

; The name of a validator's parameter: the type's, in lower case -- but not "errors", which
; is the list every validator collects into (CoverageEligibilityResponse has an Errors).
(define (fhir-validator-var-name type-name)
    (let ((v (escape-csharp-keyword (first-letter-downcase type-name))))
        (if (string=? v "errors") "errorsElement" v)))

; --- Generate a Validate{Name}() method ---

(define (fhir-validate-element el)
    (let ((var-name (fhir-validator-var-name
                        (if (string=? "resource" (gi el))
                            (name-of el)
                            (fhir-model-explicit-name el))))
          (method-name (if (string=? "resource" (gi el))
                           (name-of el)
                           (fhir-validator-name el)))
          (type-name (if (string=? "resource" (gi el))
                         (name-of el)
                         (fhir-validator-type-name el))))
        ($
"
    public static List<""string> Validate"method-name"("type-name" "var-name")
    {
        var errors = new List<""string>();
"
            (for (node-list-filter
                    (lambda (p) (not (and (string=? "resource" (gi el)) (resource-property? p))))
                    (children (select-children "properties" el)))
                (lambda (property) (fhir-validate-item property el)))
"
        return errors;
    }
"       )))

; --- Per-property validation ---
; Fire's fhir.xml: most properties have type/ref directly on property,
; only choice types have a variants child element.

(define (fhir-validate-item property el)
    (let ((var-name (fhir-validator-var-name
                        (if (string=? "resource" (gi el))
                            (name-of el)
                            (fhir-model-explicit-name el)))))
        (if (is-variant? property)
            ; Choice type — has variants child with multiple variant children
            (fhir-validate-variant-group property el var-name)
            ; Simple property — type/ref on property directly
            (let ((field-name (escape-csharp-keyword (name-of property)))
                  (vtype (% "type" property))
                  (ref-attr (% "ref" property)))
                (if (true? "iscollection" property)
                    (fhir-validate-collection-direct vtype ref-attr property field-name var-name el)
                    (fhir-validate-single-direct vtype ref-attr property field-name var-name el))))))

; Validate a variant group (choice type) — mutual exclusion + individual validation
; A choice of three types or more says its exclusion once, in a local function the block of
; each type calls with its own name: written out it is a line for every PAIR of types
; (value[x] of an extension: fifty types, 2450 pairs). The messages and their order are the
; same either way.
(define (fhir-validate-variant-group property el var-name)
    (let* ((variant-nodes (children (select-children "variants" property)))
           (shared? (> (node-list-length variant-nodes) 2))
           (only-name ($ "only_" (string-replace (model-item-name property (node-list-first variant-nodes) #t) "@" ""))))
        ($
        (if shared? ($
"
        void "only-name"(string set)
        {"
            (for variant-nodes (lambda (other-variant)
                (let ((other-name (model-item-name property other-variant #t))) ($
"
            if (set != \""other-name"\" && "var-name"."other-name" != null)
                errors.Add($\""var-name"."other-name" must be null if "var-name".{set} is not null.\");"
                ))))
"
        }
") "")
        (for variant-nodes (lambda (variant)
            (let ((field-name (model-item-name property variant #t)))
                ($
"
        if ("var-name"."field-name" != null)
        {"
                    (if shared? ($
"
            "only-name"(\""field-name"\");")
                    (for (node-list-filter (lambda (v) (not (node-list=? v variant))) variant-nodes)
                        (lambda (other-variant)
                            (let ((other-name (model-item-name property other-variant #t))) ($
"
            if ("var-name"."other-name" != null)
                errors.Add(\""var-name"."other-name" must be null if "var-name"."field-name" is not null.\");"
                            )))))
                    (fhir-validate-primitive-direct (% "type" variant) (or (% "ref" variant) (% "ref" property)) property ($ var-name"."field-name) var-name el "")
"
        }
")))))))

; Returns true if the FHIR type maps to a C# string and should use IsNullOrWhiteSpace
(define (fhir-is-string-type? vtype)
    (and vtype (case vtype
        (("string" "markdown" "uri" "url" "canonical" "oid" "uuid" "base64Binary") #t)
        (else #f))))

; Validate a non-collection, non-variant field
(define (fhir-validate-single-direct vtype ref-attr property field-name var-name el)
    (let ((condition (if (fhir-is-string-type? vtype)
                        ($ "!string.IsNullOrWhiteSpace(" var-name "." field-name ")")
                        ($ var-name "." field-name " != null")))
          (not-null-msg (if (fhir-is-string-type? vtype)
                            " must not be null or whitespace."
                            " must not be null."))
          (checks (fhir-validate-primitive-direct vtype ref-attr property ($ var-name"."field-name) var-name el "")))
    ; (a field nothing is checked of and that may be absent: no block at all)
    (if (and (string=? checks "") (not (true? "notnull" property))) ""
    ($
"
        if ("condition")
        {"
    checks
"
        }
"   (if (true? "notnull" property) ($
"        else
        {
            errors.Add(\""var-name"."field-name""not-null-msg"\");
        }
") "")))))

; Validate a collection field
(define (fhir-validate-collection-direct vtype ref-attr property field-name var-name el)
    (let ((checks (fhir-validate-primitive-direct vtype ref-attr property "_item" "_item" el "    ")))
    (if (string=? checks "") ""
    ($
"
        if ("var-name"."field-name" != null)
            foreach (var _item in "var-name"."field-name")
            {"
    checks
"
            }
"))))

; --- Primitive/element type validation ---

(define (fhir-validate-primitive-direct vtype ref-attr property field var-name el indent)
    (let ((path-name ($ (if (string=? "resource" (gi el)) (name-of el) (fhir-model-explicit-name el)) "." (name-of property))))
        (case vtype
            (("element")
                (if ref-attr
                    (let ((target (element-with-id ref-attr)))
                        (if target ($
indent"
"indent"                errors.AddRange(Validate"(fhir-validator-name target)"("field"));")
                            ""))
                    ""))
            (("id") ($
indent"
"indent"                { var e = FhirValidator.ValidateId("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("date") ($
indent"
"indent"                { var e = FhirValidator.ValidateDate("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("dateTime") ($
indent"
"indent"                { var e = FhirValidator.ValidateDateTime("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("instant") ($
indent"
"indent"                { var e = FhirValidator.ValidateInstant("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("time") ($
indent"
"indent"                { var e = FhirValidator.ValidateTime("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("positiveInt") ($
indent"
"indent"                { var e = FhirValidator.ValidatePositiveInt("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("unsignedInt") ($
indent"
"indent"                { var e = FhirValidator.ValidateUnsignedInt("field"); if (e != null) errors.Add(e + \" at "path-name"\"); }"))
            (("code")
                (if ref-attr
                    (let ((codeset (element-with-id ref-attr)))
                        (if codeset ($
indent"
"indent"                switch ("field")
"indent"                {
"                           (for (children (select-children "codes" codeset)) (lambda (code) ($
indent"                    case \""(% "value" code)"\":
"                           )))
indent"                        break;
"indent"                    default:
"indent"                        errors.Add($\"Invalid code '{"field"}' for "path-name".\");
"indent"                        break;
"indent"                }")
                            ""))
                    ""))
            (else ""))))

; --- ModelValidator.cs dispatch ---

(define (fhir-validator-dispatch-file)
    (file "ModelValidator.cs" ($
"using "(project-name)".Model;

namespace "(project-name)".Model.Validator;

public static partial class ModelValidator
{
    public static List<""string> ValidateResource(Resource resource)
    {
        return resource switch
        {
"           (for-model-resources (lambda (resource) (case (name-of resource) (("Resource") "") (else ($
"            "(name-of resource)" r => Validate"(name-of resource)"(r),
")))))
"            _ => [$\"Resource type {resource.resourceType} not supported.\"],
        };
    }
}
"   )))

; --- Top-level dispatchers (return sosofo for use inside make directory) ---

(define (fhir-validator-all-resources)
    (for-nodes (node-list-filter
        (lambda (r) (not (string=? "Resource" (name-of r))))
        (model-resources))
        (lambda (r) (fhir-validator-resource-file r))))

(define (fhir-validator-all-elements)
    (for-nodes (node-list-filter
        (lambda (el) (not (string=? "Element" (name-of el))))
        (children (current-node)))
        (lambda (el) (fhir-validator-global-element-file el))))
