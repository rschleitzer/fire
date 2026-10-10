; fhirmodel.scm — Typed FHIR model class generator
; Generates {project-dir}/Model/{Name}.cs for resources and global elements

; --- Type mapping (dates stay as string, not DateTime) ---

(define (fhir-model-type variant)
    (case (% "type" variant)
        (("boolean") "bool")
        (("integer" "positiveInt" "unsignedInt") "int")
        (("integer64") "long")
        (("decimal") "decimal")
        (("element") (fhir-model-class-ref variant))
        (else "string")))

; Resolve element reference to a C# class name
; Backbone elements (nested under a resource) get Resource.Nested form
; Global elements get their plain name
(define (fhir-model-class-ref variant)
    (let ((target (ref variant)))
        (if target
            (fhir-model-type-name target)
            "object")))

; Build the dot-qualified name for nested classes
; e.g., Patient.Contact for a backbone element under Patient
(define (fhir-model-type-name element)
    (let ((parent-element (parent (parent element))))
        (if (string=? "element" (gi parent-element))
            ($ (fhir-model-type-name parent-element) "." (fhir-model-explicit-name element))
            (if (string=? "resource" (gi parent-element))
                ($ (name-of parent-element) "." (fhir-model-explicit-name element))
                (fhir-model-explicit-name element)))))

; Use explicit name if available, otherwise use name
(define (fhir-model-explicit-name element)
    (let ((explicit (% "explicit" element)))
        (if explicit explicit (name-of element))))

; --- Resource class file ---

(define (fhir-model-resource-file resource)
    (file ($ (name-of resource) ".cs") ($
"namespace "(project-name)".Model;
"       (case (name-of resource)
            (("Resource") ($
"
public class Resource
{
    public string? resourceType { get; set; }
    public string? id { get; set; }
    public Meta? meta { get; set; }
    public string? implicitRules { get; set; }
    public string? language { get; set; }
}
"))
            (else ($
"
public class "(name-of resource)" : Resource
{"
                (for (node-list-filter (lambda (p) (not (resource-property? p)))
                         (children (select-children "properties" resource)))
                    (lambda (property) (fhir-model-item property)))
                (for-children-of (select-children "elements" resource)
                    (lambda (el) (fhir-model-element-class el "    ")))
"
}
"))
        )
    ))
)

; --- Global element class file ---

(define (fhir-model-global-element-file el)
    (file ($ (name-of el) ".cs") ($
"namespace "(project-name)".Model;
"
        (fhir-model-element-class el "")
    ))
)

; --- Nested (backbone) element class ---

(define (fhir-model-element-class element indent) ($
"
"indent"public class "(fhir-model-explicit-name element)"
"indent"{"
    (for-children-of (select-children "properties" element)
        (lambda (property) (fhir-model-item-indented property indent)))
    (for-children-of (select-children "elements" element)
        (lambda (child) (fhir-model-element-class child ($ indent "    "))))
"
"indent"}
"))

; --- Property line generator ---

(define (fhir-model-item property)
    (fhir-model-item-indented property ""))

(define (fhir-model-item-indented property indent)
    (let ((variant? (is-variant? property)))
        (for (variants-of property) (lambda (variant) ($
"
"indent"    public "(if (true? "iscollection" property) ($ "List<""") "")(fhir-model-type variant)(if (true? "iscollection" property) ">?" "?")" "(model-item-name property variant variant?)" { get; set; }"
        )))
    ))

; --- Model .csproj ---

(define (fhir-model-csproj)
    (file ($ (project-name) ".Model.csproj") ($
<![CDATA["<Project Sdk=\"Microsoft.NET.Sdk\">

  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>

</Project>
"]]>)))

; --- OperationOutcomeException.cs ---

(define (fhir-model-operation-outcome-exception)
    (file "OperationOutcomeException.cs" ($
"namespace "(project-name)".Model;

public class OperationOutcomeException(OperationOutcome operationOutcome)
    : Exception(operationOutcome.issue?.FirstOrDefault()?.diagnostics ?? \"An error occurred.\")
{
    public OperationOutcome OperationOutcome => operationOutcome;
}
")))

; --- Top-level dispatchers (return sosofo for use inside make directory) ---

(define (fhir-model-all-resources)
    (sosofo-append
        (fhir-model-operation-outcome-exception)
        (for-nodes (model-resources) (lambda (r) (fhir-model-resource-file r)))))

(define (fhir-model-all-elements)
    (for-nodes (node-list-filter
        (lambda (el) (not (string=? "Element" (name-of el))))
        (children (current-node)))
        (lambda (el) (fhir-model-global-element-file el))))
