(define (inherited-property? element property-name)
    (case (gi element)
        (("resource")
            (case property-name
                (("id" "meta" "implicitRules" "language" "text") #t)
                (else #f)
            )
        )
        (("element")
            (case property-name
                (("id") #t)
                (else #f)
            )
        )
        (else #f)
    )
)

(define (data-type-icon variant children-available? reuse?)
    (case (% "type" variant)
        (("element")
            (case (name-of (ref variant))
                (("Reference") "reference.png")
                (else (if reuse? "reuse.png" (if children-available? "element.gif" "datatype.gif")))
            )
        )
        (else "primitive.png")
    )
)

(define (data-type-text variant children-available? reuse?)
    (case (% "type" variant)
        (("element")
            (case (name-of (ref variant))
                (("Reference") "Reference to another Resource")
                (else (if reuse? "Reference to another Element" (if children-available? "Element" "Data Type")))
            )
        )
        (else "Primitive Data Type")
    )
)

(define (data-type-link variant children-available?)
    (case (% "type" variant)
        (("element")
            (case (name-of (ref variant))
                (("Reference") ($ fhir-doc-root"/references.html#Reference"))
                (else ($ fhir-doc-root (if children-available? "/backboneelement.html" ($ "/datatypes.html#"(name-of (ref variant))))))
            )
        )
        (else ($ fhir-doc-root"/datatypes.html#"(% "type" variant)))
    )
)

(define (data-type-name variant children-available?)
    (case (% "type" variant)
        (("element")
            (if children-available? "BackboneElement" (name-of (ref variant)))
        )
        (else (% "type" variant))
    )
)


<![CDATA[
(define (property-documentation element property tree-shift-images tree-line-prefix element-path) (let* ((resource-name (name)) (resource-downcase (downcase-string resource-name))(property-name (name-of property)))
    (if (inherited-property? element property-name) ""
        (let ((variant? (is-variant? property)))
            (if variant?
                ($
"                    <tr>
                        <td style='white-space: nowrap; background-image: url(tbl_bck"
                        tree-line-prefix
                        (if (last-sibling? property) "0" "1")
                        "1.png); background-color: inherit;'><img src='tbl_spacer.png' alt='.' style='background-color: inherit' />"
                        tree-shift-images
                        "<img src='tbl_vjoin-open.png' alt='.' style='background-color: inherit' />"
                    "<img src='icon_choice.gif' alt='.' style='background-color: inherit' title='Choice of Types' class='hierarchy' /> "property-name" </td>
                        <td></td>
                        <td>0..1</td>
                        <td></td>
                        <td>"(data (select-children "description" property))"</td>
                    </tr>
"                   (for-children-of (select-children "variants" property) (lambda (variant) (let ((children-available? (and (string=? "element" (% "type" variant))(node-list=? element (parent (parent (ref variant))))))) ($
"                    <tr>
                        <td style='white-space: nowrap; background-image: url(tbl_bck"
                        tree-line-prefix
                        (if (last-sibling? property) "0" "1")
                        (if (last-sibling? variant) "0" "1")
                        (if children-available? "1" "0")
                        ".png); background-color: inherit;'><img src='tbl_spacer.png' alt='.' style='background-color: inherit' />"
                        tree-shift-images
                        "<img src='tbl_vline.png' alt='.' style='background-color: inherit' /><img src='tbl_vjoin"
                        (if (last-sibling? variant) "_end" "")
                        ".png' alt='.' style='background-color: inherit' /><img src='icon_"
                        (data-type-icon variant #f #f)
                        "' alt='.' style='background-color: inherit' title='"
                        (data-type-text variant #f #f)
                        "' class='hierarchy' /> "
                        property-name
                        (first-letter-upcase (data-type-name variant #f))
                        "</td>
                        <td></td>
                        <td></td>
                        <td><a href='"
                        (data-type-link variant #f)
                        "'>"
                        (data-type-name variant #f)
                        "</a></td>
                        <td></td>
                    </tr>
"                   ))))
                )
                (let ((render-row (lambda (variant)
                    (let* (
                        (children-available? (and (string=? "element" (% "type" variant)) (% "ref" variant) (node-list=? element (parent (parent (ref variant))))))
                        (reuse? (and children-available?
                            (> (node-list-count
                                (node-list-filter
                                    (lambda (p) (and (% "ref" p) (string=? (% "ref" p) (% "ref" variant))))
                                    (children (select-children "properties" element))))
                               1))))
                        ($
"                    <tr>
                        <td style='white-space: nowrap; background-image: url(tbl_bck"
                            tree-line-prefix
                            (if (last-sibling? property) "0" "1")
                            (if (and children-available? (not reuse?)) "1" "0")
                            ".png); background-color: inherit;'><img src='tbl_spacer.png' alt='.' style='background-color: inherit' />"
                            tree-shift-images
                            "<img src='tbl_vjoin"
                            (if (last-sibling? property) "_end" "")
                            (if children-available? "-open" "")
                            ".png' alt='.' style='background-color: inherit' />"
                            "<img src='icon_"
                            (data-type-icon variant children-available? reuse?)
                            "' alt='.' style='background-color: inherit' title='"
                            (data-type-text variant children-available? reuse?)
                            "' class='hierarchy' /> "property-name" </td>
                        <td>"
                        (if (true? "modifier" property) ($ "<a style='padding-left: 3px; padding-right: 3px; color: black' href='"fhir-doc-root"/conformance-rules.html#isModifier' title='This element is a modifier element'>?!</a>")"")
                        (if (true? "summary" property) ($ "<a style='padding-left: 3px; padding-right: 3px; color: black' href='"fhir-doc-root"/elementdefinition-definitions.html#ElementDefinition.isSummary' title='This element is included in summaries'>Σ</a>")"")
                        "</td>
                        <td>"(if (true? "notnull" property) "1" "0")".."(if (true? "iscollection" property) "*" "1")"</td>
                        <td>"
                        (if reuse?
                            ($
                                "see "(downcase-string (name-of (ref variant)))
                            )
                            ($
                        "<a href='"
                            (data-type-link variant children-available?)
                            "'>"
                            (data-type-name variant children-available?)
                            "</a>"
                            )
                        )
                        (case (% "type" variant)
                            (("element")
                                (case (% "ref" variant)
                                    (("reference")
                                        (let
                                            ((active-targets (node-list-filter (lambda (target) (active? (element-with-id (downcase-string (% "resource" target))))) (children (select-children "targets" variant)))))
                                            ($
                                                "("
                                                (for active-targets (lambda (target) ($
                                            "<a href='"(downcase-string (% "resource" target))".html'>"(let ((target (% "resource" target))) (if (string=? "Resource" target) "Any" target))"</a>"
                                                    (if (node-list=? target (node-list-last active-targets)) "" " | ")
                                                )))
                                                ")"
                                            )
                                        )
                                    )
                                    (else "")
                                )
                            )
                            (else "")
                        )
                    "</td>
                        <td>"
                        (case (% "type" variant)
                            (("code")
                                (if (% "ref" variant)
                                    (for (children (select-children "codes" (ref variant))) (lambda (code) ($
                                        ($ (if (first-sibling? code) "" " | ")(% "value" code))
                                    )))
                                    (data (select-children "description" property))
                                )
                            )
                            (else (data (select-children "description" property)))
                        )
                        "<br />"
                        "</td>
                    </tr>
"                       (if (and children-available? (not reuse?)) (for-children-of (select-children "properties" (ref variant)) (lambda (child) (property-documentation (ref variant) child ($  tree-shift-images "<img src='tbl_"(if (last-sibling? property) "blank" "vline")".png' alt='.' style='background-color: inherit' />") ($  tree-line-prefix (if (last-sibling? property) "0" "1")) ($ element-path (downcase-string (name-of property))))))"")
                )))))
                    (let ((variants-children (children (select-children "variants" property))))
                        (if (node-list-empty? variants-children)
                            (render-row property)
                            (for-children-of (select-children "variants" property) render-row)
                        )
                    )
                )
            )
        )
    )
))



(define (html-documentation-file) (file ($ (downcase-string (name))".html")
  (let* ((resource-name (name)) (resource-downcase (downcase-string resource-name))) ($
    (html-header resource-name #f #f #f)
"                <h1 class='self-link-parent'>Resource "resource-name"</h1>

                <p><a href='/" (route-prefix) "/"resource-name"?_sort=-_lastUpdated'>Browse latest "resource-name" resources on this server.</a></p>

                <p>"(data (select-children "description" (current-node)))"</p>

                <h2 class='self-link-parent'>References </h2>

                <p>This resource is referenced by
"       (for (node-list-filter (lambda (resource) (active? resource)) (children (select-children "resources" (tree-root (current-node))))) (lambda (resource)
            (let ((resource-references
                (for (node-list-filter (lambda (search) (string=? "reference" (% "type" search))) (children (select-children "searches" resource))) (lambda (search)
                    (for-children-of (select-children "paths" search) (lambda (path)
                        ; Match if the path's <targets> list includes the current resource —
                        ; OR the FHIR wildcard <target resource="Resource"/> at either the
                        ; path level OR the underlying property level (Provenance.target is
                        ; the canonical wildcard case: the property says "any Resource" but
                        ; the search-param's explicit list omits this engine's resources).
                        (let* ((path-targets (children (select-children "targets" path)))
                               (path-target-match? (not (node-list-empty? (node-list-filter (lambda (target) (or (string=? (name) (% "resource" target)) (string=? "Resource" (% "resource" target)))) path-targets))))
                               (parts (children (select-children "parts" path)))
                               (last-part (if (node-list-empty? parts) #f (node-list-last parts)))
                               (prop (if last-part (ref last-part) #f))
                               (prop-target-wildcard? (if prop (not (node-list-empty? (node-list-filter (lambda (target) (string=? "Resource" (% "resource" target))) (children (select-children "targets" prop))))) #f)))
                            (if (or path-target-match? prop-target-wildcard?) "#t" ""))
                    ))
                ))))
                (if (string=? "" resource-references) "" (if (string=? resource-name (name-of resource))
"                    itself,
"                   ($
"                    <a href='"(downcase-string (name-of resource))".html#"(name-of resource)"'>"(name-of resource)"</a>,
"                   )
                ))
            )
        ))
"                </p>

                <h2 class='self-link-parent'>Resource Content</h2>

                <table class='structure' id='"resource-name"'>
                    <tr style='border: 1px #F0F0F0 solid; font-size: 11px; font-family: verdana; vertical-align: top'>
                        <th><a href='"fhir-doc-root"/formats.html#table' title='The logical name of the element'>Name</a></th>
                        <th><a href='"fhir-doc-root"/formats.html#table' title='Information about the use of the element'>Flags</a></th>
                        <th><a href='"fhir-doc-root"/formats.html#table' title='Minimum and Maximum # of times the the element can appear in the instance'>Card.</a></th>
                        <th><a href='"fhir-doc-root"/formats.html#table' title='Reference to the type of the element'>Type</a></th>
                        <th><a href='"fhir-doc-root"/formats.html#table' title='Additional information about the element'>Description &amp; Constraints</a><span style='float: right'><a href='"fhir-doc-root"/formats.html#table' title='Legend for this format'><img src='help16.png' alt='doco' style='background-color: inherit' /></a></span></th>
                    </tr>
                    <tr>
                        <td style='white-space: nowrap; background-image: url(tbl_bck1.png); background-color: inherit;'><img src='tbl_spacer.png' alt='.' style='background-color: inherit' /><img src='icon_resource.png' alt='.' style='background-color: inherit' title='Resource' class='hierarchy' /> <a href='"fhir-doc-root"/"resource-downcase"-definitions.html#"resource-name"' title='"resource-name" : Measurements and simple assertions made about a patient, device or other subject.'>"resource-name"</a><a name='"resource-name"'> </a></td>
                        <td></td>
                        <td></td>
                        <td><a href='"fhir-doc-root"/domainresource.html'>DomainResource</a></td>
                        <td>"(data (select-children "description" (current-node)))"</td>
                    </tr>
"    (for-children-of (select-children "properties" (current-node)) (lambda (property) (property-documentation (current-node) property "" "" (id (current-node)))))
"                </table>

                <h2 class='self-link-parent'>Search Parameters</h2>

                <p>Search parameters for this resource. The <a href='"fhir-doc-root"/search.html#all'>common parameters</a> also apply. See <a href='"fhir-doc-root"/search.html'>Searching</a> for more information about searching in REST, messaging, and services.</p>
                <table class='list'>
                    <tr><td><b>Name</b></td><td><b>Type</b></td><td><b>Expression</b></td></tr>
"   (for-children-of (select-children "searches" (current-node)) (lambda (search) (if (string=? "_lastUpdated" (name-of search)) ""
        (let*
            (
                (paths (children (select-children "paths" search)))
                (multi-path? (> (node-list-count paths) 1))
            )
            ($
"                    <tr><td><a name='sp-"(name-of search)"'> </a>"(name-of search)"</td><td><a href='"fhir-doc-root"/search.html#"(% "type" search)"'>"(% "type" search)"</a></td><td>"
                (if multi-path? "(" "")
                (for paths (lambda (path) ($
                    (let*
                        (
                            (path-string ($
                                resource-name
                                (for-children-of (select-children "parts" path) (lambda (part) ($
                                    "."(name-of (ref part))
                                )))
                                (if (string=? "reference" (% "type" search)) ($
                                    "<br />("(for-children-of (select-children "targets" path) (lambda (target) (if (active? (element-with-id (downcase-string (% "resource" target)))) ($ "<a href='"(downcase-string (% "resource" target))".html'>"(% "resource" target)"</a>"(if (last-sibling? target) "" ", "))"")))")"
                                )"")
                            ))
                            (casts (children (select-children "casts" path)))
                            (multi-cast? (> (node-list-count casts) 1))
                        )
                        (if multi-cast?
                            ($
                                "("
                                (for casts (lambda (cast)
                                    ($
                                        path-string" as "
                                        (let*
                                            ((cast-target (% "to" cast))
                                            (element-reference (element-with-id (downcase-string cast-target)))
                                            (element-reference? (equal? 1 (node-list-count element-reference))))
                                            (if element-reference?
                                                ($
                                                    (name-of element-reference)
                                                )
                                                cast-target
                                            )
                                        )
                                        (if (last-sibling? cast) "" " | ")
                                    )
                                ))
                                ")"
                            )
                            path-string
                        )
                    )
                    (if (last-sibling? path) "" " | ")
                )))
                (if multi-path? ")"  "")
                    "</td></tr>
"           )
    ))))
"                </table>

"
    (html-footer)

  ))
))

(define (html-resourcelist-file) (file "resourcelist.html"  ($
    (html-header "Resource List" #f #f #f)
"<div id='tabs-4' aria-labelledby='ui-id-4' class='ui-tabs-panel ui-widget-content ui-corner-bottom' role='tabpanel' aria-hidden='false' style='display: block;'>

<p><b>FHIR</b></p>
<ul style='width: 90%; -moz-column-count: 4; -moz-column-gap: 10px; -webkit-column-count: 4; -webkit-column-gap: 10px; column-count: 4; column-gap: 10px'>
"   (for-active-resources (lambda (resource) ($
"  <li><a href='"(downcase-string (name-of resource))".html' title='"(data (select-children "description" resource))"'>"(name-of resource)"</a></li>
"   )))
"</ul>

</div>"
    (html-footer)
)))
]]>
