<![CDATA[


(define (js-resource-file) (file ($ (name)".js") (let* ((resource-name (name-of (current-node)))(resource-variable (first-letter-downcase resource-name))) (let ((fhir (route-prefix))) ($
"function buildResource(){
    var jsonElement = document.getElementById('json')
    var element = JSON.parse(jsonElement.childNodes[0].textContent.replace('-/->', '-->'))
    jsonElement.parentElement.removeChild(jsonElement)
    build"(id (current-node))"(element).then(("(id (current-node))"Html) => {
    let elementString =
`        <h1>"(name)"</h1>
"   (case (name)
        (("OperationOutcome") "")
        (else ($
"        <table><tbody>
          <tr>
            <td><input type='button' value='Save' onclick='save()'/></td>
            <td><input type='button' value='New' onclick='New"resource-name"()'/></td>
            <td><input type='button' style='color: red' value='Rollback' onclick='rollback()'></input></td>
            <td><a href='/"fhir"/"(name)"/${element.id}/_history'>History</a></td>
            <td><a download='"(name)"/${element.id}_${element.meta.versionId}.json' href='/"fhir"/"(name)"/${element.id}/_history/${element.meta.versionId}?_format=json'>JSON</a></td>
            <td><a download='"(name)"/${element.id}_${element.meta.versionId}.xml' href='/"fhir"/"(name)"/${element.id}/_history/${element.meta.versionId}?_format=xml'>XML</a></td>
          </tr>
        </tbody></table>
"       ))
)
"${"(id (current-node))"Html
}        <table>
${buildLinks(element.id)
}        </table>
`;
    document.getElementById('body').insertAdjacentHTML('beforeend', elementString);
    });
}

function buildLinks(id) {
    let links = 
`"
    (for (node-list-filter (lambda (resource) (active? resource)) (children (select-children "resources" (tree-root (current-node))))) (lambda (resource)
        (let ((resource-row
            (for (node-list-filter (lambda (search) (string=? "reference" (% "type" search))) (children (select-children "searches" resource))) (lambda (search)
                (for-children-of (select-children "paths" search) (lambda (path)
                    ; Match on path's <targets> (current resource OR FHIR wildcard "Resource"),
                    ; with fallback to the underlying property's <targets> for the wildcard
                    ; case (Provenance.target is the canonical example).
                    (let* ((path-targets (children (select-children "targets" path)))
                           (path-target-match? (not (node-list-empty? (node-list-filter (lambda (target) (or (string=? (name) (% "resource" target)) (string=? "Resource" (% "resource" target)))) path-targets))))
                           (parts (children (select-children "parts" path)))
                           (last-part (if (node-list-empty? parts) #f (node-list-last parts)))
                           (prop (if last-part (ref last-part) #f))
                           (prop-target-wildcard? (if prop (not (node-list-empty? (node-list-filter (lambda (target) (string=? "Resource" (% "resource" target))) (children (select-children "targets" prop))))) #f)))
                        (if (or path-target-match? prop-target-wildcard?) ($
"<td><a href='/"fhir"/"(name-of resource)"?"(name-of search)"="(name)"/${id}&amp;_sort=-_lastUpdated&amp;_count=100"(bundle-elements resource)"'>"(name-of resource)"."(name-of search)"</a></td>"
                        )""))
                ))
            ))))
            (if (string=? "" resource-row) "" ($
"            <tr>"resource-row"</tr>
"           ))
        )
    ))
"`;
    return links;
}

"   (js-property-templates (current-node) #t)
"
"    (js-toggle-variant-functions (current-node))
"function save() {
    let the"(name)" = { resourceType: '"resource-name"'};
    get"resource-name"Properties(the"(name)", document.getElementById('"(id (current-node))"'));
    if (the"(name)".id)
    {
        fetch(`/"fhir"/"resource-name"/${the"(name)".id}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7'
            },
            body: JSON.stringify(the"(name)")
        })
        .then((response) => {
            if (!response.ok) {
                return response.text().then((data) => {
                    try {
                        var parser = new DOMParser();
                        var newDocument = parser.parseFromString(data, 'text/html');
                        var jsonEl = newDocument.getElementById('json');
                        var oo = JSON.parse(jsonEl.childNodes[0].textContent.replace('-/->', '-->'));
                        alert(oo.issue.map(i => i.diagnostics).join('\\n'));
                    } catch(e) {
                        alert('Save failed: ' + response.status + ' ' + response.statusText);
                    }
                });
            }
            window.location.assign(`/"fhir"/"resource-name"/${the"(name)".id}/_history/${parseInt(the"(name)".meta.versionId) + 1}`)
        })
        .catch((error) => {
            console.error('Error:', error);
        });
    }
    else
    {
        if (the"(name)".meta)
            the"(name)".meta = null;
        fetch(`/"fhir"/"resource-name"`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7'
            },
            body: JSON.stringify(the"(name)")
        })
        .then((response) => {
            if (!response.ok) {
                return response.text().then((data) => {
                    try {
                        var parser = new DOMParser();
                        var newDocument = parser.parseFromString(data, 'text/html');
                        var jsonEl = newDocument.getElementById('json');
                        var oo = JSON.parse(jsonEl.childNodes[0].textContent.replace('-/->', '-->'));
                        alert(oo.issue.map(i => i.diagnostics).join('\\n'));
                    } catch(e) {
                        alert('Save failed: ' + response.status + ' ' + response.statusText);
                    }
                });
            }
            return response.text().then((data) => {
                let parser = new DOMParser();
                let newDocument = parser.parseFromString(data, 'text/html');
                let jsonElement = newDocument.getElementById('json')
                let element = JSON.parse(jsonElement.childNodes[0].textContent)
                window.location.assign(`/"fhir"/"resource-name"/${element.id}/_history/1`)
            });
        })
        .catch((error) => {
            console.error('Error:', error);
        });
    }
}

function New"resource-name"() {
        let the"(name)" = document.getElementById('"(downcase-string resource-variable)"');
        if (the"(name)") {
            // Called from detail page - replace existing form
            let parent = the"(name)".parentElement;
            parent.removeChild(the"(name)");
            table = document.createElement('table');
            parent.appendChild(table);
            "(downcase-string resource-variable)"Template().then(html => { table.insertAdjacentHTML('afterbegin', html); });
        } else {
            // Called from bundle page - replace entire page body
            build"(id (current-node))"({ meta: { versionId: \"0\" } }).then((html) => {
                let bodyElement = document.body;
                bodyElement.innerHTML = '';
                let elementString = `
        <h1>"(name)"</h1>
"           (case (name)
                (("OperationOutcome") "")
                (else ($
"        <table><tbody>
          <tr>
            <td><input type='button' value='Save' onclick='save()'/></td>
            <td><input type='button' value='New' onclick='New"resource-name"()'/></td>
          </tr>
        </tbody></table>
"               ))
            )
"${html}`;
                bodyElement.insertAdjacentHTML('beforeend', elementString);
            });
        }
}

function rollback() {
    let the"(name)" = { resourceType: '"resource-name"'};
    get"resource-name"Properties(the"(name)", document.getElementById('"(id (current-node))"'));
    if (window.confirm('This "resource-name" version will be rolled back and cannot be recovered!')) {
        fetch(`/"fhir"/"resource-name"/${the"(name)".id}/_rollback/${parseInt(the"(name)".meta.versionId)}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7'
            }
        })
        .then((response) => response.text())
        .then((data) => {
            window.location.assign(`/"fhir"/"resource-name"/${the"(name)".id}/_history/${parseInt(the"(name)".meta.versionId) - 1}`)
        })
        .catch((error) => {
            console.error('Error:', error);
            var parser = new DOMParser();
            var oldDocument = window.document;
            var newDocument = parser.parseFromString(data, 'text/html');
            oldDocument.children[0].replaceWith(newDocument.children[0]);
        });
    }
}

function "(id (current-node))"Template() {
    const "(id (current-node))" = { meta: { versionId: \"0\" } };
    return build"(id (current-node))"("(id (current-node))").then(html => { return html; });
}

function get"resource-name"Properties("resource-variable"_, "resource-variable"Element) {
    for (let row of "resource-variable"Element.children)
    {
        let propertyName = row.children[0].textContent.trim();
        let tableDetail = row.children[1];
        let notNull = tableDetail.children.length == 0 || tableDetail.children[0].value == '-';
        switch (propertyName)
        {
"   (js-get-element-properties (current-node) #t)
"        }
    }
}

"
    (for-children-of (select-children "elements" (current-node)) (lambda (element) (js-element-code element #t)))
)))))


(define (js-property-templates element backbone?) ($
"
function build"(id element)"(element) {
    if (element == undefined)
        return Promise.resolve('');

"
    (for (node-list-filter (lambda (property) (and (string=? "reference" (name-of property)) (string=? "Reference" (name-of element)))) (children (select-children "properties" element))) (lambda (property) ($
"    let reference_ = '';
    if (element.reference != undefined) {
        let [resourceType, id] = element.reference.split('/');
        if (resourceType == \"Binary\") {
            reference_ = `<img src=\"${buildUri(element.reference)}\" alt=\"${element.reference}\"/>`;
        }
        else {
            reference_ = getResourceName(resourceType, id).then(name => {
                return `<a href='${buildUri(element.reference)}'>${name}</a>`;
            });
        }
    }
"   )))
    (for (node-list-filter (lambda (property) (and (not (and (string=? "reference" (name-of property)) (string=? "Reference" (name-of element)))))) (children (select-children "properties" element))) (lambda (property) ($
"    let "(name-of property)"_ = "
        (let ((variant? (is-variant? property))) ($
           (if variant?
                ($
                    " '';
"                   (for (variants-of property) (lambda (variant) (let ((field-name (first-letter-downcase (model-item-name property variant variant?)))) ($
"    if (element."field-name" != undefined)
        "(name-of property)"_ = "
                        ($
                            (case (% "type" variant)
                                (("element") ($"build"(% "ref" variant)"(element."field-name")"  ))
                                (("code") ($ "buildString(element."field-name")"))
                                (("boolean") ($ "buildBoolean(element."field-name")"  ))
                                (("integer" "positiveInt" "unsignedInt" "decimal" "integer64") ($ "buildNumber(element."field-name")"))
                                (else ($ "buildString(element."field-name")"))
                            )
                        )
                        ";
"                   ))))
                )
                (for (variants-of property) (lambda (variant) ($
                    "element."(name-of property)" != undefined ? "
                    (case (% "type" variant)
                        (("element") (let ((element (ref variant)))
                            (if (true? "iscollection" property)
                                ($ "buildArray(element."(name-of property)", build"(% "ref" variant)")")
                                ($ "build"(% "ref" variant)"(element."(name-of property)")")
                            )
                        ))
                        (("code")
                            (if (true? "iscollection" property)
                                ($ 
                                    (if (% "ref" variant)
                                        ($ "buildSimpleArray(element."(name-of property)", build"(first-letter-downcase (name-of (ref variant)))")")
                                        ($ "buildSimpleArray(element."(name-of property)", buildString)")
                                    )
                                )
                                ($ "buildString(element."(name-of property)")")
                            )
                        )
                        (("boolean")($ (if (true? "notnull" property) "" ($ "" ))"buildBoolean(element."(name-of property)")"))
                        (("xhtml")($ "element."(name-of property)))
                        (("url" "uri") ($ "`<a href='${buildUri(element."(name-of property)")}'>${element."(name-of property)"}</a>`"))
                        (("integer" "positiveInt" "unsignedInt" "integer64" "decimal")
                            (if (true? "iscollection" property)
                                ($ "buildSimpleArray(element."(name-of property)", buildNumber)")
                                ($ "buildNumber(element."(name-of property)")")
                            )
                        )
                        (else
                            (if (true? "iscollection" property)
                                ($ "buildSimpleArray(element."(name-of property)", buildString)")
                                (if (and (string=? "reference" (name-of property)) (string=? "Reference" (name-of element)))
                                    ($ "buildReference(element.reference)")
                                    ($ "buildString(element."(name-of property)")")
                                )
                            )
                        )
                    )
                    " : "(case (% "type" variant)
                            (("element") "''")
                            (("boolean") (if (true? "notnull" property) "buildBoolean(false)" "''"))
                            (("integer" "positiveInt" "unsignedInt" "integer64" "decimal") (if (true? "iscollection" property) "''" (if (true? "notnull" property) "buildNumber('')" "''")))
                            (else (if (true? "iscollection" property) "''" (if (true? "notnull" property) "buildString('')" "''")))
                        )";
"
                )))
            )
        ))
    )))
"
    return Promise.all(["(for-children-of (select-children "properties" element) (lambda (property) ($ (name-of property)"_, " )))"]).then(values => {
    let "(id element)" =
`<table class='grid'><tbody id="(id element)">"
    (for-children-of (select-children "properties" element) (lambda (property)
        (let ((variant? (is-variant? property))) ($
"      <tr>
        <td>"(name-of property)(if (true? "notnull" property) " <span style='color: red;'>*</span>" "")"</td>
"           (if variant?
                ($
"        <td>
          <select onchange='toggle"(name-of element)(first-letter-upcase (name-of property))"(event)'>
            <option value='-'>-</option>
"                   (for (variants-of property) (lambda (variant)
                        (let*
                            (
                                (suffix (variant-suffix property variant variant?))
                                (field-name ($ (name-of property) suffix))
                            )
                            ($
"            <option value='"suffix"'${element."field-name" != undefined ? ' selected' : ''}>"
                                (case (% "type" variant)
                                    (("element") (name-of (ref variant)))
                                    ((#f) "String")
                                    (else (% "type" variant))
                                )
"</option>
"                           )
                        )
                    ))
"          </select>
        </td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"
                )
                (for (variants-of property) (lambda (variant)
                    (case (% "type" variant)
                        (("element") (let ((element (ref variant)))
                            (if (true? "iscollection" property)
                                ($
"        <td><input type='button' onclick='add"(name-of element)"ArrayItem(event)' value='+'/></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                               )
                                ($
"        <td><input type='button' onclick='toggle"(name-of element)"(event)' value='${element."(name-of property)" != undefined ? '-' : '+'}'></input></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                               )
                            )
                        ))
                        (("code")
                            (if (true? "iscollection" property)
                                ($ 
                                    (if (% "ref" variant)
                                        ($
"        <td><input type='button' onclick='addObjectArrayItem(event, "(first-letter-downcase (name-of (ref variant)))"Template())' value='+'/></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                                       )
                                        ($
"        <td><input type='button' onclick='addStringArrayItem(event)' value='+'/></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                                       )
                                    )
                                )
                                (if (% "ref" variant)
                                    ($
"        <td>
          <select>
"                                       (if (true? "notnull" property) "" ($
"            <option value='-'>-</option>
"                                       ))
                                        (for (children (select-children "codes" (ref variant))) (lambda (code) (let ((value (case (% "value" code) (("<") "&lt;")(("<=") "&lt;=") (else (% "value" code)))))($
"            <option value='"value"'${element."(name-of property)" == '"value"' ? ' selected' : ''}>"value"</option>
"                                       ))))
"          </select>
        </td>
"                                   )
                                    ($
"        <td>"(if (true? "notnull" property) "" ($ "<input type='button' onclick='toggleString(event)' value='${element."(name-of property)" != undefined ? '-' : '+'}'/>"))"</td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                                   )
                                )
                            )
                        )
                        (("boolean")($
"        <td>"(if (true? "notnull" property) "" ($ "<input type='button' onclick='toggleBoolean(event)' value='${element."(name-of property)" != undefined ? '-' : '+'}'/>"))"</td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                       ))
                        (("xhtml")($
"        <td></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                       ))
                        (("url" "uri") ($
"        <td>
          <select onchange='toggleUriString(event)'>
            <option value='-'${element."(name-of property)" == undefined ? ' selected' : ''}>-</option>
            <option value='link'${element."(name-of property)" != undefined ? ' selected' : ''}>link</option>
            <option value='edit'>edit</option>
          </select>
        </td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                       ))
                        (("integer" "positiveInt" "unsignedInt" "decimal")
                            (if (true? "iscollection" property)
                                ($
"        <td><input type='button' onclick='addStringArrayItem(event)' value='+'/></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                               )
                                ($
"        <td>"(if (true? "notnull" property) "" ($ "<input type='button' onclick='toggleString(event)' value='${element."(name-of property)" != undefined ? '-' : '+'}'></input>"))"</td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                               )
                            )
                        )
                        (else
                            (if (true? "iscollection" property)
                                ($
"        <td><input type='button' onclick='addStringArrayItem(event)' value='+'/></td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                               )
                                (if (and (string=? "reference" (name-of property)) (string=? "Reference" (name-of element)))
                                    ($
"        <td>
          <select onchange='toggleReferenceString(event)'>
            <option value='-'>-</option>
            <option value='link' selected='true'>link</option>
            <option value='edit'>edit</option>
          </select>
        </td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                                   )
                                    ($
"        <td>"(if (true? "notnull" property) "" ($ "<input type='button' onclick='toggleString(event)' value='${element."(name-of property)" != undefined ? '-' : '+'}'></input>"))"</td>
        <td>${values["(number->string (- (child-number property) 1))"]}</td>
"                                   )
                                )
                            )
                        )
                    )
                ))
            )
"      </tr>
"
        ))
    ))
    (if (simplified? (current-node)) "" ($
"      <tr>
        <td>extension</td>
        <td><input type='button' onclick='addExtensionArrayItem(event)' value='+'/></td>
        <td>${element.extension != undefined ? buildSimpleArray(element.extension, buildextension) : ''}</td>
      </tr>
"       (if backbone? ($
"      <tr>
        <td>modifierExtension</td>
        <td><input type='button' onclick='addExtensionArrayItem(event)' value='+'/></td>
        <td>${element.modifierExtension != undefined ? buildSimpleArray(element.modifierExtension, buildextension) : ''}</td>
      </tr>
"       )"")
    ))
"</tbody></table>`;
    return "(id element)";
    });
}
"
))

(define (js-element-code element backbone?) ($
    (js-toggle-variant-functions element)
"function "(id element)"Template() {
    const element = {};
    return build"(id element)"(element).then(html => { return html; });
}
"   (js-property-templates element backbone?)

    (let* (
        (element-name (name-of element))
        (element-variable (first-letter-downcase element-name))
        ) ($
"
function toggle"element-name"(event) {
    "(id element)"Template().then(contents => { toggleObject(event, contents); });
}

function add"element-name"ArrayItem(event) {
    "(id element)"Template().then(contents => { addObjectArrayItem(event, contents); });
}

function get"element-name"(instance, propertyName, row) {
    if (row.children[2].children.length > 0) {
        let "element-variable"_ = {};
        get"element-name"Properties("element-variable"_, row.children[2].children[0].children[0]);
        instance[propertyName] = "element-variable"_;
    }
}

function get"element-name"Array(instance, propertyName, row) {
    let "element-variable"Array = [];
    let "element-variable"ArrayTableDetail = row.children[2];
    for (let "element-variable"ArrayTable of "element-variable"ArrayTableDetail.children) {
        let "element-variable"ArrayBody = "element-variable"ArrayTable.children[0];
        if ("element-variable"ArrayBody.children.length == 0)
            break;
        let childRow = "element-variable"ArrayBody.children[0];
        let "element-variable"TableDetail = childRow.children[1];
        let "element-variable"Table = "element-variable"TableDetail.children[0];
        let "element-variable"Body = "element-variable"Table.children[0];
        let "element-variable"_ = {};
        get"element-name"Properties("element-variable"_, "element-variable"Body);
        "element-variable"Array.push("element-variable"_);
    }

    if ("element-variable"Array.length > 0)
        instance[propertyName] = "element-variable"Array;
}

function get"element-name"Properties("element-variable"_, "element-variable"Element) {
    for (let row of "element-variable"Element.children) {
        let tableDetail = row.children[1];
        let notNull = tableDetail.children.length == 0 || tableDetail.children[0].value == '-';
        let propertyName = row.children[0].textContent.trim();
        switch (propertyName) {
"    (js-get-element-properties element backbone?)
"        }
    }
}

"
    (for-children-of (select-children "elements" element) (lambda (element) (js-element-code element backbone?)))
))))

(define (js-get-element-properties element backbone?)
    (let* (
        (element-name ($ (name-of element)"_"))
        (element-variable (first-letter-downcase element-name))
        ) ($
        (for-children-of (select-children "properties" element) (lambda (property)
        (let ((variant? (is-variant? property))) ($
"            case '"(name-of property)(if (true? "notnull" property) " *" "")"':
"           (if variant?
                ($
"                {
                    let selectCell = row.children[1];
                    let select = selectCell.children[0];
                    for (let option of select.children) {
                        if (option.selected)
                        {
                            switch (option.value)
                            {
"                   (for (variants-of property) (lambda (variant) (let ((suffix (variant-suffix property variant variant?))) ($
"                                case '"suffix"':
"                        (case (% "type" variant)
                            (("code")
                                (if (% "ref" variant)
                                    ($
"                                    getCode("element-variable", '"(name-of property)suffix"', row);
"                                   )
                                    ($
"                                    "element-variable"."(name-of property)suffix" = getString(row);
"                                   )
                                )
                            )
                            (("integer" "positiveInt" "unsignedInt") ($
"                                    "element-variable"."(name-of property)suffix" = getInteger(row);
"                           ))
                            (("element") ($
"                                    get"(name-of (ref variant))"("element-variable", '"(name-of property)suffix"', row);
"                           ))
                            (("boolean")($
"                                    "element-variable"."(name-of property)suffix" = getBoolean(row);
"                           ))
                            (("decimal")($
"                                    "element-variable"."(name-of property)suffix" = getDecimal(row);
"                           ))
                            (else ($
"                                    "element-variable"."(name-of property)suffix" = getString(row);
"                           ))
                        )
"                                    break;
"                   ))))
"                            }
                            break;
                        }
                    }
                }
"               )
                (for (variants-of property) (lambda (variant)
                    (case (% "type" variant)
                        (("code")
                            (if (% "ref" variant)
                                ($
"                getCode"(if (true? "iscollection" property) "Array" "")"("element-variable", '"(name-of property)"', row);
"                               )
                                (get-string-property element element-variable property)
                            )
                        )
                        (("integer" "positiveInt" "unsignedInt")
                            (if (true? "iscollection" property)
                                ($
"                getIntegerArray("element-variable", '"(name-of property)"', row);
"                               )
                                ($
                                    (if (true? "notnull" property) "" ($
"                if (notNull)
    "                               ))
"                "element-variable"."(name-of property)" = getInteger(row);
"                               )
                            ))
                        (("element") ($
"                get"(name-of (ref variant))(if (true? "iscollection" property) "Array" "")"("element-variable", '"(name-of property)"', row);
"                       ))
                        (("boolean")($
                            (if (true? "notnull" property) "" ($
"                if (notNull)
    "                       ))
"                "element-variable"."(name-of property)" = getBoolean(row);
"                       ))
                        (("decimal")
                            (if (true? "iscollection" property)
                                ($
"                getDecimalArray("element-variable", '"(name-of property)"', row);
"                               )
                                ($
                                    (if (true? "notnull" property) "" ($
"                if (notNull)
    "                               ))
"                "element-variable"."(name-of property)" = getDecimal(row);
"                               )
                            ))
                        (("xhtml") ($
"                "element-variable"."(name-of property)" = getXhtml(row);
"                       ))
                        (("uri" "url") ($
"                if (tableDetail.children[0].value == 'link')
                    "element-variable"."(name-of property)" = getUriLink(row);
                else if (tableDetail.children[0].value == 'edit')
                    "element-variable"."(name-of property)" = getString(row);
"                       ))
                        (else
                            (get-string-property element element-variable property)
                        )
                    )
                ))
            )
"                break;
"       ))
    ))
    (if (simplified? (current-node)) "" ($
"            case 'extension':
                getExtensionArray("element-variable", propertyName, row);
                break;
"       (if backbone? ($
"            case 'modifierExtension':
                getExtensionArray("element-variable", propertyName, row);
                break;
"       )"")
    ))
)))

(define (get-string-property element element-variable property)
    (if (true? "iscollection" property)
        ($
"                getStringArray("element-variable", '"(name-of property)"', row)
"       )
        ($
            (if (true? "notnull" property) "" ($
"                if ("(if (and (string=? "reference" (name-of property)) (string=? "Reference" (name-of element))) "tableDetail.children[0].value == 'link')
                    reference_.reference = getReferenceLink(row);
                else if (tableDetail.children[0].value == 'edit'" "notNull")")
    "                       ))
"                "element-variable"."(name-of property)" = getString(row);
"       )
    )
)

(define primitive-extension-types
    (list "Base64Binary" "Boolean" "Canonical" "Code" "Date" "DateTime" "Decimal" "Id" "Instant" "Integer" "Markdown" "Oid" "PositiveInt" "String" "Time" "UnsignedInt" "Uri" "Url" "Uuid")
)

(define complex-extension-types
    (list "Address" "Age" "Annotation" "Attachment" "CodeableConcept" "CodeableReference" "Coding" "ContactPoint" "Count" "Distance" "Duration" "HumanName" "Identifier" "Money" "Period" "Quantity" "Range" "Ratio"
          "RatioRange" "Reference" "SampledData" "Signature" "Timing" "ContactDetail" "Contributor" "DataRequirement" "Expression" "ParameterDefinition" "RelatedArtifact" "TriggerDefinition" "UsageContext" "Dosage")
)

(define (js-elements-file) (file "Elements.js" (let ((fhir (route-prefix))) ($
    (for (node-list-filter (lambda (element) (case (name-of element) (("Element" "ElementDefinition") #f) (else #t))) (children (select-children "elements" (parent (current-node))))) (lambda (element) (js-element-code element #f)))
    (if (not (true? "simplified" (parent (current-node)))) ($
"function extensionTemplate() {
    const element = {};
    return `
${buildextension(element)
}`;
}

function buildextension(element) {
    if (element == undefined)
        return '';

    let extension =
`    <table class='grid'><tbody>
      <tr>
        <td>url</td>
        <td></td>
        <td><input style='width:100%;' value='${element.url != undefined ? element.url : ''}'/></td>
      </tr>
      <tr>
        <td>value</td>
        <td>
          <select onchange='toggleExtension(event)'>
            <option value='-'>-</option>
"       (apply $ (map
            (lambda (type) ($
"            <option value='"type"'${element.value"type" != undefined ? ' selected' : ''}>"(first-letter-downcase type)"</option>
"           ))
            primitive-extension-types
        ))
        (apply $ (map
            (lambda (type) ($
"            <option value='"type"'${element.value"type" != undefined ? ' selected' : ''}>"type"</option>
"           ))
            complex-extension-types
        ))
"          </select>
        </td>
        <td>
"       (apply $ (map
            (lambda (type)
                (case type
                    (("Boolean") ($ "
    ${element.value"type" != undefined ? buildBoolean(element.value"type") : ''}"))
                    (("Integer" "PositiveInt" "UnsignedInt") ($ "
    ${element.value"type" != undefined ? buildNumber(element.value"type") : ''}"))
                    (else ($ "
    ${element.value"type" != undefined ? buildString(element.value"type") : ''}"))
                )
            )
            primitive-extension-types
        ))
        (apply $ (map
            (lambda (type) ($
                ($ "
    ${element.value"type" != undefined ? build"(downcase-string type)"(element.value"type") : ''}")
            ))
            complex-extension-types
        ))
"        </td>
      </tr>
    </tbody></table>
`;
    return extension;
}

function toggleExtension(event) {
    let select = event.target;
    let tableDetail = select.parentElement.parentElement.children[2];
    if (tableDetail.children.length > 0) {
        let tableDetailChild = tableDetail.children[0];
        tableDetail.removeChild(tableDetailChild);
    }
    switch (select.value) {
        case 'Base64Binary':
        case 'Canonical':
        case 'Code':
        case 'Date':
        case 'DateTime':
        case 'Decimal':
        case 'Id':
        case 'Instant':
        case 'Integer':
        case 'Markdown':
        case 'Oid':
        case 'PositiveInt':
        case 'String':
        case 'Time':
        case 'UnsignedInt':
        case 'Uri':
        case 'Url':
        case 'Uuid':
        {
            let input = document.createElement('input');
            input.setAttribute('style', 'width:100%;');
            tableDetail.appendChild(input);
            input.focus();
            break;
        }
        case 'Boolean': {
            let input = document.createElement('input');
            input.type = 'checkbox';
            tableDetail.appendChild(input);
            input.focus();
            break;
        }
"       (apply $ (map
            (lambda (type) ($
"        case '"type"': {
            let table = document.createElement('table');
            tableDetail.appendChild(table);
            table.insertAdjacentHTML('afterbegin', `<""tbody>${"(first-letter-downcase type)"Template()}<""/tbody>`);
            break;
        }
"           ))
            complex-extension-types
        ))
"   }
}

function addExtensionArrayItem(event) {
    addObjectArrayItem(event, extensionTemplate());
}

function getExtension(instance, propertyName, row) {
    if (row.children[2].children.length > 0) {
        let extension = {};
        getExtensionProperties(extension, row.children[2].children[0].children[0]);
        instance[propertyName] = extension;
    }
}

function getExtensionArray(instance, propertyName, row) {
    let extensionArray = [];
    let extensionArrayTableDetail = row.children[2];
    for (let extensionArrayTable of extensionArrayTableDetail.children) {
        let extensionArrayBody = extensionArrayTable.children[0];
        for (let childRow of extensionArrayBody.children) {
            let extensionTableDetail = childRow.children[1];
            let extensionTable = extensionTableDetail.children[0];
            let extensionBody = extensionTable.children[0];
            let extension = {};
            getExtensionProperties(extension, extensionBody);
            extensionArray.push(extension);
        }
    }
    if (extensionArray.length > 0)
        instance[propertyName] = extensionArray;
}

function getExtensionProperties(extension, extensionElement) {
    for (let row of extensionElement.children) {
        let tableDetail = row.children[1];
        let propertyName = row.children[0].textContent.trim();
        switch (propertyName) {
            case 'url':
                extension.url = getString(row);
                break;
            case 'value':
                {
                    let selectCell = row.children[1];
                    let select = selectCell.children[0];
                    for (let option of select.children) {
                        if (option.selected)
                        {
                            switch (option.value)
                            {
"        (apply $ (map
            (lambda (type) ($
"                                case '"type"':
                                    extension.value"type" = get"
                (case type
                    (("Boolean" "Integer" "Decimal") type)
                    (("PositiveInt" "UnsignedInt") "Integer")
                    (else "String")
                )
                                    "(row);
                                    break;
"           ))
            primitive-extension-types
        ))
        (apply $ (map
            (lambda (type) ($
"                                case '"type"':
                                    get"type"(extension, 'value"type"', row);
                                    break;
"           ))
            complex-extension-types
        ))
"                           }
                            break;
                        }
                    }
                }
                break;
        }
    }
}
"   )"")
"
function buildBoolean(b) {
    return `<input type='checkbox'${b ? ' checked' : ''}/>`;
}

function toggleBoolean(event) {
    let button = event.target;
    let tableDetail = button.parentElement.parentElement.children[2];
    let tableDetailChild = tableDetail.children[0];
    switch (button.value) {
        case '-':
            tableDetail.removeChild(tableDetailChild);
            button.value = '+';
            break;
        case '+': {
                let input = document.createElement('input');
                input.type = 'checkbox';
                tableDetail.appendChild(input);
                input.focus();
            }
            button.value = '-';
            break;
    }
}

function buildArray(a, fn) {
    let promises = a.map(s => fn(s).then(result => 
`<table><tbody>
    <tr><td><input type='button' value='-' onclick='removeArrayItem(event)'/></td>
    <td>${result}</td></tr></tbody></table>
`));
    return Promise.all(promises).then(results => results.join(''));
}

function buildSimpleArray(a, fn) {
    if (!Array.isArray(a)) a = [a];
    var ret = a.map((s) =>
`<table><tbody>
    <tr><td><input type='button' value='-' onclick='removeArrayItem(event)'/></td>
    <td>${fn(s)}</td></tr></tbody></table>
`).join('');
    return ret;
}

function buildString(s) {
    return `<input style='width:100%;' value = '${s.replaceAll('\\'', '&quot;')}'></input>`;
}

function buildNumber(s) {
    return `<input style='width:100%;' value = '${s}'></input>`;
}

function toggleString(event) {
    let button = event.target;
    let tableDetail = button.parentElement.parentElement.children[2];
    let tableDetailChild = tableDetail.children[0];
    switch (button.value) {
        case '-':
            tableDetail.removeChild(tableDetailChild);
            button.value = '+';
            break;
        case '+': {
                let input = document.createElement('input');
                tableDetail.appendChild(input);
                input.focus();
            }
            button.value = '-';
            break;
    }
}

function addStringArrayItem(event) {
    var input = event.target;
    var objectRow = input.parentElement.parentElement.children[2];
    let table = document.createElement('table');
    objectRow.appendChild(table);
    table.insertAdjacentHTML('afterbegin', `<""tbody><""tr><""td><""input type='button' value='-' onclick='removeArrayItem(event)'><""/input><""/td><""td><""input style='width:100%;'><""/input><""/td><""/tr><""/tbody>`);
    table.children[0].children[0].children[1].children[0].focus();
}

function toggleReferenceString(event) {
    let select = event.target;
    let tableDetail = select.parentElement.parentElement.children[2];
    let tableDetailChild = tableDetail.children[0];
    switch (select.value) {
        case '-':
            if (tableDetailChild)
                tableDetail.removeChild(tableDetailChild);
            break;
        case 'edit':
            if (tableDetailChild) {
                let hrefParts = tableDetailChild.href ? tableDetailChild.href.split('/') : tableDetailChild.src.split('/');
                let text = hrefParts.slice(-2).join('/');
                tableDetail.removeChild(tableDetailChild);
                let input = document.createElement('input');
                input.value = text;
                tableDetail.appendChild(input);
                input.focus();
            }
            else {
                let input = document.createElement('input');
                tableDetail.appendChild(input);
                input.focus();
            }

            break;
        case 'link':
            if (tableDetailChild) {
                let reference = tableDetailChild.value;
                tableDetail.removeChild(tableDetailChild);
                let [resourceType, id] = reference.split('/');
                if (resourceType == 'Binary') {
                    let img = document.createElement('img');                    
                    img.src = buildUri(reference);
                    img.alt = reference;
                    tableDetail.appendChild(img);
                }
                else {
                    let a = document.createElement('a');
                    getResourceName(resourceType, id).then(name => {
                        a.href = buildUri(reference);
                        a.text = name;
                        tableDetail.appendChild(a);
                    });
                }
            }
            break;
    }
}

function toggleUriString(event) {
    let select = event.target;
    let tableDetail = select.parentElement.parentElement.children[2];
    let tableDetailChild = tableDetail.children[0];
    switch (select.value) {
        case '-':
            if (tableDetailChild)
                tableDetail.removeChild(tableDetailChild);
            break;
        case 'edit':
            if (tableDetailChild) {
                let text = tableDetailChild.text;
                tableDetail.removeChild(tableDetailChild);
                let input = document.createElement('input');
                input.value = text;
                tableDetail.appendChild(input);
                input.focus();
            }
            else {
                let input = document.createElement('input');
                tableDetail.appendChild(input);
                input.focus();
            }

            break;
        case 'link':
            if (tableDetailChild) {
                let reference = tableDetailChild.value;
                tableDetail.removeChild(tableDetailChild);
                let a = document.createElement('a');
                a.href = buildUri(reference);
                a.text = reference;
                tableDetail.appendChild(a);
            }
            break;
    }
}

function buildUri(reference) {
    if (reference.startsWith('http'))
        return reference;
    else
        return '/"fhir"/' + reference;
}

function toggleObject(event, contents) {
    var input = event.target;
    var objectRow = input.parentElement.parentElement.children[2];
    if (input.value == '-')
    {
        let table = objectRow.children[0];
        objectRow.removeChild(table);
        input.value = '+';
    }
    else
    {
        let table = document.createElement('table');
        objectRow.appendChild(table);
        table.insertAdjacentHTML('afterbegin', contents);
        input.value = '-';
    }
}

function addObjectArrayItem(event, contents) {
    var input = event.target;
    var objectRow = input.parentElement.parentElement.children[2];
    let table = document.createElement('table');
    objectRow.appendChild(table);
    table.insertAdjacentHTML('afterbegin', `<""tbody><""tr><""td><""input type='button' value='-' onclick='removeArrayItem(event)'><""/input><""/td><""td>${contents}<""/td><""/tr><""/tbody>`);
}

function removeArrayItem(event) {
    var input = event.target;
    var objectRow = input.parentElement.parentElement.parentElement.parentElement;
    objectRow.parentElement.removeChild(objectRow);
}

function getStringArray(instance, propertyName, row)
{
    let stringArray = [];
    let stringArrayTableDetail = row.children[2];
    for (let stringArrayTable of stringArrayTableDetail.children) {
        let stringArrayBody = stringArrayTable.children[0];
        if (stringArrayBody.children.length == 0)
            break;
        let childRow = stringArrayBody.children[0];
        let stringTableDetail = childRow.children[1];
        let stringInput = stringTableDetail.children[0];
        stringArray.push(stringInput.value)
    }
    if (stringArray.length > 0)
        instance[propertyName] = stringArray;
}

function getIntegerArray(instance, propertyName, row)
{
    let intArray = [];
    let intArrayTableDetail = row.children[2];
    for (let intArrayTable of intArrayTableDetail.children) {
        let intArrayBody = intArrayTable.children[0];
        if (intArrayBody.children.length == 0)
            break;
        let childRow = intArrayBody.children[0];
        let intTableDetail = childRow.children[1];
        let intInput = intTableDetail.children[0];
        intArray.push(parseInt(intInput.value))
    }
    if (intArray.length > 0)
        instance[propertyName] = intArray;
}

function getDecimalArray(instance, propertyName, row)
{
    let decArray = [];
    let decArrayTableDetail = row.children[2];
    for (let decArrayTable of decArrayTableDetail.children) {
        let decArrayBody = decArrayTable.children[0];
        if (decArrayBody.children.length == 0)
            break;
        let childRow = decArrayBody.children[0];
        let decTableDetail = childRow.children[1];
        let decInput = decTableDetail.children[0];
        decArray.push(parseFloat(decInput.value))
    }
    if (decArray.length > 0)
        instance[propertyName] = decArray;
}

function getString(row) {
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    return tableDetail.children[0].value;
}

function getReferenceLink(row) {
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    let tableDetailChild = tableDetail.children[0];
    if (tableDetailChild.tagName === 'A') {
        let hrefParts = tableDetailChild.href.split('/');
        let text = hrefParts.slice(-2).join('/');
        return text;
    } else if (tableDetailChild.tagName === 'IMG') {
        let srcParts = tableDetailChild.src.split('/');
        let text = srcParts.slice(-2).join('/');
        return text;
    }
    return tableDetailChild.textContent || '';
}

function getUriLink(row)
{
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    return tableDetail.children[0].text;
}

function getXhtml(row) {
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    return tableDetail.innerHTML;
}

function getBoolean(row) {
    return row.children[2].children[0].checked;
}

function getCode(instance, propertyName, row)
{
    let selectCell = row.children[1];
    let select = selectCell.children[0];
    for (let option of select.children) {
        if (option.value == '-')
            continue;

        if (option.selected) {
            instance[propertyName] = option.value;
            break;
        }
    }
}

function getCodeArray(instance, propertyName, row)
{
    let codeArray = [];
    let codeArrayTableDetail = row.children[2];
    for (let codeArrayTable of codeArrayTableDetail.children) {
        let codeArrayBody = codeArrayTable.children[0];
        if (codeArrayBody.children.length == 0)
            break;
        let childRow = codeArrayBody.children[0];
        let codeTableDetail = childRow.children[1];
        let select = codeTableDetail.children[0];
        for (let option of select.children) {
            if (option.selected) {
                codeArray.push(option.value);
                break;
            }
        }
    }
    if (codeArray.length > 0)
        instance[propertyName] = codeArray;
}

function getInteger(row) {
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    return parseInt(tableDetail.children[0].value);
}

function getDecimal(row) {
    let tableDetail = row.children[2];
    if (tableDetail.children.length == 0)
        return tableDetail.textContent;
    return parseFloat(tableDetail.children[0].value);
}

function getResourceName(resourceType, id) {
    if (!id)
        return Promise.resolve('');

    switch (resourceType) {
"
    (let* ((all-resources (children (current-node)))
           (name-resources (node-list->list (node-list-filter
               (lambda (r) (and (active? r)
                   (not (node-list-empty?
                       (node-list-filter
                           (lambda (s) (and (string=? "name" (% "name" s))
                                           (string=? "string" (% "type" s))))
                           (children (select-children "searches" r)))))))
               all-resources)))
           (title-resources (node-list->list (node-list-filter
               (lambda (r) (and (active? r)
                   (node-list-empty?
                       (node-list-filter
                           (lambda (s) (and (string=? "name" (% "name" s))
                                           (string=? "string" (% "type" s))))
                           (children (select-children "searches" r))))
                   (not (node-list-empty?
                       (node-list-filter
                           (lambda (s) (and (string=? "title" (% "name" s))
                                           (string=? "string" (% "type" s))))
                           (children (select-children "searches" r)))))))
               all-resources))))
        ($
            (apply $ (map (lambda (r)
                (let ((rn (name-of r)))
                    ($"        case '" rn "':
            return fetch(`/" (route-prefix) "/" rn "/$autocomplete?_id=${id}&_autocomplete=name`)
                .then(r => r.json())
                .then(a => a.length > 0 ? a[0] : `" rn "/${id}`);
")))
                name-resources))
            (apply $ (map (lambda (r)
                (let ((rn (name-of r)))
                    ($"        case '" rn "':
            return fetch(`/" (route-prefix) "/" rn "/$autocomplete?_id=${id}&_autocomplete=title`)
                .then(r => r.json())
                .then(a => a.length > 0 ? a[0] : `" rn "/${id}`);
")))
                title-resources))))
"        default: return Promise.resolve(`${resourceType}/${id}`);
    }
}
"
; alle Codesets des Dokuments, nicht nur die globalen: Codesets, die innerhalb einer
; Resource deklariert sind, werden auch von anderen Resourcen referenziert (z.B.
; TherapeuticConcept.interventionThresholds.status auf TherapyProgressStatus)
(for (select-descendants "codeset" (tree-root (current-node))) (lambda (codeset) ($
"
function "(first-letter-downcase (name-of codeset))"Template() {
    return `
          <select>
"                   (for (children (select-children "codes" codeset)) (lambda (code) (let ((value (case (% "value" code) (("<") "&lt;")(("<=") "&lt;=") (else (% "value" code)))))($
"            <option value='"value"'>"value"</option>
"                   ))))
"          </select>
`;
}

function build"(first-letter-downcase (name-of codeset))"(code) {

    var select = 
`              <select>
"                                         (for (children (select-children "codes" codeset)) (lambda (code) (let ((value (case (% "value" code) (("<") "&lt;")(("<=") "&lt;=") (else (% "value" code)))))($
"            <option value='"value"'${code == '"value"' ? ' selected' : ''}>"value"</option>
"                                             ))))
"          </select>
`;
    return select;
}
"
)))
))))

(define (js-toggle-variant-functions element) 
    (for-children-of (select-children "properties" element) (lambda (property)
        (let ((variant? (is-variant? property))) 
            (if variant? ($
"function toggle"(name-of element)(first-letter-upcase (name-of property))"(event) {
    let select = event.target;
    let tableDetail = select.parentElement.parentElement.children[2];
    switch (select.value) {
        case '-': {
            let tableDetailChild = tableDetail.children[0];
            tableDetail.removeChild(tableDetailChild);
            break;
        }
"               (for (variants-of property) (lambda (variant) ($
"        case '"(variant-suffix property variant variant?)"': {
            if (tableDetail.children.length > 0) {
                let tableDetailChild = tableDetail.children[0];
                tableDetail.removeChild(tableDetailChild);
            }
"
                    (case (% "type" variant)
                        (("element") ($
"            let table = document.createElement('table');
            tableDetail.appendChild(table);
            "(% "ref" variant)"Template().then(html => { table.insertAdjacentHTML('afterbegin', `<""tbody>${html}<""/tbody>`); });
"                       ))
                        (("boolean") ($
"            let input = document.createElement('input');
            input.type = 'checkbox';
            tableDetail.appendChild(input);
            input.focus();
"                       ))
                        (else ($
"            let input = document.createElement('input');
            tableDetail.appendChild(input);
            input.focus();
"                       ))
                    )
"            break;
        }
"               )))
"    }
}

"           )"")
        )
    ))
)

]]>
