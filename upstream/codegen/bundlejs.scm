<![CDATA[
; --- bundle-elements: &_elements= query parameter for bundle search links ---

(define (bundle-elements resource)
  ($
    "&amp;_elements=id,meta"
    (case (name-of resource)
        (("Binary") ",contentType")
        (("Observation") ",status,code,effective,subject,device")
        (("Patient") ",identifier,name,gender,birthDate,address,active")
        (("Practitioner") ",identifier,name,address,active")
        (else "")
    )
  )
)

; --- Bundle.js ---

(define (bundle-js)
    (file "Bundle.js" ($
"function buildBundle() {
    var jsonElement = document.getElementById('json');
    if (jsonElement == null)
        return;
    var bundle = JSON.parse(jsonElement.childNodes[0].textContent.replace('-/->', '-->'));
    jsonElement.parentElement.removeChild(jsonElement);

    var body = document.body;
    body.innerHTML = '';

    var total = bundle.total ?? 0;
    body.insertAdjacentHTML('beforeend', `<p>The query returned ${total} resources.</p>`);

    body.insertAdjacentHTML('beforeend', `
    <table><tbody><tr>
      <td><input type='button' value='New' onclick='newResource()'></input></td>
      <td><input type='button' style='color: red' value='Delete selected' onclick='deleteSelected()'></input></td>
      <td><input type='button' style='color: red' value='Delete all' onclick='deleteAll()'></input></td>
    </tr></tbody></table>`);

    // Pagination links
    var linkHtml = '<table><tbody><tr>';
    var selfUrl = null;
    if (bundle.link) {
        for (var link of bundle.link) {
            if (link.relation == 'self') {
                selfUrl = link.url;
                var separator = selfUrl.indexOf('?') >= 0 ? '&' : '?';
                linkHtml += `<td><a href='${selfUrl}${separator}_format=json' download='Bundle.json'>JSON</a></td>`;
                linkHtml += `<td><a href='${selfUrl}${separator}_format=xml' download='Bundle.xml'>XML</a></td>`;
            }
            else if (link.relation == 'first')
                linkHtml += `<td><a href='${link.url}'>first</a></td>`;
            else if (link.relation == 'last')
                linkHtml += `<td><a href='${link.url}'>last</a></td>`;
            else if (link.relation == 'previous')
                linkHtml += `<td><a href='${link.url}'>previous</a></td>`;
            else if (link.relation == 'next')
                linkHtml += `<td><a href='${link.url}'>next</a></td>`;
        }
    }
    linkHtml += '</tr></tbody></table>';
    body.insertAdjacentHTML('beforeend', linkHtml);

    // Table header
    var headerColumnsString = '';
    var entries = bundle.entry ?? [];
    if (entries.length > 0) {
        switch (entries[0].resource.resourceType) {
"
    (for-active-resources (lambda (resource)
        (let ((rn (name-of resource)))
            ($"            case '"rn"':
"               (case rn
                    (("Binary") ($"                headerColumnsString = '<th>Typ</th><th>Bild</th>';
"                   ))
                    (("Observation") ($"                headerColumnsString = '<th>Status</th><th>Code</th><th>Time</th><th>Start</th><th>End</th><th>Subject</th><th>Device</th>';
"                   ))
                    (("Patient") ($"                headerColumnsString = '<th>Identifier</th><th>Given</th><th>Family</th><th>Gender</th><th>Birth</th><th>Address</th><th>Zip</th><th>City</th><th>Active</th>';
"                   ))
                    (("Practitioner") ($"                headerColumnsString = '<th>Identifier</th><th>Family</th><th>Given</th><th>Address</th><th>Zip</th><th>City</th><th>Active</th>';
"                   ))
                    (else "")
                )
"                break;
"           )
        )
    ))
"        }
    }

    body.insertAdjacentHTML('beforeend', `
        <table class='grid'>
          <thead>
            <tr id='header'>
              <th><input type='checkbox' onclick='toggleAll(event)' id='toggleAll'></input></th>
              <th>Type</th>
              <th>Id</th>
              <th>Version</th>
              <th>Last Updated</th>
              ${headerColumnsString}
            </tr>
          </thead>
          <tbody id='body'></tbody>
        </table>`);

    var tableBody = document.getElementById('body');
    entries.forEach((entry) => {
        var resource = entry.resource;
        var rowString = getMetaDataElements(resource);
        switch (resource.resourceType) {
"   (for-active-resources (lambda (resource)
        (let ((rn (name-of resource)))
            ($"            case '"rn"':
"               (case rn
                    (("Binary") ($
"                rowString += `
    <td>${resource.contentType ?? \"\"}</td>
    <td>${resource.contentType?.startsWith(\"image/\") ? \"<img src='/" (route-prefix) "/Binary/\" + resource.id + \"' style='max-height:60px;max-width:80px;'>\" : \"\"}</td>
`;
"))
                    (("Observation") ($
"                rowString += `
    <td>${resource.status ?? \"\"}</td>
    <td>${resource.code?.coding?.at(0).code ?? \"\"}</td>
    <td>${resource.effectiveDateTime ?? \"\"}</td>
    <td>${resource.effectivePeriod?.start ?? \"\"}</td>
    <td>${resource.effectivePeriod?.end ?? \"\"}</td>
    <td><a href=\"/" (route-prefix) "/${resource.subject?.reference ?? \"\"}\">${resource.subject?.reference ?? \"\"}</a></td>
    <td><a href=\"/" (route-prefix) "/${resource.device?.reference ?? \"\"}\">${resource.device?.reference ?? \"\"}</a></td>
`;
"                    ))
                    (("Patient") ($
"                rowString += `
    <td>${resource.identifier?.at(0).value ?? \"\"}</td>
    <td>${resource.name?.at(0).given ?? \"\"}</td>
    <td>${resource.name?.at(0).family ?? \"\"}</td>
    <td>${resource.gender ?? \"\"}</td>
    <td>${resource.birthDate ?? \"\"}</td>
    <td>${resource.address?.at(0).line?.at(0) ?? \"\"}</td>
    <td>${resource.address?.at(0).postalCode ?? \"\"}</td>
    <td>${resource.address?.at(0).city ?? \"\"}</td>
    <td>${resource.active ?? \"\"}</td>
`;
"                    ))
                    (("Practitioner") ($
"                rowString += `
    <td>${resource.identifier?.at(0).value ?? \"\"}</td>
    <td>${resource.name?.at(0).family ?? \"\"}</td>
    <td>${resource.name?.at(0).given ?? \"\"}</td>
    <td>${resource.address?.at(0).line?.at(0) ?? \"\"}</td>
    <td>${resource.address?.at(0).postalCode ?? \"\"}</td>
    <td>${resource.address?.at(0).city ?? \"\"}</td>
    <td>${resource.active ?? \"\"}</td>
`;
"                    ))
                    (else "")
                )
"                break;
"           )
        )
    ))
"        }
        rowString += '</tr>';
        tableBody.insertAdjacentHTML('beforeend', rowString);
    });
}

function getMetaDataElements(resource) {
    var entryColumnsString = `<tr>
    <td><input type=\"checkbox\" id=\"${resource.resourceType}/${resource.id}\"></input>
    </td><td><a href=\"${getHistoryHref(resource)}\">${resource.resourceType}</a></td>
    <td>${resource.id}</td>
    <td>${resource.meta?.versionId ?? ''}</td>
    <td>${resource.meta?.lastUpdated ?? ''}</td>`;
    return entryColumnsString;
}

function getHistoryHref(resource) {
    var pathParts = window.location.pathname.split('/');
    if (pathParts[pathParts.length - 1] === '_history') {
        return `_history/${resource.meta.versionId}`;
    }
    return `${resource.resourceType}/${resource.id}/_history/${resource.meta.versionId}`;
}

function deleteSelected() {
    if (window.confirm('All selected resources will be deleted!')) {
        var body = document.getElementById('body');
        var ids = \"\";
        var resourceName = \"\";
        for (var row of body.children) {
            var td = row.children[0];
            var input = td.children[0];
            if (input.checked) {
                var inputId = input.id;
                var idParts = inputId.split('/');
                if (resourceName == \"\")
                    resourceName = idParts[0];
                else
                    ids += \",\";
                ids += idParts[1];
            }
        }

        if (ids.length > 0) {
            fetch(`/" (route-prefix) "/${resourceName}?_id=${ids}`, {
                method: 'DELETE',
                headers: {
                    'Content-Type': 'application/json',
                    'Accept': 'text/html'
                },
                body: ''
            })
            .then((response) => response.text())
            .then(() => { window.location.reload(); })
            .catch((error) => { console.error('Error:', error); });
        }
    }
}

function deleteAll() {
    if (window.confirm('All resources of this search result will be deleted!')) {
        fetch(window.location.href, {
            method: 'DELETE',
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'text/html'
            },
            body: ''
        })
        .then((response) => response.text())
        .then(() => { window.location.reload(); })
        .catch((error) => { console.error('Error:', error); });
    }
}

function toggleAll(event) {
    var checkBox = event.target;
    var state = checkBox.checked;
    var body = document.getElementById('body');
    for (var row of body.children) {
        var td = row.children[0];
        var input = td.children[0];
        input.checked = state;
    }
}

function newResource() {
    var pathParts = window.location.pathname.split('/').filter(part => part !== '');
    var fhirIndex = pathParts.indexOf('" (route-prefix) "');
    if (fhirIndex === -1 || fhirIndex + 1 >= pathParts.length) {
        alert('Cannot determine resource type from URL');
        return;
    }
    var resourceType = pathParts[fhirIndex + 1];
    var functionName = 'New' + resourceType;
    if (typeof window[functionName] === 'function') {
        window[functionName]();
        return;
    }
    var script = document.createElement('script');
    script.src = '/js/' + resourceType + '.js';
    script.onload = function() {
        if (typeof window[functionName] === 'function') {
            window[functionName]();
        } else {
            alert('Creating new ' + resourceType + ' resources is not yet supported');
        }
    };
    script.onerror = function() {
        alert('Creating new ' + resourceType + ' resources is not yet supported');
    };
    document.head.appendChild(script);
}
")))

(define (operationoutcome-js)
    (file "OperationOutcome.js" ($
"function buildResource(){
    var jsonElement = document.getElementById('json')
    var element = JSON.parse(jsonElement.childNodes[0].textContent.replace('-/->', '-->'))
    jsonElement.parentElement.removeChild(jsonElement)
    let elementString =
`        <h1>OperationOutcome</h1>
${buildoperationoutcome(element)
}        <table>
`;
    document.getElementById('body').insertAdjacentHTML('beforeend', elementString);
}

function buildoperationoutcome(element) {
    if (element == undefined)
        return '';

    let operationoutcome =
`${element.issue != undefined ? buildoperationoutcomeissue(element.issue[0]) : ''}`;
    return operationoutcome;
}

function buildoperationoutcomeissue(element) {
    if (element == undefined)
        return '';

    let operationoutcomeissue =
`    <p>
      <b>severity: </b>
      ${element.severity != undefined ? element.severity : ''}
    </p>
    <p>
      <b>code: </b>
      ${element.code != undefined ? element.code : ''}
    </p>
    <p>
      <b>diagnostics: </b>
      <br/>
      ${element.diagnostics != undefined ? element.diagnostics : ''}
    </p>
`;
    return operationoutcomeissue;
}
")))
]]>
