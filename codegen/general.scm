(define (rust-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml") "String")
    (("boolean") "bool")
    (("integer" "positiveInt" "unsignedInt") "i32")
    (("integer64") "i64")
    (("decimal") "f64")
    (("date") "NaiveDate")
    (("dateTime" "instant") ($ "DateTime<""Utc>"))
    (("time") "NaiveTime")
    (("base64Binary") ($ "Vec<""u8>"))
    (("oid" "uuid") "String")
    (else fhir-type)))

(define (postgres-type fhir-type)
  (case fhir-type
    (("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml" "oid" "uuid") "TEXT")
    (("boolean") "BOOLEAN")
    (("integer" "positiveInt" "unsignedInt") "INTEGER")
    (("integer64") "BIGINT")
    (("decimal") "DECIMAL")
    (("date") "DATE")
    (("dateTime" "instant") "TIMESTAMPTZ")
    (("time") "TIME")
    (("base64Binary") "BYTEA")
    (else "JSONB")))

(define (postgres-index-type search-type)
  (case search-type
    (("string" "uri" "reference") "gin")
    (("token") "gin")
    (("date" "number" "quantity") "btree")
    (("composite") "gin")
    (else "btree")))

(define (search-param-column-type search-type)
  (case search-type
    (("string" "uri") "TEXT")
    (("token") "TEXT")
    (("reference") "TEXT")
    (("date") "TIMESTAMPTZ")
    (("number") "DECIMAL")
    (("quantity") "JSONB")
    (("composite") "JSONB")
    (else "TEXT")))

(define (is-primitive? fhir-type)
  (member fhir-type
    '("string" "code" "uri" "url" "canonical" "id" "markdown" "xhtml"
      "boolean" "integer" "positiveInt" "unsignedInt" "integer64"
      "decimal" "date" "dateTime" "instant" "time"
      "base64Binary" "oid" "uuid")))

; Get SQL cast suffix for bind parameters (::type)
; Returns empty string for TEXT since it doesn't need casting
(define (sql-cast-suffix pg-type)
  (case pg-type
    (("DATE") "::date")
    (("BOOLEAN") "::boolean")
    (("TIMESTAMPTZ") "::timestamptz")
    (("INTEGER") "::integer")
    (("BIGINT") "::bigint")
    (("DECIMAL") "::decimal")
    (else "")))

; Get list of target resource types from a reference search parameter
; Returns empty list if no targets found
; search: search node
(define (search-target-resources search)
  (let* ((paths-node (select-children "paths" search))
         (first-paths (if (node-list-empty? paths-node)
                          #f
                          (node-list-first paths-node)))
         (path-nodes (if first-paths
                          (select-children "path" first-paths)
                          (empty-node-list)))
         (first-path (if (node-list-empty? path-nodes)
                         #f
                         (node-list-first path-nodes))))
    (if first-path
        (let ((targets-node (select-children "targets" first-path)))
          (if (node-list-empty? targets-node)
              '()
              (let* ((first-targets (node-list-first targets-node))
                     (target-nodes (select-children "target" first-targets)))
                (if (node-list-empty? target-nodes)
                    '()
                    (map (lambda (t) (% "resource" t))
                         (node-list->list target-nodes))))))
        '())))

; Get the specific HumanName field from a search parameter's path
; Returns one of: "all", "family", "given", "prefix", "suffix", "text"
; or #f if not a HumanName search
; Examples:
;   - name: patient.name -> "all"
;   - family: patient.name/humanname.family -> "family"
;   - given: patient.name/humanname.given -> "given"
(define (search-humanname-field search)
  (let* ((paths-node (select-children "paths" search))
         (first-paths (if (node-list-empty? paths-node)
                          #f
                          (node-list-first paths-node)))
         (path-nodes (if first-paths
                          (select-children "path" first-paths)
                          (empty-node-list)))
         (first-path (if (node-list-empty? path-nodes)
                         #f
                         (node-list-first path-nodes))))
    (if first-path
        (let* ((parts-node (select-children "parts" first-path))
               (first-parts (if (node-list-empty? parts-node)
                                #f
                                (node-list-first parts-node)))
               (part-nodes (if first-parts
                                (select-children "part" first-parts)
                                (empty-node-list)))
               (part-list (node-list->list part-nodes))
               (part-count (length part-list)))
          (cond
            ; 1 part pointing to HumanName -> search all fields
            ((and (= part-count 1)
                  (let ((ref (% "ref" (car part-list))))
                    (or (string-ci=? ref "patient.name")
                        (string-ci=? ref "practitioner.name")
                        (string-ci=? ref "relatedperson.name")
                        (string-ci=? ref "person.name"))))
             "all")
            ; 2 parts with second pointing to specific HumanName field
            ((= part-count 2)
             (let ((ref2 (% "ref" (cadr part-list))))
               (cond
                 ((string-ci=? ref2 "humanname.family") "family")
                 ((string-ci=? ref2 "humanname.given") "given")
                 ((string-ci=? ref2 "humanname.prefix") "prefix")
                 ((string-ci=? ref2 "humanname.suffix") "suffix")
                 ((string-ci=? ref2 "humanname.text") "text")
                 (else #f))))
            (else #f)))
        #f)))
