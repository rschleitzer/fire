(declare-flow-object-class element                  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element            "UNREGISTERED::James Clark//Flow Object Class::empty-element")
(declare-flow-object-class document-type            "UNREGISTERED::James Clark//Flow Object Class::document-type")
(declare-flow-object-class processing-instruction   "UNREGISTERED::James Clark//Flow Object Class::processing-instruction")
(declare-flow-object-class entity                   "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class entity-ref               "UNREGISTERED::James Clark//Flow Object Class::entity-ref")
(declare-flow-object-class formatting-instruction   "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(declare-characteristic preserve-sdata?             "UNREGISTERED::James Clark//Characteristic::preserve-sdata?" #f)

(define debug (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

(define-language latin1
    (toupper
        (#\a #\A) (#\b #\B) (#\c #\C) (#\d #\D) (#\e #\E) (#\f #\F) (#\g #\G) (#\h #\H) (#\i #\I) (#\j #\J)
        (#\k #\K) (#\l #\L) (#\m #\M) (#\n #\N) (#\o #\O) (#\p #\P) (#\q #\Q) (#\r #\R) (#\s #\S) (#\t #\T)
        (#\u #\U) (#\v #\V) (#\w #\W) (#\x #\X) (#\y #\Y) (#\z #\Z) (#\U-00E4 #\U-00C4) (#\U-00F6 #\U-00D6) (#\U-00FC #\U-00DC)
    )
    (tolower
        (#\A #\a) (#\B #\b) (#\C #\c) (#\D #\d) (#\E #\e) (#\F #\f) (#\G #\g) (#\H #\h) (#\I #\i) (#\J #\j)
        (#\K #\k) (#\L #\l) (#\M #\m) (#\N #\n) (#\O #\o) (#\P #\p) (#\Q #\q) (#\R #\r) (#\S #\s) (#\T #\t)
        (#\U #\u) (#\V #\v) (#\W #\w) (#\X #\x) (#\Y #\y) (#\Z #\z) (#\U-00C4 #\U-00E4) (#\U-00D6 #\U-00F6) (#\U-00DC #\U-00FC)
    )
)

(declare-default-language latin1)

(define (downcase-string s)
    (list->string
        (map char-downcase
            (string->list s))))

(define (upcase-string s)
    (list->string
        (map char-upcase
            (string->list s))))

(define (first-letter-downcase s)
    ($ (downcase-string (substring s 0 1)) (substring s 1 (string-length s)))
)

(define (first-letter-upcase s)
    ($ (upcase-string (substring s 0 1)) (substring s 1 (string-length s)))
)

(define (repl-substring? string target pos)
    (let* (
        (could-match (<= (+ pos (string-length target)) (string-length string)))
        (match (if could-match (substring string pos (+ pos (string-length target))) ""))
        )
        (and could-match (string=? match target))))

(define (repl-substring string target repl pos)
    (let ((matches (repl-substring? string target pos)))
        (if matches
            ($
                (substring string 0 pos)
                repl
                (substring string (+ pos (string-length target)) (string-length string))
            )
            string
        )))

(define (string-replace string target repl)
  (let loop ((str string) (pos 0))
    (if (>= pos (string-length str))
    str
    (loop (repl-substring str target repl pos) 
        (if (repl-substring? str target pos)
            (+ (string-length repl) pos)
            (+ 1 pos))))))

(define (string-remove c str)
    (apply $
        (map
            (lambda (x) (if (char=? x c) (string) (string x)))
            (string->list str)
        )
    )
)

(define (shorten str)
    (apply $
        (map
            (lambda (c) (case c ((#\a #\e #\i #\o #\u) (string)) (else (string c))))
            (string->list str)
        )
    )
)

(define $ string-append)

(define % attribute-string)

(define (file filename contents)
    (make entity system-id: filename (! contents)))

(define (! contents) (make formatting-instruction data: contents))

(define (name-of node) (% "name" node))

(define (name) (name-of (current-node)))

(define (select-children name node)
    (select-elements (children node) name))

(define (select-descendants name node)
    (select-elements (descendants node) name))

(define (for-children func)
    (for-children-of (current-node) func))

(define (for-children-of element func)
    (for (children element) func))

(define (for-selected-children element func)
    (for-selected-children-of (current-node) element func))

(define (for-selected-children-of node element func)
    (for (select-children element node ) func))

(define (for-selected-descendants element func)
    (for-selected-descendants-of (current-node) element func))

(define (for-selected-descendants-of node element func)
    (for (select-elements (descendants node) element ) func))

(define (for nl func)
    (apply $ (map func (node-list->list nl))))

(define (for-nodes nl func)
    (apply sosofo-append (map func (node-list->list nl))))

(define (for-list nl func)
    (apply append (map func (node-list->list nl))))

(define (for-files nl func)
    (apply sosofo-append (map func (node-list->list nl))))

(define (true? attribute node)
    (let ((attrstring (% attribute node))) (if attrstring (string=? "true" attrstring) #f)))

(define (false? attribute node)
    (let ((attrstring (% attribute node))) (if attrstring (string=? "false" attrstring) #f)))

(define (link node)
    (let ((linkattr (% "link" node))) (if linkattr (element-with-id linkattr) #f)))

(define (name-of-link node)
    (name-of (link node)))

(define (ref node)
    (element-with-id (% "ref" node)))

(define (ref-entity key)
    (case (gi key)
        (("key") (ref key))
        (("reference" "parent")
            (let 
                ((collattr (% (if (string=? "parent" (gi key)) "child" "collection") key)))
                (if collattr
                    (parent (parent (element-with-id collattr)))
                    (element-with-id (% "ref" (element-with-id (% "key" key)))))))
    )
)

(define complete-fhir? #f)

(define (active? resource) (or (true? "active" resource) (and complete-fhir? (not (or (not (name-of resource))(string=? "Resource" (name-of resource)))))))
(define (model? resource) (or (active? resource)(string=? "model" (% "active" resource)) complete-fhir?))
;(define (active? resource) (case (name-of resource) (("CodeableConcept" "CapabilityStatement" "OperationDefinition" "Patient" "SearchParameter" "CodeSystem" "ValueSet" "StructureDefinition" "CompartmentDefnition" "ChargeItem") #t) (else #f)))
;(define (active? resource) (case (name-of resource) (("Observation") #t) (else #f)))
;(define (model? resource) (active? resource))

(define (active-resources) (node-list-filter (lambda (resource) (active? resource)) (children (current-node))))
(define (model-resources) (node-list-filter (lambda (resource) (model? resource)) (children (current-node))))

(define (active-resources-with-searches) (node-list-filter (lambda (resource) (not (node-list-empty? (children (select-children "searches" resource)))))(active-resources)))
(define (active-resources-with-reference-searches) (node-list-filter (lambda (resource) (not (node-list-empty? (node-list-filter (lambda (search) (case (% "type" search) (("reference") #t) (else #f))) (children (select-children "searches" resource))))))(active-resources)))
(define (reference-searches-of resource) (node-list-filter (lambda (search) (case (% "type" search) (("reference") #t) (else #f))) (children (select-children "searches" resource))))

(define (for-active-resources func) (for (active-resources) func))
(define (for-model-resources func) (for (model-resources) func))

(define (fhir-name) (let ((fhir (name-of (tree-root (current-node)))))(if fhir fhir "Fhir")))

(define (dotnet-version) "9")
(define fhir-doc-root "https://hl7.org/fhir")

(define (sql item db) (case db ((PostgreSql Sqlite) ($ "\\\"" (truncate item) "\\\"")) ((SqlServer) ($ "[" (truncate item) "]"))))
(define (truncate name) (if (> (string-length name) 63) (shorten name) name))

(define (db-moniker db) (case db ((PostgreSql) "Postgres")((SqlServer) "Sql")((Sqlite) "Sqlite")))
(define (db-driver db) (case db ((PostgreSql) "Npgsql")((SqlServer) "Sql")((Sqlite) "Sqlite")))
(define (db-position parameter db) (case db ((PostgreSql) "")((SqlServer Sqlite) ($ "$\""parameter"\", "))))
(define (db-parmprefix db) (case db ((PostgreSql) "$")((SqlServer Sqlite) "@")))

(define (is-variant? property) (> (node-list-count (children (select-children "variants" property))) 1))

(define (variant-suffix property variant variant?) 
    (if variant?
        (case (% "type" variant)
            (("element") (name-of (ref variant)))
            ((#f) "String")
            (else (first-letter-upcase (% "type" variant)))
        )
        ""
    )
)

(define (escape-csharp-keyword identifier)
    ($ (case identifier (("event" "operator" "abstract" "class" "base" "params" "short" "for" "ref" "double" "case" "const" "object") "@") (else "")) identifier)
)

; Get the property node referenced by a search parameter
(define (search-property search)
    (let* ((paths-node (select-children "paths" search))
           (path-nodes (if (node-list-empty? paths-node)
                          (empty-node-list)
                          (select-children "path" (node-list-first paths-node))))
           (first-path (if (node-list-empty? path-nodes)
                          #f
                          (node-list-first path-nodes)))
           (parts-node (if first-path (select-children "parts" first-path) (empty-node-list)))
           (part-nodes (if (node-list-empty? parts-node)
                          (empty-node-list)
                          (select-children "part" (node-list-first parts-node))))
           (first-part (if (node-list-empty? part-nodes)
                          #f
                          (node-list-first part-nodes)))
           (part-ref (if first-part (% "ref" first-part) #f)))
      (if part-ref (element-with-id part-ref) #f)))

; Check if a search parameter references a collection property
(define (search-is-collection? search)
    (let ((property (search-property search)))
      (if property
          (true? "iscollection" property)
          #f)))

; Check if a search parameter references an Identifier datatype (use _value vs _code)
(define (search-is-identifier? search)
    (let ((property (search-property search)))
      (if property
          (let ((ref-attr (% "ref" property)))
            (if ref-attr
                (string=? "identifier" ref-attr)
                #f))
          #f)))

; Check if a property has choice type variants
(define (property-has-variants? property)
    (if property
        (let ((variants-node (select-children "variants" property)))
          (not (node-list-empty? variants-node)))
        #f))

; Get the variants from a property
(define (property-variants property)
    (if property
        (let ((variants-node (select-children "variants" property)))
          (if (not (node-list-empty? variants-node))
              (select-children "variant" (node-list-first variants-node))
              (empty-node-list)))
        (empty-node-list)))

; Check if a search parameter references a simple code type (not Coding/CodeableConcept)
(define (search-is-simple-code? search)
    (let ((property (search-property search)))
      (if property
          (string=? "code" (% "type" property))
          #f)))
