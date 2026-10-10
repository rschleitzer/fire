; search.scm -- what goes into which search table, for server/fire/Model.scaly:
; one line per table of a resource type, the fields separated by blanks:
;   kind table shape arguments
; kind is the table's (s string, t token, d date, r reference, n number,
; q quantity), shape names the way upstream's extractor for that table
; reads the resource -- chosen here as models.scm chooses it, with its own
; predicates --, and fire/Search.scaly has one routine per shape:
;   names F            every part of every name of the array F
;   each F p           of every item of the array F its member at p (a.b)
;   each2 F s p        of every item of the array s of every item of F, p
;   eachlist F p       of every item of F every string of its array p
;   address F          every address of F, its parts joined
;   list F             every string of the array F
;   value F            the string F
;   identifier F       system and value of every item of F
;   telecom F x        system and value of every item of F (x: of system x)
;   cc F               the codings of the concept F
;   cclist F           ... of every concept of the array F
;   ccpath F p         the array of codings at F.p
;   ccin F p           the codings of the concept p of every item of F
;   variant k:m ...    the first member k that is there, by m (b a boolean,
;                      t the word true, v its text; for a date d the value,
;                      p a period's start)
;   date F             the date F
;   period F           the period F, from its start to its end
;   first F p          the first date at p in the items of the array F
;   firstperiod F p    the first start of a period p in the items of F
;   path F p           the date at F.p
;   pathperiod F p     the start of the period at F.p
;   ref F              the reference F, or every one of the array F
;   refin F p          the reference p of every item of F
;   refin2 F s p       the reference p of every item of s of every item of F
;   number F           the number F, or every one of the array F
;   quantity p ...     every quantity the paths p (a.b) reach
;   none               nothing (upstream writes none either)
; A parameter that is a column of the resource's own table has a line too,
;   i table inline column kind
; where table is the name its table would have (the lines are found by it)
; and kind the column's (b boolean, t text, d instant, i int, l long,
; m decimal).

(define (scaly-part-prop part)
    (let ((r (% "ref" part)))
        (if r (let ((p (element-with-id r))) (if (node-list-empty? p) #f p)) #f)))

(define (scaly-part-name parts n fallback)
    (if (> (length parts) n)
        (let ((prop (scaly-part-prop (list-ref parts n))))
            (if prop (% "name" prop) fallback))
        fallback))

(define (scaly-joined names)
    (if (null? names) ""
        ($ (car names) (apply $ (map (lambda (n) ($ "." n)) (cdr names))))))

; The shape of a string search (model-string-section and
; repo-string-extractor-call); the lists are the resource's string searches
; that share one extractor.
(define (scaly-string-shape s hn-all addr-fields)
    (let ((parts (search-path-parts s))
          (sn (% "name" s)))
        (cond
            ((and (search-is-humanname? s) (equal? "all" (search-humanname-field s)))
                (let* ((named (filter (lambda (x) (string=? "name" (% "name" x))) hn-all))
                       (primary (if (null? named) (car hn-all) (car named))))
                    ($ "names " (search-fhir-field-name primary))))
            ((search-is-humanname? s)
                (let ((field (search-humanname-field s)))
                    ($ (if (or (string=? "given" field) (string=? "prefix" field) (string=? "suffix" field))
                           "eachlist " "each ")
                       (search-fhir-field-name s) " " field)))
            ((and (search-is-address? s) (= 1 (length parts)))
                ($ "address " (search-fhir-field-name s)))
            ((search-is-address? s)
                (let ((last (search-property-last s)))
                    ($ "each " (search-fhir-field-name (car addr-fields)) " " (if last (% "name" last) ""))))
            ((search-is-address-through-backbone? s) "none")
            ((> (length parts) 1)
                (let ((prop (search-property s))
                      (last (search-property-last s)))
                    ($ "each " (if prop (% "name" prop) sn) " "
                       (scaly-joined (append (map part-field-name (path-middle-parts parts))
                                             (list (if last (% "name" last) sn)))))))
            ((search-is-collection? s) ($ "list " (or (search-fhir-field-name s) sn)))
            (else ($ "value " (or (search-fhir-field-name s) sn))))))

; The shape of a token search (token-category and its extractors).
(define (scaly-token-shape s contactpoints)
    (let* ((parts (search-path-parts s))
           (sn (% "name" s))
           (category (token-category s))
           (first-prop (if (null? parts) #f (scaly-part-prop (car parts))))
           (first-field (if first-prop (% "name" first-prop) sn))
           (first-coll (if first-prop (true? "iscollection" first-prop) #f)))
        (cond
            ; repo-token-index-line asks for a ContactPoint before anything else
            ((search-is-contactpoint? s)
                ($ "telecom "
                   (search-fhir-field-name (if (null? contactpoints) s (car contactpoints))) " "
                   (if (string=? "telecom" sn) "-" sn)))
            ((string=? "identifier" category) ($ "identifier " (search-fhir-field-name s)))
            ((string=? "codeableconcept" category)
                (let* ((multi (> (length parts) 1))
                       (last-prop (if multi (scaly-part-prop (cadr parts)) #f))
                       (last-field (if last-prop (% "name" last-prop) sn))
                       (last-coding (if last-prop
                           (let ((r (% "ref" last-prop))) (if r (string=? "coding" r) #f)) #f)))
                    (cond
                        ((and (not multi) (not first-coll)) ($ "cc " first-field))
                        ((not multi) ($ "cclist " first-field))
                        ((and (not first-coll) last-coding) ($ "ccpath " first-field " " last-field))
                        (else ($ "ccin " first-field " " last-field)))))
            ((string=? "variant" category)
                (let ((prop (search-property s)))
                    ($ "variant"
                       (apply $ (map (lambda (v)
                           (let ((vtype (% "type" v)))
                               ($ " " (if prop (% "name" prop) sn) (first-letter-upcase vtype) ":"
                                  (case vtype (("boolean") "b") (("dateTime") "t") (else "v")))))
                           (node-list->list (if prop (property-variants prop) (empty-node-list))))))))
            ((string=? "code-thru-coll" category)
                (let* ((count (length parts))
                       (second-prop (if (> count 1) (scaly-part-prop (cadr parts)) #f))
                       (second-field (if second-prop (% "name" second-prop) sn))
                       (second-coll (if second-prop (true? "iscollection" second-prop) #f)))
                    (if (>= count 3)
                        (let ((third (scaly-part-name parts 2 sn)))
                            (if second-coll
                                ($ "each2 " (if first-prop first-field "") " " second-field " " third)
                                ($ "each " (if first-prop first-field "") " " second-field "." third)))
                        ($ "each " (if first-prop first-field "") " " second-field))))
            ((search-is-collection? s) ($ "list " (or (search-fhir-field-name s) sn)))
            (else ($ "value " (or (search-fhir-field-name s) sn))))))

; The shape of a date search (model-date-section).
(define (scaly-date-shape s)
    (let* ((parts (search-path-parts s))
           (sn (% "name" s))
           (prop (search-property s))
           (field (if prop (% "name" prop) sn)))
        (cond
            ((> (length parts) 1)
                (let* ((first-prop (scaly-part-prop (car parts)))
                       (first-field (if first-prop (% "name" first-prop) sn))
                       (first-coll (if first-prop (true? "iscollection" first-prop) #f))
                       (last-prop (scaly-part-prop (cadr parts)))
                       (last-field (if last-prop (% "name" last-prop) sn))
                       (last-ref (if last-prop (% "ref" last-prop) #f))
                       (period (and last-ref (string=? "period" last-ref))))
                    ($ (cond ((and first-coll period) "firstperiod ")
                             (first-coll "first ")
                             (period "pathperiod ")
                             (else "path "))
                       first-field " " last-field)))
            ((and prop (string=? "variant" (% "type" prop)))
                ($ "variant"
                   (apply $ (map (lambda (v)
                       (let* ((vtype (% "type" v))
                              (vref (% "ref" v))
                              (key ($ field (first-letter-upcase (if (string=? "element" vtype) vref vtype)))))
                           (cond
                               ((or (string=? "dateTime" vtype) (string=? "instant" vtype) (string=? "date" vtype))
                                   ($ " " key ":d"))
                               ((and (string=? "element" vtype) vref (string=? "period" vref))
                                   ($ " " key ":p"))
                               (else ""))))
                       (node-list->list (property-variants prop))))))
            ((and prop (let ((r (% "ref" prop))) (and r (string=? "period" r)))) ($ "period " field))
            (else ($ "date " field)))))

; The shape of a reference search (model-ref-section, repo-reference-index-line).
(define (scaly-reference-shape s)
    (let* ((parts (search-path-parts s))
           (sn (% "name" s))
           (prop (search-property s)))
        (cond
            ((<= (length parts) 1) ($ "ref " (if prop (% "name" prop) sn)))
            ((>= (length parts) 3)
                ($ "refin2 " (scaly-part-name parts 0 sn) " " (scaly-part-name parts 1 "") " " (scaly-part-name parts 2 "")))
            (else ($ "refin " (scaly-part-name parts 0 sn) " " (scaly-part-name parts 1 ""))))))

(define (scaly-number-shape s)
    (let ((prop (search-property s)))
        (if (> (length (search-path-parts s)) 1) "none"
            ($ "number " (if prop (% "name" prop) (% "name" s))))))

; The paths of a quantity search, each once (quantity-path-literals).
(define (scaly-quantity-shape s)
    (let* ((paths-node (select-children "paths" s))
           (paths (if (node-list-empty? paths-node) '()
                      (node-list->list (select-children "path" (node-list-first paths-node))))))
        ($ "quantity"
           (let loop ((ps paths) (seen '()))
               (if (null? ps) (apply $ (map (lambda (p) ($ " " p)) (reverse seen)))
                   (let ((path (scaly-joined (quantity-path-keys (car ps)))))
                       (loop (cdr ps) (if (member path seen) seen (cons path seen)))))))))

; The lines of a resource's search tables.
(define (scaly-search-lines resource)
    (let* ((searches (node-list->list (table-searches resource)))
           (of (lambda (type) (filter (lambda (s) (string=? type (% "type" s))) searches)))
           (strings (of "string"))
           (tokens (filter (lambda (s) (not (search-is-inline? s))) (of "token")))
           (dates (filter (lambda (s) (not (search-is-inline-instant? s))) (of "date")))
           (hn-all (filter (lambda (s) (and (search-is-humanname? s) (equal? "all" (search-humanname-field s)))) strings))
           (addr-fields (filter (lambda (s) (and (search-is-address? s) (> (length (search-path-parts s)) 1))) strings))
           (contactpoints (filter (lambda (s) (string=? "contactpoint" (token-category s))) tokens))
           (line (lambda (kind s shape) ($ kind " " (search-table-name s) " " shape "\\n"))))
        ($ (apply $ (map (lambda (s)
                (line "i" s ($ "inline " (% "name" s) " "
                    (let ((prop (search-property s)))
                        (case (if prop (% "type" prop) "string")
                            (("boolean") "b")
                            (("instant") "d")
                            (("integer" "positiveInt" "unsignedInt") "i")
                            (("integer64") "l")
                            (("decimal") "m")
                            (else "t"))))))
                (node-list->list (inline-searches resource))))
           (apply $ (map (lambda (s) (line "s" s (scaly-string-shape s hn-all addr-fields))) strings))
           (apply $ (map (lambda (s) (line "t" s (scaly-token-shape s contactpoints))) tokens))
           (apply $ (map (lambda (s) (line "d" s (scaly-date-shape s))) dates))
           (apply $ (map (lambda (s) (line "r" s (scaly-reference-shape s))) (of "reference")))
           (apply $ (map (lambda (s) (line "n" s (scaly-number-shape s))) (of "number")))
           (apply $ (map (lambda (s) (line "q" s (scaly-quantity-shape s))) (of "quantity"))))))

(define (scaly-searches resource) ($
"    if name.equals(\"" (name-of resource) "\")
        return \"" (scaly-search-lines resource) "\"
"))

; A resource's reference parameters, one a line: its name, the path of the
; reference in the resource (a.b) and the active types it may point to --
; what upstream's SearchHelper answers as GetReferencePropertyName and
; GetActiveTargetTypes (boilerplate.scm: search-dot-path,
; search-helper-target-case).
(define (scaly-dot-path parts)
    (if (null? parts) #f
        (let loop ((ps parts) (names '()))
            (if (null? ps) (scaly-joined (reverse names))
                (let ((prop (scaly-part-prop (car ps))))
                    (if prop (loop (cdr ps) (cons (% "name" prop) names)) #f))))))

(define (scaly-reference-line s active-names)
    (let* ((paths-node (select-children "paths" s))
           (path-nodes (if (node-list-empty? paths-node) (empty-node-list)
                          (select-children "path" (node-list-first paths-node))))
           (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
           (targets-node (if first-path (select-children "targets" first-path) (empty-node-list)))
           (targets (if (node-list-empty? targets-node) '()
                        (node-list->list (children (node-list-first targets-node)))))
           (path (scaly-dot-path (search-path-parts s))))
        ($ (% "name" s) " " (if path path "-")
           (apply $ (map (lambda (t)
               (if (member (% "resource" t) active-names) ($ " " (% "resource" t)) ""))
               targets))
           "\\n")))

(define (scaly-references resource active-names) ($
"    if name.equals(\"" (name-of resource) "\")
        return \""
    (apply $ (map (lambda (s) (scaly-reference-line s active-names))
        (node-list->list (reference-searches-of resource))))
"\"
"))
