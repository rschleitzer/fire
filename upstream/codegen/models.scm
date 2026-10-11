; Search generator - generates Fire/Search/{Resource}.cs
; Called from (element resource ...) context
; Groups extraction methods by pattern type, generates shared methods once

; --- Helpers ---

; Convert kebab-case to PascalCase: "address-city" -> "AddressCity"
(define (kebab-to-pascal str)
    (apply $ (map first-letter-upcase (string-split str #\-))))

; Scheme filter (not built-in in all implementations)
(define (filter pred lst)
    (cond
        ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

; Check if search references an Address type
(define (search-is-address? search)
    (let ((prop (search-property search)))
        (if prop
            (let ((ref-attr (% "ref" prop)))
                (if ref-attr (string=? "address" ref-attr) #f))
            #f)))

; Check if last property in path is Address type
(define (search-last-is-address? search)
    (let ((prop (search-property-last search)))
        (if prop
            (let ((ref-attr (% "ref" prop)))
                (if ref-attr (string=? "address" ref-attr) #f))
            #f)))

; Check if search navigates through a backbone to reach an Address
; (first property is NOT address, but some property in the path is)
(define (search-is-address-through-backbone? search)
    (and (not (search-is-address? search))
         (let ((parts (search-path-parts search)))
             (and (> (length parts) 1)
                  (let loop ((ps parts))
                      (cond ((null? ps) #f)
                            ((let* ((p (car ps))
                                    (r (% "ref" p))
                                    (prop (if r (element-with-id r) #f))
                                    (ref-attr (if prop (% "ref" prop) #f)))
                                (and ref-attr (string=? "address" ref-attr)))
                             #t)
                            (else (loop (cdr ps)))))))))

; Check if last property in path is CodeableConcept
(define (search-last-is-codeableconcept? search)
    (let ((prop (search-property-last search)))
        (if prop
            (let ((ref-attr (% "ref" prop)))
                (if ref-attr
                    (or (string=? "codeableconcept" ref-attr)
                        (string=? "coding" ref-attr))
                    #f))
            #f)))

; Get path parts for a search
(define (search-path-parts search)
    (let* ((paths-node (select-children "paths" search))
           (first-paths (if (node-list-empty? paths-node) #f (node-list-first paths-node)))
           (path-nodes (if first-paths (select-children "path" first-paths) (empty-node-list)))
           (first-path (if (node-list-empty? path-nodes) #f (node-list-first path-nodes)))
           (parts-node (if first-path (select-children "parts" first-path) (empty-node-list)))
           (first-parts (if (node-list-empty? parts-node) #f (node-list-first parts-node)))
           (part-nodes (if first-parts (select-children "part" first-parts) (empty-node-list))))
        (node-list->list part-nodes)))

; The parts between the first (the iterated array) and the last (the value).
; Provenance.agent.who.display has "who" here; two-part paths have none.
(define (path-middle-parts parts)
    (if (< (length parts) 3) '()
        (reverse (cdr (reverse (cdr parts))))))

(define (part-field-name part)
    (let ((ref (% "ref" part)))
        (if ref
            (let ((prop (element-with-id ref)))
                (if prop (% "name" prop) ""))
            "")))

; JSON accessors for the middle parts, e.g. ["who"]? - empty for two-part paths.
(define (path-middle-access parts)
    (apply $ (map (lambda (part) ($ "[\"" (part-field-name part) "\"]?")) (path-middle-parts parts))))

; -------------------------------------------------------------------
; A search on ONE type of a choice property: its expression narrows
; value[x] by ofType, which the model keeps as the cast of the path.
; String, token and reference searches of that kind are read by walking
; every path (model-cast-section), and so is a token search with several
; paths to a concept; the predicates are here because the repository asks
; them too.
; -------------------------------------------------------------------

(define (search-path-nodes search)
    (let ((paths-node (select-children "paths" search)))
        (if (node-list-empty? paths-node) '()
            (node-list->list (select-children "path" (node-list-first paths-node))))))

(define (path-part-nodes path)
    (let ((parts-node (select-children "parts" path)))
        (if (node-list-empty? parts-node) '()
            (node-list->list (select-children "part" (node-list-first parts-node))))))

(define (part-property part)
    (let ((ref (% "ref" part)))
        (if ref
            (let ((prop (element-with-id ref)))
                (if (and prop (not (node-list-empty? prop))) prop #f))
            #f)))

; The type a path is cast to, or #f.
(define (path-cast path)
    (let ((casts-node (select-children "casts" path)))
        (if (node-list-empty? casts-node) #f
            (let ((cast-nodes (select-children "cast" (node-list-first casts-node))))
                (if (node-list-empty? cast-nodes) #f
                    (% "to" (node-list-first cast-nodes)))))))

(define (path-is-cast? path)
    (and (path-cast path)
         (let loop ((ps (path-part-nodes path)))
             (cond ((null? ps) #f)
                   ((let ((prop (part-property (car ps))))
                        (and prop (property-has-variants? prop))) #t)
                   (else (loop (cdr ps)))))))

(define (search-is-cast? search)
    (or (and (member (% "type" search) '("string" "token" "reference"))
             (let loop ((ps (search-path-nodes search)))
                 (cond ((null? ps) #f)
                       ((path-is-cast? (car ps)) #t)
                       (else (loop (cdr ps))))))
        (search-has-concept-paths? search)
        (search-ends-in-concept-list? search)))

; A token search through a backbone LIST to a LIST of concepts or codings
; (Provenance.agent.role, Provenance.signature.type): walked as well --
; the extractor by category takes the inner list for one concept. (A list
; of codings below a single element, meta.security, it reads rightly.)
(define (search-ends-in-concept-list? search)
    (and (string=? "token" (% "type" search))
         (let* ((paths (search-path-nodes search))
                (parts (if (null? paths) '() (path-part-nodes (car paths))))
                (first-prop (if (null? parts) #f (part-property (car parts))))
                (last-prop (if (null? parts) #f (part-property (car (reverse parts))))))
             (and (> (length parts) 1)
                  first-prop
                  (true? "iscollection" first-prop)
                  last-prop
                  (true? "iscollection" last-prop)
                  (member (downcase-string (cast-path-leaf (car paths))) '("codeableconcept" "coding"))
                  #t))))

; A token search with SEVERAL paths, each ending at a CodeableConcept or a
; Coding (Observation.code | Observation.component.code): read the same
; way, because the extractors by category read the first path alone.
(define (search-has-concept-paths? search)
    (and (string=? "token" (% "type" search))
         (let loop ((ps (search-path-nodes search)) (seen '()))
             (cond ((null? ps) (> (length seen) 1))
                   ((not (member (downcase-string (cast-path-leaf (car ps))) '("codeableconcept" "coding"))) #f)
                   (else (let ((keys (cast-path-keys (car ps))))
                             (loop (cdr ps) (if (member keys seen) seen (cons keys seen)))))))))

; The JSON keys of a path, first to last; a choice property under its name
; for the type the path is cast to (value[x] as CodeableConcept:
; valueCodeableConcept).
(define (cast-path-keys path)
    (let ((cast (path-cast path)))
        (map (lambda (part)
                (let* ((prop (part-property part))
                       (nm (if prop (% "name" prop) "")))
                    (if (and prop cast (property-has-variants? prop))
                        ($ nm (first-letter-upcase cast))
                        nm)))
             (path-part-nodes path))))

; What stands at the end of a path: the type cast to when the path ends at
; the choice, else the last property's own.
(define (cast-path-leaf path)
    (let* ((parts (path-part-nodes path))
           (last-prop (if (null? parts) #f (part-property (car (reverse parts)))))
           (cast (path-cast path)))
        (cond
            ((and last-prop cast (property-has-variants? last-prop)) cast)
            ((and last-prop (% "ref" last-prop)) (% "ref" last-prop))
            ((and last-prop (% "type" last-prop)) (% "type" last-prop))
            (else "string"))))

; Get the last property in a multi-part path
(define (search-property-last search)
    (let ((parts (search-path-parts search)))
        (if (null? parts) #f
            (let ((last-part (car (reverse parts))))
                (let ((ref (% "ref" last-part)))
                    (if ref (element-with-id ref) #f))))))

; ===================================================================
; Main model function
; ===================================================================

(define (model)
    (let* ((resource (current-node))
           (rn (name-of resource))
           (has-history (history? resource))
           (inlines (inline-searches resource))
           (inline-list (node-list->list inlines))
           (searches-node (select-children "searches" resource))
           (all-searches (if (node-list-empty? searches-node) (empty-node-list)
                            (children (node-list-first searches-node)))))
        (file ($ rn ".cs")
            ($
"using System.Text.Json.Nodes;

namespace " (project-name) ".Search;

"
                (model-resource-record rn inline-list has-history)
                (if has-history (model-history-record rn) "")
                (model-search-extractor rn resource inline-list all-searches has-history)
            ))))

; --- Resource record ---

(define (model-resource-record rn inline-list has-history)
    ($"public record " rn "Resource(
    string id,
    int versionId,
    DateTime lastUpdated,
    byte[] content"
    (let loop ((items inline-list))
        (if (null? items) ""
            (let* ((s (car items))
                   (prop (search-property s))
                   (prop-type (if prop (% "type" prop) "code"))
                   (cs-type (csharp-type prop-type))
                   (col (search-query-camel s)))
                ($",
    " cs-type "? " col (loop (cdr items))))))
    ");

"))

; --- History record ---

(define (model-history-record rn)
    ($"public record " rn "HistoryRow(
    string id,
    int versionId,
    DateTime lastUpdated,
    byte[]? content,
    string historyOperation);

"))

; ===================================================================
; Search extractor class — groups methods by extraction pattern
; ===================================================================

(define (model-search-extractor rn resource inline-list all-searches has-history)
    (let* ((table-search-list (node-list->list (table-searches resource)))
           (cast-searches (filter (lambda (s) (and (search-is-cast? s) (not (search-is-inline? s)))) table-search-list))
           (string-searches (filter (lambda (s) (and (string=? "string" (% "type" s)) (not (search-is-cast? s)))) table-search-list))
           (token-searches (filter (lambda (s) (and (string=? "token" (% "type" s)) (not (search-is-inline? s)) (not (search-is-cast? s)))) table-search-list))
           (date-searches (filter (lambda (s) (and (string=? "date" (% "type" s)) (not (search-is-inline-instant? s)))) table-search-list))
           (ref-searches (filter (lambda (s) (and (string=? "reference" (% "type" s)) (not (search-is-cast? s)))) table-search-list))
           (number-searches (filter (lambda (s) (string=? "number" (% "type" s))) table-search-list))
           (quantity-searches (filter (lambda (s) (string=? "quantity" (% "type" s))) table-search-list)))
        ($"public static class " rn "SearchExtractor
{
" (model-inject-meta rn has-history)
    (model-extract-inline-params rn inline-list)
    (model-string-section rn string-searches)
    (model-token-section rn token-searches)
    (model-date-section rn date-searches)
    (model-ref-section rn ref-searches)
    (model-number-section rn number-searches)
    (model-quantity-section rn quantity-searches)
    (model-cast-section rn cast-searches)
    (model-helpers rn)
"}
")))

; --- InjectMeta ---

(define (model-inject-meta rn has-history)
    ($
"    public static void InjectMeta(JsonNode content, string id, int versionId, DateTime lastUpdated)
    {
        var obj = content.AsObject();
        obj[\"id\"] = id;
        var meta = obj[\"meta\"]?.AsObject();
        if (meta == null)
        {
            meta = new JsonObject();
            obj[\"meta\"] = meta;
        }
        meta[\"versionId\"] = versionId.ToString();
        meta[\"lastUpdated\"] = lastUpdated.ToString(\"yyyy-MM-ddTHH:mm:ss.fffzzz\");
    }

"))

; --- ExtractInlineParams ---

(define (model-extract-inline-params rn inline-list)
    (if (null? inline-list) ""
        (let ((single? (= 1 (length inline-list))))
            ($"    public static "
                (if single?
                    ; Single inline → direct return type
                    (let* ((s (car inline-list))
                           (prop (search-property s))
                           (prop-type (if prop (% "type" prop) "code"))
                           (cs-type (csharp-type prop-type)))
                        ($ cs-type "?"))
                    ; Multiple → tuple
                    ($"("
                        (let loop ((items inline-list) (first? #t))
                            (if (null? items) ""
                                (let* ((s (car items))
                                       (prop (search-property s))
                                       (prop-type (if prop (% "type" prop) "code"))
                                       (cs-type (csharp-type prop-type))
                                       (col (search-query-camel s)))
                                    ($ (if first? "" ", ") cs-type "? " col (loop (cdr items) #f)))))
                        ")"))
" ExtractInlineParams(JsonNode content)
    {
"
                (let loop ((items inline-list))
                    (if (null? items) ""
                        (let* ((s (car items))
                               (prop (search-property s))
                               (prop-type (if prop (% "type" prop) "code"))
                               (cs-type (csharp-type prop-type))
                               (col (search-query-camel s))
                               (json-name (if prop (% "name" prop) col)))
                            ; FHIR `instant` is the only date-ish type that gets inlined
                            ; (see search-is-inline-instant? in migration.scm). It's encoded as a
                            ; JSON string, so GetValue<""DateTime>() throws — read raw and parse.
                            ; The value handed to the database is the instant in UTC: a
                            ; DateTime of another kind goes out as a time without zone and
                            ; is stored as the server's wall clock read it (an hour off for
                            ; +01:00 on a machine in that zone).
                            (case prop-type
                                (("instant")
                                    ($"        var " col "Str = content[\"" json-name "\"]?.GetValue<""string>();
        DateTime? " col " = " col "Str != null ? DateTimeOffset.Parse(" col "Str, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.AssumeUniversal).UtcDateTime : null;
" (loop (cdr items))))
                                (else
                                    ($"        " cs-type "? " col " = content[\"" json-name "\"]?.GetValue<"cs-type">();
" (loop (cdr items))))))))
                "        return "
                (if single?
                    (search-query-camel (car inline-list))
                    ($"("
                        (let loop ((items inline-list) (first? #t))
                            (if (null? items) ""
                                ($ (if first? "" ", ") (search-query-camel (car items)) (loop (cdr items) #f))))
                        ")"))
                ";
    }

"))))

; ===================================================================
; STRING SECTION — ordered: HumanName-all, HumanName-fields,
;                           Address-all, Address-field (once), other
; ===================================================================

(define (model-string-section rn searches)
    (if (null? searches) ""
        (let* ((hn-all (filter (lambda (s)
                    (and (search-is-humanname? s)
                         (equal? "all" (search-humanname-field s))))
                    searches))
               (hn-fields (filter (lambda (s)
                    (and (search-is-humanname? s)
                         (let ((f (search-humanname-field s)))
                             (and f (not (equal? "all" f))))))
                    searches))
               (addr-all (filter (lambda (s)
                    (and (search-is-address? s)
                         (= 1 (length (search-path-parts s)))))
                    searches))
               (addr-fields (filter (lambda (s)
                    (and (search-is-address? s)
                         (> (length (search-path-parts s)) 1)))
                    searches))
               (addr-bb-all (filter (lambda (s)
                    (and (search-is-address-through-backbone? s)
                         (search-last-is-address? s)))
                    searches))
               (addr-bb-fields (filter (lambda (s)
                    (and (search-is-address-through-backbone? s)
                         (not (search-last-is-address? s))))
                    searches))
               (other (filter (lambda (s)
                    (and (not (search-is-humanname? s))
                         (not (search-is-address? s))
                         (not (search-is-address-through-backbone? s))))
                    searches)))
            ($"    // -- String search params --

"
                ; HumanName "all" — generate once, prefer "name" over "phonetic"
                (if (null? hn-all) ""
                    (let* ((name-search (filter (lambda (s) (string=? "name" (% "name" s))) hn-all))
                           (s (if (null? name-search) (car hn-all) (car name-search)))
                           (fhir-field (search-fhir-field-name s)))
                        (model-humanname-all-extract rn (% "name" s)
                            ($ "Extract" (kebab-to-pascal (% "name" s))) fhir-field)))
                ; HumanName fields — per unique field
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn)))
                           (field (search-humanname-field s))
                           (fhir-field (search-fhir-field-name s)))
                        (model-humanname-field-extract rn sn method-name fhir-field field)))
                    hn-fields))
                ; Address "all" — generate once
                (if (null? addr-all) ""
                    (let* ((s (car addr-all))
                           (fhir-field (search-fhir-field-name s)))
                        (model-address-all-extract rn (% "name" s)
                            ($ "Extract" (kebab-to-pascal (% "name" s))) fhir-field)))
                ; Address field — generate ExtractAddressField once
                (if (null? addr-fields) ""
                    (let* ((s (car addr-fields))
                           (fhir-field (search-fhir-field-name s)))
                        (model-address-field-extract rn fhir-field)))
                ; Address through backbone "all" — generate once
                (if (null? addr-bb-all) ""
                    (let* ((s (car addr-bb-all))
                           (parts (search-path-parts s))
                           (first-prop (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)))
                           (bb-field (if first-prop (% "name" first-prop) ""))
                           (last-prop (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)))
                           (addr-field (if last-prop (% "name" last-prop) "")))
                        (model-address-bb-all-extract rn (% "name" s)
                            ($ "Extract" (kebab-to-pascal (% "name" s))) bb-field addr-field)))
                ; Address through backbone field — generate once
                (if (null? addr-bb-fields) ""
                    (let* ((s (car addr-bb-fields))
                           (parts (search-path-parts s))
                           (first-prop (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)))
                           (bb-field (if first-prop (% "name" first-prop) ""))
                           (second-prop (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)))
                           (addr-field (if second-prop (% "name" second-prop) "")))
                        (model-address-bb-field-extract rn bb-field addr-field)))
                ; Other string searches
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn))))
                        (model-simple-string-extract rn sn method-name s)))
                    other))
            ))))

; ===================================================================
; TOKEN SECTION — ordered: Identifier, ContactPoint (once),
;                          CodeableConcept, code-thru-collection, variant, simple
; ===================================================================

; Classify a token search into one category (priority cascade).
; email/phone are handled by the telecom extractor — skip them.
(define (token-category s)
    (cond
        ((search-is-identifier? s) "identifier")
        ((search-is-contactpoint? s)
         (if (string=? "telecom" (% "name" s)) "contactpoint" "skip"))
        ((or (search-is-codeableconcept? s) (search-last-is-codeableconcept? s)) "codeableconcept")
        ((search-has-variants? s) "variant")
        ((> (length (search-path-parts s)) 1) "code-thru-coll")
        (else "simple")))

(define (model-token-section rn searches)
    (if (null? searches) ""
        (let* ((identifiers      (filter (lambda (s) (string=? "identifier"      (token-category s))) searches))
               (contactpoints    (filter (lambda (s) (string=? "contactpoint"    (token-category s))) searches))
               (codeableconcepts (filter (lambda (s) (string=? "codeableconcept" (token-category s))) searches))
               (code-thru-coll   (filter (lambda (s) (string=? "code-thru-coll"  (token-category s))) searches))
               (variants         (filter (lambda (s) (string=? "variant"         (token-category s))) searches))
               (simple-values    (filter (lambda (s) (string=? "simple"          (token-category s))) searches)))
            ($"    // -- Token search params --

"
                ; Identifier
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn)))
                           (fhir-field (search-fhir-field-name s)))
                        (model-identifier-extract rn sn method-name fhir-field)))
                    identifiers))
                ; ContactPoint — once
                (if (null? contactpoints) ""
                    (let* ((s (car contactpoints))
                           (fhir-field (search-fhir-field-name s)))
                        (model-contactpoint-extract rn "telecom"
                            ($ "Extract" (kebab-to-pascal "telecom")) fhir-field)))
                ; CodeableConcept
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn))))
                        (model-codeableconcept-extract rn sn method-name s)))
                    codeableconcepts))
                ; Code through collection
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn))))
                        (model-code-through-collection-extract rn sn method-name s)))
                    code-thru-coll))
                ; Variant
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn))))
                        (model-variant-token-extract rn sn method-name s)))
                    variants))
                ; Simple value
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn))))
                        (model-simple-token-extract rn sn method-name s)))
                    simple-values))
            ))))

; ===================================================================
; DATE SECTION
; ===================================================================

(define (model-date-section rn searches)
    (if (null? searches) ""
        ($"    // -- Date search params --

"
            (apply $ (map (lambda (s)
                (let* ((sn (% "name" s))
                       (method-name ($ "Extract" (kebab-to-pascal sn)))
                       (parts (search-path-parts s))
                       (prop (search-property s))
                       (fhir-field (if prop (% "name" prop) sn))
                       (is-variant (and prop (string=? "variant" (% "type" prop))))
                       (is-period (and prop (let ((r (% "ref" prop))) (and r (string=? "period" r))))))
                    (cond ((> (length parts) 1)
                           (model-date-backbone-extract rn sn method-name s))
                          (is-variant
                           (model-date-variant-extract rn sn method-name prop))
                          (is-period
                           (model-date-period-extract rn sn method-name fhir-field))
                          (else
                           (model-date-extract rn sn method-name fhir-field)))))
                searches)))))

; ===================================================================
; REFERENCE SECTION — ordered: direct (once), backbone (per-param)
; ===================================================================

(define (model-ref-section rn searches)
    (if (null? searches) ""
        (let* ((direct (filter (lambda (s) (<= (length (search-path-parts s)) 1)) searches))
               (backbone (filter (lambda (s) (> (length (search-path-parts s)) 1)) searches)))
            ($"    // -- Reference search params --

"
                ; Direct references — generate ExtractReference once
                (if (null? direct) ""
                    (model-direct-ref-extract rn))
                ; Backbone references — per-param
                (apply $ (map (lambda (s)
                    (let* ((sn (% "name" s))
                           (method-name ($ "Extract" (kebab-to-pascal sn)))
                           (parts (search-path-parts s))
                           (part-count (length parts))
                           (first-prop (if (not (null? parts))
                               (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)) #f))
                           (fhir-field (if first-prop (% "name" first-prop) sn))
                           (second-prop (if (> part-count 1)
                               (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)) #f))
                           (second-field (if second-prop (% "name" second-prop) "")))
                        (if (>= part-count 3)
                            (let* ((third-prop (let ((r (% "ref" (caddr parts)))) (if r (element-with-id r) #f)))
                                   (third-field (if third-prop (% "name" third-prop) "")))
                                (model-backbone-ref-extract-3 rn sn method-name fhir-field second-field third-field))
                            (model-backbone-ref-extract rn sn method-name fhir-field second-field))))
                    backbone))
            ))))

; ===================================================================
; EXTRACTION METHOD GENERATORS (CDATA blocks)
; ===================================================================

; --- HumanName all fields ---
(define (model-humanname-all-extract rn sn method-name fhir-field)
    ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var names = content[\""fhir-field"\"]?.AsArray();
        if (names == null) return results;
        foreach (var name in names)
        {
            if (name == null) continue;
            var text = name[\"text\"]?.GetValue<""string>();
            if (text != null) results.Add(text);
            var family = name[\"family\"]?.GetValue<""string>();
            if (family != null) results.Add(family);
            var given = name[\"given\"]?.AsArray();
            if (given != null)
                foreach (var g in given)
                    if (g != null) results.Add(g.GetValue<""string>());
            var prefix = name[\"prefix\"]?.AsArray();
            if (prefix != null)
                foreach (var p in prefix)
                    if (p != null) results.Add(p.GetValue<""string>());
            var suffix = name[\"suffix\"]?.AsArray();
            if (suffix != null)
                foreach (var s in suffix)
                    if (s != null) results.Add(s.GetValue<""string>());
        }
        return results;
    }

"))

; --- HumanName specific field ---
(define (model-humanname-field-extract rn sn method-name fhir-field field)
    (let ((is-array (or (string=? "given" field) (string=? "prefix" field) (string=? "suffix" field))))
        (if is-array
            ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var names = content[\""fhir-field"\"]?.AsArray();
        if (names == null) return results;
        foreach (var name in names)
        {
            var "field" = name?[\""field"\"]?.AsArray();
            if ("field" == null) continue;
            foreach (var g in "field")
                if (g != null) results.Add(g.GetValue<""string>());
        }
        return results;
    }

"           )
            ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var names = content[\""fhir-field"\"]?.AsArray();
        if (names == null) return results;
        foreach (var name in names)
        {
            var "field" = name?[\""field"\"]?.GetValue<""string>();
            if ("field" != null) results.Add("field");
        }
        return results;
    }

"           )
        )
    )
)

; --- Address all fields ---
(define (model-address-all-extract rn sn method-name fhir-field)
    ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var addresses = content[\""fhir-field"\"]?.AsArray();
        if (addresses == null) return results;
        foreach (var addr in addresses)
        {
            if (addr == null) continue;
            var parts = new List<""string>();
            var lines = addr[\"line\"]?.AsArray();
            if (lines != null)
                foreach (var l in lines)
                    if (l != null) parts.Add(l.GetValue<""string>());
            AddIfPresent(parts, addr, \"city\");
            AddIfPresent(parts, addr, \"state\");
            AddIfPresent(parts, addr, \"postalCode\");
            AddIfPresent(parts, addr, \"country\");
            if (parts.Count > 0)
                results.Add(string.Join(\" \", parts));
        }
        return results;
    }

"))

; --- Address field (generic, generated once) ---
(define (model-address-field-extract rn fhir-field)
    ($
"    public static List<""string> ExtractAddressField(JsonNode content, string fieldName)
    {
        var results = new List<""string>();
        var addresses = content[\""fhir-field"\"]?.AsArray();
        if (addresses == null) return results;
        foreach (var addr in addresses)
        {
            var val = addr?[fieldName]?.GetValue<""string>();
            if (val != null) results.Add(val);
        }
        return results;
    }

"))

; --- Address through backbone all fields (e.g. Organization contact.address) ---
(define (model-address-bb-all-extract rn sn method-name bb-field addr-field)
    ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var containers = content[\""bb-field"\"]?.AsArray();
        if (containers == null) return results;
        foreach (var container in containers)
        {
            if (container == null) continue;
            var addr = container[\""addr-field"\"];
            if (addr == null) continue;
            var parts = new List<""string>();
            var lines = addr[\"line\"]?.AsArray();
            if (lines != null)
                foreach (var l in lines)
                    if (l != null) parts.Add(l.GetValue<""string>());
            AddIfPresent(parts, addr, \"city\");
            AddIfPresent(parts, addr, \"state\");
            AddIfPresent(parts, addr, \"postalCode\");
            AddIfPresent(parts, addr, \"country\");
            if (parts.Count > 0)
                results.Add(string.Join(\" \", parts));
        }
        return results;
    }

"))

; --- Address through backbone field (e.g. Organization contact.address.city) ---
(define (model-address-bb-field-extract rn bb-field addr-field)
    ($
"    public static List<""string> ExtractAddressFieldThroughBackbone(JsonNode content, string fieldName)
    {
        var results = new List<""string>();
        var containers = content[\""bb-field"\"]?.AsArray();
        if (containers == null) return results;
        foreach (var container in containers)
        {
            var val = container?[\""addr-field"\"]?[fieldName]?.GetValue<""string>();
            if (val != null) results.Add(val);
        }
        return results;
    }

"))

; --- Simple string ---
; Handles: simple single-value, collection arrays, and multi-part backbone strings
(define (model-simple-string-extract rn sn method-name search)
    (let* ((parts (search-path-parts search))
           (is-multi-part (> (length parts) 1))
           (prop (search-property search))
           (fhir-field (if prop (% "name" prop) sn))
           (is-coll (search-is-collection? search)))
        (cond
            ; Multi-part: iterate backbone array, access nested field
            (is-multi-part
                (let* ((last-prop (search-property-last search))
                       (last-field (if last-prop (% "name" last-prop) sn))
                       (middle (path-middle-access parts)))
                    ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var items = content[\""fhir-field"\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            var val = item?"middle"[\""last-field"\"]?.GetValue<""string>();
            if (val != null) results.Add(val);
        }
        return results;
    }

")))
            ; Collection: iterate array of strings
            (is-coll
                ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var arr = content[\""fhir-field"\"]?.AsArray();
        if (arr != null)
            foreach (var item in arr)
            {
                var val = item?.GetValue<""string>();
                if (val != null) results.Add(val);
            }
        return results;
    }

"))
            ; Simple: single value
            (else
                ($
"    public static List<""string> "method-name"(JsonNode content)
    {
        var results = new List<""string>();
        var val = content[\""fhir-field"\"]?.GetValue<""string>();
        if (val != null) results.Add(val);
        return results;
    }

")))))

; --- Identifier ---
(define (model-identifier-extract rn sn method-name fhir-field)
    ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var identifiers = content[\""fhir-field"\"]?.AsArray();
        if (identifiers == null) return results;
        foreach (var id in identifiers)
        {
            if (id == null) continue;
            results.Add((id[\"system\"]?.GetValue<""string>(), id[\"value\"]?.GetValue<""string>()));
        }
        return results;
    }

"))

; --- ContactPoint (telecom with optional filter) ---
(define (model-contactpoint-extract rn sn method-name fhir-field)
    ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content, string? filterSystem = null)
    {
        var results = new List<""(string?, string?)>();
        var telecoms = content[\""fhir-field"\"]?.AsArray();
        if (telecoms == null) return results;
        foreach (var tc in telecoms)
        {
            if (tc == null) continue;
            var sys = tc[\"system\"]?.GetValue<""string>();
            if (filterSystem != null && sys != filterSystem) continue;
            results.Add((sys, tc[\"value\"]?.GetValue<""string>()));
        }
        return results;
    }

"))

; --- CodeableConcept ---
; Four cases:
;   1. Single-part, NOT collection (e.g. Observation.code) → direct CC access
;   2. Single-part, IS collection (e.g. Observation.category) → iterate array, each is CC
;   3. Multi-part, first NOT collection, last is Coding (e.g. meta.security) → object nav + Coding array
;   4. Multi-part (e.g. Patient communication.language) → iterate container, access nested CC
(define (model-codeableconcept-extract rn sn method-name search)
    (let* ((parts (search-path-parts search))
           (first-prop (if (not (null? parts))
               (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)) #f))
           (first-field (if first-prop (% "name" first-prop) sn))
           (is-collection (if first-prop (true? "iscollection" first-prop) #f))
           (is-multi-part (> (length parts) 1))
           (last-prop (if is-multi-part
               (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)) #f))
           (last-field (if last-prop (% "name" last-prop) sn))
           (last-is-coding (if last-prop
               (let ((ref-attr (% "ref" last-prop)))
                   (if ref-attr (string=? "coding" ref-attr) #f))
               #f)))
        (cond
            ; Case 1: single CC (not collection, single-part path)
            ((and (not is-multi-part) (not is-collection))
                ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var cc = content[\"" first-field "\"];
        if (cc == null) return results;
        var codings = cc[\"coding\"]?.AsArray();
        if (codings != null)
            foreach (var coding in codings)
                if (coding != null)
                    results.Add((coding[\"system\"]?.GetValue<""string>(), coding[\"code\"]?.GetValue<""string>()));
        return results;
    }

"))
            ; Case 2: collection of CCs (single-part, iscollection=true)
            ((and (not is-multi-part) is-collection)
                ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            if (item == null) continue;
            var codings = item[\"coding\"]?.AsArray();
            if (codings != null)
                foreach (var coding in codings)
                    if (coding != null)
                        results.Add((coding[\"system\"]?.GetValue<""string>(), coding[\"code\"]?.GetValue<""string>()));
        }
        return results;
    }

"))
            ; Case 3: multi-part, object nav + Coding array (e.g. meta.security)
            ((and is-multi-part (not is-collection) last-is-coding)
                ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var codings = content[\"" first-field "\"]?[\"" last-field "\"]?.AsArray();
        if (codings == null) return results;
        foreach (var coding in codings)
            if (coding != null)
                results.Add((coding[\"system\"]?.GetValue<""string>(), coding[\"code\"]?.GetValue<""string>()));
        return results;
    }

"))
            ; Case 4: multi-part backbone (e.g. communication.language)
            (else
                ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var comms = content[\"" first-field "\"]?.AsArray();
        if (comms == null) return results;
        foreach (var comm in comms)
        {
            var lang = comm?[\"" last-field "\"];
            if (lang == null) continue;
            var codings = lang[\"coding\"]?.AsArray();
            if (codings != null)
                foreach (var coding in codings)
                    if (coding != null)
                        results.Add((coding[\"system\"]?.GetValue<""string>(), coding[\"code\"]?.GetValue<""string>()));
        }
        return results;
    }

")))))

; --- Code through collection (e.g. address.use) ---
(define (model-code-through-collection-extract rn sn method-name search)
    (let* ((parts (search-path-parts search))
           (part-count (length parts))
           (first-prop (if (not (null? parts))
               (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)) #f))
           (first-field (if first-prop (% "name" first-prop) ""))
           (second-prop (if (> part-count 1)
               (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)) #f))
           (second-field (if second-prop (% "name" second-prop) sn))
           (second-is-coll (if second-prop (true? "iscollection" second-prop) #f)))
        (if (>= part-count 3)
            ; 3-part path: first[] → second (object or array) → third field
            (let* ((third-prop (let ((r (% "ref" (caddr parts)))) (if r (element-with-id r) #f)))
                   (third-field (if third-prop (% "name" third-prop) sn)))
                (if second-is-coll
                    ; Middle is collection: double iteration
                    ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            var subs = item?[\"" second-field "\"]?.AsArray();
            if (subs == null) continue;
            foreach (var sub in subs)
            {
                var val = sub?[\"" third-field "\"]?.GetValue<""string>();
                if (val != null) results.Add((null, val));
            }
        }
        return results;
    }

")
                    ; Middle is NOT collection: single iteration with nested access
                    ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            var val = item?[\"" second-field "\"]?[\"" third-field "\"]?.GetValue<""string>();
            if (val != null) results.Add((null, val));
        }
        return results;
    }

")))
            ; 2-part path: first[] → second field (existing behavior)
            ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            var val = item?[\"" second-field "\"]?.GetValue<""string>();
            if (val != null) results.Add((null, val));
        }
        return results;
    }

"))))

; --- Simple value token (plain string/code property, single-part path) ---
(define (model-simple-token-extract rn sn method-name search)
    (let* ((prop (search-property search))
           (fhir-field (if prop (% "name" prop) sn))
           (is-coll (search-is-collection? search)))
        (if is-coll
            ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?)>();
        var arr = content[\""fhir-field"\"]?.AsArray();
        if (arr == null) return results;
        foreach (var item in arr)
        {
            var v = item?.GetValue<""string>();
            if (v != null) results.Add((null, v));
        }
        return results;
    }

")
            ($
"    public static List<""(string? system, string? value)> "method-name"(JsonNode content)
    {
        var v = content[\""fhir-field"\"]?.GetValue<""string>();
        return v != null ? new List<""(string?, string?)> { (null, v) } : new List<""(string?, string?)>();
    }

"))))

; --- Variant token (e.g. deceased with boolean/dateTime variants) ---
(define (model-variant-token-extract rn sn method-name search)
    (let* ((prop (search-property search))
           (fhir-field (if prop (% "name" prop) sn))
           (variants (if prop (property-variants prop) (empty-node-list)))
           (variant-list (node-list->list variants)))
        ($
"    public static (string? system, string? value)? " method-name "(JsonNode content)
    {
"
        (let loop ((vlist variant-list))
            (if (null? vlist) ""
                (let* ((v (car vlist))
                       (vtype (% "type" v))
                       (suffix (first-letter-upcase vtype))
                       (json-prop ($ fhir-field suffix)))
                    ($
                        (case vtype
                            (("boolean")
                                ($"        if (content[\"" json-prop "\"] != null)
            return (null, content[\"" json-prop "\"]!.GetValue<""bool>().ToString().ToLowerInvariant());
"))
                            (("dateTime")
                                ($"        if (content[\"" json-prop "\"] != null)
            return (null, \"true\");
"))
                            (else ($"        if (content[\"" json-prop "\"] != null)
            return (null, content[\"" json-prop "\"]?.ToString());
")))
                        (loop (cdr vlist))))))
"        return null;
    }

")))

; --- Date extraction (variant, e.g. effective[x] → effectiveDateTime, effectivePeriod, effectiveInstant) ---
(define (model-date-variant-extract rn sn method-name prop)
    (let* ((fhir-field (% "name" prop))
           (variants (property-variants prop))
           (variant-list (node-list->list variants)))
        ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
"
        (let loop ((vlist variant-list))
            (if (null? vlist) ""
                (let* ((v (car vlist))
                       (vtype (% "type" v))
                       (vref (% "ref" v))
                       (suffix (first-letter-upcase (if (string=? "element" vtype) vref vtype)))
                       (json-prop ($ fhir-field suffix)))
                    ($
                        (cond
                            ((or (string=? "dateTime" vtype) (string=? "instant" vtype) (string=? "date" vtype))
                                ($"        var " (downcase-string json-prop) " = content[\"" json-prop "\"]?.GetValue<""string>();
        if (" (downcase-string json-prop) " != null) return FhirDateHelper.ParseFhirDateRange(" (downcase-string json-prop) ");
"))
                            ((and (string=? "element" vtype) vref (string=? "period" vref))
                                ; a Period: from its start to its end, an open side
                                ; unbounded, as the direct Period (model-date-period-extract)
                                ($"        var " (downcase-string json-prop) " = content[\"" json-prop "\"];
        if (" (downcase-string json-prop) " != null)
        {
            var start = " (downcase-string json-prop) "[\"start\"]?.GetValue<""string>();
            var end = " (downcase-string json-prop) "[\"end\"]?.GetValue<""string>();
            if (start != null || end != null)
                return (start != null ? FhirDateHelper.ParseFhirDateRange(start).low : DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc),
                        end != null ? FhirDateHelper.ParseFhirDateRange(end).high : DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc));
        }
"))
                            (else ""))
                        (loop (cdr vlist))))))
"        return null;
    }

")))

; --- Date extraction (simple single-part) ---
(define (model-date-extract rn sn method-name fhir-field)
    ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var bd = content[\""fhir-field"\"]?.GetValue<""string>();
        if (bd == null) return null;
        return FhirDateHelper.ParseFhirDateRange(bd);
    }

"))

; Direct (single-part) Period property: index start..end as the date range so a
; date search matches when the queried instant falls within the validity window.
(define (model-date-period-extract rn sn method-name fhir-field)
    ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var period = content[\""fhir-field"\"];
        if (period == null) return null;
        var start = period[\"start\"]?.GetValue<""string>();
        var end = period[\"end\"]?.GetValue<""string>();
        if (start == null && end == null) return null;
        var low = start != null ? FhirDateHelper.ParseFhirDateRange(start).low : DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc);
        var high = end != null ? FhirDateHelper.ParseFhirDateRange(end).high : DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc);
        return (low, high);
    }

"))

; --- Date extraction (backbone, e.g. qualification.period) ---
; The first path part may be a collection (e.g. conversation.message, iterated
; with AsArray) or a single element (e.g. therapeuticconcept.period, accessed
; directly). The last part may itself be a Period element (index its start) or a
; plain date/dateTime field. All four combinations are handled below.
(define (model-date-backbone-extract rn sn method-name search)
    (let* ((parts (search-path-parts search))
           (first-prop (if (not (null? parts))
               (let ((r (% "ref" (car parts)))) (if r (element-with-id r) #f)) #f))
           (first-field (if first-prop (% "name" first-prop) sn))
           (first-collection (if first-prop (true? "iscollection" first-prop) #f))
           (last-prop (if (> (length parts) 1)
               (let ((r (% "ref" (cadr parts)))) (if r (element-with-id r) #f)) #f))
           (last-field (if last-prop (% "name" last-prop) sn))
           (last-ref (if last-prop (% "ref" last-prop) #f))
           (is-period (and last-ref (string=? "period" last-ref))))
        (cond
            ; Collection first part, Period last part: iterate, read period.start
            ((and first-collection is-period)
            ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return null;
        foreach (var item in items)
        {
            var period = item?[\"" last-field "\"];
            if (period == null) continue;
            var start = period[\"start\"]?.GetValue<""string>();
            if (start != null) return FhirDateHelper.ParseFhirDateRange(start);
        }
        return null;
    }

"))
            ; Collection first part, plain date last part: iterate, read field
            (first-collection
            ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return null;
        foreach (var item in items)
        {
            var bd = item?[\"" last-field "\"]?.GetValue<""string>();
            if (bd != null) return FhirDateHelper.ParseFhirDateRange(bd);
        }
        return null;
    }

"))
            ; Single first part, Period last part: the period from its start to its end
            (is-period
            ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var period = content[\"" first-field "\"]?[\"" last-field "\"];
        if (period == null) return null;
        var start = period[\"start\"]?.GetValue<""string>();
        var end = period[\"end\"]?.GetValue<""string>();
        if (start == null && end == null) return null;
        var low = start != null ? FhirDateHelper.ParseFhirDateRange(start).low : DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc);
        var high = end != null ? FhirDateHelper.ParseFhirDateRange(end).high : DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc);
        return (low, high);
    }

"))
            ; Single first part, plain date last part: read nested field directly
            (else
            ($
"    public static (DateTime low, DateTime high)? " method-name "(JsonNode content)
    {
        var bd = content[\"" first-field "\"]?[\"" last-field "\"]?.GetValue<""string>();
        if (bd == null) return null;
        return FhirDateHelper.ParseFhirDateRange(bd);
    }

")))))

; --- Direct reference (generic, generated once) ---
(define (model-direct-ref-extract rn)
    ($
"    public static List<""(string? type, string? id, string? version)> ExtractReference(JsonNode content, string propertyName)
    {
        var results = new List<""(string?, string?, string?)>();
        var node = content[propertyName];
        if (node == null) return results;

        if (node is JsonArray arr)
            foreach (var item in arr) AddReference(results, item);
        else
            AddReference(results, node);

        return results;
    }

"))

; --- Backbone reference (e.g. link.other) ---
(define (model-backbone-ref-extract rn sn method-name fhir-field last-field)
    ($
"    public static List<""(string? type, string? id, string? version)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?, string?)>();
        var links = content[\""fhir-field"\"]?.AsArray();
        if (links == null) return results;
        foreach (var link in links)
        {
            var other = link?[\"" last-field "\"];
            if (other != null) AddReference(results, other);
        }
        return results;
    }

"))

; --- Backbone reference 3-part (e.g. plannedWeek.exercise.medium) ---
(define (model-backbone-ref-extract-3 rn sn method-name first-field second-field third-field)
    ($
"    public static List<""(string? type, string? id, string? version)> "method-name"(JsonNode content)
    {
        var results = new List<""(string?, string?, string?)>();
        var items = content[\"" first-field "\"]?.AsArray();
        if (items == null) return results;
        foreach (var item in items)
        {
            var subs = item?[\"" second-field "\"]?.AsArray();
            if (subs == null) continue;
            foreach (var sub in subs)
            {
                var refNode = sub?[\"" third-field "\"];
                if (refNode != null) AddReference(results, refNode);
            }
        }
        return results;
    }

"))

; ===================================================================
; NUMBER SECTION
; ===================================================================

(define (model-number-section rn searches)
    (if (null? searches) ""
        ($"    // -- Number search params --

"
            (apply $ (map (lambda (s)
                (let* ((sn (% "name" s))
                       (method-name ($ "Extract" (kebab-to-pascal sn)))
                       (parts (search-path-parts s))
                       (prop (search-property s))
                       (fhir-field (if prop (% "name" prop) sn)))
                    (if (> (length parts) 1)
                        (model-number-backbone-extract rn sn method-name s)
                        (model-number-extract rn sn method-name fhir-field))))
                searches)))))

(define (model-number-extract rn sn method-name fhir-field)
    ($"    public static List<""decimal?> " method-name "(JsonNode content)
    {
        var results = new List<""decimal?>();
        var node = content[\"" fhir-field "\"];
        if (node == null) return results;
        if (node is JsonArray arr)
        {
            foreach (var item in arr)
                results.Add(item == null ? null : item.GetValue<""decimal>());
        }
        else
        {
            results.Add(node.GetValue<""decimal>());
        }
        return results;
    }

"))

(define (model-number-backbone-extract rn sn method-name search)
    ($"    // TODO: multi-part path extractor for number search '" sn "' not yet implemented by codegen.
    public static List<""decimal?> " method-name "(JsonNode content) => new List<""decimal?>();

"))

; -------------------------------------------------------------------
; Quantity search params: every Quantity a path of the search reaches,
; as (value, system, code, unit). A path may run through backbone arrays
; (Observation.component.value[x]); the walk flattens them. A choice
; property is read under its JSON name for the Quantity variant
; (value[x] -> valueQuantity).
; -------------------------------------------------------------------

; The JSON keys of one path element, first to last.
(define (quantity-path-keys path)
    (let* ((parts-node (select-children "parts" path))
           (parts (if (node-list-empty? parts-node) '()
                      (node-list->list (select-children "part" (node-list-first parts-node))))))
        (let loop ((ps parts))
            (if (null? ps) '()
                (let* ((ref (% "ref" (car ps)))
                       (prop (if ref (element-with-id ref) #f))
                       (nm (if prop (% "name" prop) "")))
                    (cons (if (and (null? (cdr ps)) prop (property-has-variants? prop)) ($ nm "Quantity") nm)
                          (loop (cdr ps))))))))

; One path as a C# string array literal.
(define (quantity-path-literal keys)
    ($ "new[] { "
       (let loop ((ks keys) (first #t))
           (if (null? ks) ""
               ($ (if first "" ", ") "\"" (car ks) "\"" (loop (cdr ks) #f))))
       " }"))

; The paths of a search as literals, each once (the model repeats a path
; for every type a choice property may take).
(define (quantity-path-literals search)
    (let* ((paths-node (select-children "paths" search))
           (paths (if (node-list-empty? paths-node) '()
                      (node-list->list (select-children "path" (node-list-first paths-node))))))
        (let loop ((ps paths) (seen '()))
            (if (null? ps) (reverse seen)
                (let ((lit (quantity-path-literal (quantity-path-keys (car ps)))))
                    (loop (cdr ps) (if (member lit seen) seen (cons lit seen))))))))

(define (model-quantity-section rn searches)
    (if (null? searches) ""
        ($"    // -- Quantity search params --

"
            (apply $ (map (lambda (s)
                (let* ((sn (% "name" s))
                       (method-name ($ "Extract" (kebab-to-pascal sn))))
                    ($"    public static List<""(decimal value, string? system, string? code, string? unit)> " method-name "(JsonNode content)
    {
        var results = new List<""(decimal value, string? system, string? code, string? unit)>();
"
                        (apply $ (map (lambda (lit)
                            ($"        CollectQuantities(content, " lit ", 0, results);
")) (quantity-path-literals s)))
"        return results;
    }

")))
                searches))
"    private static void CollectQuantities(JsonNode? node, string[] path, int at, List<""(decimal value, string? system, string? code, string? unit)> results)
    {
        if (node == null) return;
        if (node is JsonArray arr)
        {
            foreach (var item in arr)
                CollectQuantities(item, path, at, results);
            return;
        }
        if (node is not JsonObject obj) return;
        if (at < path.Length)
        {
            CollectQuantities(obj[path[at]], path, at + 1, results);
            return;
        }
        if (obj[\"value\"] is not JsonValue number || !number.TryGetValue(out decimal value)) return;
        results.Add((value, QuantityText(obj[\"system\"]), QuantityText(obj[\"code\"]), QuantityText(obj[\"unit\"])));
    }

    private static string? QuantityText(JsonNode? node) =>
        node is JsonValue text && text.TryGetValue(out string? s) ? s : null;

")))

; -------------------------------------------------------------------
; Searches on one type of a choice property (search-is-cast?): every path
; walked, backbone arrays flattened, and what stands at its end read as a
; string, a reference, or a token -- the codings of a CodeableConcept, a
; Coding, or a plain value.
; -------------------------------------------------------------------

(define (cast-path-adder search path)
    (let ((leaf (downcase-string (cast-path-leaf path))))
        (case (% "type" search)
            (("string") "AddReachedText")
            (("reference") "AddReachedReference")
            (else (cond ((string=? "codeableconcept" leaf) "AddReachedConcept")
                        ((string=? "coding" leaf) "AddReachedCoding")
                        (else "AddReachedCode"))))))

; The statements of a search, one for each path, each once.
(define (cast-path-statements search)
    (let loop ((ps (search-path-nodes search)) (seen '()))
        (if (null? ps) (reverse seen)
            (let ((line ($"        foreach (var node in Reach(content, " (quantity-path-literal (cast-path-keys (car ps))) "))
            " (cast-path-adder search (car ps)) "(results, node);
")))
                (loop (cdr ps) (if (member line seen) seen (cons line seen)))))))

; What a search of that kind answers with: strings, tokens as (system,
; value), references as (type, id, version).
(define (cast-result-type search)
    (case (% "type" search)
        (("string") "string")
        (("reference") "(string?, string?, string?)")
        (else "(string?, string?)")))

(define (model-cast-section rn searches)
    (if (null? searches) ""
        ($"    // -- Search params on one type of a choice --

"
            (apply $ (map (lambda (s)
                (let* ((sn (% "name" s))
                       (method-name ($ "Extract" (kebab-to-pascal sn)))
                       )
                    ($"    public static List<""" (cast-result-type s) "> " method-name "(JsonNode content)
    {
        var results = new List<""" (cast-result-type s) ">();
"
                        (apply $ (cast-path-statements s))
"        return results;
    }

")))
                searches))
"    private static List<""JsonNode> Reach(JsonNode? node, string[] path)
    {
        var found = new List<""JsonNode>();
        Reach(node, path, 0, found);
        return found;
    }

    private static void Reach(JsonNode? node, string[] path, int at, List<""JsonNode> found)
    {
        if (node == null) return;
        if (node is JsonArray arr)
        {
            foreach (var item in arr)
                Reach(item, path, at, found);
            return;
        }
        if (at == path.Length)
        {
            found.Add(node);
            return;
        }
        if (node is JsonObject obj)
            Reach(obj[path[at]], path, at + 1, found);
    }

    private static string? ReachedText(JsonNode? node) =>
        node is JsonValue text && text.TryGetValue(out string? s) ? s : null;

    private static void AddReachedText(List<""string> results, JsonNode node)
    {
        var text = ReachedText(node);
        if (text != null) results.Add(text);
    }

    private static void AddReachedReference(List<""(string?, string?, string?)> results, JsonNode node)
    {
        if (node is JsonObject) AddReference(results, node);
    }

    private static void AddReachedConcept(List<""(string?, string?)> results, JsonNode node)
    {
        if (node is not JsonObject concept || concept[\"coding\"] is not JsonArray codings) return;
        foreach (var coding in codings)
            if (coding != null) AddReachedCoding(results, coding);
    }

    private static void AddReachedCoding(List<""(string?, string?)> results, JsonNode node)
    {
        if (node is JsonObject coding)
            results.Add((ReachedText(coding[\"system\"]), ReachedText(coding[\"code\"])));
    }

    private static void AddReachedCode(List<""(string?, string?)> results, JsonNode node)
    {
        if (node is not JsonValue value) return;
        if (value.TryGetValue(out string? text)) results.Add((null, text));
        else if (value.TryGetValue(out bool flag)) results.Add((null, flag ? \"true\" : \"false\"));
    }

")))

; ===================================================================
; HELPERS (always emitted)
; ===================================================================

(define (model-helpers rn)
    ($
"    // -- Helpers --

    // A reference by its LAST parts: Type/id, with what stands before them
    // (the base of an absolute URL) left out, and Type/id/_history/version
    // with its version apart. One part alone is the id.
    private static void AddReference(List<""(string?, string?, string?)> results, JsonNode? refNode)
    {
        var reference = refNode?[\"reference\"]?.GetValue<""string>();
        if (reference == null) return;
        var parts = reference.Split('/');
        int n = parts.Length;
        string? version = null;
        if (n >= 3 && parts[n - 2] == \"_history\")
        {
            version = parts[n - 1];
            n -= 2;
        }
        if (n >= 2)
            results.Add((parts[n - 2], parts[n - 1], version));
        else
            results.Add((null, version == null ? reference : parts[0], version));
    }

    private static void AddIfPresent(List<""string> parts, JsonNode node, string fieldName)
    {
        var val = node[fieldName]?.GetValue<""string>();
        if (val != null) parts.Add(val);
    }
"))
