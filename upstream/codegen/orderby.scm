; Generic BuildOrderBy generation.
; Emits sort cases for inline, _lastUpdated, table searches, forward-chain refs.
; And for reverse chains, where the model declares one: a param with ref and chain below
; repos, for another resource -- the key is _has:Resource:reference:parameter, as the
; reverse-chain search writes it, and the value the first one's that points here:
;   SELECT t.status FROM Observation t JOIN Observation.search_subject rs
;   ON rs.resource_id = t.id WHERE rs.target_id = r.id LIMIT 1
; A model without repos has none.

; The repo block a model declares for a resource below repos, if it has one
(define (find-service-repo-for rn-lc)
    (let* ((root (tree-root (current-node)))
           (repos-nl (select-children "repos" root))
           (all-repos (if (node-list-empty? repos-nl)
                          (empty-node-list)
                          (select-children "repo" (node-list-first repos-nl)))))
      (node-list-filter (lambda (r)
                           (let ((ref (% "ref" r)))
                             (if ref (string=? (downcase-string ref) rn-lc) #f)))
                         all-repos)))

(define (sortable-chain-type? t)
    (or (string=? "string" t)
        (string=? "token" t)
        (string=? "date" t)
        (string=? "number" t)))

(define (reverse-chain-sort-param? param rn-lc)
    (let ((chain-attr (% "chain" param))
          (ref-attr (% "ref" param)))
      (if (and chain-attr ref-attr)
          (let* ((ref-search (element-with-id ref-attr))
                 (src-res (if ref-search (parent (parent ref-search)) #f))
                 (src-id (if src-res (% "id" src-res) #f))
                 (chain-search (element-with-id chain-attr))
                 (chain-type (if chain-search (% "type" chain-search) #f)))
            (and src-id
                 (not (string=? src-id rn-lc))
                 chain-type
                 (sortable-chain-type? chain-type)))
          #f)))

(define (reverse-chain-sort-params rn)
    (let* ((rn-lc (downcase-string rn))
           (repo-nl (find-service-repo-for rn-lc)))
      (if (node-list-empty? repo-nl)
          '()
          (let ((all-params (node-list->list (select-descendants "param" (node-list-first repo-nl)))))
            (filter (lambda (p) (reverse-chain-sort-param? p rn-lc)) all-params)))))

(define (reverse-chain-sort-case param db)
    (let* ((ref-search (element-with-id (% "ref" param)))
           (src-res (parent (parent ref-search)))
           (src-rn (% "name" src-res))
           (src-schema src-rn)
           (ref-name (% "name" ref-search))
           (ref-table (search-table-name ref-search))
           (chain-search (element-with-id (% "chain" param)))
           (chain-name (% "name" chain-search))
           (chain-type (% "type" chain-search))
           (key ($ "_has:" src-rn ":" ref-name ":" chain-name)))
      (cond
        ((search-is-inline? chain-search)
         ($"                \""key"\" => \"("(db-select-top1 db)" t."(db-quote-col-cs (search-col chain-search) db)" FROM "(tbl-public src-rn db)" t JOIN "(tbl src-schema ref-table db)" rs ON rs.resource_id = t.id WHERE rs.target_id = r.id"(db-trailing-limit1 db)")\",
"))
        ((search-is-last-updated? chain-search)
         ($"                \""key"\" => \"("(db-select-top1 db)" t.last_updated FROM "(tbl-public src-rn db)" t JOIN "(tbl src-schema ref-table db)" rs ON rs.resource_id = t.id WHERE rs.target_id = r.id"(db-trailing-limit1 db)")\",
"))
        (else
         (let* ((chain-table (search-table-name chain-search))
                (col (if (string=? "date" chain-type) "value_low" "value")))
           ($"                \""key"\" => \"("(db-select-top1 db)" c."col" FROM "(tbl src-schema ref-table db)" rs JOIN "(tbl src-schema chain-table db)" c ON c.resource_id = rs.resource_id WHERE rs.target_id = r.id"(db-trailing-limit1 db)")\",
"))))))

(define (repo-build-order-by schema inline-list all-table-search-list string-searches token-searches date-searches number-searches ref-searches db)
    (let* ((hn-family (filter (lambda (s) (and (search-is-humanname? s) (equal? "family" (search-humanname-field s)))) string-searches))
           (hn-given (filter (lambda (s) (and (search-is-humanname? s) (equal? "given" (search-humanname-field s)))) string-searches))
           (plain-strings (filter (lambda (s) (not (search-is-humanname? s))) string-searches)))
        ($
"    private static string BuildOrderBy(string sortParam)
    {
        if (string.IsNullOrEmpty(sortParam)) return \"r.last_updated DESC, r.id ASC\";

        var parts = new List<""string>();
        foreach (var field in sortParam.Split(','))
        {
            var trimmed = field.Trim();
            var desc = trimmed.StartsWith(\"-\");
            var key = desc ? trimmed[1..] : trimmed;
            var dir = desc ? \"DESC\" : \"ASC\";

            var col = key switch
            {
"
            (apply $ (map (lambda (s)
                ($"                \"" (% "name" s)"\" => \"r."(db-quote-col-cs (search-col s) db)"\",
"))
                inline-list))
            "                \"_lastUpdated\" => \"r.last_updated\",
"
            (apply $ (map (lambda (s)
                ; (a date of a list has a row for every item: the first one is the key)
                ($"                \"" (% "name" s)"\" => \"("(db-select-top1 db)" d.value_low FROM " (tbl schema (search-table-name s) db)" d WHERE d.resource_id = r.id"(if (search-is-date-list? s) " ORDER BY d.id" "")(db-trailing-limit1 db)")\",
"))
                date-searches))
            (apply $ (map (lambda (s)
                ($"                \"" (% "name" s)"\" => \"("(db-select-top1 db)" n.value FROM " (tbl schema (search-table-name s) db)" n WHERE n.resource_id = r.id"(db-trailing-limit1 db)")\",
"))
                number-searches))
            (apply $ (map (lambda (s)
                ($"                \""(% "name" s)"\" => \"("(db-select-top1 db)" LOWER(s.value) FROM "(tbl schema (search-table-name s) db)" s WHERE s.resource_id = r.id"(db-trailing-limit1 db)")\",
"))
                hn-family))
            (apply $ (map (lambda (s)
                ($"                \""(% "name" s)"\" => \"("(db-select-top1 db)" LOWER(s.value) FROM " (tbl schema (search-table-name s) db)" s WHERE s.resource_id = r.id"(db-trailing-limit1 db)")\",
"))
                hn-given))
            (apply $ (map (lambda (s)
                ($"                \""(% "name" s)"\" => \"("(db-select-top1 db)" LOWER(s.value) FROM " (tbl schema (search-table-name s) db)" s WHERE s.resource_id = r.id"(db-trailing-limit1 db)")\",
"))
                plain-strings))
            (apply $ (map (lambda (s)
                ($"                \""(% "name" s)"\" => \"("(db-select-top1 db)" t.value FROM " (tbl schema (search-table-name s) db)" t WHERE t.resource_id = r.id"(db-trailing-limit1 db)")\",
"))
                token-searches))
            (apply $ (map (lambda (p) (reverse-chain-sort-case p db)) (reverse-chain-sort-params schema)))
            ; (through a reference: put together when it is asked for, see ChainSort below --
            ; written out, every reference times every type it may point to times every key
            ; of that type was most of a repository's text)
"                _ when ChainSort(key) is { } chained => chained,
"
            (apply $ (map (lambda (ref-s)
                ($"                \""(% "name" ref-s)"\" => throw new ArgumentException($\"Sorting by reference parameter '"(% "name" ref-s)"' requires chaining syntax: _sort="(% "name" ref-s)":<""TargetType>.<""property>.\"),
"))
                ref-searches))
"                _ => throw new ArgumentException($\"Sort '{key}' is not supported.\")
            };
            parts.Add($\"CASE WHEN {col} IS NULL THEN 1 ELSE 0 END, {col} {dir}\");
        }
        // Eindeutiger Schlusssortierschlüssel: LIMIT/OFFSET braucht eine
        // vollständige Ordnung, sonst können Zeilen mit gleichen Sortierwerten
        // beim Blättern doppelt erscheinen oder ganz ausfallen.
        parts.Add(\"r.id ASC\");
        return string.Join(\", \", parts);
    }

"
        (repo-chain-sort schema ref-searches))))
