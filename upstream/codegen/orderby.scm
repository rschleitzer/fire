; Generic BuildOrderBy generation.
; Emits sort cases for inline, _lastUpdated, table searches, forward-chain refs.
; Reverse-chain sort cases are project-specific and live in a project-local
; override of this file.
; The helpers chain-sort-cases-for-target and chain-sort-cases-for-ref remain
; in repositories.scm.

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
                ($"                \"" (% "name" s)"\" => \"("(db-select-top1 db)" d.value_low FROM " (tbl schema (search-table-name s) db)" d WHERE d.resource_id = r.id"(db-trailing-limit1 db)")\",
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
            (apply $ (map (lambda (ref-s) (chain-sort-cases-for-ref schema ref-s db)) ref-searches))
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

")))
