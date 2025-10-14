(define (migration)
  (file "migrations/001_initial_schema.sql"
        (for-active-resources (lambda (resource)
          (let ((table-name (downcase-string (name-of resource)))
                (searches (select-children "searches" resource)))
            ($"-- Create "table-name" current table
CREATE TABLE "table-name" (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
"(for-selected-children-of searches "search" (lambda (search) ($
    (let ((search-name (% "name" search))
          (search-type (% "type" search))
          (is-collection (search-is-collection? search)))
      (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT"(if is-collection "[]" "")(trailing-comma search)"
"))
            (("token")
              (if (search-is-simple-code? search)
                  ; Simple code type - single column
                  ($"    "(string-replace search-name "-" "_")" TEXT"(if is-collection "[]" "")(trailing-comma search)"
")
                  ; Coding/CodeableConcept/Identifier - system and code/value columns
                  (let ((token-suffix (if (search-is-identifier? search) "_value" "_code")))
                    ($"    "(string-replace search-name "-" "_")"_system TEXT"(if is-collection "[]" "")",
    "(string-replace search-name "-" "_")token-suffix" TEXT"(if is-collection "[]" "")(trailing-comma search)"
"))))
            (("date")
              (let* ((property (search-property search))
                     (has-variants (property-has-variants? property))
                     (variants (if has-variants (property-variants property) (empty-node-list)))
                     (variant-list (node-list->list variants))
                     (variant-types (map (lambda (v) (% "type" v)) variant-list))
                     (has-datetime (or (member "dateTime" variant-types) (member "instant" variant-types)))
                     (has-period (let loop ((vlist variant-list))
                                   (cond ((null? vlist) #f)
                                         ((and (string=? "element" (% "type" (car vlist)))
                                               (string=? "period" (% "ref" (car vlist)))) #t)
                                         (else (loop (cdr vlist)))))))
                  (if has-variants
                    ($
                      (if has-datetime ($"    "(string-replace search-name "-" "_")"_datetime TIMESTAMPTZ"(if has-period "," (trailing-comma search))"
")"")
                      (if has-period ($"    "(string-replace search-name "-" "_")"_period_start TIMESTAMPTZ,
    "(string-replace search-name "-" "_")"_period_end TIMESTAMPTZ"(trailing-comma search)"
")"")
                    )
                    ($"    "(string-replace search-name "-" "_")" DATE"(trailing-comma search)"
"                   )
                  )
              )
            )
            (("number")
              ($"    "(string-replace search-name "-" "_")" NUMERIC(20, 6)"(trailing-comma search)"
"))
            (("quantity")
              ($"    "(string-replace search-name "-" "_")"_value NUMERIC(20, 6),
    "(string-replace search-name "-" "_")"_unit TEXT,
    "(string-replace search-name "-" "_")"_system TEXT"(trailing-comma search)"
"))
            (("uri")
              ($"    "(string-replace search-name "-" "_")" TEXT"(trailing-comma search)"
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT"(if is-collection "[]" "")" DEFAULT '{}'"(trailing-comma search)"
"))
            (("composite")
              "")  ; TODO: Handle composite searches when components are defined
            (("special")
              "")  ; Skip special searches like _text
            (else "")))
        )
      ))
");

-- Create indexes for current table
CREATE INDEX idx_"table-name"_last_updated ON "table-name" (last_updated);
"(for-selected-children-of searches "search" (lambda (search) ($
    (let* ((search-name (% "name" search))
           (search-type (% "type" search))
           (is-collection (search-is-collection? search))
           (col-name (string-replace search-name "-" "_")))
      (case search-type
            (("string")
              ($"CREATE INDEX idx_"table-name"_"col-name"_name ON "table-name(if is-collection " USING GIN" "")" ("col-name"_name);
"))
            (("token")
              (if (search-is-simple-code? search)
                  ; Simple code - single index
                  ($"CREATE INDEX idx_"table-name"_"col-name" ON "table-name(if is-collection " USING GIN" "")" ("col-name");
")
                  ; Coding/CodeableConcept - index on code/value column
                  (let ((token-suffix (if (search-is-identifier? search) "_value" "_code")))
                    ($"CREATE INDEX idx_"table-name"_"col-name token-suffix" ON "table-name(if is-collection " USING GIN" "")" ("col-name token-suffix");
"))))
            (("date")
              (let* ((property (search-property search))
                     (has-variants (property-has-variants? property))
                     (variants (if has-variants (property-variants property) (empty-node-list)))
                     (variant-list (node-list->list variants))
                     (variant-types (map (lambda (v) (% "type" v)) variant-list))
                     (has-datetime (or (member "dateTime" variant-types) (member "instant" variant-types)))
                     (has-period (let loop ((vlist variant-list))
                                   (cond ((null? vlist) #f)
                                         ((and (string=? "element" (% "type" (car vlist)))
                                               (string=? "period" (% "ref" (car vlist)))) #t)
                                         (else (loop (cdr vlist)))))))
                (if has-variants
                    ($
                      (if has-datetime
                          ($"CREATE INDEX idx_"table-name"_"col-name"_datetime ON "table-name" ("col-name"_datetime);
")
                          "")
                      (if has-period
                          ($"CREATE INDEX idx_"table-name"_"col-name"_period ON "table-name" ("col-name"_period_start, "col-name"_period_end);
")
                          ""))
                    ($"CREATE INDEX idx_"table-name"_"col-name" ON "table-name" ("col-name");
"))))
            (("number")
              ($"CREATE INDEX idx_"table-name"_"col-name" ON "table-name" ("col-name");
"))
            (("quantity")
              ($"CREATE INDEX idx_"table-name"_"col-name"_value ON "table-name" ("col-name"_value);
"))
            (("uri")
              ($"CREATE INDEX idx_"table-name"_"col-name" ON "table-name" ("col-name");
"))
            (("reference")
              ($"CREATE INDEX idx_"table-name"_"col-name"_reference ON "table-name(if is-collection " USING GIN" "")" ("col-name"_reference);
"))
            (else ""))
  ))
))
"
-- Create GIN index for JSONB content
CREATE INDEX idx_"table-name"_content ON "table-name" USING GIN (content);

-- Create "table-name" history table
CREATE TABLE "table-name"_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
"(for-selected-children-of searches "search" (lambda (search)
    (let ((search-name (% "name" search))
          (search-type (% "type" search))
          (is-collection (search-is-collection? search)))
      (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT"(if is-collection "[]" "")",
"))
            (("token")
              (if (search-is-simple-code? search)
                  ; Simple code type - single column
                  ($"    "(string-replace search-name "-" "_")" TEXT"(if is-collection "[]" "")",
")
                  ; Coding/CodeableConcept/Identifier - system and code/value columns
                  (let ((token-suffix (if (search-is-identifier? search) "_value" "_code")))
                    ($"    "(string-replace search-name "-" "_")"_system TEXT"(if is-collection "[]" "")",
    "(string-replace search-name "-" "_")token-suffix" TEXT"(if is-collection "[]" "")",
"))))
            (("date")
              (let* ((property (search-property search))
                     (has-variants (property-has-variants? property))
                     (variants (if has-variants (property-variants property) (empty-node-list)))
                     (variant-list (node-list->list variants))
                     (variant-types (map (lambda (v) (% "type" v)) variant-list))
                     (has-datetime (or (member "dateTime" variant-types) (member "instant" variant-types)))
                     (has-period (let loop ((vlist variant-list))
                                   (cond ((null? vlist) #f)
                                         ((and (string=? "element" (% "type" (car vlist)))
                                               (string=? "period" (% "ref" (car vlist)))) #t)
                                         (else (loop (cdr vlist)))))))
                (if has-variants
                    ($
                      (if has-datetime ($"    "(string-replace search-name "-" "_")"_datetime TIMESTAMPTZ,
")"")
                      (if has-period ($"    "(string-replace search-name "-" "_")"_period_start TIMESTAMPTZ,
    "(string-replace search-name "-" "_")"_period_end TIMESTAMPTZ,
")"")
                    )
                    ($"    "(string-replace search-name "-" "_")" DATE,
")
                )
            ))
            (("number")
              ($"    "(string-replace search-name "-" "_")" NUMERIC(20, 6),
"))
            (("quantity")
              ($"    "(string-replace search-name "-" "_")"_value NUMERIC(20, 6),
    "(string-replace search-name "-" "_")"_unit TEXT,
    "(string-replace search-name "-" "_")"_system TEXT,
"))
            (("uri")
              ($"    "(string-replace search-name "-" "_")" TEXT,
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT"(if is-collection "[]" "")" DEFAULT '{}',
"))
            (("composite")
              "")  ; TODO: Handle composite searches when components are defined
            (("special")
              "")  ; Skip special searches like _text
            (else "")))))
"
    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_"table-name"_history_id ON "table-name"_history (id);
CREATE INDEX idx_"table-name"_history_timestamp ON "table-name"_history (history_timestamp);
CREATE INDEX idx_"table-name"_history_last_updated ON "table-name"_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_"table-name"_history_content ON "table-name"_history USING GIN (content);

"))))))
