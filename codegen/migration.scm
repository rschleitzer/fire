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
"(for-selected-children-of searches "search" (lambda (search)
    (let ((search-name (% "name" search))
          (search-type (% "type" search))
          (is-collection (search-is-collection? search)))
      (if (not (string=? "_lastUpdated" search-name))
          (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT"(if is-collection "[]" "")",
"))
            (("token")
              ($"    "(string-replace search-name "-" "_")"_system TEXT"(if is-collection "[]" "")",
    "(string-replace search-name "-" "_")"_code TEXT"(if is-collection "[]" "")",
"))
            (("date")
              ($"    "(string-replace search-name "-" "_")" DATE,
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT"(if is-collection "[]" "")" DEFAULT '{}',
"))
            (else ""))
          ""))))
");

-- Create indexes for current table
CREATE INDEX idx_"table-name"_last_updated ON "table-name" (last_updated);

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
      (if (not (string=? "_lastUpdated" search-name))
          (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT"(if is-collection "[]" "")",
"))
            (("token")
              ($"    "(string-replace search-name "-" "_")"_system TEXT"(if is-collection "[]" "")",
    "(string-replace search-name "-" "_")"_code TEXT"(if is-collection "[]" "")",
"))
            (("date")
              ($"    "(string-replace search-name "-" "_")" DATE,
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT"(if is-collection "[]" "")" DEFAULT '{}',
"))
            (else ""))
          ""))))
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
