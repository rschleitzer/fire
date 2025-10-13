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
