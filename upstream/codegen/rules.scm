(element fhir
    (sosofo-append
        (process-children)))

(element resources
    (sosofo-append
        (if generate-fhir-model
            (make directory path: (model-dir)
                (fhir-model-csproj)
                (fhir-model-all-resources))
            (empty-sosofo))
        (make directory path: (project-dir)
            (if generate-boilerplate (boilerplate) (empty-sosofo))
            (if generate-datamigration (datamigration-cs) (empty-sosofo))
            (if generate-repositories (search-query-builders) (empty-sosofo))
            (if generate-fhir-validator
                (make directory path: "Model"
                    (make directory path: "Validator"
                        (fhir-validator-all-resources)
                        (fhir-validator-dispatch-file)))
                (empty-sosofo)))
        (if generate-fhir-client
            (make directory path: "client"
                (fhir-client-boilerplate)
                (fhir-client-all-search-descriptors))
            (empty-sosofo))
        (process-children)
        (make directory path: (project-dir)
            (if generate-handlers (make directory path: "Handlers" (generic-handlers)) (empty-sosofo))
            (make directory path: "wwwroot"
                (if generate-js (make directory path: "js" (js-elements-file)) (empty-sosofo))
                (if generate-html (html-resourcelist-file) (empty-sosofo))))))

(element resource
    (if (active? (current-node))
        (make directory path: (project-dir)
            (if generate-models (make directory path: "Search" (model)) (empty-sosofo))
            (if generate-repositories (repository) (empty-sosofo))
            (if generate-handlers (make directory path: "Handlers" (handler)) (empty-sosofo))
            (if generate-js (make directory path: "wwwroot" (make directory path: "js" (js-resource-file))) (empty-sosofo))
            (if generate-html (make directory path: "wwwroot" (html-documentation-file)) (empty-sosofo)))
        (empty-sosofo)))

(element elements
    (if (string=? "fhir" (gi (parent (current-node))))
        (sosofo-append
            (if generate-fhir-model
                (make directory path: (model-dir)
                    (fhir-model-all-elements))
                (empty-sosofo))
            (if generate-fhir-validator
                (make directory path: (project-dir)
                    (make directory path: "Model"
                        (make directory path: "Validator"
                            (fhir-validator-all-elements))))
                (empty-sosofo)))
        (empty-sosofo)))
(element codesets (empty-sosofo))
(element description (empty-sosofo))
