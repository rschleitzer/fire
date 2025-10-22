(element resources
    (sosofo-append
        (migration)
        (process-children))
)

(element resource
    (if (active? (current-node))
        (sosofo-append
            (struct)
            (repository)
            (handler))
        (empty-sosofo)))

(element elements (empty-sosofo))
(element codesets (empty-sosofo))
(element description (empty-sosofo))