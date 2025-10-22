(element resources
    (sosofo-append
        (migration)
        (repositories)
        (process-children))
)

(element resource
    (if (active? (current-node))
        (struct)
        (empty-sosofo)))

(element elements (empty-sosofo))
(element codesets (empty-sosofo))
(element description (empty-sosofo))