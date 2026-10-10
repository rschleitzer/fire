; Reverse-existence search filters -- neutral default.
;
; repositories.scm calls (repo-exists-filter-cases rn db) for every resource search
; and expects a string of C# case blocks (empty = the feature is unused). The feature
; itself is project-specific: it is driven by search params carrying exists="true" in
; a project service model, which not every project has.
;
; Wiring follows the orderby convention: a project that wants the feature points the
; existsfilter entity in its own map.dsl at a project-local existsfilter.scm;
; everyone else keeps this default and needs no boilerplate of their own.
(define (repo-exists-filter-cases rn db) "")
