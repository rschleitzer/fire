(element fhir
  (process-children))

(element resources
  (sosofo-append
    (migration)
    (process-children)))

(element resource
  (if (true? "active" (current-node))
      (sosofo-append
        ;; (generate-rust-model)
        (empty-sosofo))
      (empty-sosofo)))

;; Suppress output from direct children of fhir (except resources)
(element elements (empty-sosofo))
(element codesets (empty-sosofo))

(define (generate-rust-model)
  (let ((resource-name (name-of (current-node)))
        (snake-name (downcase-string (name-of (current-node)))))
    (file ($ "src/models/" snake-name ".rs")
          ($ "use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct "resource-name" {
    pub id: Option<""String>;
}
"))))
