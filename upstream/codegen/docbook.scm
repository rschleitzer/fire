<![CDATA[
(define (documentation-root) "server/wwwroot/")

(element book
    (make sequence
         (make entity
            system-id: ($ (documentation-root)"index.html")
            (! (docbook-header))
            (! ($ "
<h1 class='self-link-parent'>"(data (select-children "title" (current-node)))"</h1>
"           ))
            (with-mode index (process-children))
            (! (html-footer))
        )
        (make entity

            system-id: ($ (documentation-root) "documentation.html")
            (! (docbook-header))
            (! "             <h1 class='self-link-parent'>")(process-node-list (select-children "title" (current-node)))(! "</h1>
"           )
            (process-node-list (select-children "preface" (current-node)))
            (! "
              <table cellpadding='0' cellspacing='0' style='font-size: 11px; font-family: verdana; vertical-align: top;' id='toc'>
                <tbody>
")
            (with-mode toc (process-children))
            (! (html-footer))

        )
        (process-node-list (select-children "part" (current-node)))
    )
)
]]>

(element para (make element gi: "p" (process-children)))

(element part
    (make entity

        system-id: ($ (documentation-root)(id (current-node))".html")
        (! (docbook-header))
        (process-children)
        (for-nodes (select-children "chapter" (current-node)) (lambda (chapter) (sosofo-append
            (make element gi: "p"
                (make element gi: "a" attributes: (list (list "href" ($ (id chapter) ".html")))
                    (process-node-list (children (select-children "title" chapter)))
                )
            )
        )))
        (! (html-footer))

    )
)

(define (docbook-header)
    (html-header
        (data (select-children "title" (current-node)))
        (find-previous-section)
        (if (string=? "part" (gi (current-node))) "index" (id (parent (current-node))))
        (find-next-section)
    )
)

(define (find-previous-section)
    (let ((previous-section (select-elements (ipreced (current-node))(gi (current-node)))))
        (if (node-list-empty? previous-section)
            (if (node-list-empty? (parent (current-node))) #f (id (parent (current-node))))
            (let ((previous-subsection (find-subsection previous-section #f))) (if (node-list-empty? previous-subsection) (id previous-section) (id previous-subsection)))
        )
    )
)

(define (find-next-section)
    (let ((subsection (find-subsection (current-node) #t)))
        (if (node-list-empty? subsection)
            (let ((next-section (ifollow (current-node))))
                (if (node-list-empty? next-section)
                    (find-next-super-section (current-node))
                    (id next-section)
                )
            )
            (id subsection)
        )
    )
)

(define (find-subsection node forward?)
    (let ((subsections (select-children (case (gi node) (("book") "part") (("part") "chapter") (("chapter") "sect1") (else "sect2")) node)))
        (node-list-first (if forward? subsections (node-list-reverse subsections)))
    )
)

(define (find-next-super-section node)
    (if (node-list-empty? (parent node))
        #f
        (if (node-list-empty? (ifollow (parent node)))
            (find-next-super-section (parent node))
            (id (ifollow (parent node)))
        )
    )
)

(element (part title)
    (make element gi: "h1" attributes: '(("class" "self-link-parent"))
        (sosofo-append
            (! ($ (number->string (child-number (parent (current-node)))) " "))
            (process-children)
        )
    )
)

(element chapter
    (make entity

        system-id: ($ (documentation-root)(id (current-node))".html")
        (! (docbook-header))
        (process-children)
        (for-nodes (select-children "sect1" (current-node)) (lambda (sect1) (sosofo-append
            (make element gi: "p"
                (make element gi: "a" attributes: (list (list "href" ($ (id sect1) ".html")))
                    (process-node-list (children (select-children "title" sect1)))
                )
            )
        )))
        (! (html-footer))

    )
)

(element (chapter title)
    (make element gi: "h2" attributes: '(("class" "self-link-parent"))
        (sosofo-append
            (! ($
                (number->string (child-number (parent (parent (current-node))))) "." (number->string (child-number (parent (current-node)))) " "
            ))
            (process-children)
        )
    )
)

(element sect1
    (make entity

        system-id: ($ (documentation-root)(id (current-node))".html")
        (! (docbook-header))
        (process-children)
        (for-nodes (select-children "sect2" (current-node)) (lambda (sect2) (sosofo-append
            (make element gi: "p"
                (make element gi: "a" attributes: (list (list "href" ($ (id sect2) ".html")))
                    (process-node-list (children (select-children "title" sect2)))
                )
            )
        )))
        (! (html-footer))

    )
)

(element (sect1 title)
    (make element gi: "h3" attributes: '(("class" "self-link-parent"))
        (sosofo-append
            (! ($
                (number->string (child-number (parent (parent (parent (current-node)))))) "." (number->string (child-number (parent (parent (current-node))))) "." (number->string (child-number (parent (current-node)))) " "
            ))
            (process-children)
        )
    )
)

(element sect2
    (make entity

        system-id: ($ (documentation-root)(id (current-node))".html")
        (! (docbook-header))
        (process-children)
        (! (html-footer))

    )
)

(element (sect2 title)
    (make element gi: "h4" attributes: '(("class" "self-link-parent"))
        (sosofo-append
            (! ($
                (number->string (child-number (parent (parent (parent (parent (current-node))))))) "." (number->string (child-number (parent (parent (parent (current-node)))))) "." (number->string (child-number (parent (parent (current-node))))) "." (number->string (child-number (parent (current-node)))) " "
            ))
            (process-children)
        )
    )
)

(element link
    (make element gi: "a" attributes: (list (list "href" ($ (% "linkend" (current-node)) ".html")))
        (process-children)
    )
)

(element olink
    (make element gi: "a" attributes: (list (list "href" (% "targetdoc" (current-node))))
        (process-children)
    )
)

(element itemizedlist
    (make element gi: "ul" (process-children)))

(element (itemizedlist listitem)
    (make element gi: "li" (process-children)))

(element orderedlist
    (make element gi: "ol" (process-children)))

(element (orderedlist listitem)
    (make element gi: "li" (process-children)))

(element variablelist
    (make element gi: "dl" (process-children)))

(element (variablelist varlistentry term)
    (make element gi: "dt" (process-children)))

(element (variablelist varlistentry listitem)
    (make element gi: "dd" (process-children)))

(element formalpara
    (process-children))

(element (formalpara title)
    (make element gi: "h5" (process-children)))

(element (formalpara para)
    (make element gi: "p" (process-children)))

(element database
    (make element gi: "code" (process-children)))

(element parameter
    (make element gi: "code" (process-children)))

(element filename
    (make element gi: "code" (process-children)))

(element classname
    (make element gi: "code" (process-children)))

(element property
    (make element gi: "code" (process-children)))

(element literal
    (make element gi: "code" (process-children)))

(element code
    (make element gi: "code" (process-children)))

(element command
    (make element gi: "code" (process-children)))

(element emphasis
    (make element gi: "em" (process-children)))

;; Code-/Befehlsblöcke: in pre element wrappen, damit Zeilenumbrüche und
;; Einrückung erhalten bleiben (sonst kollabiert der Text zu einer Zeile).
(element programlisting
    (make element gi: "pre" attributes: '(("class" "programlisting")) (process-children)))

;; --- Bilder / Screenshots (DocBook mediaobject) ---
;; Bilddateien werden vom docu-Skript nach server/wwwroot/ kopiert und per
;; fileref (Dateiname) referenziert.
(element mediaobject
    (make element gi: "figure" (process-children)))

(element imageobject
    (process-children))

(element imagedata
    (make element gi: "img"
        attributes: (list
            (list "src" (% "fileref" (current-node)))
            (list "style" "max-width: 100%; height: auto; border: 1px solid #ccc;"))))

(element caption
    (make element gi: "figcaption" (process-children)))

;; --- Tabellen (DocBook CALS-Modell) ---
(element table
    (make element gi: "table" attributes: '(("class" "doctable")) (process-children)))

(element (table title)
    (make element gi: "caption" (process-children)))

(element informaltable
    (make element gi: "table" attributes: '(("class" "doctable")) (process-children)))

(element tgroup
    (process-children))

(element colspec (empty-sosofo))

(element thead
    (make element gi: "thead" (process-children)))

(element tbody
    (make element gi: "tbody" (process-children)))

(element row
    (make element gi: "tr" (process-children)))

(element entry
    (make element gi: "td" (process-children)))

(element (thead row entry)
    (make element gi: "th" (process-children)))
