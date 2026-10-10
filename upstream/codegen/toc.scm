<![CDATA[
(mode toc

    (element part (process-children))
    (element chapter (process-children))
    (element sect1 (process-children))
    (element sect2 (process-children))
    (default (empty-sosofo))

    (element title
         (make sequence
        (! "<tr><td style='background-image: url(tbl_bck")
        (! (tree-background-png-mask (parent (current-node))))
        (! ".png)' class='hierarchy'><img src='tbl_spacer.png' class='hierarchy'>")
        (! (tree-structure (parent (current-node))))
        (! (case (gi (parent (current-node))) (("book") "") (else ($ "<img src='tbl_vjoin"(if (last-sibling? (parent (current-node))) "_end" "") (tag-vjoin-open (parent (current-node)))".png' class='hierarchy'>"))))
        (! "<img src='icon_page_i.gif' style='background-color: inherit' class='hierarchy'>")
        (! " <a href='")
        (! (html-id (parent (current-node))))
        (! ".html' title='")
        (! (data (current-node)))
        (! "'>")
        (! (section-number (parent (current-node))))
        (! (case (gi (parent (current-node))) (("book") "") (else " ")))
        (! (data (current-node)))
        (!"</a></td></tr>
"       )
        )
    )
)

(define (tree-background-png-mask node)
    (case (gi node)
        (("book") "1")
        (else ($
            (tree-background-png-mask (parent node))
            (case (gi node)
                (("part") (if (node-list-empty? (select-children "chapter" node)) "0" "1"))
                (("chapter") (if (node-list-empty? (select-children "sect1" node)) "0" "1"))
                (("sect1") (if (node-list-empty? (select-children "sect2" node)) "0" "1"))
                (("sect2") "0")
            )
        ))
    )
)

(define (tree-structure node)
    
    ($
        (case (gi node)
            (("book" "part") "")
            (else ($
                (tree-structure (parent node))
                "<img src='tbl_"
                (if (last-sibling? (parent node)) "blank" "vline")
                ".png' class='hierarchy'>"
            ))
        )
    )
    
)

(define (tag-vjoin-open node)
    (case (gi node)
        (("book") "")
        (("part") (if (node-list-empty? (select-children "chapter" node)) "" "-open"))
        (("chapter") (if (node-list-empty? (select-children "sect1" node)) "" "-open"))
        (("sect1") (if (node-list-empty? (select-children "sect2" node)) "" "-open"))
        (("sect2") (if (node-list-empty? (select-children "sect3" node)) "" "-open"))
    )
)

(define (html-id node)
    (case (gi node)
        (("book") "documentation")
        (else (id node))
    )
)

(define (section-number node)
    (case (gi node)
        (("book") "")
        (("part") (number->string (child-number node)))
        (else ($
            (section-number (parent node))
            "."
            (number->string (child-number node))
        ))
    )
)]]>
