<![CDATA[
(mode index
    (element preface (process-children))
    (element olink
        (make element gi: "a" attributes: (list (list "href" (% "targetdoc" (current-node))))
            (process-children)
        )
    )

    ; Phrase-level markup: the default rule below suppresses every element that
    ; is not handled here, and it does so without descending into it. In titles
    ; and in the preface that would swallow the text of marked-up spans as well,
    ; leaving empty parentheses behind - "Enthaelt-Suche ()" for a title reading
    ; "Enthaelt-Suche (:contains)". These rules pass the text through. Unlike in
    ; docbook.scm they add no code or em element: the diagram boxes are a compact
    ; navigational graphic and are meant to read in one uniform font. Outside of
    ; titles and preface these elements are unreachable anyway, because the
    ; default rule stops the descent there.
    (element database (process-children))
    (element parameter (process-children))
    (element filename (process-children))
    (element classname (process-children))
    (element property (process-children))
    (element literal (process-children))
    (element code (process-children))
    (element command (process-children))
    (element emphasis (process-children))

    (default (empty-sosofo))

    (element part
        
        (make sequence
            (if (first-sibling? (current-node))
                (!
"

<div id='diagram'>
"               )
                (empty-sosofo)
            )
            (process-children)
            (!
"    </div>
"           )
        )
        
    )

    (element (part title)
        
        (make sequence
            (!"
    <p class='level-text'><b>Part "
            )
            (! (number->string (child-number (parent (current-node)))))
            (! "</b> <a href='")(!(id (parent (current-node))))(!".html'>"
            )
            (process-children)
            (!"</a></p>
    <div class='box_row box_more'>
"           )
        )
        
    )

    (element chapter 
        
        (make sequence
            (!
"        <div id='")(!(id (current-node)))(!"' class='box'>
            <p class='head'>
                <a href='")(!(id (current-node)))(!".html'>
"           )
            (process-children)
            (!
"
            </p>
        </div>
"           )
        )
        
    )

    (element (chapter title)
        
        (make sequence
            (!
"                    <span>")
            (process-children)
            (!"</span>
                </a>
            </p>
            <p class='content'>
"            )
        )
        
    )

    (element sect1 
        
        (make sequence
            (!
"                <a href='")(!(id (current-node)))(!".html'>")(process-node-list (children (select-children "title" (current-node))))(!"</a>")
            (if (last-sibling? (current-node)) (empty-sosofo)
                (!",<br>
"               )
            )
        )
        
    )

    (element (preface para)
        
        (make sequence
            (! "
<p>")
            (process-children)
            (! "</p>")
        )
        
    )
)]]>
