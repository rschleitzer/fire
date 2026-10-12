<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!-- The neutral parts are upstream's own files, included and not copied. -->
<!ENTITY config         SYSTEM "../upstream/codegen/config.scm">
<!ENTITY utilities      SYSTEM "../upstream/codegen/utilities.scm">
<!ENTITY general        SYSTEM "../upstream/codegen/general.scm">
<!ENTITY fhirmodel      SYSTEM "../upstream/codegen/fhirmodel.scm">
<!ENTITY fhirvalidator  SYSTEM "../upstream/codegen/fhirvalidator.scm">
<!ENTITY migration      SYSTEM "../upstream/codegen/migration.scm">
<!ENTITY dbupgrade      SYSTEM "../upstream/codegen/dbupgrade.scm">
<!ENTITY models         SYSTEM "../upstream/codegen/models.scm">
<!-- the pages and scripts of the web root: the same whatever serves them -->
<!ENTITY boilerplate    SYSTEM "../upstream/codegen/boilerplate.scm">
<!ENTITY bundlejs       SYSTEM "../upstream/codegen/bundlejs.scm">
<!ENTITY js             SYSTEM "../upstream/codegen/js.scm">
<!ENTITY html           SYSTEM "../upstream/codegen/html.scm">
<!ENTITY model          SYSTEM "model.scm">
<!ENTITY search         SYSTEM "search.scm">

]>

&config;
&utilities;
&general;
&fhirmodel;
&fhirvalidator;
&migration;
&dbupgrade;
&models;
&boilerplate;
&bundlejs;
&js;
&html;
&model;
&search;

(element fhir
    (make directory path: "server"
        (sosofo-append
            (scaly-program-file)
            (make directory path: (downcase-string (fhir-name))
                (sosofo-append
                    (scaly-model-file)
                    (process-node-list (select-children "resources" (current-node)))))
            (with-mode webroot
                (process-node-list (select-children "resources" (current-node)))))))

; upstream's schema functions walk the children of the node they are called
; at: the resources
(element resources (scaly-script-files))

; The web root, as upstream's rules lay it out: the scripts every page
; loads, a script and a page for each active resource, the list of them.
(mode webroot
    (element resources
        (make directory path: "wwwroot"
            (sosofo-append
                (make directory path: "js"
                    (sosofo-append
                        (bundle-js)
                        (operationoutcome-js)
                        (js-elements-file)))
                (html-resourcelist-file)
                (process-children))))
    (element resource
        (if (active? (current-node))
            (sosofo-append
                (make directory path: "js" (js-resource-file))
                (html-documentation-file))
            (empty-sosofo)))
    (default (empty-sosofo)))
