<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!-- The neutral parts are upstream's own files, included and not copied. -->
<!ENTITY config         SYSTEM "../upstream/codegen/config.scm">
<!ENTITY utilities      SYSTEM "../upstream/codegen/utilities.scm">
<!ENTITY general        SYSTEM "../upstream/codegen/general.scm">
<!ENTITY fhirmodel      SYSTEM "../upstream/codegen/fhirmodel.scm">
<!ENTITY fhirvalidator  SYSTEM "../upstream/codegen/fhirvalidator.scm">
<!ENTITY migration      SYSTEM "../upstream/codegen/migration.scm">
<!ENTITY models         SYSTEM "../upstream/codegen/models.scm">
<!ENTITY model          SYSTEM "model.scm">
<!ENTITY search         SYSTEM "search.scm">

]>

&config;
&utilities;
&general;
&fhirmodel;
&fhirvalidator;
&migration;
&models;
&model;
&search;

(element fhir
    (make directory path: "server"
        (sosofo-append
            (scaly-program-file)
            (make directory path: (downcase-string (fhir-name))
                (sosofo-append
                    (scaly-model-file)
                    (process-node-list (select-children "resources" (current-node))))))))

; upstream's schema functions walk the children of the node they are called
; at: the resources
(element resources (scaly-script-files))
