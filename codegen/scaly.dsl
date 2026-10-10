<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!-- The neutral parts are upstream's own files, included and not copied. -->
<!ENTITY utilities      SYSTEM "../upstream/codegen/utilities.scm">
<!ENTITY general        SYSTEM "../upstream/codegen/general.scm">
<!ENTITY fhirmodel      SYSTEM "../upstream/codegen/fhirmodel.scm">
<!ENTITY fhirvalidator  SYSTEM "../upstream/codegen/fhirvalidator.scm">
<!ENTITY model          SYSTEM "model.scm">

]>

&utilities;
&general;
&fhirmodel;
&fhirvalidator;
&model;

(element fhir
    (make directory path: "server"
        (make directory path: "fire"
            (scaly-model-file))))
