#!/bin/bash
export SP_ENCODING=utf-8
openjade -t sgml -d codegen/map.dsl fhir.xml
