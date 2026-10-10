#!/bin/bash
# The whole generation, from HL7's definitions to the C# server:
#   the definitions -> fhir.xml (tools/fhir-to-xml) -> server/, model/, client/
# Nothing it writes is kept in git. dazzle is on the PATH, or named by $DAZZLE.
set -eu
cd "$(dirname "$0")"
export SP_ENCODING=utf-8
dazzle="${DAZZLE:-dazzle}"
tools/fetch-definitions.sh
dotnet run --project tools/fhir-to-xml
"$dazzle" -t sgml -d codegen/map.dsl fhir.xml
"$dazzle" -t sgml -d codegen/documentation.dsl documentation/fire.xml
mkdir -p server/wwwroot/css
cp codegen/css/* server/wwwroot/css/
cp codegen/img/* server/wwwroot/
