#!/bin/bash
# tools/generate.sh -- everything the Scaly server is built from that is
# generated, none of it in git:
#   server/fire.scaly         the program: the model handed to the runtime
#   server/fire/Model.scaly   the model as one text of records, by
#                             codegen/scaly.dsl from model/fhir.xml
#   server/fire/Schema.scaly  upstream's schema and index scripts as records
#                             (tools/schema.sh)
#   server/wwwroot/           the web root: scripts and pages of the model
#                             (codegen/scaly.dsl), the documentation
#                             (upstream's DocBook and its stylesheet), the
#                             style sheets and images
# The runtime itself is the package packages/fhir, written by hand.
# dazzle is on the PATH, or named by $DAZZLE. tools/measure.sh runs this
# before it builds.
set -eu
cd "$(dirname "$0")/.."
export SP_ENCODING=utf-8
rm -rf server/wwwroot
"${DAZZLE:-dazzle}" -t sgml -d codegen/scaly.dsl model/fhir.xml
"${DAZZLE:-dazzle}" -t sgml -d upstream/codegen/documentation.dsl documentation/fire.xml
mkdir -p server/wwwroot/css
cp upstream/codegen/css/* server/wwwroot/css/
cp upstream/codegen/img/* server/wwwroot/
[ -s server/wwwroot/index.html ] || { echo "generate: no server/wwwroot/index.html" >&2; exit 1; }
[ -s server/fire/Model.scaly ] || { echo "generate: no server/fire/Model.scaly" >&2; exit 1; }
tools/schema.sh fire
