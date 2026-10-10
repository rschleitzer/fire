#!/bin/bash
# tools/generate.sh -- everything the Scaly server is built from that is
# generated, none of it in git:
#   server/fire/Model.scaly              the model's types and code sets, by
#                                        codegen/scaly.dsl from model/fhir.xml
#   server/fire/postgresql/Schema.scaly  upstream's schema (tools/schema.sh)
# dazzle is on the PATH, or named by $DAZZLE. tools/measure.sh runs this
# before it builds.
set -eu
cd "$(dirname "$0")/.."
export SP_ENCODING=utf-8
"${DAZZLE:-dazzle}" -t sgml -d codegen/scaly.dsl model/fhir.xml
[ -s server/fire/Model.scaly ] || { echo "generate: no server/fire/Model.scaly" >&2; exit 1; }
tools/schema.sh
