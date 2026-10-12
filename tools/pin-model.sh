#!/bin/bash
# tools/pin-model.sh [--check] -- model/ is the input both generators read:
# fhir.xml, made of HL7's FHIR R5 definitions by tools/fhir-to-xml (the port
# of upstream's converter to Scaly: no dotnet), with the DTD it is valid
# against, held fast here. Run it when the model is meant to move; the diff
# says what moved.
#   --check   write nothing: say whether the converter still writes the
#             model that is held here, byte for byte -- and, where dotnet
#             and upstream's generated fhir.xml are there, whether
#             upstream's converter does
# The definitions are fetched once into upstream/fhir-r5 (ignored).
set -eu
cd "$(dirname "$0")/.."
upstream/tools/fetch-definitions.sh > /dev/null
mkdir -p server/build model
scaly build tools/fhir-to-xml/fhir-to-xml.scaly -o server/build/fhir-to-xml --release
if [ "${1:-}" = --check ]; then
  out="$(mktemp)"; trap 'rm -f "$out"' EXIT
  server/build/fhir-to-xml upstream/fhir-r5 "$out"
  cmp "$out" model/fhir.xml || { echo "pin-model: the converter does NOT write model/fhir.xml" >&2; exit 1; }
  echo "pin-model: the converter writes model/fhir.xml, byte for byte ($(wc -c < model/fhir.xml | tr -d ' ') bytes)"
  if [ -f upstream/fhir.xml ]; then
    cmp upstream/fhir.xml model/fhir.xml && echo "pin-model: and so does upstream's (upstream/fhir.xml)" \
      || { echo "pin-model: upstream/fhir.xml differs from model/fhir.xml" >&2; exit 1; }
  fi
  exit 0
fi
server/build/fhir-to-xml upstream/fhir-r5 model/fhir.xml
cp upstream/fire.dtd upstream/xml.dcl upstream/catalog model/
git status --short model
