#!/bin/bash
# The FHIR R5 definitions of HL7, the converter's input: fetched, not kept.
set -eu
cd "$(dirname "$0")/.."
[ -f fhir-r5/profiles-resources.json ] && { echo "fhir-r5: there"; exit 0; }
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
curl -fsSL --retry 3 -o "$tmp/definitions.zip" https://hl7.org/fhir/R5/definitions.json.zip
mkdir -p fhir-r5
unzip -q -o "$tmp/definitions.zip" -d fhir-r5
cat fhir-r5/version.info
