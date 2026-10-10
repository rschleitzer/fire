#!/bin/bash
# model/ is the input both generators read: the XML model upstream's converter
# wrote, with the DTD it is valid against, held fast here. Run after
# upstream/fire.sh when the model is meant to move; the diff says what moved.
set -eu
cd "$(dirname "$0")/.."
[ -f upstream/fhir.xml ] || { echo "pin-model: no upstream/fhir.xml -- run upstream/fire.sh first" >&2; exit 1; }
mkdir -p model
cp upstream/fhir.xml upstream/fire.dtd upstream/xml.dcl upstream/catalog model/
git status --short model
