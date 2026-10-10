# fire

A FHIR server generated from the FHIR definitions, and its generator ported
to Scaly and dazzle. Private while it is being set up.

## upstream/

The starting point, unchanged, as a reference for the port:

- `fire.dtd` — the DTD of the XML model the generator reads.
- `codegen/` — the DSSSL generator that writes the C# server from that model
  (taken at commit ced8f5ce of its repository; the two generators for the
  admin interface's TypeScript are not part of it).
- `tools/fhir-to-xml/` — the C# tool that turns HL7's FHIR definitions into
  the XML model (from the archived first version of this repository, commit
  63fd799). It predates the current DTD and has to be brought up to it.

Nothing under `upstream/` is edited here. The server product these files
come from, its configuration and any data are not in this repository.

The licence of `upstream/` is being settled; until it is, this repository
stays private.
