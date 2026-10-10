# fire

A FHIR server generated from the FHIR definitions, and its generator ported
to Scaly and dazzle. Private while it is being set up.

## upstream/

The C# side, whole and by itself: everything that is C# or makes C# lies
here and nowhere else.

- `fire.dtd` — the DTD of the XML model the generator reads.
- `codegen/` — the DSSSL generator that writes the C# server, its model and
  its client from that model (taken at commit ced8f5ce of the repository it
  was developed in; the two generators for an admin interface's TypeScript
  are not part of it).
- `tools/fhir-to-xml/` — the C# tool that turns HL7's FHIR R5 definitions
  into the XML model.
- `documentation/` — the server's documentation in DocBook; the generator
  writes its pages into the server's `wwwroot`.
- `test/`, `fire.sln`, `Dockerfile`, `docker_build` — the test
  project and what builds and ships the server.

`upstream/fire.sh` runs all of it: it fetches the definitions, writes
`fhir.xml`, and generates `server/`, `model/` and `client/` — 322 files,
none of them kept in git (`upstream/.gitignore`). The directories are made
by the generator's directory flow objects; `dotnet build fire.sln` builds
what it wrote. It needs dotnet 10 and dazzle (`DAZZLE` names one that is not
on the PATH).

Licence of `upstream/`: MIT, `upstream/LICENSE`.

## model/

The input both generators read: `fhir.xml` as upstream's converter wrote it,
with the DTD it is valid against. It is a copy held fast, so the port needs
neither dotnet nor the definitions; `tools/pin-model.sh` renews it after a
run of `upstream/fire.sh`.

## Licence

MIT, `LICENSE`. `upstream/` carries its own, `upstream/LICENSE`, also MIT,
and `model/` is under that one: it is what upstream's converter wrote.

The server product this generator was developed for, its configuration and
any data are not in this repository.
