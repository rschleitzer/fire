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

The input both generators read: `fhir.xml`, made of HL7's FHIR R5
definitions by `tools/fhir-to-xml`, with the DTD it is valid against. It is
held fast, so a build needs neither the definitions nor a converter;
`tools/pin-model.sh` makes it anew, and `tools/pin-model.sh --check` says
whether the converter still writes it byte for byte.

`tools/fhir-to-xml` is upstream's converter (`upstream/tools/fhir-to-xml`,
C#) ported to Scaly, routine for routine; both write the same file.

## documentation/

The documentation of the Scaly server in DocBook. `tools/generate.sh` makes
its pages with upstream's stylesheet and puts them into the server's web
root, where the server serves them.

## packages/fhirclient

A client of the server for Scaly programs: the requests of the RESTful API
and searches built of typed criteria. The search parameters of the model's
resources are generated beside it (`packages/fireclient`, not in git).
`tools/measure.sh client` runs its test against both servers.

## Licence

MIT, `LICENSE`. `upstream/` carries its own, `upstream/LICENSE`, also MIT.
`tools/fhir-to-xml` is a port of upstream's converter: what it takes from
it stays under `upstream/LICENSE`. `model/fhir.xml` is what that port
writes of HL7's definitions; the DTD beside it is upstream's.

The server product this generator was developed for, its configuration and
any data are not in this repository.
