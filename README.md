# fire

A FHIR R5 server generated from HL7's definitions — written in
[Scaly](https://scaly.io), and measured against the C# server the same
generator was first written for.

The Scaly server is a drop-in replacement of the C# one: it answers the same
requests with the same bytes and keeps the same tables with the same
contents, on PostgreSQL, SQLite and SQL Server, so either server can go on
with a database the other one wrote. That makes the two comparable request
for request, and the comparison is what this repository is for.

## What it does in a second

Both servers built for release, each on a PostgreSQL 18 of its own with
2000 Patients and 2000 Observations (`tools/throughput.sh postgresql`,
15 seconds a scenario, 64 connections; an M-series Mac with four
performance and six efficiency cores, the database in Docker, the load
generator on the same machine). Each figure is the median of three runs on
an otherwise idle machine.

Both servers prepare the statements they send again and again: the Scaly
server by name, the C# server through its driver (the generator sets
`Max Auto Prepare` and `No Reset On Close` where the connection string says
nothing else — before, Npgsql had PostgreSQL parse and plan every query
anew, and the C# server answered the searches 20 to 35 per cent slower).

| scenario | Scaly req/s | C# req/s | a second | Scaly instructions | C# instructions | fewer |
|---|---|---|---|---|---|---|
| metadata (no database) | 31 163 | 25 483 | 1.2x | 260 k | 643 k | 2.5x |
| read a Patient | 12 378 | 11 570 | 1.1x | 591 k | 937 k | 1.6x |
| search by name | 3 326 | 3 024 | 1.1x | 1 278 k | 1 824 k | 1.4x |
| search by reference | 7 598 | 7 044 | 1.1x | 993 k | 1 290 k | 1.3x |
| update | 2 493 | 2 605 | 1.0x | 2 500 k | 2 591 k | 1.0x |
| create | 3 267 | 3 734 | 0.9x | 1 813 k | 1 737 k | 1.0x |

"instructions" are what the server process retired for one request, as the
kernel counts them (`tools/throughput/rusage.py`); the three runs of a
figure agree within one per cent. Memory after a run: 18 to 24 MB for the
Scaly server, 221 to 244 MB for the C# server.

How to read it: the load generator shares the machine and is the limit of
the first scenario; the database is the limit of every scenario that
queries, and as both servers send it the same work they answer about as
many requests a second. What differs is what the server itself costs, and
that is counted in instructions, not in processor time: a second of
processor time on an efficiency core is worth far less than on a
performance core, and the system puts the two servers' threads on either
as it likes. In instructions the Scaly server is 1.3 to 2.5 times cheaper
for what reads, the more so the less a request leaves to the database; a
write costs both about the same — most of it is the work of sending the
statements that fill the search tables, which both servers send in one
round trip (one each, before, made a write four times as expensive). The
requests a second of a whole series vary with the machine by 20 to 30 per
cent; compare within a row.

## How it is made

```
model/fhir.xml          HL7's definitions as one XML model (tools/fhir-to-xml)
codegen/                the generator, DSSSL, run by dazzle
packages/fhir/          the runtime: a Scaly package, written by hand, that
                        knows no resource type
server/                 GENERATED: the program, the model as records, the SQL
                        of the schema, the web root with pages and scripts
packages/fhirclient/    a client for Scaly programs
documentation/          the server's documentation (DocBook)
tools/                  generating, measuring, the converter
upstream/               the C# side: its generator and what builds its server
```

The generator writes no code that knows a resource: it writes the model as
data, and the runtime serves whatever model it is handed. Nothing generated
is kept in git.

## Building and running

It needs the Scaly toolchain (`curl -fsSL https://scaly.io/install.sh | sh`)
and [dazzle](https://github.com/rschleitzer/dazzle), the DSSSL processor.

```bash
tools/generate.sh
scaly build server/fire.scaly -o server/build/fire

DatabaseType=Sqlite ConnectionString="Data Source=fire.db" \
ASPNETCORE_WEBROOT=server/wwwroot server/build/fire
```

The server listens on port 5080; `http://localhost:5080/` is its
documentation, `http://localhost:5080/fire/Patient` a search. The settings
have the names the C# server reads them by and are taken from the
environment: `DatabaseType` (`PostgreSql`, `SqlServer`, `Sqlite`),
`ConnectionString`, `Kestrel__Endpoints__MyHttpEndpoint__Url`. The
documentation's chapter on deployment has the rest.

It speaks HTTP without TLS, on the loopback address, and authenticates
nobody: it is meant to stand behind something that does.

## Measuring

The tests put the two servers side by side. Each starts a database of its
own — a container for PostgreSQL and SQL Server, a file for SQLite
(`FIRE_DATABASE=postgresql|sqlite|sqlserver`) — and needs Docker, dotnet 10
for the C# server and, for the first, a checkout of
[pyrtest](https://github.com/rschleitzer/pyrtest).

| | |
|---|---|
| `tools/measure.sh both` | pyrtest, 259 black-box tests over HTTP, against each server |
| `tools/measure.sh turns` | the servers take turns on ONE database: more than 400 requests and 239 writing steps must be answered alike (status, content type, body), and every row either writes must be the same |
| `tools/measure.sh client` | the client's test against each server, and each server's orderly end on SIGTERM |
| `tools/variant-test.sh`, `tools/upgrade-test.sh` | the same comparison for a changed model, and for a database lifted by upgrade steps |
| `tools/throughput.sh` | the table above |

Passing pyrtest does not mean answering alike — the turns found differences
the suite had not —, and where the C# server does something it should not,
the Scaly server does the same as long as an answer or a stored row would
differ otherwise. The documentation lists what a user can see of that.

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

`documentation/fire.xml` follows the outline of upstream's documentation and
takes its passages on browsing and editing from it: those stay under
`upstream/LICENSE` as well.

The server product this generator was developed for, its configuration and
any data are not in this repository.
