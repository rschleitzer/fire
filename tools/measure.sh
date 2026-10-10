#!/bin/bash
# tools/measure.sh [scaly|csharp|both|turns] -- the yardstick: build a server,
# start it on a throwaway PostgreSQL in Docker, run pyrtest against it and
# print passed / total per test file; then remove the server and the container.
#
#   scaly    the Scaly server under server/ (the default)
#   csharp   the generated C# server under upstream/server
#   both     one after the other, each on a database of its own, side by side
#   turns    what pyrtest cannot say: both servers taking turns on ONE
#            database, four times. The Scaly server creates the schema; each
#            creates, updates, deletes and creates again what the other
#            wrote, and must answer -- current and from the history -- the
#            bytes the other stored; both must store the same bytes for the
#            same request but for the id and the time.
#
# The database is a container of this script's own on a port of its own;
# nothing else is touched. The Python environment for pyrtest is made on the
# first run, outside the repository.
#
#   PYRTEST         the pyrtest checkout          (~/repos/pyrtest)
#   FIRE_PG_PORT    the throwaway PostgreSQL      (55432)
#   FIRE_HTTP_PORT  the server under measurement  (5580)
#   FIRE_MEASURE    environment and results       (~/.cache/fire-measure)
#   FIRE_SCALY_HOME a Scaly checkout to build the Scaly server with, its own
#                   tool and packages, in place of the installed toolchain
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WHAT="${1:-scaly}"
PYRTEST="${PYRTEST:-$HOME/repos/pyrtest}"
PG_PORT="${FIRE_PG_PORT:-55432}"
HTTP_PORT="${FIRE_HTTP_PORT:-5580}"
WORK="${FIRE_MEASURE:-$HOME/.cache/fire-measure}"
PG_IMAGE=postgres:18
PG_NAME="fire-measure-pg-$$"
PG_PASSWORD=throwaway
BASE="http://localhost:$HTTP_PORT"

case "$WHAT" in
  scaly) TARGETS="scaly" ;;
  csharp) TARGETS="csharp" ;;
  both|turns) TARGETS="scaly csharp" ;;
  *) echo "usage: tools/measure.sh [scaly|csharp|both|turns]" >&2; exit 2 ;;
esac

die() { echo "measure: $*" >&2; exit 1; }

[ "$PG_PORT" != 5432 ] || die "5432 is not a throwaway port"
[ -f "$PYRTEST/requirements.txt" ] || die "no pyrtest at $PYRTEST"
command -v docker > /dev/null || die "docker is not on the PATH"

listening() { lsof -nP -iTCP:"$1" -sTCP:LISTEN > /dev/null 2>&1; }

SERVER_PID=
stop_server() {
  if [ -n "$SERVER_PID" ]; then
    kill "$SERVER_PID" 2> /dev/null
    wait "$SERVER_PID" 2> /dev/null
    SERVER_PID=
  fi
}
stop_database() { docker rm -f -v "$PG_NAME" > /dev/null 2>&1; }
trap 'stop_server; stop_database' EXIT
trap 'exit 130' INT TERM

# the Python environment, renewed when pyrtest's requirements change
mkdir -p "$WORK"
VENV="$WORK/venv"
if ! cmp -s "$PYRTEST/requirements.txt" "$VENV/requirements.txt"; then
  echo "measure: making the Python environment in $VENV"
  rm -rf "$VENV"
  python3 -m venv "$VENV" > "$WORK/venv.log" 2>&1 \
    && "$VENV/bin/pip" install -q -r "$PYRTEST/requirements.txt" >> "$WORK/venv.log" 2>&1 \
    && cp "$PYRTEST/requirements.txt" "$VENV/requirements.txt" \
    || die "the Python environment failed, see $WORK/venv.log"
fi

build_scaly() {
  "$ROOT/tools/schema.sh" || die "no schema"
  mkdir -p "$ROOT/server/build"
  if [ -n "${FIRE_SCALY_HOME:-}" ]; then
    echo "measure: scaly -- built with the checkout $FIRE_SCALY_HOME, not the installed toolchain"
    (cd "$ROOT/server" && SCALY_HOME="$FIRE_SCALY_HOME" "$FIRE_SCALY_HOME/scalyc/build/scaly" build fire.scaly -o build/fire) > "$WORK/scaly.build.log" 2>&1 \
      || { cat "$WORK/scaly.build.log" >&2; die "the Scaly server does not build"; }
    return
  fi
  command -v scaly > /dev/null || die "scaly is not on the PATH"
  (cd "$ROOT/server" && env -u SCALY_HOME scaly build fire.scaly -o build/fire) > "$WORK/scaly.build.log" 2>&1 \
    || { cat "$WORK/scaly.build.log" >&2; die "the Scaly server does not build"; }
}

build_csharp() {
  [ -f "$ROOT/upstream/server/Fire.csproj" ] \
    || die "upstream/server is not generated -- run upstream/fire.sh first"
  dotnet build "$ROOT/upstream/server/Fire.csproj" -v q --nologo > "$WORK/csharp.build.log" 2>&1 \
    || { tail -20 "$WORK/csharp.build.log" >&2; die "the C# server does not build"; }
}

start_database() {
  listening "$PG_PORT" && die "port $PG_PORT is taken (FIRE_PG_PORT names another)"
  docker run -d --rm --name "$PG_NAME" -e POSTGRES_PASSWORD="$PG_PASSWORD" -e POSTGRES_DB=Fire \
    -p "127.0.0.1:$PG_PORT:5432" "$PG_IMAGE" > /dev/null || die "the PostgreSQL container does not start"
  # the image starts a server to initialize, stops it and starts the real
  # one: ready is the one that listens on TCP
  local i=0
  until docker exec "$PG_NAME" pg_isready -q -h 127.0.0.1 -U postgres -d Fire 2> /dev/null; do
    i=$((i+1)); [ $i -lt 120 ] || die "PostgreSQL is not ready after 60 s"
    sleep 0.5
  done
}

start_server() {
  listening "$HTTP_PORT" && die "port $HTTP_PORT is taken (FIRE_HTTP_PORT names another)"
  export DatabaseType=PostgreSql
  export ConnectionString="Host=localhost;Port=$PG_PORT;Database=Fire;Username=postgres;Password=$PG_PASSWORD"
  export Kestrel__Endpoints__MyHttpEndpoint__Url="$BASE"
  export externalUrl="$BASE/fire"
  if [ "$1" = scaly ]; then
    "$ROOT/server/build/fire" > "$WORK/$1.server.log" 2>&1 &
  else
    (cd "$ROOT/upstream/server" && exec dotnet bin/Debug/net10.0/Fire.dll) > "$WORK/$1.server.log" 2>&1 &
  fi
  SERVER_PID=$!
  local i=0
  until curl -s -o /dev/null "$BASE/fire/metadata"; do
    kill -0 "$SERVER_PID" 2> /dev/null \
      || { tail -20 "$WORK/$1.server.log" >&2; SERVER_PID=; die "the $1 server ended at its start"; }
    i=$((i+1)); [ $i -lt 240 ] || die "the $1 server does not answer after 120 s"
    sleep 0.5
  done
}

# pyrtest's cases per file out of the JUnit report: passed and total
tally() {
  "$VENV/bin/python" - "$1" << 'EOF'
import sys, collections, xml.etree.ElementTree as ET
passed, total = collections.Counter(), collections.Counter()
for case in ET.parse(sys.argv[1]).iter('testcase'):
    parts = case.get('classname', '').split('.')
    name = next((p for p in parts if p.startswith('test_')), parts[0]) + '.py'
    total[name] += 1
    if not any(child.tag in ('failure', 'error', 'skipped') for child in case):
        passed[name] += 1
for name in sorted(total):
    print(name, passed[name], total[name])
EOF
}

measure() {
  echo "measure: $1 -- build"
  "build_$1"
  echo "measure: $1 -- PostgreSQL on port $PG_PORT, the server on port $HTTP_PORT"
  start_database
  start_server "$1"
  echo "measure: $1 -- pyrtest"
  rm -f "$WORK/$1.xml"
  (cd "$PYRTEST" && FHIR_BASE_URL="$BASE/fire" PYTHONDONTWRITEBYTECODE=1 \
    "$VENV/bin/python" -m pytest -q -p no:cacheprovider -o addopts= --tb=no --disable-warnings \
      --junitxml="$WORK/$1.xml") > "$WORK/$1.pytest.log" 2>&1
  stop_server
  stop_database
  [ -s "$WORK/$1.xml" ] || { tail -20 "$WORK/$1.pytest.log" >&2; die "pyrtest wrote no report for $1"; }
  tally "$WORK/$1.xml" > "$WORK/$1.tally" || die "the report of $1 cannot be read"
  # pytest's own summary line, the oracle
  echo "measure: $1 -- $(tail -1 "$WORK/$1.pytest.log" | sed 's/^=* *//; s/ *=*$//')"
}

# One request of the turns to the running server: its body is kept under
# the label, its status must be the expected one.
#   call <label> <status> <method> <path> [<fixture>]
call() {
  local out="$WORK/turns.$1.body" status
  if [ $# -ge 5 ]; then
    status=$(curl -s -o "$out" -w '%{http_code}' -X "$3" -H 'Content-Type: application/fhir+json' \
      --data-binary @"$ROOT/tools/turns/$5" "$BASE/fire/$4")
  else
    status=$(curl -s -o "$out" -w '%{http_code}' -X "$3" "$BASE/fire/$4")
  fi
  [ "$status" = "$2" ] && return
  echo "turns: FAIL -- $1: $3 $4 answered $status, not $2"; TURNS_FAILED=1
}

# The id of the resource the answer under a label holds.
id_of() { sed -n 's/.*"id":"\([^"]*\)".*/\1/p' "$WORK/turns.$1.body"; }

# Two answers must be the same bytes: one server answers what the other stored.
same() {
  cmp -s "$WORK/turns.$1.body" "$WORK/turns.$2.body" && return
  echo "turns: FAIL -- $1 and $2 differ (diff $WORK/turns.$1.body $WORK/turns.$2.body)"; TURNS_FAILED=1
}

# A stored resource without what differs by nature: the id and the time.
neutral() { sed 's/"id":"[^"]*"/"id":""/; s/"lastUpdated":"[^"]*"/"lastUpdated":""/' "$WORK/turns.$1.body"; }

# Two answers must be the same bytes but for the id and the time: both
# servers store the same for the same request.
alike() {
  [ "$(neutral "$1")" = "$(neutral "$2")" ] && return
  echo "turns: FAIL -- $1 and $2 differ beyond id and time (diff $WORK/turns.$1.body $WORK/turns.$2.body)"; TURNS_FAILED=1
}

# What the tables hold of a resource, in the order it was written.
rows_of() {
  docker exec "$PG_NAME" psql -U postgres -d Fire -Atc "
    select version_id, \"active\", \"gender\", length(content), history_operation
      from \"Patient\".resource_history where id = '$1'
    union all
    select version_id, \"active\", \"gender\", length(content), 'CURRENT' from \"Patient\" where id = '$1'
    order by 1"
}

turns() {
  TURNS_FAILED=
  echo "measure: turns -- build"
  build_scaly
  build_csharp
  echo "measure: turns -- PostgreSQL on port $PG_PORT, the servers on port $HTTP_PORT"
  start_database
  local a b

  echo "turns: 1 scaly   -- creates the schema, creates A, updates A"
  start_server scaly
  call s.create 201 POST Patient patient.json;           a=$(id_of s.create)
  call s.update 200 PUT "Patient/$a" patient-update.json
  stop_server

  echo "turns: 2 csharp  -- reads A and its first version, updates A; creates, updates and deletes B"
  start_server csharp
  call c.read-a 200 GET "Patient/$a";                    same c.read-a s.update
  call c.vread-a1 200 GET "Patient/$a/_history/1";       same c.vread-a1 s.create
  call c.update-a 200 PUT "Patient/$a" patient.json
  call c.create 201 POST Patient patient.json;           b=$(id_of c.create)
  call c.update 200 PUT "Patient/$b" patient-update.json
  call c.delete 204 DELETE "Patient/$b"
  stop_server

  echo "turns: 3 scaly   -- reads A and its versions, deletes A; finds B deleted, reads its versions, creates it again"
  start_server scaly
  call s.read-a 200 GET "Patient/$a";                    same s.read-a c.update-a
  call s.vread-a2 200 GET "Patient/$a/_history/2";       same s.vread-a2 s.update
  call s.vread-a3 200 GET "Patient/$a/_history/3";       same s.vread-a3 c.update-a
  call s.delete 204 DELETE "Patient/$a"
  call s.read-b 410 GET "Patient/$b"
  call s.vread-b1 200 GET "Patient/$b/_history/1";       same s.vread-b1 c.create
  call s.vread-b2 200 GET "Patient/$b/_history/2";       same s.vread-b2 c.update
  call s.recreate 201 PUT "Patient/$b" patient.json
  stop_server

  echo "turns: 4 csharp  -- finds A deleted, reads its last version; reads B as created again; creates A again"
  start_server csharp
  call c.read-a2 410 GET "Patient/$a"
  call c.vread-a3 200 GET "Patient/$a/_history/3";       same c.vread-a3 c.update-a
  call c.read-b 200 GET "Patient/$b";                    same c.read-b s.recreate
  call c.recreate 201 PUT "Patient/$a" patient.json
  stop_server

  # the same request, the same bytes
  alike s.create c.create
  alike s.update c.update
  rows_of "$a" > "$WORK/turns.rows.a"
  rows_of "$b" > "$WORK/turns.rows.b"
  stop_database
  # A: created and updated by scaly, updated by csharp, deleted by scaly,
  # created again by csharp; B the other way round, without the second update
  local size size_update
  size=$(wc -c < "$WORK/turns.s.create.body" | tr -d ' ')
  size_update=$(wc -c < "$WORK/turns.s.update.body" | tr -d ' ')
  printf '%s\n' "1|t|male|$size|UPDATE" "2|f|female|$size_update|UPDATE" "3|t|male|$size|DELETE" "4|t|male|$size|CURRENT" > "$WORK/turns.rows.a.expected"
  printf '%s\n' "1|t|male|$size|UPDATE" "2|f|female|$size_update|DELETE" "3|t|male|$size|CURRENT" > "$WORK/turns.rows.b.expected"
  for r in a b; do
    cmp -s "$WORK/turns.rows.$r" "$WORK/turns.rows.$r.expected" && continue
    echo "turns: FAIL -- the rows of ${r} are not the expected ones:"; diff "$WORK/turns.rows.$r" "$WORK/turns.rows.$r.expected"; TURNS_FAILED=1
  done
  [ -z "$TURNS_FAILED" ] || { echo "turns: FAILED"; exit 1; }
  echo "turns: passed -- 20 requests, 9 answers the same bytes across the servers, 2 pairs alike but for id and time, 7 rows as expected"
  exit 0
}

[ "$WHAT" != turns ] || turns

for t in $TARGETS; do measure "$t"; done

echo
"$VENV/bin/python" - "$WORK" $TARGETS << 'EOF'
import sys
work, targets = sys.argv[1], sys.argv[2:]
rows, files = {}, []
for t in targets:
    for line in open(f'{work}/{t}.tally'):
        name, passed, total = line.split()
        if name not in files:
            files.append(name)
        rows[(name, t)] = (int(passed), int(total))
files.sort()
width = max(len(f) for f in files + ['all'])
print(' ' * width + ''.join(f'{t:>13}' for t in targets))
sums = {t: [0, 0] for t in targets}
for f in files:
    line = f'{f:<{width}}'
    for t in targets:
        p, n = rows.get((f, t), (0, 0))
        sums[t][0] += p
        sums[t][1] += n
        line += f'{p:>7} / {n:<3}'
    print(line.rstrip())
print((f'{"all":<{width}}' + ''.join(f'{sums[t][0]:>7} / {sums[t][1]:<3}' for t in targets)).rstrip())
EOF
