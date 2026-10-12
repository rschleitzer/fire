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
#   FIRE_DATABASE   postgresql (the default), sqlite or sqlserver
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
# the database under both servers: postgresql (a throwaway container), or
# sqlite (a file below $WORK)
DATABASE="${FIRE_DATABASE:-postgresql}"
SQLITE_FILE="$WORK/sqlite/fire.db"
MSSQL_IMAGE=mcr.microsoft.com/mssql/server:2022-latest MSSQL_PORT="${FIRE_MSSQL_PORT:-51433}" MSSQL_PASSWORD='Throwaway!1'
# a query on the throwaway SQL Server, a row a line, | between the columns
mssql() { docker exec "$PG_NAME" /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P "$MSSQL_PASSWORD" -C -h -1 -W -s '|' -d "${MSSQL_DATABASE:-master}" -Q "set nocount on; $1"; }

case "$WHAT" in
  scaly) TARGETS="scaly" ;;
  csharp) TARGETS="csharp" ;;
  both|turns) TARGETS="scaly csharp" ;;
  *) echo "usage: tools/measure.sh [scaly|csharp|both|turns]" >&2; exit 2 ;;
esac

die() { echo "measure: $*" >&2; exit 1; }

[ "$PG_PORT" != 5432 ] || die "5432 is not a throwaway port"
[ -f "$PYRTEST/requirements.txt" ] || die "no pyrtest at $PYRTEST"
[ "${FIRE_DATABASE:-postgresql}" = sqlite ] || command -v docker > /dev/null || die "docker is not on the PATH"

listening() { lsof -nP -iTCP:"$1" -sTCP:LISTEN > /dev/null 2>&1; }

SERVER_PID=
stop_server() {
  if [ -n "$SERVER_PID" ]; then
    kill "$SERVER_PID" 2> /dev/null
    wait "$SERVER_PID" 2> /dev/null
    SERVER_PID=
  fi
}
stop_database() { [ "$DATABASE" = sqlite ] || docker rm -f -v "$PG_NAME" > /dev/null 2>&1; }
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
  "$ROOT/tools/generate.sh" || die "the generated sources are not made"
  mkdir -p "$ROOT/server/build"
  if [ -n "${FIRE_SCALY_HOME:-}" ]; then
    echo "measure: scaly -- built with the checkout $FIRE_SCALY_HOME, not the installed toolchain"
    (cd "$ROOT" && SCALY_HOME="$FIRE_SCALY_HOME" "$FIRE_SCALY_HOME/scalyc/build/scaly" build server/fire.scaly -o server/build/fire) > "$WORK/scaly.build.log" 2>&1 \
      || { cat "$WORK/scaly.build.log" >&2; die "the Scaly server does not build"; }
    return
  fi
  command -v scaly > /dev/null || die "scaly is not on the PATH"
  # from the repository's root: the package fhir is looked for in ./packages
  (cd "$ROOT" && env -u SCALY_HOME scaly build server/fire.scaly -o server/build/fire) > "$WORK/scaly.build.log" 2>&1 \
    || { cat "$WORK/scaly.build.log" >&2; die "the Scaly server does not build"; }
}

build_csharp() {
  [ -f "$ROOT/upstream/server/Fire.csproj" ] \
    || die "upstream/server is not generated -- run upstream/fire.sh first"
  dotnet build "$ROOT/upstream/server/Fire.csproj" -v q --nologo > "$WORK/csharp.build.log" 2>&1 \
    || { tail -20 "$WORK/csharp.build.log" >&2; die "the C# server does not build"; }
}

start_database() {
  if [ "$DATABASE" = sqlite ]; then
    # a file of this script's own, made anew
    rm -rf "$WORK/sqlite"; mkdir -p "$WORK/sqlite"
    return
  fi
  if [ "$DATABASE" = sqlserver ]; then
    # (Microsoft's image is for x86-64; on arm64 Docker runs it emulated)
    listening "$MSSQL_PORT" && die "port $MSSQL_PORT is taken"
    docker run -d --rm --platform linux/amd64 --name "$PG_NAME" -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD="$MSSQL_PASSWORD" \
      -p "127.0.0.1:$MSSQL_PORT:1433" "$MSSQL_IMAGE" > /dev/null || die "the SQL Server container does not start"
    local j=0
    until mssql "select 1" > /dev/null 2>&1; do
      j=$((j+1)); [ $j -lt 120 ] || die "SQL Server is not ready after 120 s"
      sleep 1
    done
    return
  fi
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
  if [ "$DATABASE" = sqlite ]; then
    export DatabaseType=Sqlite
    export ConnectionString="Data Source=$SQLITE_FILE"
  fi
  if [ "$DATABASE" = sqlserver ]; then
    export DatabaseType=SqlServer
    export ConnectionString="Server=localhost,$MSSQL_PORT;Database=Fire;User Id=sa;Password=$MSSQL_PASSWORD;TrustServerCertificate=True"
  fi
  export Kestrel__Endpoints__MyHttpEndpoint__Url="$BASE"
  # the seed data both servers load into a database they create, and the
  # secret of the reseed route
  export SeedDataPath="$ROOT/tools/turns/seed"
  export ReseedSecret=turns-secret
  export externalUrl="$BASE/fire"
  if [ "$1" = scaly ]; then
    # each server its own web root, written by its own generator
    ASPNETCORE_WEBROOT="$ROOT/server/wwwroot" "$ROOT/server/build/fire" > "$WORK/$1.server.log" 2>&1 &
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

# What a request of the turns accepts: FHIR, as pyrtest's do.
ACCEPT=application/fhir+json

# One request of the turns to the running server: its body is kept under
# the label, its status must be the expected one.
#   call <label> <status> <method> <path> [<fixture>]
call() {
  local out="$WORK/turns.$1.body" status
  if [ $# -ge 5 ]; then
    status=$(curl -s -o "$out" -w '%{http_code}' -X "$3" -H "Accept: $ACCEPT" -H 'Content-Type: application/fhir+json' \
      --data-binary @"$ROOT/tools/turns/$5" "$BASE/fire/$4")
  else
    status=$(curl -s -o "$out" -w '%{http_code}' -X "$3" -H "Accept: $ACCEPT" "$BASE/fire/$4")
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

# On SQLite: a query on the file, and the columns of a table but the named
# ones, each quoted and with | between -- what a row is compared by.
lite() { sqlite3 "$SQLITE_FILE" "$@"; }
lite_columns() {
  local table=$1 not; shift; not=$(printf "'%s'," "$@")
  lite "select group_concat('quote(' || name || ')', ' || ''|'' || ') from pragma_table_info('$table') where name not in (${not%,})"
}

# On SQL Server: a query on the database Fire, and the columns of a table
# but the named ones as one expression -- each as text, NULL said so, with |
# between.
fire_mssql() { MSSQL_DATABASE=Fire mssql "$1" | sed 's/[[:space:]]*$//'; }
mssql_columns() {
  local table=$1 not; shift; not=$(printf "'%s'," "$@")
  fire_mssql "select string_agg('isnull(cast(' + quotename(name) + ' as nvarchar(max)), ''NULL'')', ' + ''|'' + ') within group (order by column_id) from sys.columns where object_id = object_id('$table') and name not in (${not%,})"
}

# What the tables hold of a resource, in the order it was written.
rows_of() {
  if [ "$DATABASE" = sqlite ]; then
    lite "select version_id, active, gender, length(content), history_operation from Patient_resource_history where id = '$1'
          union all select version_id, active, gender, length(content), 'CURRENT' from Patient where id = '$1' order by 1"
    return
  fi
  if [ "$DATABASE" = sqlserver ]; then
    fire_mssql "select version_id, cast(active as int), gender, datalength(content), history_operation from [Patient].resource_history where id = '$1'
          union all select version_id, cast(active as int), gender, datalength(content), 'CURRENT' from [Patient] where id = '$1' order by 1"
    return
  fi
  docker exec "$PG_NAME" psql -U postgres -d Fire -Atc "
    select version_id, \"active\", \"gender\", length(content), history_operation
      from \"Patient\".resource_history where id = '$1'
    union all
    select version_id, \"active\", \"gender\", length(content), 'CURRENT' from \"Patient\" where id = '$1'
    order by 1"
}

# The other types, in three steps over the first three turns: each server
# creates and updates one resource of the type; the other reads both
# versions and must answer the same bytes; and what the two wrote must be
# alike, in the answers and in the rows.
#   others <step> <server: s or c> <the other: c or s>
OTHERS="Binary:binary Observation:observation Practitioner:practitioner Provenance:provenance"
others() {
  local t type file me="$2" other="$3"
  for t in $OTHERS; do
    type=${t%%:*}; file=${t##*:}
    if [ "$1" != create ]; then
      call "$me.$type.read" 200 GET "$type/$(id_of "$other.$type.create")";            same "$me.$type.read" "$other.$type.update"
      call "$me.$type.vread" 200 GET "$type/$(id_of "$other.$type.create")/_history/1"; same "$me.$type.vread" "$other.$type.create"
    fi
    if [ "$1" != read ]; then
      call "$me.$type.create" 201 POST "$type" "$file.json"
      call "$me.$type.update" 200 PUT "$type/$(id_of "$me.$type.create")" "$file-update.json"
    fi
  done
}

# Binary beyond the others: its data with white space in it is stored as
# its bytes and answered encoded anew; asked without a wish for FHIR, the
# bytes themselves are the answer, under their own content type.
#   binary <server: s or c>
binary() {
  call "$1.Binary.spaced" 201 POST Binary binary-spaced.json
  call "$1.Binary.spaced.read" 200 GET "Binary/$(id_of "$1.Binary.spaced")"
  ACCEPT='*/*' call "$1.Binary.spaced.raw" 200 GET "Binary/$(id_of "$1.Binary.spaced")"
  ACCEPT='*/*' call "$1.Binary.spaced.format" 200 GET "Binary/$(id_of "$1.Binary.spaced")?_format=json"
  curl -s -o /dev/null -D "$WORK/turns.$1.Binary.spaced.raw.head" -H 'Accept: */*' "$BASE/fire/Binary/$(id_of "$1.Binary.spaced")"
  # (on SQLite the data stays in the content: what is compared is that)
  if [ "$DATABASE" = sqlite ]; then
    lite "select version_id, content from Binary where id = '$(id_of "$1.Binary.spaced")'" \
      | sed 's/"id":"[^"]*"/"id":""/; s/"lastUpdated":"[^"]*"/"lastUpdated":""/' > "$WORK/turns.$1.Binary.data"
    return
  fi
  if [ "$DATABASE" = sqlserver ]; then
    docker exec "$PG_NAME" /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P "$MSSQL_PASSWORD" -C -y 0 -d Fire \
      -Q "set nocount on; select cast(version_id as varchar(10)) + '|' + cast(content as varchar(max)) from [Binary] where id = '$(id_of "$1.Binary.spaced")'" \
      | sed 's/"id":"[^"]*"/"id":""/; s/"lastUpdated":"[^"]*"/"lastUpdated":""/; s/[[:space:]]*$//' > "$WORK/turns.$1.Binary.data"
    return
  fi
  docker exec "$PG_NAME" psql -U postgres -d Fire -Atc "select version_id, encode(data, 'hex') from \"Binary\".resource_data where resource_id = '$(id_of "$1.Binary.spaced")'" > "$WORK/turns.$1.Binary.data"
}

# Every row the search tables of a type hold for a resource, without the
# resource's id.
search_dump() {
  local tables sql= t
  if [ "$DATABASE" = sqlserver ]; then
    for t in $(fire_mssql "select t.name from sys.tables t join sys.schemas s on s.schema_id = t.schema_id where s.name = '$1' and t.name like 'search[_]%' order by 1"); do
      fire_mssql "select '$t|' + cast(id as varchar(10)) + '|' + $(mssql_columns "[$1].$t" resource_id id) from [$1].$t where resource_id = '$2' order by id"
    done
    return
  fi
  if [ "$DATABASE" = sqlite ]; then
    for t in $(lite "select name from sqlite_master where type = 'table' and name like '$1\_search\_%' escape '\\' order by 1"); do
      lite "select '${t#$1_}', id, $(lite_columns "$t" resource_id id) from $t where resource_id = '$2' order by id"
    done
    return
  fi
  tables=$(docker exec "$PG_NAME" psql -U postgres -d Fire -Atc \
    "select table_name from information_schema.tables where table_schema = '$1' and table_name like 'search\\_%' order by 1")
  for t in $tables; do
    sql="$sql${sql:+ union all }select '$t' as t, id, (to_jsonb(x) - 'resource_id' - 'id')::text from \"$1\".$t x where resource_id = '$2'"
  done
  [ -z "$sql" ] || docker exec "$PG_NAME" psql -U postgres -d Fire -Atc "$sql order by 1, 2"
}

# The search tables: every file of tools/turns/search/ named <Type>.json is
# POSTed and then replaced by <Type>.update.json, every other file there is
# POSTed alone; what the tables hold of each afterwards is kept.
#   searches <server: s or c>
searches() {
  local f name type
  for f in "$ROOT"/tools/turns/search/*.json; do
    name=$(basename "$f" .json); type=${name%%.*}
    case "$name" in *.update) continue ;; esac
    call "$1.search.$name" 201 POST "$type" "search/$name.json"
    search_dump "$type" "$(id_of "$1.search.$name")" > "$WORK/turns.$1.search.$name.rows"
    [ -f "$ROOT/tools/turns/search/$type.update.json" ] && [ "$name" = "$type" ] || continue
    call "$1.search.$name.update" 200 PUT "$type/$(id_of "$1.search.$name")" "search/$type.update.json"
    search_dump "$type" "$(id_of "$1.search.$name")" > "$WORK/turns.$1.search.$name.update.rows"
  done
}

# The resources the questions are about: every file of tools/turns/linked/,
# named <Type>.<id>.json, is PUT under its id. They refer to each other.
linked() {
  local f name type id
  for f in "$ROOT"/tools/turns/linked/*.json; do
    name=$(basename "$f" .json); type=${name%%.*}; id=${name#*.}
    call "$1.linked.$name" 201 PUT "$type/$id" "linked/$name.json"
  done
  # ... and those of tools/turns/gone/ are put twice and deleted: what is
  # left of a resource that is gone (its versions, a Binary's bytes)
  for f in "$ROOT"/tools/turns/gone/*.json; do
    name=$(basename "$f" .json); type=${name%%.*}; id=${name#*.}
    call "$1.gone.$name" 201 PUT "$type/$id" "gone/$name.json"
    call "$1.gone.$name.again" 200 PUT "$type/$id" "gone/$name.json"
    call "$1.gone.$name.delete" 204 DELETE "$type/$id"
  done
  # ... and rows of tools/turns/rows.sql, written past the servers: a uri
  # search has no resource here that could hold one (R5's Observation has no
  # valueCanonical), so its tables are filled by hand for the questions
  if [ "$DATABASE" = sqlserver ]; then
    # (the same rows under SQL Server's names)
    sed -E 's/"([A-Za-z]+)"/[\1]/g' "$ROOT/tools/turns/rows.sql" | docker exec -i "$PG_NAME" /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P "$MSSQL_PASSWORD" -C -b -d Fire > /dev/null \
      || { echo "turns: FAIL -- tools/turns/rows.sql"; TURNS_FAILED=1; }
    return
  fi
  if [ "$DATABASE" = sqlite ]; then
    # (the same rows under SQLite's table names)
    sed -E 's/"([A-Za-z]+)"\.([a-z_]+)/\1_\2/g' "$ROOT/tools/turns/rows.sql" | sqlite3 "$SQLITE_FILE" \
      || { echo "turns: FAIL -- tools/turns/rows.sql"; TURNS_FAILED=1; }
    return
  fi
  docker exec -i "$PG_NAME" psql -U postgres -d Fire -q < "$ROOT/tools/turns/rows.sql" \
    || { echo "turns: FAIL -- tools/turns/rows.sql"; TURNS_FAILED=1; }
}

# Ask the running server every request of tools/turns/queries.txt; status
# and body of each answer are kept under its number.
#   asks <server: s or c> <id of A> <id of B>
asks() {
  local n=0 method path rest out
  while read -r method path rest; do
    case "$method" in ''|'#'*) continue ;; esac
    # (see refusals: on SQLite this request is taken, and would write)
    [ "$DATABASE" != postgresql ] && [ "$rest" = refused/Binary.base64.json ] && continue
    n=$((n+1)); out="$WORK/turns.$1.ask.$n"
    path=${path//\{A\}/$2}; path=${path//\{B\}/$3}
    case "$method" in
      GET) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" "$BASE/fire/$path" > "$out.status" ;;
      FILE) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' "$BASE$path" > "$out.status" ;;
      AS) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $path" "$BASE/fire/$rest" > "$out.status" ;;
      POST) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" -X POST \
              -H 'Content-Type: application/x-www-form-urlencoded' --data "$rest" "$BASE/fire/$path" > "$out.status" ;;
      PUT) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" -X PUT \
              -H 'Content-Type: application/fhir+json' --data-binary @"$ROOT/tools/turns/$rest" "$BASE/fire/$path" > "$out.status" ;;
      BUNDLE) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" -X POST \
              -H 'Content-Type: application/fhir+json' --data-binary @"$ROOT/tools/turns/bundles/$path" "$BASE/fire" > "$out.status" ;;
    esac
    echo "$method $path $rest" > "$out.request"
  done < "$ROOT/tools/turns/queries.txt"
  ASKED=$n
}

# What a server does when it WRITES: every line of tools/turns/steps.txt, in
# its own turn. `zz{S}` in a path, a form or a body file is the server's own
# mark (zzs, zzc), so that each works on resources of its own; the answers
# are compared without it, without ids and times.
#   steps <server: s or c>
steps() {
  local n=0 method path rest out body label=${3:-step}
  while read -r method path rest; do
    case "$method" in ''|'#'*) continue ;; esac
    n=$((n+1)); out="$WORK/turns.$1.$label.$n"
    rm -f "$out.unlike"
    # a line that begins with ! is carried out and not compared: it clears
    # what the turns before left differently for the two servers
    case "$method" in '!'*) method=${method#!}; : > "$out.unlike" ;; esac
    path=${path//\{S\}/$1}
    # what stands behind " | " is a header of the request
    local field=""
    case "$rest" in *' | '*) field=${rest#* | }; rest=${rest%% | *} ;; '| '*) field=${rest#| }; rest="" ;; esac
    field=${field//zz\{S\}/zz$1}
    body="$WORK/turns.$1.$label.body.json"
    if [ -n "$rest" ]; then sed "s/zz{S}/zz$1/g" "$ROOT/tools/turns/$rest" > "$body"; else : > "$body"; fi
    case "$method" in
      GET|DELETE) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" ${field:+-H "$field"} -X "$method" "$BASE/fire/$path" > "$out.status" ;;
      *) curl -s -o "$out.body" -w '%{http_code} %{content_type}\n' -H "Accept: $ACCEPT" ${field:+-H "$field"} -X "$method" \
           -H 'Content-Type: application/fhir+json' --data-binary @"$body" "$BASE/fire/$path" > "$out.status" ;;
    esac
    echo "$method $path $rest" > "$out.request"
  done < "$ROOT/tools/turns/${2:-steps.txt}"
  STEPPED=$n
}

# The answers of both servers to the lines of a steps file, compared.
#   steps_alike <label> <count>
steps_alike() {
  local n=1
  while [ $n -le "$2" ]; do
    if [ -f "$WORK/turns.s.$1.$n.unlike" ]; then n=$((n+1)); continue; fi
    if ! cmp -s "$WORK/turns.s.$1.$n.status" "$WORK/turns.c.$1.$n.status" || [ "$(step_neutral "$WORK/turns.s.$1.$n.body")" != "$(step_neutral "$WORK/turns.c.$1.$n.body")" ]; then
      echo "turns: FAIL -- $1 $(cat "$WORK/turns.s.$1.$n.request"): scaly $(cat "$WORK/turns.s.$1.$n.status"), csharp $(cat "$WORK/turns.c.$1.$n.status") (diff $WORK/turns.s.$1.$n.body $WORK/turns.c.$1.$n.body)"
      TURNS_FAILED=1
    fi
    n=$((n+1))
  done
}

# A step's answer without what differs by nature.
step_neutral() { [ -f "$1" ] || return 0; sed -E 's/zz[sc]/zz?/g; s/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/<id>/g; s/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+(\\u002B|[+-])[0-9:]+/<time>/g' "$1"; }

# What must not be stored: every file of tools/turns/refused/, named
# <Type>.<case>.json, is POSTed and must be answered with 400.
refusals() {
  local f name
  for f in "$ROOT"/tools/turns/refused/*.json; do
    name=$(basename "$f" .json)
    # (where the data stays in the content it is not decoded, and not refused)
    [ "$DATABASE" != postgresql ] && [ "$name" = Binary.base64 ] && continue
    call "$1.refused.$name" 400 POST "${name%%.*}" "refused/$name.json"
  done
}

# Everything the tables hold of a resource but its id, its times and its
# bytes (their length instead), in the order it was written.
other_rows() {
  if [ "$DATABASE" = sqlserver ]; then
    fire_mssql "select $(mssql_columns "[$1].resource_history" id last_updated content history_timestamp) + '|' + cast(datalength(content) as varchar(20)) from [$1].resource_history where id = '$2'"
    fire_mssql "select $(mssql_columns "[$1]" id last_updated content) + '|' + cast(datalength(content) as varchar(20)) from [$1] where id = '$2'"
    return
  fi
  if [ "$DATABASE" = sqlite ]; then
    lite "select $(lite_columns "$1_resource_history" id last_updated content history_timestamp), length(content) from $1_resource_history where id = '$2'"
    lite "select $(lite_columns "$1" id last_updated content), length(content) from $1 where id = '$2'"
    return
  fi
  docker exec "$PG_NAME" psql -U postgres -d Fire -Atc "
    select to_jsonb(h) - 'id' - 'last_updated' - 'content' - 'history_timestamp', length(content)
      from \"$1\".resource_history h where id = '$2'
    union all
    select to_jsonb(r) - 'id' - 'last_updated' - 'content', length(content) from \"$1\" r where id = '$2'"
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
  others create s c
  refusals s
  binary s
  searches s
  linked s
  stop_server

  echo "turns: 2 csharp  -- reads A and its first version, updates A; creates, updates and deletes B"
  start_server csharp
  call c.read-a 200 GET "Patient/$a";                    same c.read-a s.update
  call c.vread-a1 200 GET "Patient/$a/_history/1";       same c.vread-a1 s.create
  call c.update-a 200 PUT "Patient/$a" patient.json
  call c.create 201 POST Patient patient.json;           b=$(id_of c.create)
  call c.update 200 PUT "Patient/$b" patient-update.json
  call c.delete 204 DELETE "Patient/$b"
  others both c s
  refusals c
  binary c
  searches c
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
  others read s c
  steps s
  STEPPED_STEPS=$STEPPED
  stop_server

  echo "turns: 4 csharp  -- finds A deleted, reads its last version; reads B as created again; creates A again"
  start_server csharp
  call c.read-a2 410 GET "Patient/$a"
  call c.vread-a3 200 GET "Patient/$a/_history/3";       same c.vread-a3 c.update-a
  call c.read-b 200 GET "Patient/$b";                    same c.read-b s.recreate
  call c.recreate 201 PUT "Patient/$a" patient.json
  steps c
  stop_server

  echo "turns: 5 scaly   -- answers the $(grep -vc '^#' "$ROOT/tools/turns/queries.txt") requests of tools/turns/queries.txt"
  rm -f "$WORK"/turns.?.ask.*
  start_server scaly
  asks s "$a" "$b"
  stop_server
  echo "turns: 6 csharp  -- answers the same"
  start_server csharp
  asks c "$a" "$b"
  stop_server
  local n=1 different=0
  while [ $n -le "$ASKED" ]; do
    if ! cmp -s "$WORK/turns.s.ask.$n.status" "$WORK/turns.c.ask.$n.status" || ! cmp -s "$WORK/turns.s.ask.$n.body" "$WORK/turns.c.ask.$n.body"; then
      different=$((different+1))
      echo "turns: FAIL -- $(cat "$WORK/turns.s.ask.$n.request"): scaly $(cat "$WORK/turns.s.ask.$n.status"), csharp $(cat "$WORK/turns.c.ask.$n.status") (diff $WORK/turns.s.ask.$n.body $WORK/turns.c.ask.$n.body)"
      TURNS_FAILED=1
    fi
    n=$((n+1))
  done

  # what one does when it writes the other does
  steps_alike step "${STEPPED_STEPS:-0}"

  # what one refuses the other refuses, in the same words
  local f
  for f in "$ROOT"/tools/turns/refused/*.json; do
    [ "$DATABASE" != postgresql ] && [ "$(basename "$f" .json)" = Binary.base64 ] && continue
    same "s.refused.$(basename "$f" .json)" "c.refused.$(basename "$f" .json)"
  done
  # the search tables hold the same rows for the same resource
  SEARCH_ROWS=0
  for f in "$WORK"/turns.s.search.*.rows; do
    cmp -s "$f" "${f/turns.s./turns.c.}" \
      || { echo "turns: FAIL -- the search rows differ: diff $f ${f/turns.s./turns.c.}"; diff "$f" "${f/turns.s./turns.c.}" | head -12; TURNS_FAILED=1; }
    SEARCH_ROWS=$((SEARCH_ROWS + $(wc -l < "$f")))
  done
  [ "$SEARCH_ROWS" -gt 0 ] || { echo "turns: FAIL -- no search rows at all"; TURNS_FAILED=1; }
  alike s.Binary.spaced c.Binary.spaced
  alike s.Binary.spaced.read c.Binary.spaced.read
  alike s.Binary.spaced.format c.Binary.spaced.format
  same s.Binary.spaced.raw c.Binary.spaced.raw
  [ "$(xxd -p "$WORK/turns.s.Binary.spaced.raw.body")" = 68656c6c6f3e3f3f3f20776f726c640001 ] \
    || { echo "turns: FAIL -- the bytes of the Binary are not the ones its data encodes"; TURNS_FAILED=1; }
  [ -s "$WORK/turns.s.Binary.data" ] && cmp -s "$WORK/turns.s.Binary.data" "$WORK/turns.c.Binary.data" \
    || { echo "turns: FAIL -- the stored data of the Binary differs: $(cat "$WORK/turns.s.Binary.data") / $(cat "$WORK/turns.c.Binary.data")"; TURNS_FAILED=1; }
  [ "$(grep -i '^content-type' "$WORK/turns.s.Binary.spaced.raw.head" | tr -d '\r')" = "$(grep -i '^content-type' "$WORK/turns.c.Binary.spaced.raw.head" | tr -d '\r')" ] \
    || { echo "turns: FAIL -- the content type of the Binary's bytes differs"; TURNS_FAILED=1; }
  # the same request, the same bytes
  alike s.create c.create
  alike s.update c.update
  local t type
  for t in $OTHERS; do
    type=${t%%:*}
    alike "s.$type.create" "c.$type.create"
    alike "s.$type.update" "c.$type.update"
    other_rows "$type" "$(id_of "s.$type.create")" > "$WORK/turns.rows.s.$type"
    other_rows "$type" "$(id_of "c.$type.create")" > "$WORK/turns.rows.c.$type"
    if [ -s "$WORK/turns.rows.s.$type" ] && cmp -s "$WORK/turns.rows.s.$type" "$WORK/turns.rows.c.$type"; then continue; fi
    echo "turns: FAIL -- the rows of $type differ between the servers:"
    diff "$WORK/turns.rows.s.$type" "$WORK/turns.rows.c.$type"; TURNS_FAILED=1
  done
  rows_of "$a" > "$WORK/turns.rows.a"
  rows_of "$b" > "$WORK/turns.rows.b"
  # the last round, after everything was compared: what empties a whole type
  # (tools/turns/last.txt). Each server finds what the other left -- and
  # leaves the same.
  echo "turns: 7 scaly, 8 csharp -- the $(grep -vc '^#\|^$' "$ROOT/tools/turns/last.txt") steps of tools/turns/last.txt"
  start_server scaly
  steps s last.txt last
  stop_server
  start_server csharp
  steps c last.txt last
  stop_server
  steps_alike last "$STEPPED"
  STEPPED=$((STEPPED + STEPPED_STEPS))

  stop_database
  # A: created and updated by scaly, updated by csharp, deleted by scaly,
  # created again by csharp; B the other way round, without the second update
  local size size_update
  size=$(wc -c < "$WORK/turns.s.create.body" | tr -d ' ')
  size_update=$(wc -c < "$WORK/turns.s.update.body" | tr -d ' ')
  # (a boolean as the database shows it: t and f, or SQLite's 1 and 0)
  local yes=t no=f
  [ "$DATABASE" != postgresql ] && yes=1 no=0
  printf '%s\n' "1|$yes|male|$size|UPDATE" "2|$no|female|$size_update|UPDATE" "3|$yes|male|$size|DELETE" "4|$yes|male|$size|CURRENT" > "$WORK/turns.rows.a.expected"
  printf '%s\n' "1|$yes|male|$size|UPDATE" "2|$no|female|$size_update|DELETE" "3|$yes|male|$size|CURRENT" > "$WORK/turns.rows.b.expected"
  for r in a b; do
    cmp -s "$WORK/turns.rows.$r" "$WORK/turns.rows.$r.expected" && continue
    echo "turns: FAIL -- the rows of ${r} are not the expected ones:"; diff "$WORK/turns.rows.$r" "$WORK/turns.rows.$r.expected"; TURNS_FAILED=1
  done

  [ -z "$TURNS_FAILED" ] || { echo "turns: FAILED"; exit 1; }
  echo "turns: passed -- Patient through four turns (20 requests, 9 answers the same bytes across the servers, 7 rows as expected); Binary, Observation, Practitioner and Provenance created, updated and read across (32 requests, 16 answers the same bytes, the rows of both servers equal); a Binary's bytes stored and answered alike; $SEARCH_ROWS rows of the search tables equal; $(ls "$ROOT"/tools/turns/refused/*.json | wc -l | tr -d ' ') invalid resources refused by both in the same words; $ASKED requests (searches, histories, bundles) answered the same by both; ${STEPPED:-0} writing steps answered alike"
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
