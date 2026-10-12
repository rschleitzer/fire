#!/bin/bash
# tools/upgrade-test.sh -- the upgrade steps, measured as everything else is:
# against the C# server. fire's own model has no upgrade step, so this makes
# one that has: tools/upgrade/model.py adds two steps to model/fhir.xml (a
# column and a table made and filled, a table made anew, the key of the
# Binary's data dropped). Then
#   1. the Scaly server of the model as it is creates a database and takes
#      the linked resources of the turns; the database is set BACK to what
#      the steps expect (tools/upgrade/before.sql) and copied twice;
#   2. both servers are generated from the model with the steps, each starts
#      on its copy and upgrades it;
#   3. the two databases must be the same: the version, the schema
#      (pg_dump), the rows the steps wrote.
# A throwaway postgres:18 on port 55436 -- or, with FIRE_DATABASE=sqlite,
# files (before.sqlite.sql, rows.sqlite.sql); scratch and logs in
# ~/.cache/fire-measure/upgrade. dazzle on the PATH, or $DAZZLE.
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DATABASE="${FIRE_DATABASE:-postgresql}"
WORK="${FIRE_MEASURE_HOME:-$HOME/.cache/fire-measure}/upgrade"
[ "$DATABASE" = postgresql ] || WORK="$WORK-$DATABASE"
MSSQL_PASSWORD='Throwaway!1'
# a script on a database of the throwaway SQL Server
mssql() { local d=$1; shift; docker exec -i "$PG_NAME" /opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P "$MSSQL_PASSWORD" -C -b -y 0 -d "$d" "$@" | sed 's/[[:space:]]*$//'; }
DAZZLE="${DAZZLE:-dazzle}"
PG_NAME=fire-upgrade-pg PG_PORT=55436 PG_PASSWORD=throwaway PORT=5582
BASE="http://localhost:$PORT"
export SP_ENCODING=utf-8
die() { echo "upgrade-test: $*" >&2; exit 1; }
SERVER_PID=""
# dotnet run leaves the server it started behind: whatever listens goes too
stop_server() {
  [ -n "$SERVER_PID" ] && kill "$SERVER_PID" 2>/dev/null && wait "$SERVER_PID" 2>/dev/null
  SERVER_PID=""
  local left; left=$(lsof -nP -t -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null)
  [ -z "$left" ] || { kill $left 2>/dev/null; sleep 1; }
}
trap 'stop_server; [ "$DATABASE" = sqlite ] || docker rm -f -v "$PG_NAME" > /dev/null 2>&1' EXIT
psql_() { docker exec -i "$PG_NAME" psql -U postgres -v ON_ERROR_STOP=1 "$@"; }

# start <log> <database> <command...>: a server on the database, until it listens
start() {
  local log=$1 database=$2; shift 2
  local kind=PostgreSql connection="Host=localhost;Port=$PG_PORT;Database=$database;Username=postgres;Password=$PG_PASSWORD"
  [ "$DATABASE" = sqlite ] && kind=Sqlite connection="Data Source=$WORK/$database.db"
  [ "$DATABASE" = sqlserver ] && kind=SqlServer connection="Server=localhost,$PG_PORT;Database=$database;User Id=sa;Password=$MSSQL_PASSWORD;TrustServerCertificate=True"
  DatabaseType=$kind ConnectionString="$connection" \
  Kestrel__Endpoints__MyHttpEndpoint__Url="$BASE" "$@" > "$log" 2>&1 &
  SERVER_PID=$!
  local i=0
  while [ $i -lt 120 ]; do
    grep -q "Now listening" "$log" && return 0
    kill -0 "$SERVER_PID" 2>/dev/null || { cat "$log" >&2; return 1; }
    sleep 1; i=$((i+1))
  done
  return 1
}

rm -rf "$WORK"; mkdir -p "$WORK"
echo "upgrade-test: the model with two upgrade steps"
mkdir -p "$WORK/model" && cp "$ROOT"/model/* "$WORK/model/"
python3 "$ROOT/tools/upgrade/model.py" "$ROOT/model/fhir.xml" "$WORK/model/fhir.xml" || die "the model with steps is not made"

echo "upgrade-test: 1 -- a database of the model as it is, with resources, set back"
"$ROOT/tools/generate.sh" > "$WORK/generate.log" 2>&1 || die "tools/generate.sh"
mkdir -p "$ROOT/server/build"
(cd "$ROOT" && scaly build server/fire.scaly -o server/build/fire) > "$WORK/build.a.log" 2>&1 || { cat "$WORK/build.a.log" >&2; die "the server of the model as it is does not build"; }
if [ "$DATABASE" = sqlserver ]; then
  docker rm -f -v "$PG_NAME" > /dev/null 2>&1
  docker run -d --rm --platform linux/amd64 --name "$PG_NAME" -e ACCEPT_EULA=Y -e MSSQL_SA_PASSWORD="$MSSQL_PASSWORD" -p "127.0.0.1:$PG_PORT:1433" mcr.microsoft.com/mssql/server:2022-latest > /dev/null || die "no SQL Server"
  until mssql master -Q "select 1" > /dev/null 2>&1; do sleep 1; done
  # (it answers its own tool before it takes logins from outside)
  sleep 8
fi
if [ "$DATABASE" = postgresql ]; then
  docker rm -f -v "$PG_NAME" > /dev/null 2>&1
  docker run -d --rm --name "$PG_NAME" -e POSTGRES_PASSWORD="$PG_PASSWORD" -e POSTGRES_DB=Fire -p "$PG_PORT:5432" postgres:18 > /dev/null || die "no PostgreSQL"
  until docker exec "$PG_NAME" pg_isready -U postgres -q; do sleep 1; done; sleep 2
fi
start "$WORK/a.log" Fire "$ROOT/server/build/fire" || die "the server of the model as it is does not start"
for f in "$ROOT"/tools/turns/linked/*.json; do
  name=$(basename "$f" .json)
  curl -s -o /dev/null -X PUT -H 'Content-Type: application/fhir+json' --data-binary @"$f" "$BASE/fire/${name%%.*}/${name#*.}"
done
stop_server
if [ "$DATABASE" = sqlserver ]; then
  mssql Fire < "$ROOT/tools/upgrade/before.sqlserver.sql" > "$WORK/before.log" 2>&1 || { cat "$WORK/before.log" >&2; die "tools/upgrade/before.sqlserver.sql"; }
  # the copies: a backup restored under two names
  mssql master -Q "BACKUP DATABASE [Fire] TO DISK = '/var/opt/mssql/data/fire.bak'" > "$WORK/copies.log" 2>&1 || die "the backup"
  for d in FireS FireC; do
    mssql master -Q "RESTORE DATABASE [$d] FROM DISK = '/var/opt/mssql/data/fire.bak' WITH MOVE 'Fire' TO '/var/opt/mssql/data/$d.mdf', MOVE 'Fire_log' TO '/var/opt/mssql/data/${d}_log.ldf'" >> "$WORK/copies.log" 2>&1 || { tail -3 "$WORK/copies.log" >&2; die "the copy $d"; }
  done
elif [ "$DATABASE" = sqlite ]; then
  sqlite3 "$WORK/Fire.db" < "$ROOT/tools/upgrade/before.sqlite.sql" || die "tools/upgrade/before.sqlite.sql"
  cp "$WORK/Fire.db" "$WORK/FireS.db" && cp "$WORK/Fire.db" "$WORK/FireC.db" || die "the copies"
else
  psql_ -d Fire -q < "$ROOT/tools/upgrade/before.sql" || die "tools/upgrade/before.sql"
  psql_ -q -c 'create database "FireS" template "Fire"' -c 'create database "FireC" template "Fire"' || die "the copies"
fi

echo "upgrade-test: 2 -- the Scaly server of the model with steps upgrades its copy"
mkdir -p "$WORK/scaly" && ln -s "$ROOT/packages" "$WORK/scaly/packages"
(cd "$WORK/scaly" && "$DAZZLE" -t sgml -d "$ROOT/codegen/scaly.dsl" "$WORK/model/fhir.xml") > "$WORK/generate.s.log" 2>&1 || die "the Scaly generator on the model with steps"
FIRE_OUT="$WORK/scaly" "$ROOT/tools/schema.sh" fire || die "tools/schema.sh on the model with steps"
mkdir -p "$WORK/scaly/server/build"
(cd "$WORK/scaly" && scaly build server/fire.scaly -o server/build/fire) > "$WORK/build.s.log" 2>&1 || { cat "$WORK/build.s.log" >&2; die "the Scaly server of the model with steps does not build"; }
start "$WORK/s.log" FireS "$WORK/scaly/server/build/fire" || die "the Scaly server does not start on its copy (see $WORK/s.log)"
stop_server

echo "upgrade-test:      the C# server of the same model upgrades its copy"
mkdir -p "$WORK/csharp" && rsync -a --exclude bin --exclude obj --exclude server --exclude model --exclude client --exclude fhir-r5 "$ROOT/upstream/" "$WORK/csharp/"
cp "$WORK/model/fhir.xml" "$WORK/csharp/fhir.xml"
(cd "$WORK/csharp" && "$DAZZLE" -t sgml -d codegen/map.dsl fhir.xml) > "$WORK/generate.c.log" 2>&1 || die "upstream's generator on the model with steps"
(cd "$WORK/csharp/server" && dotnet build -v q) > "$WORK/build.c.log" 2>&1 || { tail -5 "$WORK/build.c.log" >&2; die "the C# server of the model with steps does not build"; }
(cd "$WORK/csharp/server" && start "$WORK/c.log" FireC dotnet run --no-build) || die "the C# server does not start on its copy (see $WORK/c.log)"
stop_server

echo "upgrade-test: 3 -- the two databases"
FAILED=""
for d in FireS FireC; do
  if [ "$DATABASE" = sqlserver ]; then
    mssql $d -Q "set nocount on; select value from [fire_meta].dbinfo where name = 'version'" > "$WORK/$d.version"
    mssql $d -Q "set nocount on; select s.name + '.' + t.name + '.' + c.name + ' ' + ty.name + ' ' + cast(c.max_length as varchar(10)) + ' ' + cast(c.is_nullable as varchar(1)) from sys.columns c join sys.tables t on t.object_id = c.object_id join sys.schemas s on s.schema_id = t.schema_id join sys.types ty on ty.user_type_id = c.user_type_id order by 1; select 'index ' + s.name + '.' + t.name + '.' + i.name from sys.indexes i join sys.tables t on t.object_id = i.object_id join sys.schemas s on s.schema_id = t.schema_id where i.name is not null and i.is_primary_key = 0 order by 1; select 'key ' + s.name + '.' + t.name + ' ' + cast(count(*) as varchar(10)) from sys.foreign_keys f join sys.tables t on t.object_id = f.parent_object_id join sys.schemas s on s.schema_id = t.schema_id group by s.name, t.name order by 1" > "$WORK/$d.schema"
    mssql $d < "$ROOT/tools/upgrade/rows.sqlserver.sql" > "$WORK/$d.rows"
    continue
  fi
  if [ "$DATABASE" = sqlite ]; then
    sqlite3 "$WORK/$d.db" "select value from fire_meta_dbinfo where name = 'version'" > "$WORK/$d.version"
    sqlite3 "$WORK/$d.db" .schema > "$WORK/$d.schema"
    sqlite3 "$WORK/$d.db" < "$ROOT/tools/upgrade/rows.sqlite.sql" > "$WORK/$d.rows"
    continue
  fi
  psql_ -d $d -At -c "select value from fire_meta.dbinfo where name = 'version'" > "$WORK/$d.version"
  docker exec "$PG_NAME" pg_dump -U postgres --schema-only --no-owner -d $d | grep -v '^\\\(un\)\?restrict\|^-- Dumped' > "$WORK/$d.schema"
  psql_ -d $d -At < "$ROOT/tools/upgrade/rows.sql" > "$WORK/$d.rows"
done
grep -h "Upgrading database\|Database upgraded" "$WORK/s.log" | sed 's/^ *//' > "$WORK/s.steps"
grep -h "Upgrading database\|Database upgraded" "$WORK/c.log" | sed 's/^ *//' > "$WORK/c.steps"
[ "$(cat "$WORK/FireS.version")" = "5.0.2" ] || { echo "upgrade-test: FAIL -- the Scaly server left the version $(cat "$WORK/FireS.version")"; FAILED=1; }
cmp -s "$WORK/FireS.version" "$WORK/FireC.version" || { echo "upgrade-test: FAIL -- versions: $(cat "$WORK/FireS.version") / $(cat "$WORK/FireC.version")"; FAILED=1; }
cmp -s "$WORK/FireS.schema" "$WORK/FireC.schema" || { echo "upgrade-test: FAIL -- the schemas differ: diff $WORK/FireS.schema $WORK/FireC.schema"; diff "$WORK/FireS.schema" "$WORK/FireC.schema" | head -20; FAILED=1; }
cmp -s "$WORK/FireS.rows" "$WORK/FireC.rows" || { echo "upgrade-test: FAIL -- the rows differ: diff $WORK/FireS.rows $WORK/FireC.rows"; diff "$WORK/FireS.rows" "$WORK/FireC.rows" | head -20; FAILED=1; }
cmp -s "$WORK/s.steps" "$WORK/c.steps" || { echo "upgrade-test: FAIL -- the steps the servers name differ:"; diff "$WORK/s.steps" "$WORK/c.steps"; FAILED=1; }
[ -s "$WORK/FireS.rows" ] || { echo "upgrade-test: FAIL -- no rows to compare"; FAILED=1; }
[ -z "$FAILED" ] || { echo "upgrade-test: FAILED"; exit 1; }
echo "upgrade-test: passed on $DATABASE -- both servers took the database from 5.0.0 to $(cat "$WORK/FireS.version") in $(grep -c 'Database upgraded' "$WORK/s.steps") steps; the schemas are the same ($(wc -l < "$WORK/FireS.schema" | tr -d ' ') lines), and so are the $(wc -l < "$WORK/FireS.rows" | tr -d ' ') rows the steps wrote"
