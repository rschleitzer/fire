#!/bin/bash
# tools/variant-test.sh <variant> -- what fire's own model does not have,
# measured as everything else is: against the C# server. A variant is a
# directory of tools/variants/: model.py makes a model of model/fhir.xml,
# steps.txt is what both servers are asked. Both servers are GENERATED from
# that model; each creates a database of its own and answers the steps, and
# then the same must hold of both: the schema (pg_dump), the answer to
# every step (status, Location, ETag, content type, the body without ids
# and times), and every row of every table (without ids and times).
# A throwaway postgres:18 on port 55437 -- or, with FIRE_DATABASE=sqlite, a
# file each; scratch and answers in ~/.cache/fire-measure/variant-<variant>.
# dazzle on the PATH, or $DAZZLE.
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VARIANT=${1:?usage: tools/variant-test.sh <variant>}
HERE="$ROOT/tools/variants/$VARIANT"
[ -f "$HERE/model.py" ] || { echo "variant-test: no variant $VARIANT" >&2; exit 2; }
DATABASE="${FIRE_DATABASE:-postgresql}"
WORK="${FIRE_MEASURE_HOME:-$HOME/.cache/fire-measure}/variant-$VARIANT"
[ "$DATABASE" = sqlite ] && WORK="$WORK-sqlite"
DAZZLE="${DAZZLE:-dazzle}"
PG_NAME=fire-variant-pg PG_PORT=55437 PG_PASSWORD=throwaway PORT=5583
BASE="http://localhost:$PORT"
export SP_ENCODING=utf-8
die() { echo "variant-test: $*" >&2; exit 1; }
SERVER_PID=""
# dotnet run leaves the server it started behind: whatever listens goes too
stop_server() {
  [ -n "$SERVER_PID" ] && kill "$SERVER_PID" 2>/dev/null && wait "$SERVER_PID" 2>/dev/null
  SERVER_PID=""
  local left; left=$(lsof -nP -t -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null)
  [ -z "$left" ] || { kill $left 2>/dev/null; sleep 1; }
}
trap 'stop_server; [ "$DATABASE" = sqlite ] || docker rm -f -v "$PG_NAME" > /dev/null 2>&1' EXIT

# start <log> <database> <command...>: a server on the database, until it listens
start() {
  local log=$1 database=$2; shift 2
  local kind=PostgreSql connection="Host=localhost;Port=$PG_PORT;Database=$database;Username=postgres;Password=$PG_PASSWORD"
  [ "$DATABASE" = sqlite ] && kind=Sqlite connection="Data Source=$WORK/$database.db"
  DatabaseType=$kind SeedDataPath="$ROOT/tools/turns/seed" ReseedSecret=variant-secret \
  ConnectionString="$connection" \
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

# steps <server: s or c>: every line of the variant's steps.txt
steps() {
  local n=0 method path rest out field args
  while read -r method path rest; do
    case "$method" in ''|'#'*) continue ;; esac
    n=$((n+1)); out="$WORK/$1.$n"
    field=""
    case "$rest" in *' | '*) field=${rest#* | }; rest=${rest%% | *} ;; '| '*) field=${rest#| }; rest="" ;; esac
    args=(-s -o "$out.body" -D "$out.head" -w '%{http_code}\n' -H 'Accept: application/fhir+json')
    [ -z "$field" ] || args+=(-H "$field")
    case "$method" in
      GET|DELETE) curl "${args[@]}" -X "$method" "$BASE/fire/$path" > "$out.status" ;;
      BUNDLE) curl "${args[@]}" -X POST -H 'Content-Type: application/fhir+json' --data-binary @"$HERE/$path" "$BASE/fire" > "$out.status" ;;
      *) if [ -n "$rest" ]; then
           curl "${args[@]}" -X "$method" -H 'Content-Type: application/fhir+json' --data-binary @"$HERE/$rest" "$BASE/fire/$path" > "$out.status"
         else
           curl "${args[@]}" -X "$method" -H 'Content-Type: application/fhir+json' --data-binary '' "$BASE/fire/$path" > "$out.status"
         fi ;;
    esac
    grep -i '^\(location\|etag\|content-type\):' "$out.head" | tr -d '\r' | sort >> "$out.status"
    echo "$method $path $rest${field:+ | $field}" > "$out.request"
  done < "$HERE/steps.txt"
  STEPPED=$n
}

# An answer, or a row, without what differs by nature: ids the server made, times.
neutral() { [ -f "$1" ] || return 0; sed -E 's/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/<id>/g; s/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+(\\u002B|[+-])[0-9:]+/<time>/g; s/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z/<time>/g' "$1"; }

# rows <database>: every row of every table, the content as text
rows() {
  local tables sql= t
  if [ "$DATABASE" = sqlite ]; then
    for t in $(sqlite3 "$WORK/$1.db" "select name from sqlite_master where type = 'table' order by 1"); do
      sqlite3 "$WORK/$1.db" "select '$t', $(sqlite3 "$WORK/$1.db" "select group_concat('quote(' || name || ')', ' || ''|'' || ') from pragma_table_info('$t') where name not in ('last_updated', 'history_timestamp', 'content')") $(sqlite3 "$WORK/$1.db" "select ', content' from pragma_table_info('$t') where name = 'content'") from $t"
    done > "$WORK/$1.rows.raw" || return 1
    neutral "$WORK/$1.rows.raw" | sort > "$WORK/$1.rows"
    return
  fi
  tables=$(docker exec "$PG_NAME" psql -U postgres -d "$1" -Atc \
    "select quote_ident(table_schema) || '.' || quote_ident(table_name) from information_schema.tables
      where table_schema not in ('pg_catalog', 'information_schema') and table_type = 'BASE TABLE' order by 1")
  for t in $tables; do
    sql="$sql${sql:+ union all }select '$t' as t, (to_jsonb(x) - 'last_updated' - 'history_timestamp' - 'content' - 'data')::text
      || coalesce(' ' || case when to_jsonb(x) ? 'content' then convert_from(decode(substr(to_jsonb(x)->>'content', 3), 'hex'), 'UTF8') end, '')
      || coalesce(' ' || (to_jsonb(x)->>'data'), '') from $t x"
  done
  docker exec "$PG_NAME" psql -U postgres -d "$1" -Atc "$sql" > "$WORK/$1.rows.raw" || return 1
  neutral "$WORK/$1.rows.raw" | sort > "$WORK/$1.rows"
}

rm -rf "$WORK"; mkdir -p "$WORK"
echo "variant-test: $VARIANT -- the model"
mkdir -p "$WORK/model" && cp "$ROOT"/model/* "$WORK/model/"
python3 "$HERE/model.py" "$ROOT/model/fhir.xml" "$WORK/model/fhir.xml" || die "the variant's model is not made"

echo "variant-test: the Scaly server of it"
mkdir -p "$WORK/scaly" && ln -s "$ROOT/packages" "$WORK/scaly/packages"
(cd "$WORK/scaly" && "$DAZZLE" -t sgml -d "$ROOT/codegen/scaly.dsl" "$WORK/model/fhir.xml") > "$WORK/generate.s.log" 2>&1 || die "the Scaly generator"
grep -q ':E:' "$WORK/generate.s.log" && die "the Scaly generator: $(grep -m1 ':E:' "$WORK/generate.s.log" | cut -c1-200)"
FIRE_OUT="$WORK/scaly" "$ROOT/tools/schema.sh" fire || die "tools/schema.sh"
mkdir -p "$WORK/scaly/server/build"
(cd "$WORK/scaly" && scaly build server/fire.scaly -o server/build/fire) > "$WORK/build.s.log" 2>&1 || { cat "$WORK/build.s.log" >&2; die "the Scaly server does not build"; }

echo "variant-test: the C# server of it"
mkdir -p "$WORK/csharp" && rsync -a --exclude bin --exclude obj --exclude server --exclude model --exclude client --exclude fhir-r5 "$ROOT/upstream/" "$WORK/csharp/"
cp "$WORK/model/fhir.xml" "$WORK/csharp/fhir.xml"
(cd "$WORK/csharp" && "$DAZZLE" -t sgml -d codegen/map.dsl fhir.xml) > "$WORK/generate.c.log" 2>&1 || die "upstream's generator"
(cd "$WORK/csharp/server" && dotnet build -v q) > "$WORK/build.c.log" 2>&1 || { grep -m5 'error' "$WORK/build.c.log" >&2; die "the C# server does not build"; }

if [ "$DATABASE" != sqlite ]; then
  docker rm -f -v "$PG_NAME" > /dev/null 2>&1
  docker run -d --rm --name "$PG_NAME" -e POSTGRES_PASSWORD="$PG_PASSWORD" -e POSTGRES_DB=FireS -p "$PG_PORT:5432" postgres:18 > /dev/null || die "no PostgreSQL"
  until docker exec "$PG_NAME" pg_isready -U postgres -q; do sleep 1; done; sleep 2
  docker exec "$PG_NAME" psql -U postgres -q -c 'create database "FireC"' || die "the second database"
fi

echo "variant-test: scaly answers the steps"
start "$WORK/s.log" FireS "$WORK/scaly/server/build/fire" || die "the Scaly server does not start (see $WORK/s.log)"
steps s; stop_server
echo "variant-test: csharp answers the same"
(cd "$WORK/csharp/server" && start "$WORK/c.log" FireC dotnet run --no-build) || die "the C# server does not start (see $WORK/c.log)"
steps c; stop_server

FAILED="" UNLIKE=0
n=1
while [ $n -le "$STEPPED" ]; do
  if [ "$(neutral "$WORK/s.$n.status")" != "$(neutral "$WORK/c.$n.status")" ] || [ "$(neutral "$WORK/s.$n.body")" != "$(neutral "$WORK/c.$n.body")" ]; then
    echo "variant-test: FAIL -- $n $(cat "$WORK/s.$n.request"): scaly $(head -1 "$WORK/s.$n.status"), csharp $(head -1 "$WORK/c.$n.status") (diff $WORK/s.$n.body $WORK/c.$n.body)"
    FAILED=1; UNLIKE=$((UNLIKE+1))
  fi
  n=$((n+1))
done
for d in FireS FireC; do
  if [ "$DATABASE" = sqlite ]; then
    sqlite3 "$WORK/$d.db" .schema > "$WORK/$d.schema"
  else
    docker exec "$PG_NAME" pg_dump -U postgres --schema-only --no-owner -d $d | grep -v '^\\\(un\)\?restrict\|^-- Dumped' > "$WORK/$d.schema"
  fi
  rows $d || die "the rows of $d"
done
cmp -s "$WORK/FireS.schema" "$WORK/FireC.schema" || { echo "variant-test: FAIL -- the schemas differ: diff $WORK/FireS.schema $WORK/FireC.schema"; FAILED=1; }
cmp -s "$WORK/FireS.rows" "$WORK/FireC.rows" || { echo "variant-test: FAIL -- the rows differ: diff $WORK/FireS.rows $WORK/FireC.rows"; FAILED=1; }
[ -s "$WORK/FireS.rows" ] || { echo "variant-test: FAIL -- no rows to compare"; FAILED=1; }
[ -z "$FAILED" ] || { echo "variant-test: FAILED ($UNLIKE of $STEPPED steps unlike)"; exit 1; }
echo "variant-test: passed -- $VARIANT on $DATABASE: $STEPPED steps answered alike, the schemas the same ($(wc -l < "$WORK/FireS.schema" | tr -d ' ') lines), $(wc -l < "$WORK/FireS.rows" | tr -d ' ') rows equal"
