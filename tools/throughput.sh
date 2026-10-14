#!/bin/bash
# tools/throughput.sh [postgresql|sqlite|both] -- what the two servers do in a
# second. Each server, built for release, on a database of its own that
# tools/throughput/load.py fills through the server itself; then oha runs
# the scenarios below against it, one after the other, and the table says
# for each: requests a second, the processor time the server took for
# them (in cores), the median and the 99th percentile of the latency, and
# what was not answered with 2xx. The server's resident memory is read
# after the last scenario.
#   metadata   the CapabilityStatement: no database
#   read       a Patient by id, a random one of the loaded
#   search     Patients by the beginning of a family name, ten a page
#   reference  the Observations of one Patient, by the reference
#   update     a new version of a random Patient
#   create     a new Patient
#
# The load generator runs on the same machine and takes cores of its own --
# and it is the limit for the simple scenarios (oha gives some 44 000
# requests a second here, whatever answers them). So requests a second
# compare the two servers within one run and are not a machine's capacity;
# what a server COSTS is the other column: requests for a core.
#   FIRE_CSHARP_OPTIONS   more of the C# server's connection string (the
#       generated server prepares its statements unless it says otherwise:
#       "Max Auto Prepare=0;No Reset On Close=false" measures it without).
#   FIRE_LOAD (thousands of resources of each type, 1 to 9: 2)
#   FIRE_SECONDS (10 a scenario)   FIRE_CONNECTIONS (64)
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WHAT=${1:-both}
WORK="${FIRE_MEASURE:-$HOME/.cache/fire-measure}/throughput"
THOUSANDS=${FIRE_LOAD:-2} SECONDS_EACH=${FIRE_SECONDS:-10} CONNECTIONS=${FIRE_CONNECTIONS:-64}
N=$((THOUSANDS * 1000))
# a random id of the loaded, as oha makes a URL from a pattern
ANY="[0-$((THOUSANDS - 1))][0-9]{3}"
PG_NAME=fire-throughput-pg PG_PORT=55438 PG_PASSWORD=throwaway PORT=5585
# (the address by number: the Scaly server listens on 127.0.0.1, and a load
# generator that tries ::1 for `localhost` counts refused connections)
BASE="http://127.0.0.1:$PORT"
SCENARIOS="metadata read search reference update create"
die() { echo "throughput: $*" >&2; exit 1; }
command -v oha > /dev/null || die "oha is not on the PATH (brew install oha)"
SERVER_PID=""
stop_server() {
  [ -n "$SERVER_PID" ] && kill "$SERVER_PID" 2>/dev/null && wait "$SERVER_PID" 2>/dev/null
  SERVER_PID=""
  local left; left=$(lsof -nP -t -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null)
  [ -z "$left" ] || { kill $left 2>/dev/null; sleep 1; }
}
trap 'stop_server; docker rm -f -v "$PG_NAME" > /dev/null 2>&1' EXIT
mkdir -p "$WORK"

echo "throughput: building both servers for release"
"$ROOT/tools/generate.sh" > "$WORK/generate.log" 2>&1 || die "tools/generate.sh"
(cd "$ROOT" && scaly build server/fire.scaly -o server/build/fire-release --release) > "$WORK/build.s.log" 2>&1 || { cat "$WORK/build.s.log" >&2; die "the Scaly server does not build"; }
(cd "$ROOT/upstream/server" && dotnet build -c Release -v q) > "$WORK/build.c.log" 2>&1 || { tail -5 "$WORK/build.c.log" >&2; die "the C# server does not build"; }

# scenario <name> <duration>: oha's report as JSON
scenario() {
  local put='{"resourceType":"Patient","active":true,"name":[{"family":"fam00x","given":["Updated"]}],"gender":"male","birthDate":"1970-01-02"}'
  local post='{"resourceType":"Patient","active":true,"name":[{"family":"Created","given":["Under","Load"]}],"gender":"female","birthDate":"1980-05-06","identifier":[{"system":"urn:throughput","value":"created"}]}'
  local common=(--no-tui --output-format json -c "$CONNECTIONS" -z "$2" -H 'Accept: application/fhir+json')
  case "$1" in
    metadata)  oha "${common[@]}" "$BASE/fire/metadata" ;;
    read)      oha "${common[@]}" --rand-regex-url "$BASE/fire/Patient/tp$ANY" ;;
    search)    oha "${common[@]}" --rand-regex-url "$BASE/fire/Patient\\?family=fam[0-9]{2}x&_count=10" ;;
    reference) oha "${common[@]}" --rand-regex-url "$BASE/fire/Observation\\?subject=Patient/tp$ANY&_count=10" ;;
    update)    oha "${common[@]}" -m PUT -T application/fhir+json -d "$put" --rand-regex-url "$BASE/fire/Patient/tp$ANY" ;;
    create)    oha "${common[@]}" -m POST -T application/fhir+json -d "$post" "$BASE/fire/Patient" ;;
  esac
}

# measure <database> <server>
measure() {
  local database=$1 server=$2 kind connection out="$WORK/$1.$2"
  if [ "$database" = sqlite ]; then
    rm -rf "$out.db"*; kind=Sqlite connection="Data Source=$out.db"
  else
    docker rm -f -v "$PG_NAME" > /dev/null 2>&1
    docker run -d --rm --name "$PG_NAME" -e POSTGRES_PASSWORD="$PG_PASSWORD" -e POSTGRES_DB=Fire -p "127.0.0.1:$PG_PORT:5432" postgres:18 > /dev/null || die "no PostgreSQL"
    until docker exec "$PG_NAME" pg_isready -q -h 127.0.0.1 -U postgres -d Fire 2> /dev/null; do sleep 0.5; done
    kind=PostgreSql connection="Host=localhost;Port=$PG_PORT;Database=Fire;Username=postgres;Password=$PG_PASSWORD"
  fi
  export DatabaseType=$kind ConnectionString="$connection" Kestrel__Endpoints__MyHttpEndpoint__Url="http://localhost:$PORT" externalUrl="$BASE/fire"
  # (the servers' output is not kept: upstream writes a line for every request)
  if [ "$server" = scaly ]; then
    "$ROOT/server/build/fire-release" > /dev/null 2>&1 &
  else
    (cd "$ROOT/upstream/server" && ConnectionString="$connection${FIRE_CSHARP_OPTIONS:+;$FIRE_CSHARP_OPTIONS}" exec dotnet bin/Release/net10.0/Fire.dll) > /dev/null 2>&1 &
  fi
  SERVER_PID=$!
  local i=0
  until curl -s -o /dev/null "$BASE/fire/metadata"; do
    kill -0 "$SERVER_PID" 2> /dev/null || die "the $server server ended at its start on $database"
    i=$((i+1)); [ $i -lt 240 ] || die "the $server server does not answer"; sleep 0.5
  done
  python3 "$ROOT/tools/throughput/load.py" "$BASE" "$N" > "$out.load" 2>&1 || { cat "$out.load" >&2; die "the load on $server, $database"; }
  local s pid before
  pid=$(lsof -nP -t -iTCP:"$PORT" -sTCP:LISTEN | head -1)
  for s in $SCENARIOS; do
    # a short run first, not counted: connections, statements, the JIT
    scenario "$s" 3s > /dev/null 2>&1
    # the processor time the server itself takes for the run (not the
    # database server's, where there is one)
    before=$(ps -o cputime= -p "$pid")
    scenario "$s" "${SECONDS_EACH}s" > "$out.$s.json" 2> "$out.$s.err"
    echo "$before $(ps -o cputime= -p "$pid")" > "$out.$s.cpu"
  done
  ps -o rss= -p "$pid" | tr -d ' ' > "$out.rss"
  stop_server
  [ "$database" = sqlite ] || docker rm -f -v "$PG_NAME" > /dev/null 2>&1
}

# the numbers of oha's report: requests a second, a percentile of the
# latency in milliseconds, the answers that were not 2xx
report() {
  python3 - "$1" <<'PY'
import json, sys
def seconds(t):
    p = t.replace('-', ':').split(':')
    return float(p[-1]) + 60 * int(p[-2]) + (3600 * int(p[-3]) if len(p) > 2 else 0)
try:
    r = json.load(open(sys.argv[1]))
    a, b = open(sys.argv[1][:-5] + '.cpu').read().split()
    cores = (seconds(b) - seconds(a)) / r['summary']['total']
except Exception:
    print('- - - - -'); sys.exit()
codes = r.get('statusCodeDistribution', {})
# not answered with 2xx: another status, or no answer at all (a request cut
# off by the end of the run is neither)
bad = sum(v for k, v in codes.items() if not k.startswith('2'))
bad += sum(v for k, v in r.get('errorDistribution', {}).items() if 'deadline' not in k)
if not any(k.startswith('2') for k in codes):
    print('- - - - %d' % bad); sys.exit()
lat = r.get('latencyPercentiles', {})
ms = lambda v: '-' if v is None else '%.2fms' % (v * 1000)
print('%.0f %.2f %s %s %d' % (r['summary']['requestsPerSec'], cores, ms(lat.get('p50')), ms(lat.get('p99')), bad))
PY
}

case "$WHAT" in postgresql|sqlite) DATABASES=$WHAT ;; both) DATABASES="postgresql sqlite" ;; *) die "usage: tools/throughput.sh [postgresql|sqlite|both]" ;; esac
for d in $DATABASES; do
  for s in scaly csharp; do
    echo "throughput: $s on $d"
    measure "$d" "$s"
  done
done
for d in $DATABASES; do
  echo
  echo "$d: $N resources of each type, $CONNECTIONS connections, ${SECONDS_EACH} s a scenario"
  printf '%-10s | %8s %6s %9s %9s %5s | %8s %6s %9s %9s %5s | %6s %9s\n' scenario "scaly/s" cores p50 p99 "!2xx" "csharp/s" cores p50 p99 "!2xx" "a sec." "a core"
  for s in $SCENARIOS; do
    read -r ar ac a50 a99 abad <<< "$(report "$WORK/$d.scaly.$s.json")"
    read -r br bc b50 b99 bbad <<< "$(report "$WORK/$d.csharp.$s.json")"
    printf '%-10s | %8s %6s %9s %9s %5s | %8s %6s %9s %9s %5s | %6s %9s\n' "$s" "$ar" "$ac" "$a50" "$a99" "$abad" "$br" "$bc" "$b50" "$b99" "$bbad" \
      "$(awk -v x="$ar" -v y="$br" 'BEGIN { if (y > 0) printf "%.1fx", x / y }')" \
      "$(awk -v x="$ar" -v y="$br" -v c="$ac" -v e="$bc" 'BEGIN { if (y > 0 && c > 0 && e > 0) printf "%.1fx", (x / c) / (y / e) }')"
  done
  echo "(cores: the processor time the server process took, per second of the run; the last two columns: scaly's requests against csharp's, a second and a core)"
  printf 'memory after the run: scaly %s MB, csharp %s MB\n' "$(( $(cat "$WORK/$d.scaly.rss") / 1024 ))" "$(( $(cat "$WORK/$d.csharp.rss") / 1024 ))"
done
