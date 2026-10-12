# load.py <base> <n>: n Patients (tp0000..) and an Observation each (to0000..),
# put through the server in transaction Bundles of a hundred.
import sys, json, urllib.request
base, n = sys.argv[1], int(sys.argv[2])
for start in range(0, n, 100):
    entries = []
    for i in range(start, min(start + 100, n)):
        entries.append({'request': {'method': 'PUT', 'url': f'Patient/tp{i:04d}'}, 'resource': {
            'resourceType': 'Patient', 'id': f'tp{i:04d}', 'active': i % 3 != 0,
            'identifier': [{'system': 'urn:throughput', 'value': f'{i}'}],
            'name': [{'family': f'fam{i % 100:02d}x', 'given': [f'Given{i}', 'Zoë']}],
            'gender': ('female', 'male', 'other')[i % 3], 'birthDate': f'{1940 + i % 70}-{1 + i % 12:02d}-{1 + i % 28:02d}',
            'address': [{'line': [f'{i} Main Street'], 'city': f'City{i % 50}', 'postalCode': f'{10000 + i % 9000}', 'country': 'DE'}]}})
        entries.append({'request': {'method': 'PUT', 'url': f'Observation/to{i:04d}'}, 'resource': {
            'resourceType': 'Observation', 'id': f'to{i:04d}', 'status': 'final',
            'code': {'coding': [{'system': 'http://loinc.org', 'code': '8867-4', 'display': 'Heart rate'}]},
            'subject': {'reference': f'Patient/tp{i:04d}'}, 'effectiveDateTime': f'2026-{1 + i % 12:02d}-{1 + i % 28:02d}T10:00:00Z',
            'valueQuantity': {'value': 50 + i % 60, 'unit': '/min', 'system': 'http://unitsofmeasure.org', 'code': '/min'}}})
    body = json.dumps({'resourceType': 'Bundle', 'type': 'transaction', 'entry': entries}).encode()
    r = urllib.request.urlopen(urllib.request.Request(base + '/fire', data=body, method='POST',
        headers={'Content-Type': 'application/fhir+json', 'Accept': 'application/fhir+json'}), timeout=300)
    answer = json.load(r)
    bad = [e['response']['status'] for e in answer.get('entry', []) if not e['response']['status'].startswith('2')]
    if bad: sys.exit(f'load: {len(bad)} entries refused, the first {bad[0]}')
print(n, 'patients and observations loaded')
