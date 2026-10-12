# tools/variants/nohistory/model.py <model> <variant>: fire's model with two
# types that keep no history -- Practitioner, and Binary, whose bytes lie
# in a table of their own.
import re, sys

text = open(sys.argv[1], encoding='utf-8').read()
for name in ('Practitioner', 'Binary'):
    found = re.search(r'<resource name="%s"[^>]*>' % name, text)
    assert found and 'history=' not in found.group(0), name
    text = text.replace(found.group(0), found.group(0)[:-1] + ' history="false">')
open(sys.argv[2], 'w', encoding='utf-8').write(text)
