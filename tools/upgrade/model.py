# tools/upgrade/model.py <model> <model with steps>: the model of
# tools/upgrade-test.sh -- fire's own, at version 5.0.2, with two upgrade
# steps and the ids their newsearch actions point at.
import re, sys

text = open(sys.argv[1], encoding='utf-8').read()

def name_search(resource, name):
    """Give the search `name` of `resource` an id, and answer it."""
    global text
    start = text.index('<resource name="%s"' % resource)
    end = text.index('</resource>', start)
    found = re.compile(r'<search name="%s" ([^>]*?)>' % re.escape(name)).search(text, start, end)
    assert found, (resource, name)
    ident = '%s.searches.%s' % (resource.lower(), name)
    if ' id="' not in found.group(0):
        text = text[:found.end() - 1] + ' id="%s">' % ident + text[found.end():]
    return ident

gender = name_search('Patient', 'gender')
family = name_search('Patient', 'family')
period = name_search('Practitioner', 'qualification-period')

head = '<fhir name="Fire" version="5.0.0">'
assert head in text
text = text.replace(head, '''<fhir name="Fire" version="5.0.2">
  <upgrades>
    <upgrade to="5.0.1">
      <newsearch ref="%s"/>
      <newsearch ref="%s"/>
    </upgrade>
    <upgrade to="5.0.2">
      <dropsearch resource="Practitioner" name="qualification-period"/>
      <newsearch ref="%s"/>
      <keepbinarydata/>
    </upgrade>
  </upgrades>''' % (gender, family, period))
open(sys.argv[2], 'w', encoding='utf-8').write(text)
