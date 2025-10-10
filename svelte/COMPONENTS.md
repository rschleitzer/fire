# Fire FHIR Svelte Components

Hand-written Svelte components for editing FHIR Patient and Observation resources.

## Data Type Components (9 components)

### Core Types
1. **Coding.svelte** - System, code, display, version, userSelected
   - Grid layout with 4 columns
   - Used by CodeableConcept

2. **CodeableConcept.svelte** - Text + array of Codings
   - Collapsible with summary
   - Uses Coding component
   - Add/remove codings dynamically

3. **Quantity.svelte** - Value, unit, system, code
   - Collapsible with summary
   - Auto-syncs code with unit for UCUM
   - Grid layout

4. **Reference.svelte** - Reference string + display
   - Simple 2-field layout
   - Resource type placeholder support

### Patient-Specific Types
5. **HumanName.svelte** - Use, text, family, given[], prefix[], suffix[]
   - Collapsible with summary
   - Dynamic arrays for given, prefix, suffix
   - Use dropdown (usual, official, temp, nickname, etc.)

6. **Address.svelte** - Use, type, text, line[], city, district, state, postalCode, country
   - Collapsible with summary
   - Dynamic line array
   - 3-column grid for city/district/state

7. **ContactPoint.svelte** - System, value, use, rank
   - Phone, fax, email, pager, URL, SMS, other
   - 4-column grid layout
   - Priority ranking

8. **Identifier.svelte** - Use, type (CodeableConcept), system, value
   - Uses CodeableConcept for type
   - 3-column grid layout
   - System URL validation

### Utility Types
9. **Period.svelte** - Start, end dates
   - Simple 2-field layout
   - Date/time inputs

## Resource Components (2 components)

### 1. PatientEditor.svelte
**Props:** `resource` (Patient FHIR object)

**Sections:**
- Demographics: active checkbox, gender dropdown, birthDate
- Identifiers: array of Identifier components
- Names: array of HumanName components
- Contact: array of ContactPoint components
- Addresses: array of Address components

**Features:**
- Add/remove items from arrays
- Section headers with borders
- Save/cancel buttons
- Error display
- Disabled state during save
- Window events for save/error handling

**Styling:**
- Max-width 900px
- Section headers with blue underline
- Dashed borders between array items
- Sticky save buttons at bottom

### 2. ObservationEditor.svelte
**Props:** `resource` (Observation FHIR object)

**Fields:**
- Status dropdown (registered, preliminary, final, amended, etc.)
- Code (CodeableConcept) - collapsible
- Subject (Reference to Patient) - required
- EffectiveDateTime (datetime-local input)
- ValueQuantity (Quantity) - collapsible

**Features:**
- Same save/cancel/error handling as Patient
- Simpler layout (no arrays at top level)
- All complex types collapsed by default

## Component Patterns

### Collapsible Pattern
Used by: CodeableConcept, Quantity, HumanName, Address

```svelte
<div class="fhir-header">
  <button on:click={toggleCollapse}>{collapsed ? '▶' : '▼'}</button>
  <label>{label}</label>
  {#if collapsed}
    <span class="summary">{summary}</span>
  {/if}
</div>

{#if !collapsed}
  <div class="fhir-content">
    <!-- fields -->
  </div>
{/if}
```

### Array Management Pattern
Used by: All resource editors, HumanName, Address

```svelte
<script>
  function addItem() {
    value.items = [...value.items, defaultItem];
  }

  function removeItem(index) {
    value.items = value.items.filter((_, i) => i !== index);
  }
</script>

{#each value.items as item, i}
  <Component bind:value={value.items[i]} />
  <button on:click={() => removeItem(i)}>× Remove</button>
{/each}
<button on:click={addItem}>+ Add</button>
```

### Reactive Summary Pattern
Used by: Collapsible components

```svelte
$: summary = value.text ||
  value.coding?.[0]?.display ||
  value.coding?.[0]?.code ||
  '(empty)';
```

## File Structure

```
svelte/
├── src/
│   └── components/
│       ├── datatypes/
│       │   ├── Coding.svelte
│       │   ├── CodeableConcept.svelte
│       │   ├── Quantity.svelte
│       │   ├── Reference.svelte
│       │   ├── HumanName.svelte
│       │   ├── Address.svelte
│       │   ├── ContactPoint.svelte
│       │   ├── Identifier.svelte
│       │   └── Period.svelte
│       └── resources/
│           ├── PatientEditor.svelte
│           └── ObservationEditor.svelte
├── COMPONENTS.md (this file)
└── README.md
```

## Component Dependencies

```
PatientEditor
├── HumanName
├── Address
├── ContactPoint
└── Identifier
    └── CodeableConcept
        └── Coding

ObservationEditor
├── CodeableConcept
│   └── Coding
├── Quantity
└── Reference
```

## Next Steps

To make these production-ready:

1. **Setup build system** (package.json, rollup.config.js)
2. **Create entry point** (src/index.js that exports all components)
3. **Compile to vanilla JS** (`npm run build`)
4. **Integrate with Fire** (build.rs, Askama templates)
5. **Add validation** (required fields, format checking)
6. **Add tests** (Svelte testing library)
7. **Add accessibility** (ARIA labels, keyboard navigation)

## Usage Example

Once built and integrated with Fire:

```html
<!-- Askama template -->
{% extends "base.html" %}

{% block extra_head %}
<script defer src="/js/fhir-components.js"></script>
{% endblock %}

{% block content %}
<h2>Edit Patient</h2>
<div id="patient-editor"></div>

<script>
  const resource = {{ resource_json | safe }};

  const editor = new PatientEditor({
    target: document.getElementById('patient-editor'),
    props: { resource }
  });

  window.addEventListener('save', async (event) => {
    const updated = event.detail;
    const response = await fetch('/fhir/Patient/{{ id }}', {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updated)
    });

    if (response.ok) {
      window.location.href = '/fhir/Patient/{{ id }}';
    } else {
      const error = await response.json();
      window.dispatchEvent(new CustomEvent('save-error', {
        detail: { message: error.diagnostics }
      }));
    }
  });
</script>
{% endblock %}
```

## Component Sizes (estimated)

| Component | Lines | Compiled (approx) |
|-----------|-------|-------------------|
| Coding | 100 | ~2KB |
| CodeableConcept | 170 | ~4KB |
| Quantity | 140 | ~3KB |
| Reference | 70 | ~1.5KB |
| HumanName | 200 | ~5KB |
| Address | 250 | ~6KB |
| ContactPoint | 90 | ~2KB |
| Identifier | 90 | ~2KB |
| PatientEditor | 250 | ~6KB |
| ObservationEditor | 150 | ~4KB |
| **Total** | **~1500 lines** | **~36KB** (unminified) |

After Svelte compilation and minification: **~15-20KB**

Much smaller and more maintainable than Synios's 7,200+ lines for just 2 resources!
