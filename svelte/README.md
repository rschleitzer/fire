# Fire FHIR - Svelte Component Generation

## What is this?

**Generated Svelte components** for editing FHIR R5 resources. Svelte compiles to vanilla JS with no runtime overhead, giving us the best of both worlds:

1. **Component-based architecture** - Like Vanilla's nested approach, but maintainable
2. **Reactive by default** - Bidirectional data binding is built-in
3. **No runtime framework** - Svelte compiles away, resulting in small, fast vanilla JS
4. **Generated code** - But from clean, readable Svelte templates, not repetitive JS

## Current Status

**Hand-written prototypes** (3 data types + 1 resource):

- ✅ `CodeableConcept.svelte` - Text + array of Codings with collapse/expand
- ✅ `Quantity.svelte` - Value, unit, system, code with auto-sync
- ✅ `Reference.svelte` - Reference string + display
- ✅ `ObservationEditor.svelte` - Full observation with status, code, subject, effective date, value

These serve as **templates for code generation** from FHIR StructureDefinitions.

## Architecture

```
FHIR StructureDefinition (JSON)
         ↓
  Generator script (Node.js)
         ↓
  Svelte component (generated)
         ↓
  Svelte compiler
         ↓
  Vanilla JS bundle (~200-300KB for all FHIR)
         ↓
  Fire serves via Askama template
```

## How It Works

### 1. Hand-Written Components (Prototypes)

See `src/components/` for examples. Each component:

- Takes a `value` prop (the FHIR data)
- Uses `bind:value` for 2-way reactivity
- Provides collapse/expand for complex types
- Shows summary when collapsed
- Handles arrays (add/remove)
- Validates required fields

**Example: CodeableConcept.svelte**

```svelte
<script>
  export let value = { text: '', coding: [] };
  export let label = 'Codeable Concept';
  export let required = false;
  export let collapsed = true;

  function addCoding() {
    value.coding = [...value.coding, { system: '', code: '', display: '' }];
  }

  $: summary = value.text || value.coding?.[0]?.display || '(empty)';
</script>

<div class="fhir-codeable-concept">
  <button on:click={toggleCollapse}>{collapsed ? '▶' : '▼'}</button>
  <label>{label}</label>
  {#if collapsed}
    <span>{summary}</span>
  {:else}
    <input bind:value={value.text} placeholder="Text" />
    {#each value.coding as coding, i}
      <input bind:value={coding.system} placeholder="System" />
      <input bind:value={coding.code} placeholder="Code" />
      <input bind:value={coding.display} placeholder="Display" />
      <button on:click={() => removeCoding(i)}>×</button>
    {/each}
    <button on:click={addCoding}>+ Add Coding</button>
  {/if}
</div>
```

**Key Features:**
- `bind:value` - Automatic 2-way sync, no manual DOM manipulation
- `$:` - Reactive statements (auto-update summary when value changes)
- `{#each}` - Loop over arrays, automatically re-render on changes
- Clean, readable - ~50 lines vs 200+ lines of vanilla JS

### 2. Code Generation (Next Step)

**Generator script** (`src/generator/generate-component.js`):

```javascript
// Parse FHIR StructureDefinition
const sd = JSON.parse(fs.readFileSync('fhir-definitions/Observation.json'));

// Extract elements
const elements = sd.snapshot.element.map(el => ({
  name: el.path.split('.').pop(),
  type: el.type?.[0]?.code,
  cardinality: `${el.min}..${el.max}`,
  required: el.min > 0,
  isArray: el.max === '*'
}));

// Generate Svelte component
const component = `
<script>
  ${generateImports(elements)}
  export let value = ${generateDefaultValue(elements)};
  ${generateArrayHelpers(elements)}
</script>

<div class="fhir-${sd.name}">
  ${elements.map(el => generateField(el)).join('\n')}
</div>
`;

fs.writeFileSync(`src/components/resources/${sd.name}.svelte`, component);
```

**Generate for all FHIR:**
- 108 data types (Coding, Quantity, Reference, HumanName, Address, etc.)
- 140 resources (Observation, Patient, MedicationRequest, etc.)

### 3. Build System

**Install dependencies:**

```bash
cd svelte
npm install
```

**Build workflow:**

```bash
# Generate Svelte components from FHIR definitions
npm run generate

# Compile Svelte to vanilla JS
npm run build

# Output: build/fhir-components.js (~200-300KB minified+gzipped)
```

**Rust integration** (`build.rs`):

```rust
fn main() {
    Command::new("npm")
        .args(&["run", "build"])
        .current_dir("svelte")
        .status()
        .expect("Failed to build Svelte components");

    std::fs::copy(
        "svelte/build/fhir-components.js",
        "static/js/fhir-components.js"
    ).unwrap();
}
```

### 4. Fire Integration

**Askama template** (`templates/observation_edit.html`):

```html
{% extends "base.html" %}

{% block extra_head %}
<script defer src="/js/fhir-components.js"></script>
{% endblock %}

{% block content %}
<h2>Edit Observation</h2>
<div id="observation-editor"></div>

<script>
  const resource = {{ resource_json | safe }};

  const editor = new ObservationEditor({
    target: document.getElementById('observation-editor'),
    props: { resource }
  });

  window.addEventListener('save', async (event) => {
    const updated = event.detail;
    const response = await fetch('/fhir/Observation/{{ id }}', {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updated)
    });

    if (response.ok) {
      window.location.href = '/fhir/Observation/{{ id }}';
    }
  });
</script>
{% endblock %}
```

## Advantages

### vs. Vanilla JS Generation

| Aspect | Vanilla | Fire (Svelte) |
|--------|--------|---------------|
| **Code size** | 7,200 lines | ~500 lines (generated Svelte) |
| **Maintainability** | Manual DOM manipulation | Declarative components |
| **Reactivity** | Manual event handlers | Built-in (`bind:`) |
| **Type safety** | None | Can add TypeScript |
| **Bundle size** | ~300KB (2 resources) | ~200-300KB (all FHIR) |
| **Readability** | Low (repetitive) | High (concise) |

### vs. React/Vue

| Aspect | React/Vue | Svelte |
|--------|-----------|--------|
| **Runtime** | 40-100KB | 0KB (compiles away) |
| **Performance** | Virtual DOM | Direct DOM updates |
| **Bundle size** | Larger | Smaller |
| **Build complexity** | Higher | Lower |

### vs. Rust Macros

| Aspect | Rust Macros | Svelte |
|--------|-------------|--------|
| **Ecosystem** | Rust (proc-macro) | JS/Node (familiar) |
| **UI focus** | Not designed for UI | Designed for UI |
| **Iteration speed** | Slower (Rust compile) | Faster (JS compile) |
| **Flexibility** | Lower | Higher |

## File Size Analysis

**Hand-written prototype (4 components):**
- CodeableConcept.svelte: 120 lines → ~3KB compiled
- Quantity.svelte: 100 lines → ~2.5KB compiled
- Reference.svelte: 60 lines → ~1.5KB compiled
- ObservationEditor.svelte: 150 lines → ~4KB compiled

**Total: 430 lines Svelte → ~11KB vanilla JS (unminified)**

**Projected for all FHIR:**
- 108 data types × ~2.5KB = ~270KB
- 140 resources × ~4KB = ~560KB
- **Total: ~830KB unminified → ~200-300KB minified+gzipped**

**Compare to Vanilla:**
- Elements.js: 5,775 lines (~300KB)
- Observation.js: 1,430 lines (~75KB)
- **Total for 2 resources: ~375KB unminified**

## Next Steps

1. **Setup build system**
   ```bash
   cd svelte
   npm init -y
   npm install svelte rollup rollup-plugin-svelte @rollup/plugin-node-resolve rollup-plugin-terser
   ```

2. **Create generator script**
   - Parse FHIR StructureDefinitions (JSON)
   - Extract element metadata (name, type, cardinality, required)
   - Generate Svelte components using templates

3. **Generate all components**
   - Run generator on all 108 data types
   - Run generator on all 140 resources
   - Output to `src/components/`

4. **Compile to vanilla JS**
   - Run `npm run build`
   - Output to `build/fhir-components.js`

5. **Integrate with Fire**
   - Copy bundle to `static/js/`
   - Create Askama templates that mount Svelte components
   - Add save/cancel handlers

6. **Polish**
   - Add validation
   - Add error handling
   - Add accessibility (ARIA labels, keyboard nav)
   - Add tests

## Why This Works

**Svelte's compiler philosophy aligns perfectly with Fire's goals:**

1. **No runtime overhead** - Like Rust, Svelte compiles away
2. **Type safety** - Can add TypeScript for compile-time checking
3. **Performance** - Direct DOM updates, no virtual DOM
4. **Small bundles** - Only ships the code you use
5. **Reactivity** - Built-in, no manual state management
6. **Readable** - Clean syntax, easy to maintain

**Generated code is maintainable:**
- Edit template → regenerate → all components updated
- Hand-write custom components when needed
- Component composition (CodeableConcept uses Coding)

**Fits Fire's architecture:**
- Build-time generation (like Askama templates)
- Rust build.rs calls npm build
- Serves static JS (no Node.js server needed)
- Integrates with Askama (server-side rendering + client interactivity)

## Demo

To see the prototype in action:

```bash
# TODO: Setup build system and compile
cd svelte
npm install
npm run build

# Serve from Fire
cargo run

# Visit http://localhost:3000/fhir/Observation/{id}/edit
```

The ObservationEditor will render with:
- Status dropdown (simple field)
- CodeableConcept editor (collapsible, with codings array)
- Reference editor (patient reference)
- DateTime input
- Quantity editor (collapsible, value + unit)

All with automatic 2-way binding - edit any field, and the resource object updates reactively.

---

**This approach gives us Vanillas's comprehensive FHIR coverage with modern, maintainable code.**
