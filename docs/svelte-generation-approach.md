# Fire FHIR Editing - Svelte Component Generation

## Vision

Generate **Svelte components** from FHIR StructureDefinitions, compile to vanilla JS, integrate with Fire's Askama templates. Clean, maintainable, performant.

## Architecture

```
FHIR StructureDefinitions (XML/JSON)
         ↓
    Build script (Rust or Node)
         ↓
    Svelte components (generated)
         ↓
    svelte compiler
         ↓
    Vanilla JS bundle (no runtime)
         ↓
    Fire serves via Askama
```

## Example: CodeableConcept Component

### Generated Svelte Component

**File: `svelte/src/components/CodeableConcept.svelte`**

```svelte
<script>
  export let value = { text: '', coding: [] };
  export let label = 'Codeable Concept';
  export let required = false;

  function addCoding() {
    value.coding = [...value.coding, { system: '', code: '', display: '' }];
  }

  function removeCoding(index) {
    value.coding = value.coding.filter((_, i) => i !== index);
  }
</script>

<div class="fhir-codeable-concept">
  <label class="fhir-label">
    {label}
    {#if required}<span class="required">*</span>{/if}
  </label>

  <div class="form-group">
    <label>Text</label>
    <input type="text" bind:value={value.text} placeholder="Human-readable text" />
  </div>

  <div class="form-group">
    <label>Codings</label>
    {#each value.coding as coding, i}
      <div class="coding-row">
        <input type="text" bind:value={coding.system} placeholder="System (e.g., http://loinc.org)" />
        <input type="text" bind:value={coding.code} placeholder="Code" />
        <input type="text" bind:value={coding.display} placeholder="Display" />
        <button type="button" on:click={() => removeCoding(i)}>×</button>
      </div>
    {/each}
    <button type="button" on:click={addCoding}>+ Add Coding</button>
  </div>
</div>

<style>
  .fhir-codeable-concept {
    border: 1px solid #ddd;
    padding: 1rem;
    border-radius: 4px;
    margin-bottom: 1rem;
  }
  .coding-row {
    display: grid;
    grid-template-columns: 2fr 1fr 2fr auto;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
  }
  .required {
    color: red;
  }
</style>
```

**Compiles to:** `~3KB` of vanilla JS (vs 200+ lines of manual JS in Synios)

### Generated Observation Component

**File: `svelte/src/components/Observation.svelte`**

```svelte
<script>
  import CodeableConcept from './CodeableConcept.svelte';
  import Quantity from './Quantity.svelte';
  import Reference from './Reference.svelte';

  export let resource = {
    resourceType: 'Observation',
    id: '',
    status: 'final',
    code: { text: '', coding: [] },
    subject: { reference: '' },
    effectiveDateTime: '',
    valueQuantity: { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' }
  };

  // Reactive: automatically syncs with parent
  $: console.log('Resource updated:', resource);

  function save() {
    // Dispatch event to parent
    const event = new CustomEvent('save', { detail: resource });
    window.dispatchEvent(event);
  }
</script>

<form on:submit|preventDefault={save}>
  <div class="form-group">
    <label for="status">Status *</label>
    <select id="status" bind:value={resource.status} required>
      <option value="registered">Registered</option>
      <option value="preliminary">Preliminary</option>
      <option value="final">Final</option>
      <option value="amended">Amended</option>
      <option value="corrected">Corrected</option>
      <option value="cancelled">Cancelled</option>
      <option value="entered-in-error">Entered in Error</option>
      <option value="unknown">Unknown</option>
    </select>
  </div>

  <CodeableConcept bind:value={resource.code} label="Code" required={true} />

  <Reference bind:value={resource.subject} label="Subject" resourceType="Patient" required={true} />

  <div class="form-group">
    <label for="effectiveDateTime">Effective Date/Time</label>
    <input type="datetime-local" id="effectiveDateTime" bind:value={resource.effectiveDateTime} />
  </div>

  <Quantity bind:value={resource.valueQuantity} label="Value (Quantity)" />

  <div class="button-group">
    <button type="submit" class="btn btn-primary">Save</button>
    <button type="button" class="btn btn-secondary" on:click={() => history.back()}>Cancel</button>
  </div>
</form>

<style>
  .form-group {
    margin-bottom: 1.5rem;
  }
  .button-group {
    display: flex;
    gap: 1rem;
    margin-top: 2rem;
  }
</style>
```

## Integration with Fire

### Askama Template

**File: `templates/observation_edit.html`**

```html
{% extends "base.html" %}

{% block title %}Edit Observation - Fire FHIR Server{% endblock %}

{% block extra_head %}
<!-- Compiled Svelte bundle (no runtime needed) -->
<script defer src="/js/fhir-components.js"></script>
{% endblock %}

{% block content %}
<h2>Edit Observation: {{ id }}</h2>

<!-- Mount point for Svelte component -->
<div id="observation-editor"></div>

<script>
  // Initialize Svelte component with FHIR resource data
  const resource = {{ resource_json | safe }};

  // Create Svelte component instance
  const editor = new ObservationEditor({
    target: document.getElementById('observation-editor'),
    props: { resource }
  });

  // Listen for save event
  window.addEventListener('save', async (event) => {
    const updatedResource = event.detail;

    try {
      const response = await fetch('/fhir/Observation/{{ id }}', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(updatedResource)
      });

      if (response.ok) {
        window.location.href = '/fhir/Observation/{{ id }}';
      } else {
        const error = await response.json();
        alert('Validation error: ' + error.diagnostics);
      }
    } catch (err) {
      alert('Network error: ' + err.message);
    }
  });
</script>
{% endblock %}
```

## Build System

### Project Structure

```
fire/
├── svelte/
│   ├── package.json
│   ├── rollup.config.js
│   ├── src/
│   │   ├── components/
│   │   │   ├── primitives/
│   │   │   │   ├── String.svelte
│   │   │   │   ├── Boolean.svelte
│   │   │   │   ├── DateTime.svelte
│   │   │   │   └── ...
│   │   │   ├── datatypes/
│   │   │   │   ├── CodeableConcept.svelte
│   │   │   │   ├── Coding.svelte
│   │   │   │   ├── Quantity.svelte
│   │   │   │   ├── Reference.svelte
│   │   │   │   ├── HumanName.svelte
│   │   │   │   ├── Address.svelte
│   │   │   │   └── ... (108 data types)
│   │   │   ├── resources/
│   │   │   │   ├── Observation.svelte
│   │   │   │   ├── Patient.svelte
│   │   │   │   └── ... (140 resources)
│   │   │   └── index.js
│   │   └── generator/
│   │       ├── parse-fhir.js        # Parse StructureDefinitions
│   │       ├── generate-component.js # Generate .svelte files
│   │       └── templates/
│   │           ├── datatype.template.svelte
│   │           └── resource.template.svelte
│   └── build/
│       └── fhir-components.js       # Compiled output
├── static/
│   └── js/
│       └── fhir-components.js       # Copied from svelte/build/
├── templates/
│   ├── observation_edit.html
│   ├── patient_edit.html
│   └── ...
└── build.rs                         # Rust build script (runs npm build)
```

### Generator Script

**File: `svelte/src/generator/generate-component.js`**

```javascript
const fs = require('fs');
const path = require('path');

// Parse FHIR StructureDefinition JSON
function parseStructureDefinition(sdPath) {
  const sd = JSON.parse(fs.readFileSync(sdPath, 'utf8'));

  return {
    name: sd.name,
    type: sd.type,
    elements: sd.snapshot.element
      .filter(el => el.path.includes('.'))
      .map(el => ({
        name: el.path.split('.').pop(),
        type: el.type?.[0]?.code || 'string',
        cardinality: `${el.min}..${el.max}`,
        required: el.min > 0,
        isArray: el.max === '*'
      }))
  };
}

// Generate Svelte component from template
function generateComponent(structDef, template) {
  let component = template;

  // Replace placeholders
  component = component.replace(/{{NAME}}/g, structDef.name);

  // Generate imports
  const imports = structDef.elements
    .filter(el => isComplexType(el.type))
    .map(el => `import ${el.type} from './${el.type}.svelte';`)
    .join('\n');

  component = component.replace('{{IMPORTS}}', imports);

  // Generate props
  const props = structDef.elements
    .map(el => {
      const defaultValue = getDefaultValue(el);
      return `  ${el.name}: ${defaultValue},`;
    })
    .join('\n');

  component = component.replace('{{PROPS}}', props);

  // Generate form fields
  const fields = structDef.elements
    .map(el => generateField(el))
    .join('\n\n');

  component = component.replace('{{FIELDS}}', fields);

  return component;
}

function generateField(element) {
  const { name, type, required, isArray } = element;

  if (isPrimitive(type)) {
    return `
<div class="form-group">
  <label for="${name}">${humanize(name)}${required ? ' *' : ''}</label>
  <input type="${getInputType(type)}" id="${name}" bind:value={value.${name}} ${required ? 'required' : ''} />
</div>`;
  }

  if (isArray) {
    return `
<div class="form-group">
  <label>${humanize(name)}${required ? ' *' : ''}</label>
  {#each value.${name} as item, i}
    <${type} bind:value={value.${name}[i]} />
    <button type="button" on:click={() => remove${name}(i)}>Remove</button>
  {/each}
  <button type="button" on:click={add${name}}>+ Add ${humanize(name)}</button>
</div>`;
  }

  return `<${type} bind:value={value.${name}} label="${humanize(name)}" required={${required}} />`;
}

// Helper functions
function isPrimitive(type) {
  return ['string', 'boolean', 'integer', 'decimal', 'dateTime', 'date', 'code', 'uri'].includes(type);
}

function isComplexType(type) {
  return !isPrimitive(type) && type !== 'BackboneElement';
}

function getInputType(type) {
  const types = {
    'string': 'text', 'code': 'text', 'uri': 'url',
    'boolean': 'checkbox', 'integer': 'number', 'decimal': 'number',
    'date': 'date', 'dateTime': 'datetime-local'
  };
  return types[type] || 'text';
}

function getDefaultValue(element) {
  if (element.isArray) return '[]';
  if (isPrimitive(element.type)) {
    return { 'boolean': 'false', 'integer': '0', 'decimal': '0.0' }[element.type] || "''";
  }
  return '{}';
}

function humanize(str) {
  return str.replace(/([A-Z])/g, ' $1').replace(/^./, s => s.toUpperCase());
}

// Main generator
function generateAll() {
  const fhirPath = './fhir-r5-definitions';
  const outputPath = './src/components';

  // Generate data type components
  const dataTypes = fs.readdirSync(`${fhirPath}/datatypes`);
  dataTypes.forEach(file => {
    const sd = parseStructureDefinition(`${fhirPath}/datatypes/${file}`);
    const template = fs.readFileSync('./src/generator/templates/datatype.template.svelte', 'utf8');
    const component = generateComponent(sd, template);
    fs.writeFileSync(`${outputPath}/datatypes/${sd.name}.svelte`, component);
    console.log(`Generated ${sd.name}.svelte`);
  });

  // Generate resource components
  const resources = fs.readdirSync(`${fhirPath}/resources`);
  resources.forEach(file => {
    const sd = parseStructureDefinition(`${fhirPath}/resources/${file}`);
    const template = fs.readFileSync('./src/generator/templates/resource.template.svelte', 'utf8');
    const component = generateComponent(sd, template);
    fs.writeFileSync(`${outputPath}/resources/${sd.name}.svelte`, component);
    console.log(`Generated ${sd.name}.svelte`);
  });
}

generateAll();
```

### Svelte Template

**File: `svelte/src/generator/templates/datatype.template.svelte`**

```svelte
<script>
{{IMPORTS}}

  export let value = {
{{PROPS}}
  };
  export let label = '{{NAME}}';
  export let required = false;

  // Array manipulation functions (generated for array fields)
  // ... add/remove functions ...
</script>

<div class="fhir-{{NAME}}">
  <label class="fhir-label">
    {label}
    {#if required}<span class="required">*</span>{/if}
  </label>

{{FIELDS}}
</div>

<style>
  .fhir-{{NAME}} {
    border: 1px solid #e0e0e0;
    padding: 1rem;
    border-radius: 4px;
    margin-bottom: 0.5rem;
  }
  .fhir-label {
    font-weight: 600;
    display: block;
    margin-bottom: 0.5rem;
  }
  .required {
    color: #e74c3c;
  }
</style>
```

### Build Configuration

**File: `svelte/package.json`**

```json
{
  "name": "fire-fhir-components",
  "version": "1.0.0",
  "scripts": {
    "generate": "node src/generator/generate-component.js",
    "build": "npm run generate && rollup -c",
    "dev": "rollup -c -w"
  },
  "devDependencies": {
    "@rollup/plugin-node-resolve": "^15.0.0",
    "rollup": "^3.0.0",
    "rollup-plugin-svelte": "^7.0.0",
    "rollup-plugin-terser": "^7.0.0",
    "svelte": "^4.0.0"
  }
}
```

**File: `svelte/rollup.config.js`**

```javascript
import svelte from 'rollup-plugin-svelte';
import resolve from '@rollup/plugin-node-resolve';
import { terser } from 'rollup-plugin-terser';

export default {
  input: 'src/components/index.js',
  output: {
    format: 'iife',
    name: 'FHIRComponents',
    file: 'build/fhir-components.js',
    globals: {}
  },
  plugins: [
    svelte({
      compilerOptions: {
        // Generate code that works without a runtime
        generate: 'dom',
        hydratable: false
      }
    }),
    resolve({ browser: true }),
    terser()
  ]
};
```

**File: `svelte/src/components/index.js`**

```javascript
// Export all generated components
export { default as ObservationEditor } from './resources/Observation.svelte';
export { default as PatientEditor } from './resources/Patient.svelte';
export { default as CodeableConcept } from './datatypes/CodeableConcept.svelte';
export { default as Quantity } from './datatypes/Quantity.svelte';
// ... 140+ more exports
```

### Rust Build Integration

**File: `build.rs`**

```rust
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=svelte/src");

    // Run npm build to generate Svelte components
    let status = Command::new("npm")
        .args(&["run", "build"])
        .current_dir("svelte")
        .status()
        .expect("Failed to build Svelte components");

    if !status.success() {
        panic!("Svelte build failed");
    }

    // Copy built JS to static directory
    std::fs::copy(
        "svelte/build/fhir-components.js",
        "static/js/fhir-components.js"
    ).expect("Failed to copy Svelte bundle");
}
```

## Advantages

### vs. Synios Approach
- **7,200 lines** → **~500 lines** (generated Svelte is much more concise)
- **Repetitive vanilla JS** → **Composable Svelte components**
- **Manual DOM manipulation** → **Reactive bindings**
- **No type safety** → **Can add TypeScript to Svelte**
- **Hard to maintain** → **Edit templates, regenerate**

### vs. React/Vue
- **No runtime** (Svelte compiles away) → smaller bundle
- **Better performance** (no virtual DOM)
- **Simpler** (less boilerplate than React)
- **No build server needed** (compiles to static JS)

### vs. Rust Macros
- **Familiar JS ecosystem** (npm, Svelte, Rollup)
- **Easy to iterate** (change template, rebuild)
- **Better for UI** (Svelte is designed for this)
- **Gradual adoption** (can hand-write some components)

## Next Steps

1. **Prototype**: Hand-write 2-3 Svelte components (CodeableConcept, Quantity, Observation)
2. **Test integration**: Serve from Fire, verify size/performance
3. **Build generator**: Script to parse FHIR StructureDefinitions → Svelte
4. **Generate all**: 108 data types + 140 resources
5. **Polish**: Styling, validation, error handling, accessibility

## Bundle Size Estimate

- **Svelte runtime**: 0KB (compiles away)
- **Per component**: ~1-2KB (primitives) to ~5KB (complex resources)
- **Total for all FHIR**: ~200-300KB minified+gzipped
- **Compare to**: Synios ~300KB unminified vanilla JS for just 2 resources

Much better, and infinitely more maintainable.
