<script>
  import CodeableConcept from './CodeableConcept.svelte';

  export let value = { use: '', type: {}, system: '', value: '' };
  export let label = 'Identifier';
  export let required = false;

  // Ensure type object has coding array
  $: {
    if (!value.type) value.type = { text: '', coding: [] };
    if (!value.type.coding) value.type.coding = [];
  }

  $: summary = value.value || '(empty)';
</script>

<div class="fhir-identifier">
  <div class="id-grid">
    <div class="form-group">
      <label for="{label}-use">Use</label>
      <select id="{label}-use" bind:value={value.use}>
        <option value="">-- Select --</option>
        <option value="usual">Usual</option>
        <option value="official">Official</option>
        <option value="temp">Temp</option>
        <option value="secondary">Secondary</option>
        <option value="old">Old</option>
      </select>
    </div>

    <div class="form-group">
      <label for="{label}-system">System</label>
      <input
        type="url"
        id="{label}-system"
        bind:value={value.system}
        placeholder="http://hl7.org/fhir/sid/us-ssn"
      />
    </div>

    <div class="form-group">
      <label for="{label}-value">Value {#if required}<span class="required">*</span>{/if}</label>
      <input
        type="text"
        id="{label}-value"
        bind:value={value.value}
        placeholder="e.g., 123-45-6789"
        required={required}
      />
    </div>
  </div>

  <CodeableConcept bind:value={value.type} label="Type" collapsed={true} />
</div>

<style>
  .fhir-identifier {
    padding: 0.75rem;
    background: #fafafa;
    border-radius: 4px;
    margin-bottom: 0.5rem;
  }

  .id-grid {
    display: grid;
    grid-template-columns: 1fr 2fr 1fr;
    gap: 0.75rem;
    align-items: end;
    margin-bottom: 0.75rem;
  }

  .form-group > label {
    display: block;
    font-weight: 500;
    margin-bottom: 0.25rem;
    font-size: 0.75rem;
    color: #555;
  }

  .form-group input,
  .form-group select {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 0.875rem;
  }

  .required {
    color: #e74c3c;
  }
</style>
