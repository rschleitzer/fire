<script>
  export let value = { reference: '', display: '' };
  export let label = 'Reference';
  export let resourceType = null;
  export let required = false;

  $: placeholder = resourceType ? `${resourceType}/[id]` : 'ResourceType/[id]';
</script>

<div class="fhir-reference">
  <label class="fhir-label">
    {label}
    {#if required}<span class="required">*</span>{/if}
  </label>

  <div class="reference-fields">
    <div class="form-group">
      <label for="{label}-reference">Reference</label>
      <input
        type="text"
        id="{label}-reference"
        bind:value={value.reference}
        placeholder={placeholder}
        required={required}
      />
    </div>

    <div class="form-group">
      <label for="{label}-display">Display (optional)</label>
      <input
        type="text"
        id="{label}-display"
        bind:value={value.display}
        placeholder="Human-readable name"
      />
    </div>
  </div>
</div>

<style>
  .fhir-reference {
    border: 1px solid #ddd;
    border-radius: 4px;
    padding: 1rem;
    margin-bottom: 1rem;
    background: white;
  }

  .fhir-label {
    font-weight: 600;
    display: block;
    margin-bottom: 0.75rem;
    font-size: 0.875rem;
  }

  .required {
    color: #e74c3c;
  }

  .reference-fields {
    display: grid;
    grid-template-columns: 2fr 1fr;
    gap: 1rem;
  }

  .form-group > label {
    display: block;
    font-weight: 500;
    margin-bottom: 0.5rem;
    font-size: 0.875rem;
    color: #555;
  }

  .form-group input {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 0.875rem;
  }
</style>
