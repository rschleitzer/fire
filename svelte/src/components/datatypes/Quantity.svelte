<script>
  export let value = { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' };
  export let label = 'Quantity';
  export let required = false;
  export let collapsed = true;

  function toggleCollapse() {
    collapsed = !collapsed;
  }

  // Sync unit and code (often the same for UCUM)
  $: if (value.system === 'http://unitsofmeasure.org' && value.unit && !value.code) {
    value.code = value.unit;
  }

  $: summary = value.value != null ? `${value.value} ${value.unit || value.code || ''}`.trim() : '(empty)';
</script>

<div class="fhir-quantity">
  <div class="fhir-header">
    <button type="button" class="collapse-btn" on:click={toggleCollapse}>
      {collapsed ? '▶' : '▼'}
    </button>
    <label class="fhir-label">
      {label}
      {#if required}<span class="required">*</span>{/if}
    </label>
    {#if collapsed}
      <span class="summary">{summary}</span>
    {/if}
  </div>

  {#if !collapsed}
    <div class="fhir-content">
      <div class="quantity-grid">
        <div class="form-group">
          <label for="{label}-value">Value</label>
          <input
            type="number"
            id="{label}-value"
            bind:value={value.value}
            step="any"
            placeholder="e.g., 85.5"
          />
        </div>

        <div class="form-group">
          <label for="{label}-unit">Unit</label>
          <input
            type="text"
            id="{label}-unit"
            bind:value={value.unit}
            placeholder="e.g., kg, cm, mg/dL"
          />
        </div>
      </div>

      <div class="form-group">
        <label for="{label}-system">System</label>
        <input
          type="url"
          id="{label}-system"
          bind:value={value.system}
          placeholder="http://unitsofmeasure.org"
        />
      </div>

      <div class="form-group">
        <label for="{label}-code">Code</label>
        <input
          type="text"
          id="{label}-code"
          bind:value={value.code}
          placeholder="UCUM code (often same as unit)"
        />
      </div>
    </div>
  {/if}
</div>

<style>
  .fhir-quantity {
    border: 1px solid #ddd;
    border-radius: 4px;
    margin-bottom: 1rem;
    background: white;
  }

  .fhir-header {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem 1rem;
    background: #f8f9fa;
    border-bottom: 1px solid #ddd;
  }

  .collapse-btn {
    background: none;
    border: none;
    cursor: pointer;
    font-size: 0.875rem;
    padding: 0;
    width: 20px;
    text-align: left;
  }

  .fhir-label {
    font-weight: 600;
    margin: 0;
    font-size: 0.875rem;
  }

  .summary {
    color: #666;
    font-size: 0.875rem;
    font-style: italic;
  }

  .required {
    color: #e74c3c;
  }

  .fhir-content {
    padding: 1rem;
  }

  .quantity-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
    margin-bottom: 1rem;
  }

  .form-group {
    margin-bottom: 1rem;
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
