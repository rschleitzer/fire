<script>
  export let value = { use: '', type: '', text: '', line: [], city: '', district: '', state: '', postalCode: '', country: '' };
  export let label = 'Address';
  export let required = false;
  export let collapsed = true;

  function toggleCollapse() {
    collapsed = !collapsed;
  }

  function addLine() {
    value.line = [...value.line, ''];
  }

  function removeLine(index) {
    value.line = value.line.filter((_, i) => i !== index);
  }

  $: summary = value.text || [
    ...(value.line || []),
    value.city,
    value.state,
    value.postalCode,
    value.country
  ].filter(Boolean).join(', ') || '(empty)';
</script>

<div class="fhir-address">
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
      <div class="address-grid">
        <div class="form-group">
          <label for="{label}-use">Use</label>
          <select id="{label}-use" bind:value={value.use}>
            <option value="">-- Select --</option>
            <option value="home">Home</option>
            <option value="work">Work</option>
            <option value="temp">Temp</option>
            <option value="old">Old</option>
            <option value="billing">Billing</option>
          </select>
        </div>

        <div class="form-group">
          <label for="{label}-type">Type</label>
          <select id="{label}-type" bind:value={value.type}>
            <option value="">-- Select --</option>
            <option value="postal">Postal</option>
            <option value="physical">Physical</option>
            <option value="both">Both</option>
          </select>
        </div>
      </div>

      <div class="form-group">
        <label for="{label}-text">Text (full address)</label>
        <input
          type="text"
          id="{label}-text"
          bind:value={value.text}
          placeholder="e.g., 123 Main St, Anytown, ST 12345"
        />
      </div>

      <div class="form-group">
        <label>Street Address Lines</label>
        {#each value.line as line, i}
          <div class="array-item">
            <input type="text" bind:value={value.line[i]} placeholder="Address line {i + 1}" />
            <button type="button" class="btn-remove-sm" on:click={() => removeLine(i)}>×</button>
          </div>
        {/each}
        <button type="button" class="btn-add-sm" on:click={addLine}>+ Add Line</button>
      </div>

      <div class="address-grid-3">
        <div class="form-group">
          <label for="{label}-city">City</label>
          <input
            type="text"
            id="{label}-city"
            bind:value={value.city}
            placeholder="e.g., Anytown"
          />
        </div>

        <div class="form-group">
          <label for="{label}-district">District</label>
          <input
            type="text"
            id="{label}-district"
            bind:value={value.district}
            placeholder="e.g., County"
          />
        </div>

        <div class="form-group">
          <label for="{label}-state">State/Province</label>
          <input
            type="text"
            id="{label}-state"
            bind:value={value.state}
            placeholder="e.g., CA"
          />
        </div>
      </div>

      <div class="address-grid">
        <div class="form-group">
          <label for="{label}-postalCode">Postal Code</label>
          <input
            type="text"
            id="{label}-postalCode"
            bind:value={value.postalCode}
            placeholder="e.g., 12345"
          />
        </div>

        <div class="form-group">
          <label for="{label}-country">Country</label>
          <input
            type="text"
            id="{label}-country"
            bind:value={value.country}
            placeholder="e.g., US"
          />
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .fhir-address {
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

  .address-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
    margin-bottom: 1rem;
  }

  .address-grid-3 {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr;
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

  .form-group input,
  .form-group select {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 0.875rem;
  }

  .array-item {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
  }

  .array-item input {
    flex: 1;
  }

  .btn-remove-sm {
    background: #e74c3c;
    color: white;
    border: none;
    border-radius: 4px;
    width: 32px;
    height: 32px;
    cursor: pointer;
    font-size: 1.25rem;
    line-height: 1;
  }

  .btn-remove-sm:hover {
    background: #c0392b;
  }

  .btn-add-sm {
    background: #3498db;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.4rem 0.75rem;
    cursor: pointer;
    font-size: 0.75rem;
    font-weight: 500;
  }

  .btn-add-sm:hover {
    background: #2980b9;
  }
</style>
