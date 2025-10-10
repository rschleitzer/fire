<script>
  import Coding from './Coding.svelte';

  export let value = { text: '', coding: [] };
  export let label = 'Codeable Concept';
  export let required = false;
  export let collapsed = true;

  function addCoding() {
    value.coding = [...value.coding, { system: '', version: '', code: '', display: '', userSelected: false }];
    collapsed = false;
  }

  function removeCoding(index) {
    value.coding = value.coding.filter((_, i) => i !== index);
  }

  function toggleCollapse() {
    collapsed = !collapsed;
  }

  // Display summary for collapsed view
  $: summary = value.text || value.coding?.[0]?.display || value.coding?.[0]?.code || '(empty)';
</script>

<div class="fhir-codeable-concept">
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
      <div class="form-group">
        <label for="{label}-text">Text</label>
        <input
          type="text"
          id="{label}-text"
          bind:value={value.text}
          placeholder="Human-readable text"
        />
      </div>

      <div class="form-group">
        <label>Codings</label>
        {#each value.coding as coding, i}
          <div class="coding-item">
            <Coding bind:value={value.coding[i]} label="Coding {i + 1}" />
            <button type="button" class="btn-remove" on:click={() => removeCoding(i)} title="Remove coding">
              × Remove
            </button>
          </div>
        {/each}
        <button type="button" class="btn-add" on:click={addCoding}>
          + Add Coding
        </button>
      </div>
    </div>
  {/if}
</div>

<style>
  .fhir-codeable-concept {
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

  .coding-item {
    position: relative;
    margin-bottom: 0.75rem;
  }

  .btn-remove {
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

  .btn-remove:hover {
    background: #c0392b;
  }

  .btn-add {
    background: #3498db;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.5rem 1rem;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
  }

  .btn-add:hover {
    background: #2980b9;
  }
</style>
