<script>
  export let value = { use: '', text: '', family: '', given: [], prefix: [], suffix: [] };
  export let label = 'Human Name';
  export let required = false;
  export let collapsed = true;

  // Ensure all array fields exist
  $: {
    if (!value.given) value.given = [];
    if (!value.prefix) value.prefix = [];
    if (!value.suffix) value.suffix = [];
  }

  function toggleCollapse() {
    collapsed = !collapsed;
  }

  function addGiven() {
    value.given = [...value.given, ''];
  }

  function removeGiven(index) {
    value.given = value.given.filter((_, i) => i !== index);
  }

  function addPrefix() {
    value.prefix = [...value.prefix, ''];
  }

  function removePrefix(index) {
    value.prefix = value.prefix.filter((_, i) => i !== index);
  }

  function addSuffix() {
    value.suffix = [...value.suffix, ''];
  }

  function removeSuffix(index) {
    value.suffix = value.suffix.filter((_, i) => i !== index);
  }

  $: summary = value.text || [
    ...(value.prefix || []),
    ...(value.given || []),
    value.family,
    ...(value.suffix || [])
  ].filter(Boolean).join(' ') || '(empty)';
</script>

<div class="fhir-human-name">
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
      <div class="name-grid">
        <div class="form-group">
          <label for="{label}-use">Use</label>
          <select id="{label}-use" bind:value={value.use}>
            <option value="">-- Select --</option>
            <option value="usual">Usual</option>
            <option value="official">Official</option>
            <option value="temp">Temp</option>
            <option value="nickname">Nickname</option>
            <option value="anonymous">Anonymous</option>
            <option value="old">Old</option>
            <option value="maiden">Maiden</option>
          </select>
        </div>

        <div class="form-group span-3">
          <label for="{label}-text">Text (full name)</label>
          <input
            type="text"
            id="{label}-text"
            bind:value={value.text}
            placeholder="e.g., Dr. John Q. Public, MD"
          />
        </div>
      </div>

      <div class="form-group">
        <label for="{label}-family">Family Name {#if required}<span class="required">*</span>{/if}</label>
        <input
          type="text"
          id="{label}-family"
          bind:value={value.family}
          placeholder="e.g., Smith"
          required={required}
        />
      </div>

      <div class="form-group">
        <label>Given Names</label>
        {#each value.given as given, i}
          <div class="array-item">
            <input type="text" bind:value={value.given[i]} placeholder="Given name" />
            <button type="button" class="btn-remove-sm" on:click={() => removeGiven(i)}>×</button>
          </div>
        {/each}
        <button type="button" class="btn-add-sm" on:click={addGiven}>+ Add Given Name</button>
      </div>

      <div class="name-grid">
        <div class="form-group">
          <label>Prefixes</label>
          {#each value.prefix as prefix, i}
            <div class="array-item">
              <input type="text" bind:value={value.prefix[i]} placeholder="e.g., Dr." />
              <button type="button" class="btn-remove-sm" on:click={() => removePrefix(i)}>×</button>
            </div>
          {/each}
          <button type="button" class="btn-add-sm" on:click={addPrefix}>+ Add Prefix</button>
        </div>

        <div class="form-group">
          <label>Suffixes</label>
          {#each value.suffix as suffix, i}
            <div class="array-item">
              <input type="text" bind:value={value.suffix[i]} placeholder="e.g., MD" />
              <button type="button" class="btn-remove-sm" on:click={() => removeSuffix(i)}>×</button>
            </div>
          {/each}
          <button type="button" class="btn-add-sm" on:click={addSuffix}>+ Add Suffix</button>
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .fhir-human-name {
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

  .name-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
    margin-bottom: 1rem;
  }

  .span-3 {
    grid-column: span 2;
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
