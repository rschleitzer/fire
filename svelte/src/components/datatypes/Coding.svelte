<script>
  export let value = { system: '', version: '', code: '', display: '', userSelected: false };
  export let label = 'Coding';
  export let required = false;

  $: summary = value.display || value.code || '(empty)';
</script>

<div class="fhir-coding">
  <div class="coding-grid">
    <div class="form-group">
      <label for="{label}-system">System</label>
      <input
        type="url"
        id="{label}-system"
        bind:value={value.system}
        placeholder="http://loinc.org"
        required={required}
      />
    </div>

    <div class="form-group">
      <label for="{label}-code">Code {#if required}<span class="required">*</span>{/if}</label>
      <input
        type="text"
        id="{label}-code"
        bind:value={value.code}
        placeholder="29463-7"
        required={required}
      />
    </div>

    <div class="form-group span-2">
      <label for="{label}-display">Display</label>
      <input
        type="text"
        id="{label}-display"
        bind:value={value.display}
        placeholder="Body weight"
      />
    </div>

    <div class="form-group">
      <label for="{label}-version">Version</label>
      <input
        type="text"
        id="{label}-version"
        bind:value={value.version}
        placeholder="2.73"
      />
    </div>

    <div class="form-group">
      <label for="{label}-userSelected">
        <input
          type="checkbox"
          id="{label}-userSelected"
          bind:checked={value.userSelected}
        />
        User Selected
      </label>
    </div>
  </div>
</div>

<style>
  .fhir-coding {
    padding: 0.75rem;
    background: #fafafa;
    border-radius: 4px;
    margin-bottom: 0.5rem;
  }

  .coding-grid {
    display: grid;
    grid-template-columns: 2fr 1fr 2fr 1fr;
    gap: 0.75rem;
    align-items: end;
  }

  .span-2 {
    grid-column: span 2;
  }

  .form-group > label {
    display: block;
    font-weight: 500;
    margin-bottom: 0.25rem;
    font-size: 0.75rem;
    color: #555;
  }

  .form-group input[type="text"],
  .form-group input[type="url"] {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 0.875rem;
  }

  .form-group input[type="checkbox"] {
    margin-right: 0.5rem;
  }

  .required {
    color: #e74c3c;
  }
</style>
