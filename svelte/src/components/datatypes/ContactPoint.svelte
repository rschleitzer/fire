<script>
  export let value = { system: '', value: '', use: '', rank: null };
  export let label = 'Contact Point';
  export let required = false;

  $: summary = value.value || '(empty)';
</script>

<div class="fhir-contact-point">
  <div class="contact-grid">
    <div class="form-group">
      <label for="{label}-system">System</label>
      <select id="{label}-system" bind:value={value.system}>
        <option value="">-- Select --</option>
        <option value="phone">Phone</option>
        <option value="fax">Fax</option>
        <option value="email">Email</option>
        <option value="pager">Pager</option>
        <option value="url">URL</option>
        <option value="sms">SMS</option>
        <option value="other">Other</option>
      </select>
    </div>

    <div class="form-group span-2">
      <label for="{label}-value">Value {#if required}<span class="required">*</span>{/if}</label>
      <input
        type="text"
        id="{label}-value"
        bind:value={value.value}
        placeholder="e.g., (555) 123-4567 or email@example.com"
        required={required}
      />
    </div>

    <div class="form-group">
      <label for="{label}-use">Use</label>
      <select id="{label}-use" bind:value={value.use}>
        <option value="">-- Select --</option>
        <option value="home">Home</option>
        <option value="work">Work</option>
        <option value="temp">Temp</option>
        <option value="old">Old</option>
        <option value="mobile">Mobile</option>
      </select>
    </div>

    <div class="form-group">
      <label for="{label}-rank">Rank</label>
      <input
        type="number"
        id="{label}-rank"
        bind:value={value.rank}
        min="1"
        placeholder="Priority"
      />
    </div>
  </div>
</div>

<style>
  .fhir-contact-point {
    padding: 0.75rem;
    background: #fafafa;
    border-radius: 4px;
    margin-bottom: 0.5rem;
  }

  .contact-grid {
    display: grid;
    grid-template-columns: 1fr 2fr 1fr 0.5fr;
    gap: 0.75rem;
    align-items: end;
  }

  .span-2 {
    grid-column: span 1;
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
