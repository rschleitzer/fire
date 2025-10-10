<script>
  import CodeableConcept from '../datatypes/CodeableConcept.svelte';
  import Quantity from '../datatypes/Quantity.svelte';
  import Reference from '../datatypes/Reference.svelte';

  export let resource = {
    resourceType: 'Observation',
    id: '',
    status: 'final',
    code: { text: '', coding: [] },
    subject: { reference: '' },
    effectiveDateTime: '',
    valueQuantity: { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' }
  };

  // Ensure all nested objects and arrays exist (initialize if undefined)
  $: {
    if (!resource.code) resource.code = { text: '', coding: [] };
    if (!resource.code.coding) resource.code.coding = [];
    if (!resource.subject) resource.subject = { reference: '' };
    if (!resource.valueQuantity) resource.valueQuantity = { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' };
  }

  let errorMessage = '';
  let saving = false;

  function handleSubmit() {
    errorMessage = '';
    saving = true;

    // Dispatch save event with resource data
    const event = new CustomEvent('save', {
      detail: resource,
      bubbles: true,
      composed: true
    });
    window.dispatchEvent(event);
  }

  function handleCancel() {
    window.history.back();
  }

  // Listen for save completion/error from parent
  function handleSaveComplete(event) {
    saving = false;
  }

  function handleSaveError(event) {
    saving = false;
    errorMessage = event.detail.message;
  }
</script>

<svelte:window on:save-complete={handleSaveComplete} on:save-error={handleSaveError} />

<div class="observation-editor">
  <form on:submit|preventDefault={handleSubmit}>
    <div class="form-group">
      <label for="status">Status <span class="required">*</span></label>
      <select id="status" bind:value={resource.status} required disabled={saving}>
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
      <input
        type="datetime-local"
        id="effectiveDateTime"
        bind:value={resource.effectiveDateTime}
        disabled={saving}
      />
    </div>

    <Quantity bind:value={resource.valueQuantity} label="Value (Quantity)" />

    {#if errorMessage}
      <div class="error-message">
        <strong>Error:</strong> {errorMessage}
      </div>
    {/if}

    <div class="button-group">
      <button type="submit" class="btn btn-primary" disabled={saving}>
        {saving ? 'Saving...' : 'Save Observation'}
      </button>
      <button type="button" class="btn btn-secondary" on:click={handleCancel} disabled={saving}>
        Cancel
      </button>
    </div>
  </form>
</div>

<style>
  .observation-editor {
    max-width: 800px;
    margin: 0 auto;
  }

  .form-group {
    margin-bottom: 1.5rem;
  }

  .form-group > label {
    display: block;
    font-weight: 600;
    margin-bottom: 0.5rem;
    color: #2c3e50;
  }

  .form-group input,
  .form-group select {
    width: 100%;
    padding: 0.75rem;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 1rem;
  }

  .form-group input:focus,
  .form-group select:focus {
    outline: none;
    border-color: #3498db;
    box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
  }

  .required {
    color: #e74c3c;
  }

  .error-message {
    padding: 1rem;
    background: #fee;
    border: 1px solid #fcc;
    border-radius: 4px;
    color: #c00;
    margin-bottom: 1.5rem;
  }

  .button-group {
    display: flex;
    gap: 1rem;
    margin-top: 2rem;
  }

  .btn {
    padding: 0.75rem 1.5rem;
    border: none;
    border-radius: 4px;
    font-weight: 600;
    cursor: pointer;
    transition: background 0.2s;
    font-size: 1rem;
  }

  .btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .btn-primary {
    background: #3498db;
    color: white;
  }

  .btn-primary:hover:not(:disabled) {
    background: #2980b9;
  }

  .btn-secondary {
    background: #95a5a6;
    color: white;
  }

  .btn-secondary:hover:not(:disabled) {
    background: #7f8c8d;
  }
</style>
