<script>
  import HumanName from '../datatypes/HumanName.svelte';
  import Address from '../datatypes/Address.svelte';
  import ContactPoint from '../datatypes/ContactPoint.svelte';
  import Identifier from '../datatypes/Identifier.svelte';

  export let resource = {
    resourceType: 'Patient',
    id: '',
    identifier: [],
    active: true,
    name: [],
    telecom: [],
    gender: '',
    birthDate: '',
    address: []
  };

  // Ensure all array fields exist (initialize if undefined)
  $: {
    if (!resource.identifier) resource.identifier = [];
    if (!resource.name) resource.name = [];
    if (!resource.telecom) resource.telecom = [];
    if (!resource.address) resource.address = [];
  }

  let errorMessage = '';
  let saving = false;

  function addName() {
    resource.name = [...resource.name, { use: '', text: '', family: '', given: [], prefix: [], suffix: [] }];
  }

  function removeName(index) {
    resource.name = resource.name.filter((_, i) => i !== index);
  }

  function addTelecom() {
    resource.telecom = [...resource.telecom, { system: '', value: '', use: '', rank: null }];
  }

  function removeTelecom(index) {
    resource.telecom = resource.telecom.filter((_, i) => i !== index);
  }

  function addAddress() {
    resource.address = [...resource.address, { use: '', type: '', text: '', line: [], city: '', state: '', postalCode: '', country: '' }];
  }

  function removeAddress(index) {
    resource.address = resource.address.filter((_, i) => i !== index);
  }

  function addIdentifier() {
    resource.identifier = [...resource.identifier, { use: '', type: {}, system: '', value: '' }];
  }

  function removeIdentifier(index) {
    resource.identifier = resource.identifier.filter((_, i) => i !== index);
  }

  function handleSubmit() {
    errorMessage = '';
    saving = true;

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

  function handleSaveComplete(event) {
    saving = false;
  }

  function handleSaveError(event) {
    saving = false;
    errorMessage = event.detail.message;
  }
</script>

<svelte:window on:save-complete={handleSaveComplete} on:save-error={handleSaveError} />

<div class="patient-editor">
  <form on:submit|preventDefault={handleSubmit}>
    <h3>Demographics</h3>

    <div class="form-group">
      <label for="active">
        <input
          type="checkbox"
          id="active"
          bind:checked={resource.active}
          disabled={saving}
        />
        Active
      </label>
    </div>

    <div class="form-group">
      <label for="gender">Gender</label>
      <select id="gender" bind:value={resource.gender} disabled={saving}>
        <option value="">-- Select --</option>
        <option value="male">Male</option>
        <option value="female">Female</option>
        <option value="other">Other</option>
        <option value="unknown">Unknown</option>
      </select>
    </div>

    <div class="form-group">
      <label for="birthDate">Birth Date</label>
      <input
        type="date"
        id="birthDate"
        bind:value={resource.birthDate}
        disabled={saving}
      />
    </div>

    <h3>Identifiers</h3>
    {#each resource.identifier as identifier, i}
      <div class="array-section">
        <Identifier bind:value={resource.identifier[i]} label="Identifier {i + 1}" />
        <button type="button" class="btn-remove" on:click={() => removeIdentifier(i)} disabled={saving}>
          × Remove Identifier
        </button>
      </div>
    {/each}
    <button type="button" class="btn-add" on:click={addIdentifier} disabled={saving}>
      + Add Identifier
    </button>

    <h3>Names</h3>
    {#each resource.name as name, i}
      <div class="array-section">
        <HumanName bind:value={resource.name[i]} label="Name {i + 1}" />
        <button type="button" class="btn-remove" on:click={() => removeName(i)} disabled={saving}>
          × Remove Name
        </button>
      </div>
    {/each}
    <button type="button" class="btn-add" on:click={addName} disabled={saving}>
      + Add Name
    </button>

    <h3>Contact</h3>
    {#each resource.telecom as telecom, i}
      <div class="array-section">
        <ContactPoint bind:value={resource.telecom[i]} label="Contact {i + 1}" />
        <button type="button" class="btn-remove" on:click={() => removeTelecom(i)} disabled={saving}>
          × Remove Contact
        </button>
      </div>
    {/each}
    <button type="button" class="btn-add" on:click={addTelecom} disabled={saving}>
      + Add Contact
    </button>

    <h3>Addresses</h3>
    {#each resource.address as address, i}
      <div class="array-section">
        <Address bind:value={resource.address[i]} label="Address {i + 1}" />
        <button type="button" class="btn-remove" on:click={() => removeAddress(i)} disabled={saving}>
          × Remove Address
        </button>
      </div>
    {/each}
    <button type="button" class="btn-add" on:click={addAddress} disabled={saving}>
      + Add Address
    </button>

    {#if errorMessage}
      <div class="error-message">
        <strong>Error:</strong> {errorMessage}
      </div>
    {/if}

    <div class="button-group">
      <button type="submit" class="btn btn-primary" disabled={saving}>
        {saving ? 'Saving...' : 'Save Patient'}
      </button>
      <button type="button" class="btn btn-secondary" on:click={handleCancel} disabled={saving}>
        Cancel
      </button>
    </div>
  </form>
</div>

<style>
  .patient-editor {
    max-width: 900px;
    margin: 0 auto;
  }

  h3 {
    margin-top: 2rem;
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 2px solid #3498db;
    color: #2c3e50;
  }

  h3:first-of-type {
    margin-top: 0;
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

  .form-group input[type="checkbox"] {
    margin-right: 0.5rem;
  }

  .form-group input[type="date"],
  .form-group select {
    width: 100%;
    max-width: 400px;
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

  .array-section {
    position: relative;
    margin-bottom: 1rem;
    padding-bottom: 1rem;
    border-bottom: 1px dashed #ddd;
  }

  .btn-remove {
    background: #e74c3c;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.5rem 1rem;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
    margin-top: 0.5rem;
  }

  .btn-remove:hover:not(:disabled) {
    background: #c0392b;
  }

  .btn-add {
    background: #3498db;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.75rem 1.5rem;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
    margin-bottom: 2rem;
  }

  .btn-add:hover:not(:disabled) {
    background: #2980b9;
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
    padding-top: 2rem;
    border-top: 2px solid #ddd;
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
