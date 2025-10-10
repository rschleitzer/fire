<script>
  export let resource = {
    resourceType: 'Observation',
    id: '',
    status: 'final',
    code: { text: '', coding: [] },
    subject: { reference: '' },
    effectiveDateTime: '',
    valueQuantity: { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' },
    valueString: undefined
  };

  // Ensure nested objects exist
  $: {
    if (!resource.code) resource.code = { text: '', coding: [] };
    if (!resource.code.coding) resource.code.coding = [];
    if (!resource.subject) resource.subject = { reference: '' };
  }

  let saving = false;
  let errorMessage = '';

  // Value type selection (choice type - value[x])
  let selectedValueType = 'Quantity';

  $: {
    if (resource.valueQuantity !== undefined && resource.valueQuantity !== null) {
      selectedValueType = 'Quantity';
    } else if (resource.valueString !== undefined) {
      selectedValueType = 'string';
    }
  }

  function changeValueType() {
    // Clear all value types
    delete resource.valueQuantity;
    delete resource.valueString;
    delete resource.valueBoolean;
    delete resource.valueInteger;

    // Initialize selected type
    if (selectedValueType === 'Quantity') {
      resource.valueQuantity = { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' };
    } else if (selectedValueType === 'string') {
      resource.valueString = '';
    } else if (selectedValueType === 'boolean') {
      resource.valueBoolean = false;
    } else if (selectedValueType === 'integer') {
      resource.valueInteger = null;
    }
  }

  function addCoding() {
    resource.code.coding = [...resource.code.coding, { system: '', code: '', display: '' }];
  }

  function removeCoding(index) {
    resource.code.coding = resource.code.coding.filter((_, i) => i !== index);
  }

  function save() {
    saving = true;
    errorMessage = '';

    // Dispatch save event
    const event = new CustomEvent('save', {
      detail: resource,
      bubbles: true,
      composed: true
    });
    window.dispatchEvent(event);
  }

  function cancel() {
    window.history.back();
  }

  // Listen for save completion/error
  function handleSaveComplete(event) {
    saving = false;
  }

  function handleSaveError(event) {
    saving = false;
    errorMessage = event.detail.message;
  }
</script>

<svelte:window on:save-complete={handleSaveComplete} on:save-error={handleSaveError} />

<div class="observation">
  <table class="grid">
    <tbody>
      <!-- Status -->
      <tr>
        <td class="prop-name">status</td>
        <td class="control"></td>
        <td class="value">
          <select bind:value={resource.status} disabled={saving}>
            <option value="registered">Registered</option>
            <option value="preliminary">Preliminary</option>
            <option value="final">Final</option>
            <option value="amended">Amended</option>
            <option value="corrected">Corrected</option>
            <option value="cancelled">Cancelled</option>
            <option value="entered-in-error">Entered in Error</option>
            <option value="unknown">Unknown</option>
          </select>
        </td>
      </tr>

      <!-- Code -->
      <tr>
        <td class="prop-name">code</td>
        <td class="control"></td>
        <td class="value">
          <table class="grid nested-table">
            <tbody>
              <tr>
                <td class="prop-name">text</td>
                <td class="control"></td>
                <td class="value">
                  <input type="text" bind:value={resource.code.text} placeholder="Code text" disabled={saving} />
                </td>
              </tr>
              <tr>
                <td class="prop-name">coding</td>
                <td class="control">
                  <button class="btn-tiny" on:click={addCoding} disabled={saving} title="Add Coding">+</button>
                </td>
                <td class="value">
                  {#each resource.code.coding as coding, i}
                    <div class="array-item">
                      <button class="btn-tiny" on:click={() => removeCoding(i)} disabled={saving} title="Remove">-</button>
                      <div class="nested">
                        <table class="grid nested-table">
                          <tbody>
                            <tr>
                              <td class="prop-name">system</td>
                              <td class="control"></td>
                              <td class="value">
                                <input type="text" bind:value={coding.system} placeholder="System URI" disabled={saving} />
                              </td>
                            </tr>
                            <tr>
                              <td class="prop-name">code</td>
                              <td class="control"></td>
                              <td class="value">
                                <input type="text" bind:value={coding.code} placeholder="Code" disabled={saving} />
                              </td>
                            </tr>
                            <tr>
                              <td class="prop-name">display</td>
                              <td class="control"></td>
                              <td class="value">
                                <input type="text" bind:value={coding.display} placeholder="Display text" disabled={saving} />
                              </td>
                            </tr>
                          </tbody>
                        </table>
                      </div>
                    </div>
                  {/each}
                </td>
              </tr>
            </tbody>
          </table>
        </td>
      </tr>

      <!-- Subject (Reference) -->
      <tr>
        <td class="prop-name">subject</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.subject.reference} placeholder="Patient/123" disabled={saving} />
        </td>
      </tr>

      <!-- Effective Date/Time -->
      <tr>
        <td class="prop-name">effectiveDateTime</td>
        <td class="control"></td>
        <td class="value">
          <input type="datetime-local" bind:value={resource.effectiveDateTime} disabled={saving} />
        </td>
      </tr>

      <!-- Value[x] - Choice type -->
      <tr>
        <td class="prop-name">value[x]</td>
        <td class="control"></td>
        <td class="value">
          <div class="value-choice">
            <select bind:value={selectedValueType} on:change={changeValueType} disabled={saving}>
              <option value="Quantity">Quantity</option>
              <option value="string">string</option>
              <option value="boolean">boolean</option>
              <option value="integer">integer</option>
            </select>

            {#if selectedValueType === 'Quantity'}
              <table class="grid nested-table">
                <tbody>
                  <tr>
                    <td class="prop-name">value</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="number" step="any" bind:value={resource.valueQuantity.value} placeholder="Value" disabled={saving} />
                    </td>
                  </tr>
                  <tr>
                    <td class="prop-name">unit</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="text" bind:value={resource.valueQuantity.unit} placeholder="Unit" disabled={saving} />
                    </td>
                  </tr>
                  <tr>
                    <td class="prop-name">system</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="text" bind:value={resource.valueQuantity.system} placeholder="System" disabled={saving} />
                    </td>
                  </tr>
                  <tr>
                    <td class="prop-name">code</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="text" bind:value={resource.valueQuantity.code} placeholder="Code" disabled={saving} />
                    </td>
                  </tr>
                </tbody>
              </table>
            {:else if selectedValueType === 'string'}
              <input type="text" bind:value={resource.valueString} placeholder="String value" disabled={saving} />
            {:else if selectedValueType === 'boolean'}
              <input type="checkbox" bind:checked={resource.valueBoolean} disabled={saving} />
            {:else if selectedValueType === 'integer'}
              <input type="number" bind:value={resource.valueInteger} placeholder="Integer value" disabled={saving} />
            {/if}
          </div>
        </td>
      </tr>
    </tbody>
  </table>

  {#if errorMessage}
    <div class="error-message">
      <strong>Error:</strong> {errorMessage}
    </div>
  {/if}

  <div class="button-group">
    <button class="btn btn-primary" on:click={save} disabled={saving}>
      {saving ? 'Saving...' : 'Save'}
    </button>
    <button class="btn btn-secondary" on:click={cancel} disabled={saving}>
      Cancel
    </button>
  </div>
</div>

<style>
  .observation {
    max-width: 1200px;
    margin: 0 auto;
    padding: 1rem;
  }

  table.grid {
    width: 100%;
    border-collapse: collapse;
    border: 1px solid #ddd;
    background: white;
    margin-bottom: 1rem;
  }

  table.grid tbody tr {
    border-bottom: 1px solid #eee;
  }

  table.grid tbody tr:last-child {
    border-bottom: none;
  }

  td.prop-name {
    width: 20%;
    padding: 0.5rem;
    font-weight: 500;
    color: #555;
    vertical-align: top;
    border-right: 1px solid #eee;
    font-size: 0.875rem;
  }

  td.control {
    width: 40px;
    padding: 0.5rem 0.25rem;
    text-align: center;
    vertical-align: top;
    border-right: 1px solid #eee;
  }

  td.value {
    padding: 0.5rem;
    vertical-align: top;
  }

  .btn-tiny {
    width: 24px;
    height: 24px;
    padding: 0;
    border: 1px solid #999;
    background: #f5f5f5;
    cursor: pointer;
    font-size: 0.875rem;
    line-height: 1;
    border-radius: 3px;
  }

  .btn-tiny:hover:not(:disabled) {
    background: #e0e0e0;
  }

  .btn-tiny:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .array-item {
    margin-bottom: 0.5rem;
    display: flex;
    gap: 0.5rem;
    align-items: flex-start;
  }

  .array-item .nested {
    flex: 1;
  }

  .nested-table {
    margin: 0.5rem 0 0 0;
    font-size: 0.875rem;
  }

  .nested-table td.prop-name {
    width: 25%;
  }

  .value-choice {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .value-choice > select {
    width: 200px;
  }

  input[type="text"],
  input[type="number"],
  input[type="datetime-local"],
  select {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 3px;
    font-size: 0.875rem;
  }

  input[type="checkbox"] {
    width: auto;
  }

  .error-message {
    padding: 1rem;
    background: #fee;
    border: 1px solid #fcc;
    border-radius: 4px;
    color: #c00;
    margin-bottom: 1rem;
  }

  .button-group {
    display: flex;
    gap: 1rem;
    margin-top: 1rem;
  }

  .btn {
    padding: 0.75rem 1.5rem;
    border: none;
    border-radius: 4px;
    font-weight: 600;
    cursor: pointer;
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
