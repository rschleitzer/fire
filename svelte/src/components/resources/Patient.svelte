<script>
  export let resource = {
    resourceType: 'Patient',
    id: '',
    meta: {},
    identifier: [],
    active: true,
    name: [],
    telecom: [],
    gender: '',
    birthDate: '',
    address: [],
    maritalStatus: {},
    contact: [],
    communication: [],
    link: []
  };

  // Ensure all fields exist with proper defaults
  $: {
    if (!resource.id) resource.id = '';
    if (!resource.meta) resource.meta = {};
    if (!resource.identifier) resource.identifier = [];
    if (!resource.name) resource.name = [];
    if (!resource.telecom) resource.telecom = [];
    if (!resource.address) resource.address = [];
    if (!resource.maritalStatus) resource.maritalStatus = {};
    if (!resource.contact) resource.contact = [];
    if (!resource.communication) resource.communication = [];
    if (!resource.link) resource.link = [];

    // Ensure nested objects in arrays have proper structure
    resource.contact.forEach(contact => {
      if (!contact.name) contact.name = {};
      if (!contact.telecom) contact.telecom = [];
      if (!contact.address) contact.address = {};
    });

    resource.communication.forEach(comm => {
      if (!comm.language) comm.language = {};
    });

    resource.link.forEach(link => {
      if (!link.other) link.other = {};
    });
  }

  let saving = false;
  let errorMessage = '';

  // Helper functions for adding/removing items
  function addName() {
    resource.name = [...resource.name, { use: '', text: '', family: '', given: [], prefix: [], suffix: [] }];
  }

  function removeName(index) {
    resource.name = resource.name.filter((_, i) => i !== index);
  }

  function addIdentifier() {
    resource.identifier = [...resource.identifier, { use: '', type: {}, system: '', value: '' }];
  }

  function removeIdentifier(index) {
    resource.identifier = resource.identifier.filter((_, i) => i !== index);
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

  function addContact() {
    resource.contact = [...resource.contact, { relationship: [], name: {}, telecom: [], address: {}, gender: '', organization: {} }];
  }

  function removeContact(index) {
    resource.contact = resource.contact.filter((_, i) => i !== index);
  }

  function addCommunication() {
    resource.communication = [...resource.communication, { language: {}, preferred: false }];
  }

  function removeCommunication(index) {
    resource.communication = resource.communication.filter((_, i) => i !== index);
  }

  function addLink() {
    resource.link = [...resource.link, { other: {}, type: '' }];
  }

  function removeLink(index) {
    resource.link = resource.link.filter((_, i) => i !== index);
  }

  function addArrayItem(array, path) {
    const keys = path.split('.');
    let obj = resource;
    for (let i = 0; i < keys.length - 1; i++) {
      obj = obj[keys[i]];
    }
    const lastKey = keys[keys.length - 1];
    if (!Array.isArray(obj[lastKey])) obj[lastKey] = [];
    obj[lastKey] = [...obj[lastKey], ''];
  }

  function removeArrayItem(path, index) {
    const keys = path.split('.');
    let obj = resource;
    for (let i = 0; i < keys.length - 1; i++) {
      obj = obj[keys[i]];
    }
    const lastKey = keys[keys.length - 1];
    obj[lastKey] = obj[lastKey].filter((_, i) => i !== index);
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

<div class="patient">
  <table class="grid">
    <tbody>
      <!-- ID -->
      <tr>
        <td class="prop-name">id</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.id} placeholder="Resource ID" disabled={saving} />
        </td>
      </tr>

      <!-- Meta -->
      <tr>
        <td class="prop-name">meta</td>
        <td class="control"></td>
        <td class="value">
          <div class="nested">
            <table class="grid nested-table">
              <tbody>
                <tr>
                  <td class="prop-name">versionId</td>
                  <td class="control"></td>
                  <td class="value">
                    <input type="text" bind:value={resource.meta.versionId} placeholder="Version ID" disabled={saving} />
                  </td>
                </tr>
                <tr>
                  <td class="prop-name">lastUpdated</td>
                  <td class="control"></td>
                  <td class="value">
                    <input type="datetime-local" bind:value={resource.meta.lastUpdated} disabled={saving} />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </td>
      </tr>

      <!-- Active -->
      <tr>
        <td class="prop-name">active</td>
        <td class="control"></td>
        <td class="value">
          <input type="checkbox" bind:checked={resource.active} disabled={saving} />
        </td>
      </tr>

      <!-- Gender -->
      <tr>
        <td class="prop-name">gender</td>
        <td class="control"></td>
        <td class="value">
          <select bind:value={resource.gender} disabled={saving}>
            <option value="">-- Select --</option>
            <option value="male">Male</option>
            <option value="female">Female</option>
            <option value="other">Other</option>
            <option value="unknown">Unknown</option>
          </select>
        </td>
      </tr>

      <!-- Birth Date -->
      <tr>
        <td class="prop-name">birthDate</td>
        <td class="control"></td>
        <td class="value">
          <input type="date" bind:value={resource.birthDate} disabled={saving} />
        </td>
      </tr>

      <!-- Names (array) -->
      <tr>
        <td class="prop-name">name</td>
        <td class="control">
          <button class="btn-tiny" on:click={addName} disabled={saving} title="Add Name">+</button>
        </td>
        <td class="value">
          {#each resource.name as name, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeName(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">use</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={name.use} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="usual">Usual</option>
                          <option value="official">Official</option>
                          <option value="temp">Temp</option>
                          <option value="nickname">Nickname</option>
                          <option value="anonymous">Anonymous</option>
                          <option value="old">Old</option>
                          <option value="maiden">Maiden</option>
                        </select>
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={name.text} placeholder="Full name" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">family</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={name.family} placeholder="Family name" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">given</td>
                      <td class="control">
                        <button class="btn-tiny" on:click={() => { if (!name.given) name.given = []; name.given = [...name.given, '']; }} disabled={saving} title="Add Given">+</button>
                      </td>
                      <td class="value">
                        {#each name.given || [] as given, gi}
                          <div class="inline-array-item">
                            <button class="btn-tiny" on:click={() => { name.given = name.given.filter((_, idx) => idx !== gi); }} disabled={saving}>-</button>
                            <input type="text" bind:value={name.given[gi]} placeholder="Given name" disabled={saving} />
                          </div>
                        {/each}
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Identifiers (array) -->
      <tr>
        <td class="prop-name">identifier</td>
        <td class="control">
          <button class="btn-tiny" on:click={addIdentifier} disabled={saving} title="Add Identifier">+</button>
        </td>
        <td class="value">
          {#each resource.identifier as identifier, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeIdentifier(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">system</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={identifier.system} placeholder="System URI" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">value</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={identifier.value} placeholder="Identifier value" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Telecom (array) -->
      <tr>
        <td class="prop-name">telecom</td>
        <td class="control">
          <button class="btn-tiny" on:click={addTelecom} disabled={saving} title="Add Contact">+</button>
        </td>
        <td class="value">
          {#each resource.telecom as telecom, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeTelecom(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">system</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={telecom.system} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="phone">Phone</option>
                          <option value="fax">Fax</option>
                          <option value="email">Email</option>
                          <option value="pager">Pager</option>
                          <option value="url">URL</option>
                          <option value="sms">SMS</option>
                          <option value="other">Other</option>
                        </select>
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">value</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={telecom.value} placeholder="Contact value" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Addresses (array) -->
      <tr>
        <td class="prop-name">address</td>
        <td class="control">
          <button class="btn-tiny" on:click={addAddress} disabled={saving} title="Add Address">+</button>
        </td>
        <td class="value">
          {#each resource.address as address, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeAddress(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">city</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={address.city} placeholder="City" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">state</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={address.state} placeholder="State" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">postalCode</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={address.postalCode} placeholder="Postal Code" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Deceased -->
      <tr>
        <td class="prop-name">deceased[x]</td>
        <td class="control"></td>
        <td class="value">
          <div class="deceased-choice">
            <label>
              <input type="radio" name="deceasedType" bind:group={resource.deceasedType} value="boolean" disabled={saving} />
              Boolean:
              <input type="checkbox" bind:checked={resource.deceasedBoolean} disabled={saving || resource.deceasedType !== 'boolean'} />
            </label>
            <label>
              <input type="radio" name="deceasedType" bind:group={resource.deceasedType} value="dateTime" disabled={saving} />
              DateTime:
              <input type="datetime-local" bind:value={resource.deceasedDateTime} disabled={saving || resource.deceasedType !== 'dateTime'} />
            </label>
          </div>
        </td>
      </tr>

      <!-- Marital Status -->
      <tr>
        <td class="prop-name">maritalStatus</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.maritalStatus.text} placeholder="Marital status (text)" disabled={saving} />
        </td>
      </tr>

      <!-- Contact (array) -->
      <tr>
        <td class="prop-name">contact</td>
        <td class="control">
          <button class="btn-tiny" on:click={addContact} disabled={saving} title="Add Contact">+</button>
        </td>
        <td class="value">
          {#each resource.contact as contact, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeContact(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">name.text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={contact.name.text} placeholder="Contact name" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">gender</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={contact.gender} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="male">Male</option>
                          <option value="female">Female</option>
                          <option value="other">Other</option>
                          <option value="unknown">Unknown</option>
                        </select>
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Communication (array) -->
      <tr>
        <td class="prop-name">communication</td>
        <td class="control">
          <button class="btn-tiny" on:click={addCommunication} disabled={saving} title="Add Communication">+</button>
        </td>
        <td class="value">
          {#each resource.communication as comm, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeCommunication(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">language.text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={comm.language.text} placeholder="Language" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">preferred</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="checkbox" bind:checked={comm.preferred} disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Link (array) -->
      <tr>
        <td class="prop-name">link</td>
        <td class="control">
          <button class="btn-tiny" on:click={addLink} disabled={saving} title="Add Link">+</button>
        </td>
        <td class="value">
          {#each resource.link as link, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeLink(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">other.reference</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={link.other.reference} placeholder="Reference to other patient" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">type</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={link.type} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="replaced-by">Replaced By</option>
                          <option value="replaces">Replaces</option>
                          <option value="refer">Refer</option>
                          <option value="seealso">See Also</option>
                        </select>
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
  .patient {
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
    margin: 0;
    font-size: 0.875rem;
  }

  .nested-table td.prop-name {
    width: 25%;
  }

  .inline-array-item {
    display: flex;
    gap: 0.25rem;
    margin-bottom: 0.25rem;
    align-items: center;
  }

  input[type="text"],
  input[type="date"],
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

  .deceased-choice label {
    display: block;
    margin-bottom: 0.5rem;
  }

  .deceased-choice input[type="radio"] {
    margin-right: 0.5rem;
  }
</style>
