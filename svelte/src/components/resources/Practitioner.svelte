<script>
  import Meta from '../datatypes/Meta.svelte';

  export let resource = {
    resourceType: 'Practitioner',
    id: '',
    meta: {},
    implicitRules: '',
    language: '',
    identifier: [],
    active: true,
    name: [],
    telecom: [],
    gender: '',
    birthDate: '',
    address: [],
    qualification: [],
    communication: []
  };

  // Ensure all fields exist with proper defaults
  $: {
    if (!resource.id) resource.id = '';
    if (!resource.meta) resource.meta = {};
    if (!resource.implicitRules) resource.implicitRules = '';
    if (!resource.language) resource.language = '';
    if (!resource.identifier) resource.identifier = [];
    if (!resource.name) resource.name = [];
    if (!resource.telecom) resource.telecom = [];
    if (!resource.address) resource.address = [];
    if (!resource.qualification) resource.qualification = [];
    if (!resource.communication) resource.communication = [];

    // Ensure nested objects in arrays have proper structure
    resource.qualification.forEach(qual => {
      if (!qual.identifier) qual.identifier = [];
      if (!qual.code) qual.code = {};
      if (!qual.period) qual.period = {};
      if (!qual.issuer) qual.issuer = {};
    });

    resource.communication.forEach(comm => {
      if (!comm.language) comm.language = {};
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

  function addQualification() {
    resource.qualification = [...resource.qualification, { identifier: [], code: {}, period: {}, issuer: {} }];
  }

  function removeQualification(index) {
    resource.qualification = resource.qualification.filter((_, i) => i !== index);
  }

  function addCommunication() {
    resource.communication = [...resource.communication, { language: {}, preferred: false }];
  }

  function removeCommunication(index) {
    resource.communication = resource.communication.filter((_, i) => i !== index);
  }

  function addPhoto() {
    if (!resource.photo) resource.photo = [];
    resource.photo = [...resource.photo, { contentType: '', data: '', url: '' }];
  }

  function removePhoto(index) {
    resource.photo = resource.photo.filter((_, i) => i !== index);
  }

  function createNew() {
    // Generate a new UUID and navigate to that page
    const newId = crypto.randomUUID();
    window.location.href = `/fhir/Practitioner/${newId}`;
  }

  function cleanResource(obj) {
    // Remove empty/null/undefined fields and empty arrays/objects from resource before saving
    if (obj === null || obj === undefined) return undefined;

    if (Array.isArray(obj)) {
      const cleaned = obj.map(cleanResource).filter(item => item !== undefined);
      return cleaned.length > 0 ? cleaned : undefined;
    }

    if (typeof obj === 'object') {
      const cleaned = {};
      for (const [key, value] of Object.entries(obj)) {
        const cleanedValue = cleanResource(value);
        if (cleanedValue !== undefined && cleanedValue !== '' && cleanedValue !== null) {
          // For objects, also check if they're empty after cleaning
          if (typeof cleanedValue === 'object' && !Array.isArray(cleanedValue)) {
            if (Object.keys(cleanedValue).length > 0) {
              cleaned[key] = cleanedValue;
            }
          } else {
            cleaned[key] = cleanedValue;
          }
        }
      }
      return Object.keys(cleaned).length > 0 ? cleaned : undefined;
    }

    // Return primitive values (strings, numbers, booleans) as-is
    return obj;
  }

  async function rollback() {
    if (!resource.id || !resource.meta?.versionId) {
      errorMessage = 'Cannot rollback: No previous version available';
      return;
    }

    const currentVersionId = parseInt(resource.meta.versionId);
    if (currentVersionId <= 1) {
      errorMessage = 'Cannot rollback: Already at the first version';
      return;
    }

    if (!window.confirm('This will permanently delete version ' + currentVersionId + ' and cannot be recovered! Continue?')) {
      return;
    }

    saving = true;
    errorMessage = '';

    try {
      // Call rollback endpoint (destructive - deletes current version)
      const response = await fetch(`/fhir/Practitioner/${resource.id}/_rollback/${currentVersionId}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/fhir+json' }
      });

      if (!response.ok) {
        throw new Error('Failed to rollback to previous version');
      }

      // Reload the page to show the rolled back version
      window.location.reload();
    } catch (error) {
      saving = false;
      errorMessage = `Rollback failed: ${error.message}`;
    }
  }

  function save() {
    saving = true;
    errorMessage = '';

    // Clean the resource before saving (remove empty fields)
    const cleanedResource = cleanResource(resource);

    // Remove id field if empty (for new resources - server will generate it)
    if (cleanedResource && (!cleanedResource.id || cleanedResource.id === '')) {
      delete cleanedResource.id;
    }

    // Dispatch save event
    const event = new CustomEvent('save', {
      detail: cleanedResource,
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

<div class="practitioner">
  {#if errorMessage}
    <div class="error-message">
      <strong>Error:</strong> {errorMessage}
    </div>
  {/if}

  <div class="button-group">
    <button class="btn btn-success" on:click={createNew} disabled={saving}>
      New
    </button>
    <button class="btn btn-primary" on:click={save} disabled={saving}>
      {saving ? 'Saving...' : 'Save'}
    </button>
    <button class="btn btn-warning" on:click={rollback} disabled={saving || !resource.id || !resource.meta?.versionId || parseInt(resource.meta?.versionId || '0') <= 1}>
      Rollback
    </button>
    <button class="btn btn-secondary" on:click={cancel} disabled={saving}>
      Cancel
    </button>
    {#if resource.id}
      <a href="/fhir/Practitioner/{resource.id}/_history" class="link">History</a>
      <a href="/fhir/Practitioner/{resource.id}?_format=json" download="Practitioner_{resource.id}.json" class="link">JSON</a>
      <a href="/fhir/Practitioner/{resource.id}?_format=xml" download="Practitioner_{resource.id}.xml" class="link">XML</a>
    {/if}
  </div>

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
          <Meta bind:value={resource.meta} disabled={saving} />
        </td>
      </tr>

      <!-- Implicit Rules -->
      <tr>
        <td class="prop-name">implicitRules</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.implicitRules} placeholder="Implicit rules URI" disabled={saving} />
        </td>
      </tr>

      <!-- Language -->
      <tr>
        <td class="prop-name">language</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.language} placeholder="Language code (e.g., en-US)" disabled={saving} />
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
                        <input type="text" bind:value={identifier.system} placeholder="System URI (e.g., http://hl7.org/fhir/sid/us-npi)" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">value</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={identifier.value} placeholder="Identifier value (e.g., NPI number)" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
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
                    <tr>
                      <td class="prop-name">prefix</td>
                      <td class="control">
                        <button class="btn-tiny" on:click={() => { if (!name.prefix) name.prefix = []; name.prefix = [...name.prefix, '']; }} disabled={saving} title="Add Prefix">+</button>
                      </td>
                      <td class="value">
                        {#each name.prefix || [] as prefix, pi}
                          <div class="inline-array-item">
                            <button class="btn-tiny" on:click={() => { name.prefix = name.prefix.filter((_, idx) => idx !== pi); }} disabled={saving}>-</button>
                            <input type="text" bind:value={name.prefix[pi]} placeholder="Prefix (e.g., Dr., Prof.)" disabled={saving} />
                          </div>
                        {/each}
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">suffix</td>
                      <td class="control">
                        <button class="btn-tiny" on:click={() => { if (!name.suffix) name.suffix = []; name.suffix = [...name.suffix, '']; }} disabled={saving} title="Add Suffix">+</button>
                      </td>
                      <td class="value">
                        {#each name.suffix || [] as suffix, si}
                          <div class="inline-array-item">
                            <button class="btn-tiny" on:click={() => { name.suffix = name.suffix.filter((_, idx) => idx !== si); }} disabled={saving}>-</button>
                            <input type="text" bind:value={name.suffix[si]} placeholder="Suffix (e.g., MD, PhD)" disabled={saving} />
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
                    <tr>
                      <td class="prop-name">use</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={telecom.use} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="home">Home</option>
                          <option value="work">Work</option>
                          <option value="temp">Temp</option>
                          <option value="old">Old</option>
                          <option value="mobile">Mobile</option>
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
                      <td class="prop-name">use</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={address.use} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="home">Home</option>
                          <option value="work">Work</option>
                          <option value="temp">Temp</option>
                          <option value="old">Old</option>
                        </select>
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">type</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={address.type} disabled={saving}>
                          <option value="">-- Select --</option>
                          <option value="postal">Postal</option>
                          <option value="physical">Physical</option>
                          <option value="both">Both</option>
                        </select>
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={address.text} placeholder="Full address text" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">line</td>
                      <td class="control">
                        <button class="btn-tiny" on:click={() => { if (!address.line) address.line = []; address.line = [...address.line, '']; }} disabled={saving} title="Add Line">+</button>
                      </td>
                      <td class="value">
                        {#each address.line || [] as line, li}
                          <div class="inline-array-item">
                            <button class="btn-tiny" on:click={() => { address.line = address.line.filter((_, idx) => idx !== li); }} disabled={saving}>-</button>
                            <input type="text" bind:value={address.line[li]} placeholder="Address line" disabled={saving} />
                          </div>
                        {/each}
                      </td>
                    </tr>
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
                    <tr>
                      <td class="prop-name">country</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={address.country} placeholder="Country" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Photo (array) -->
      <tr>
        <td class="prop-name">photo</td>
        <td class="control">
          <button class="btn-tiny" on:click={addPhoto} disabled={saving} title="Add Photo">+</button>
        </td>
        <td class="value">
          {#each resource.photo || [] as photo, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removePhoto(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">contentType</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={photo.contentType} placeholder="e.g., image/jpeg" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">url</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={photo.url} placeholder="Photo URL" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Qualification (array) - Practitioner-specific -->
      <tr>
        <td class="prop-name">qualification</td>
        <td class="control">
          <button class="btn-tiny" on:click={addQualification} disabled={saving} title="Add Qualification">+</button>
        </td>
        <td class="value">
          {#each resource.qualification as qual, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeQualification(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">code.text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text"
                               value={qual.code?.text || ''}
                               on:input={(e) => { if (!qual.code) qual.code = {}; qual.code.text = e.target.value; }}
                               placeholder="Qualification (e.g., MD, PhD, Board Certified)" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">period.start</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="date"
                               value={qual.period?.start || ''}
                               on:input={(e) => { if (!qual.period) qual.period = {}; qual.period.start = e.target.value; }}
                               disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">period.end</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="date"
                               value={qual.period?.end || ''}
                               on:input={(e) => { if (!qual.period) qual.period = {}; qual.period.end = e.target.value; }}
                               disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">issuer.display</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text"
                               value={qual.issuer?.display || ''}
                               on:input={(e) => { if (!qual.issuer) qual.issuer = {}; qual.issuer.display = e.target.value; }}
                               placeholder="Issuing organization" disabled={saving} />
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
    </tbody>
  </table>
</div>

<style>
  .practitioner {
    max-width: 1200px;
    margin: 0 auto;
    padding: 0;
  }

  table.grid {
    width: 100%;
    border-collapse: collapse;
    border: 1px solid #ddd;
    background: white;
    margin-bottom: 0.5rem;
  }

  table.grid tbody tr {
    border-bottom: 1px solid #eee;
  }

  table.grid tbody tr:last-child {
    border-bottom: none;
  }

  td.prop-name {
    width: 20%;
    padding: 0.25rem 0.5rem;
    font-weight: 500;
    color: #555;
    vertical-align: top;
    border-right: 1px solid #eee;
    font-size: 0.75rem;
  }

  td.control {
    width: 32px;
    padding: 0.25rem;
    text-align: center;
    vertical-align: top;
    border-right: 1px solid #eee;
  }

  td.value {
    padding: 0.25rem 0.5rem;
    vertical-align: top;
  }

  .btn-tiny {
    width: 20px;
    height: 20px;
    padding: 0;
    border: 1px solid #999;
    background: #f5f5f5;
    cursor: pointer;
    font-size: 0.75rem;
    line-height: 1;
    border-radius: 2px;
  }

  .btn-tiny:hover:not(:disabled) {
    background: #e0e0e0;
  }

  .btn-tiny:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .array-item {
    margin-bottom: 0.25rem;
    display: flex;
    gap: 0.25rem;
    align-items: flex-start;
  }

  .array-item .nested {
    flex: 1;
  }

  .nested-table {
    margin: 0;
    font-size: 0.75rem;
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
  input[type="datetime-local"],
  input[type="number"],
  select {
    width: 100%;
    padding: 0.25rem 0.5rem;
    border: 1px solid #ddd;
    border-radius: 3px;
    font-size: 0.75rem;
  }

  input[type="checkbox"] {
    width: auto;
  }

  .error-message {
    padding: 0.5rem;
    background: #fee;
    border: 1px solid #fcc;
    border-radius: 3px;
    color: #c00;
    margin-bottom: 0.5rem;
    font-size: 0.75rem;
  }

  .button-group {
    display: flex;
    gap: 0.5rem;
    margin-top: 0;
    margin-bottom: 0.5rem;
    align-items: center;
  }

  .link {
    color: #3498db;
    text-decoration: none;
    padding: 0.25rem 0.5rem;
    border-radius: 3px;
    font-weight: 500;
    font-size: 0.75rem;
  }

  .link:hover {
    text-decoration: underline;
  }

  .btn {
    padding: 0.4rem 0.8rem;
    border: none;
    border-radius: 3px;
    font-weight: 600;
    cursor: pointer;
    font-size: 0.75rem;
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

  .btn-success {
    background: #27ae60;
    color: white;
  }

  .btn-success:hover:not(:disabled) {
    background: #229954;
  }

  .btn-warning {
    background: #f39c12;
    color: white;
  }

  .btn-warning:hover:not(:disabled) {
    background: #e67e22;
  }
</style>
