<script>
  import Meta from '../datatypes/Meta.svelte';

  export let resource = {
    resourceType: 'Observation',
    id: '',
    meta: {},
    implicitRules: '',
    language: '',
    text: {},
    contained: [],
    extension: [],
    modifierExtension: [],
    identifier: [],
    instantiatesCanonical: [],
    instantiatesReference: [],
    basedOn: [],
    triggeredBy: [],
    partOf: [],
    status: 'final',
    category: [],
    code: { text: '', coding: [] },
    subject: { reference: '', display: '' },
    focus: [],
    encounter: {},
    effectiveDateTime: '',
    issued: '',
    performer: [],
    valueQuantity: { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' },
    dataAbsentReason: {},
    interpretation: [],
    note: [],
    bodySite: {},
    bodyStructure: {},
    method: {},
    specimen: {},
    device: {},
    referenceRange: [],
    hasMember: [],
    derivedFrom: [],
    component: []
  };

  // Ensure nested objects exist
  $: {
    if (!resource.meta) resource.meta = {};
    if (!resource.code) resource.code = { text: '', coding: [] };
    if (!resource.code.coding) resource.code.coding = [];
    if (!resource.subject) resource.subject = { reference: '', display: '' };
    if (!resource.identifier) resource.identifier = [];
    if (!resource.category) resource.category = [];
    if (!resource.performer) resource.performer = [];
    if (!resource.interpretation) resource.interpretation = [];
    if (!resource.note) resource.note = [];
    if (!resource.referenceRange) resource.referenceRange = [];
    if (!resource.component) resource.component = [];
    if (!resource.triggeredBy) resource.triggeredBy = [];
    if (!resource.focus) resource.focus = [];
    if (!resource.basedOn) resource.basedOn = [];
    if (!resource.partOf) resource.partOf = [];
    if (!resource.hasMember) resource.hasMember = [];
    if (!resource.derivedFrom) resource.derivedFrom = [];
  }

  let saving = false;
  let errorMessage = '';

  // Value type selection (choice type - value[x])
  let selectedValueType = 'Quantity';

  $: {
    if (resource.valueQuantity !== undefined && resource.valueQuantity !== null && resource.valueQuantity.value !== null) {
      selectedValueType = 'Quantity';
    } else if (resource.valueString !== undefined && resource.valueString !== '') {
      selectedValueType = 'string';
    } else if (resource.valueBoolean !== undefined) {
      selectedValueType = 'boolean';
    } else if (resource.valueInteger !== undefined && resource.valueInteger !== null) {
      selectedValueType = 'integer';
    } else if (resource.valueCodeableConcept !== undefined) {
      selectedValueType = 'CodeableConcept';
    }
  }

  // Effective type selection (choice type - effective[x])
  let selectedEffectiveType = 'DateTime';

  $: {
    if (resource.effectiveDateTime !== undefined && resource.effectiveDateTime !== '') {
      selectedEffectiveType = 'DateTime';
    } else if (resource.effectivePeriod !== undefined) {
      selectedEffectiveType = 'Period';
    } else if (resource.effectiveInstant !== undefined) {
      selectedEffectiveType = 'Instant';
    }
  }

  function changeValueType() {
    // Clear all value types
    delete resource.valueQuantity;
    delete resource.valueString;
    delete resource.valueBoolean;
    delete resource.valueInteger;
    delete resource.valueCodeableConcept;
    delete resource.valueRange;
    delete resource.valueRatio;
    delete resource.valueSampledData;
    delete resource.valueTime;
    delete resource.valueDateTime;
    delete resource.valuePeriod;

    // Initialize selected type
    if (selectedValueType === 'Quantity') {
      resource.valueQuantity = { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' };
    } else if (selectedValueType === 'string') {
      resource.valueString = '';
    } else if (selectedValueType === 'boolean') {
      resource.valueBoolean = false;
    } else if (selectedValueType === 'integer') {
      resource.valueInteger = null;
    } else if (selectedValueType === 'CodeableConcept') {
      resource.valueCodeableConcept = { text: '', coding: [] };
    }
  }

  function changeEffectiveType() {
    // Clear all effective types
    delete resource.effectiveDateTime;
    delete resource.effectivePeriod;
    delete resource.effectiveInstant;
    delete resource.effectiveTiming;

    // Initialize selected type
    if (selectedEffectiveType === 'DateTime') {
      resource.effectiveDateTime = '';
    } else if (selectedEffectiveType === 'Period') {
      resource.effectivePeriod = { start: '', end: '' };
    } else if (selectedEffectiveType === 'Instant') {
      resource.effectiveInstant = '';
    }
  }

  function addCoding() {
    resource.code.coding = [...resource.code.coding, { system: '', code: '', display: '' }];
  }

  function removeCoding(index) {
    resource.code.coding = resource.code.coding.filter((_, i) => i !== index);
  }

  function addIdentifier() {
    if (!resource.identifier) resource.identifier = [];
    resource.identifier = [...resource.identifier, { system: '', value: '' }];
  }

  function removeIdentifier(index) {
    resource.identifier = resource.identifier.filter((_, i) => i !== index);
  }

  function addCategory() {
    if (!resource.category) resource.category = [];
    resource.category = [...resource.category, { text: '', coding: [] }];
  }

  function removeCategory(index) {
    resource.category = resource.category.filter((_, i) => i !== index);
  }

  function addPerformer() {
    if (!resource.performer) resource.performer = [];
    resource.performer = [...resource.performer, { reference: '', display: '' }];
  }

  function removePerformer(index) {
    resource.performer = resource.performer.filter((_, i) => i !== index);
  }

  function addNote() {
    if (!resource.note) resource.note = [];
    resource.note = [...resource.note, { text: '' }];
  }

  function removeNote(index) {
    resource.note = resource.note.filter((_, i) => i !== index);
  }

  function addReferenceRange() {
    if (!resource.referenceRange) resource.referenceRange = [];
    resource.referenceRange = [...resource.referenceRange, { low: {}, high: {}, text: '' }];
  }

  function removeReferenceRange(index) {
    resource.referenceRange = resource.referenceRange.filter((_, i) => i !== index);
  }

  function addComponent() {
    if (!resource.component) resource.component = [];
    resource.component = [...resource.component, {
      code: { text: '', coding: [] },
      valueQuantity: { value: null, unit: '', system: 'http://unitsofmeasure.org', code: '' }
    }];
  }

  function removeComponent(index) {
    resource.component = resource.component.filter((_, i) => i !== index);
  }

  function addFocus() {
    if (!resource.focus) resource.focus = [];
    resource.focus = [...resource.focus, { reference: '', display: '' }];
  }

  function removeFocus(index) {
    resource.focus = resource.focus.filter((_, i) => i !== index);
  }

  function addTriggeredBy() {
    if (!resource.triggeredBy) resource.triggeredBy = [];
    resource.triggeredBy = [...resource.triggeredBy, { observation: { reference: '' }, type: 'reflex' }];
  }

  function removeTriggeredBy(index) {
    resource.triggeredBy = resource.triggeredBy.filter((_, i) => i !== index);
  }

  function createNew() {
    // Generate a new UUID and navigate to that page
    const newId = crypto.randomUUID();
    window.location.href = `/fhir/Observation/${newId}`;
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
      const response = await fetch(`/fhir/Observation/${resource.id}/_rollback/${currentVersionId}`, {
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

<div class="observation">
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
      <a href="/fhir/Observation/{resource.id}/_history" class="link">History</a>
      <a href="/fhir/Observation/{resource.id}?_format=json" download="Observation_{resource.id}.json" class="link">JSON</a>
      <a href="/fhir/Observation/{resource.id}?_format=xml" download="Observation_{resource.id}.xml" class="link">XML</a>
    {/if}
  </div>

  <table class="grid">
    <tbody>
      <!-- ID (read-only) -->
      <tr>
        <td class="prop-name">id</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={resource.id} placeholder="Auto-generated" disabled />
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
          <input type="text" bind:value={resource.implicitRules} placeholder="URI of implicit rules" disabled={saving} />
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

      <!-- Identifier -->
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

      <!-- BasedOn -->
      <tr>
        <td class="prop-name">basedOn</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.basedOn && resource.basedOn[0] ? resource.basedOn[0].reference : ''}
                 on:input={(e) => {
                   if (!resource.basedOn) resource.basedOn = [];
                   if (!resource.basedOn[0]) resource.basedOn[0] = {};
                   resource.basedOn[0].reference = e.target.value;
                 }}
                 placeholder="Reference to request (e.g., ServiceRequest/123)"
                 disabled={saving} />
        </td>
      </tr>

      <!-- TriggeredBy (FHIR R5) -->
      <tr>
        <td class="prop-name">triggeredBy</td>
        <td class="control">
          <button class="btn-tiny" on:click={addTriggeredBy} disabled={saving} title="Add TriggeredBy">+</button>
        </td>
        <td class="value">
          {#each resource.triggeredBy as trigger, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeTriggeredBy(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">observation</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text"
                               value={trigger.observation?.reference || ''}
                               on:input={(e) => {
                                 if (!trigger.observation) trigger.observation = {};
                                 trigger.observation.reference = e.target.value;
                               }}
                               placeholder="Observation/123"
                               disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">type</td>
                      <td class="control"></td>
                      <td class="value">
                        <select bind:value={trigger.type} disabled={saving}>
                          <option value="reflex">Reflex</option>
                          <option value="repeat">Repeat</option>
                          <option value="re-run">Re-run</option>
                        </select>
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">reason</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={trigger.reason} placeholder="Reason" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- PartOf -->
      <tr>
        <td class="prop-name">partOf</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.partOf && resource.partOf[0] ? resource.partOf[0].reference : ''}
                 on:input={(e) => {
                   if (!resource.partOf) resource.partOf = [];
                   if (!resource.partOf[0]) resource.partOf[0] = {};
                   resource.partOf[0].reference = e.target.value;
                 }}
                 placeholder="Reference (e.g., Procedure/123)"
                 disabled={saving} />
        </td>
      </tr>

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

      <!-- Category -->
      <tr>
        <td class="prop-name">category</td>
        <td class="control">
          <button class="btn-tiny" on:click={addCategory} disabled={saving} title="Add Category">+</button>
        </td>
        <td class="value">
          {#each resource.category as category, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeCategory(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <input type="text" bind:value={category.text} placeholder="Category text (e.g., vital-signs)" disabled={saving} />
              </div>
            </div>
          {/each}
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
          <div class="nested">
            <table class="grid nested-table">
              <tbody>
                <tr>
                  <td class="prop-name">reference</td>
                  <td class="control"></td>
                  <td class="value">
                    <input type="text" bind:value={resource.subject.reference} placeholder="Patient/123" disabled={saving} />
                  </td>
                </tr>
                <tr>
                  <td class="prop-name">display</td>
                  <td class="control"></td>
                  <td class="value">
                    <input type="text" bind:value={resource.subject.display} placeholder="Display name" disabled={saving} />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </td>
      </tr>

      <!-- Focus (FHIR R5) -->
      <tr>
        <td class="prop-name">focus</td>
        <td class="control">
          <button class="btn-tiny" on:click={addFocus} disabled={saving} title="Add Focus">+</button>
        </td>
        <td class="value">
          {#each resource.focus as focus, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeFocus(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <input type="text" bind:value={focus.reference} placeholder="Reference (e.g., Medication/123)" disabled={saving} />
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- Encounter -->
      <tr>
        <td class="prop-name">encounter</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.encounter?.reference || ''}
                 on:input={(e) => {
                   if (!resource.encounter) resource.encounter = {};
                   resource.encounter.reference = e.target.value;
                 }}
                 placeholder="Encounter/123"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Effective[x] - Choice type -->
      <tr>
        <td class="prop-name">effective[x]</td>
        <td class="control"></td>
        <td class="value">
          <div class="value-choice">
            <select bind:value={selectedEffectiveType} on:change={changeEffectiveType} disabled={saving}>
              <option value="DateTime">DateTime</option>
              <option value="Period">Period</option>
              <option value="Instant">Instant</option>
            </select>

            {#if selectedEffectiveType === 'DateTime'}
              <input type="datetime-local" bind:value={resource.effectiveDateTime} disabled={saving} />
            {:else if selectedEffectiveType === 'Period'}
              <table class="grid nested-table">
                <tbody>
                  <tr>
                    <td class="prop-name">start</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="datetime-local" bind:value={resource.effectivePeriod.start} disabled={saving} />
                    </td>
                  </tr>
                  <tr>
                    <td class="prop-name">end</td>
                    <td class="control"></td>
                    <td class="value">
                      <input type="datetime-local" bind:value={resource.effectivePeriod.end} disabled={saving} />
                    </td>
                  </tr>
                </tbody>
              </table>
            {:else if selectedEffectiveType === 'Instant'}
              <input type="datetime-local" bind:value={resource.effectiveInstant} disabled={saving} />
            {/if}
          </div>
        </td>
      </tr>

      <!-- Issued -->
      <tr>
        <td class="prop-name">issued</td>
        <td class="control"></td>
        <td class="value">
          <input type="datetime-local" bind:value={resource.issued} placeholder="When published" disabled={saving} />
        </td>
      </tr>

      <!-- Performer -->
      <tr>
        <td class="prop-name">performer</td>
        <td class="control">
          <button class="btn-tiny" on:click={addPerformer} disabled={saving} title="Add Performer">+</button>
        </td>
        <td class="value">
          {#each resource.performer as performer, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removePerformer(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">reference</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={performer.reference} placeholder="Practitioner/123" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">display</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={performer.display} placeholder="Display name" disabled={saving} />
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          {/each}
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
              <option value="CodeableConcept">CodeableConcept</option>
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
            {:else if selectedValueType === 'CodeableConcept'}
              <input type="text" bind:value={resource.valueCodeableConcept.text} placeholder="Coded value text" disabled={saving} />
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

      <!-- DataAbsentReason -->
      <tr>
        <td class="prop-name">dataAbsentReason</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.dataAbsentReason?.text || ''}
                 on:input={(e) => {
                   if (!resource.dataAbsentReason) resource.dataAbsentReason = {};
                   resource.dataAbsentReason.text = e.target.value;
                 }}
                 placeholder="Why data is absent"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Note -->
      <tr>
        <td class="prop-name">note</td>
        <td class="control">
          <button class="btn-tiny" on:click={addNote} disabled={saving} title="Add Note">+</button>
        </td>
        <td class="value">
          {#each resource.note as note, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeNote(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <input type="text" bind:value={note.text} placeholder="Note text" disabled={saving} />
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- BodySite -->
      <tr>
        <td class="prop-name">bodySite</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.bodySite?.text || ''}
                 on:input={(e) => {
                   if (!resource.bodySite) resource.bodySite = {};
                   resource.bodySite.text = e.target.value;
                 }}
                 placeholder="Body site (e.g., left arm)"
                 disabled={saving} />
        </td>
      </tr>

      <!-- BodyStructure (FHIR R5) -->
      <tr>
        <td class="prop-name">bodyStructure</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.bodyStructure?.reference || ''}
                 on:input={(e) => {
                   if (!resource.bodyStructure) resource.bodyStructure = {};
                   resource.bodyStructure.reference = e.target.value;
                 }}
                 placeholder="BodyStructure/123"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Method -->
      <tr>
        <td class="prop-name">method</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.method?.text || ''}
                 on:input={(e) => {
                   if (!resource.method) resource.method = {};
                   resource.method.text = e.target.value;
                 }}
                 placeholder="Method (e.g., auscultation)"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Specimen -->
      <tr>
        <td class="prop-name">specimen</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.specimen?.reference || ''}
                 on:input={(e) => {
                   if (!resource.specimen) resource.specimen = {};
                   resource.specimen.reference = e.target.value;
                 }}
                 placeholder="Specimen/123"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Device -->
      <tr>
        <td class="prop-name">device</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.device?.reference || ''}
                 on:input={(e) => {
                   if (!resource.device) resource.device = {};
                   resource.device.reference = e.target.value;
                 }}
                 placeholder="Device/123"
                 disabled={saving} />
        </td>
      </tr>

      <!-- ReferenceRange -->
      <tr>
        <td class="prop-name">referenceRange</td>
        <td class="control">
          <button class="btn-tiny" on:click={addReferenceRange} disabled={saving} title="Add Reference Range">+</button>
        </td>
        <td class="value">
          {#each resource.referenceRange as range, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeReferenceRange(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <input type="text" bind:value={range.text} placeholder="Reference range text (e.g., 60-100 mg/dL)" disabled={saving} />
              </div>
            </div>
          {/each}
        </td>
      </tr>

      <!-- HasMember -->
      <tr>
        <td class="prop-name">hasMember</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.hasMember && resource.hasMember[0] ? resource.hasMember[0].reference : ''}
                 on:input={(e) => {
                   if (!resource.hasMember) resource.hasMember = [];
                   if (!resource.hasMember[0]) resource.hasMember[0] = {};
                   resource.hasMember[0].reference = e.target.value;
                 }}
                 placeholder="Observation/123 (related member)"
                 disabled={saving} />
        </td>
      </tr>

      <!-- DerivedFrom -->
      <tr>
        <td class="prop-name">derivedFrom</td>
        <td class="control"></td>
        <td class="value">
          <input type="text"
                 value={resource.derivedFrom && resource.derivedFrom[0] ? resource.derivedFrom[0].reference : ''}
                 on:input={(e) => {
                   if (!resource.derivedFrom) resource.derivedFrom = [];
                   if (!resource.derivedFrom[0]) resource.derivedFrom[0] = {};
                   resource.derivedFrom[0].reference = e.target.value;
                 }}
                 placeholder="Reference (source observation)"
                 disabled={saving} />
        </td>
      </tr>

      <!-- Component -->
      <tr>
        <td class="prop-name">component</td>
        <td class="control">
          <button class="btn-tiny" on:click={addComponent} disabled={saving} title="Add Component">+</button>
        </td>
        <td class="value">
          {#each resource.component as component, i}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => removeComponent(i)} disabled={saving} title="Remove">-</button>
              <div class="nested">
                <table class="grid nested-table">
                  <tbody>
                    <tr>
                      <td class="prop-name">code.text</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={component.code.text} placeholder="Component code" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">value</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="number" step="any" bind:value={component.valueQuantity.value} placeholder="Value" disabled={saving} />
                      </td>
                    </tr>
                    <tr>
                      <td class="prop-name">unit</td>
                      <td class="control"></td>
                      <td class="value">
                        <input type="text" bind:value={component.valueQuantity.unit} placeholder="Unit" disabled={saving} />
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
  .observation {
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

  .nested {
    width: 100%;
  }

  .nested-table {
    margin: 0.25rem 0 0 0;
    font-size: 0.75rem;
  }

  .nested-table td.prop-name {
    width: 25%;
  }

  .value-choice {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }

  .value-choice > select {
    width: 200px;
  }

  input[type="text"],
  input[type="number"],
  input[type="datetime-local"],
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
