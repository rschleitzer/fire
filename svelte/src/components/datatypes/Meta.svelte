<script>
  export let value = {};
  export let disabled = false;

  // Ensure meta has proper structure
  $: {
    if (!value.versionId) value.versionId = '';
    if (!value.lastUpdated) value.lastUpdated = '';
    if (!value.source) value.source = '';
    if (!value.profile) value.profile = [];
    if (!value.security) value.security = [];
    if (!value.tag) value.tag = [];
  }
</script>

<div class="nested">
  <table class="grid nested-table">
    <tbody>
      <tr>
        <td class="prop-name">versionId</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={value.versionId} placeholder="Version ID" disabled />
        </td>
      </tr>
      <tr>
        <td class="prop-name">lastUpdated</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={value.lastUpdated} placeholder="ISO 8601 timestamp" disabled />
        </td>
      </tr>
      <tr>
        <td class="prop-name">source</td>
        <td class="control"></td>
        <td class="value">
          <input type="text" bind:value={value.source} placeholder="Source system URI" {disabled} />
        </td>
      </tr>
      <tr>
        <td class="prop-name">profile</td>
        <td class="control">
          <button class="btn-tiny" on:click={() => { value.profile = [...value.profile, '']; }} {disabled} title="Add Profile">+</button>
        </td>
        <td class="value">
          {#each value.profile as profile, pi}
            <div class="inline-array-item">
              <button class="btn-tiny" on:click={() => { value.profile = value.profile.filter((_, idx) => idx !== pi); }} {disabled}>-</button>
              <input type="text" bind:value={value.profile[pi]} placeholder="Profile URI" {disabled} />
            </div>
          {/each}
        </td>
      </tr>
      <tr>
        <td class="prop-name">security</td>
        <td class="control">
          <button class="btn-tiny" on:click={() => { value.security = [...value.security, { system: '', code: '', display: '' }]; }} {disabled} title="Add Security">+</button>
        </td>
        <td class="value">
          {#each value.security as security, si}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => { value.security = value.security.filter((_, idx) => idx !== si); }} {disabled}>-</button>
              <div class="nested">
                <input type="text" bind:value={security.display} placeholder="Security label (e.g., confidential)" {disabled} />
              </div>
            </div>
          {/each}
        </td>
      </tr>
      <tr>
        <td class="prop-name">tag</td>
        <td class="control">
          <button class="btn-tiny" on:click={() => { value.tag = [...value.tag, { system: '', code: '', display: '' }]; }} {disabled} title="Add Tag">+</button>
        </td>
        <td class="value">
          {#each value.tag as tag, ti}
            <div class="array-item">
              <button class="btn-tiny" on:click={() => { value.tag = value.tag.filter((_, idx) => idx !== ti); }} {disabled}>-</button>
              <div class="nested">
                <input type="text" bind:value={tag.display} placeholder="Tag (e.g., workflow status)" {disabled} />
              </div>
            </div>
          {/each}
        </td>
      </tr>
    </tbody>
  </table>
</div>

<style>
  .nested {
    width: 100%;
  }

  .grid {
    width: 100%;
    border-collapse: collapse;
    border: 1px solid #ddd;
    background: white;
  }

  .nested-table {
    margin: 0;
    font-size: 0.875rem;
  }

  .grid tbody tr {
    border-bottom: 1px solid #eee;
  }

  .grid tbody tr:last-child {
    border-bottom: none;
  }

  td.prop-name {
    width: 25%;
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

  .inline-array-item {
    display: flex;
    gap: 0.25rem;
    margin-bottom: 0.25rem;
    align-items: center;
  }

  input[type="text"] {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ddd;
    border-radius: 3px;
    font-size: 0.875rem;
  }
</style>
