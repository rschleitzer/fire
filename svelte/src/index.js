// Export resource components (handle both display and editing)
export { default as Patient } from './components/resources/Patient.svelte';
export { default as Observation } from './components/resources/Observation.svelte';

// Export data type components (for standalone use if needed)
export { default as CodeableConcept } from './components/datatypes/CodeableConcept.svelte';
export { default as Coding } from './components/datatypes/Coding.svelte';
export { default as Quantity } from './components/datatypes/Quantity.svelte';
export { default as Reference } from './components/datatypes/Reference.svelte';
export { default as HumanName } from './components/datatypes/HumanName.svelte';
export { default as Address } from './components/datatypes/Address.svelte';
export { default as ContactPoint } from './components/datatypes/ContactPoint.svelte';
export { default as Identifier } from './components/datatypes/Identifier.svelte';
