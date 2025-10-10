import svelte from 'rollup-plugin-svelte';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';

const production = !process.env.ROLLUP_WATCH;

export default {
  input: 'src/index.js',
  output: {
    format: 'iife',
    name: 'FHIRComponents',
    file: 'build/fhir-components.js',
    sourcemap: !production
  },
  plugins: [
    svelte({
      compilerOptions: {
        // Enable runtime checks when not in production
        dev: !production,
        // Generate code that works without a separate runtime
        generate: 'dom',
        hydratable: false
      },
      emitCss: false // Inline CSS into JS
    }),

    // Resolve imports from node_modules
    resolve({
      browser: true,
      dedupe: ['svelte']
    }),

    // Convert CommonJS to ES modules
    commonjs(),

    // Minify in production
    production && terser()
  ],

  watch: {
    clearScreen: false
  }
};
