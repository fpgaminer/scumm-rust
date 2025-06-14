import { defineConfig } from 'vite'
import wasm from "vite-plugin-wasm";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [wasm()],
  build: {
    target: 'esnext' //browsers can handle the latest ES features
  },
  server: {
    port: 5178,
  },
})