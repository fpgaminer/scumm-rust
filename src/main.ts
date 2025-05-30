import './style.css'
import { start } from "../pkg/scumm_rust"

// Initialize the WASM module
//init().then(() => {
//  console.log("WASM module initialized successfully");
  // Call the start function
  start();
//}).catch(err => {
//  console.error("Failed to initialize WASM module:", err);
//});

