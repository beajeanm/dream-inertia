import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";

export default defineConfig({
  plugins: [svelte()],
  build: {
    outDir: "assets/build/",
    manifest: true,
    rollupOptions: {
      input: "src/js/main.ts",
    },
  },
  optimizeDeps: {
    include: [
      "svelte",
      "@inertiajs/inertia",
      "@inertiajs/inertia-svelte",
      "@inertiajs/progress",
      "axios",
    ],
  },
});
