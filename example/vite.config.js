import { fileURLToPath, URL } from "node:url";

import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import AutoImport from "unplugin-auto-import/vite";
import Components from "unplugin-vue-components/vite";

// https://vitejs.dev/config/
export default defineConfig({
  build: {
    manifest: "manifest.json",
    rollupOptions: {
      input: "src/js/main.js",
    },
  },
  plugins: [
    vue(),
    Components({
      dirs: ["src/js/components"],
    }),
    AutoImport({
      imports: ["vue"],
    }),
  ],
  resolve: {
    alias: {
      "@": fileURLToPath(new URL("./src/js", import.meta.url)),
    },
  },
});
