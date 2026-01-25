import { defineConfig } from 'vite';
import path from 'path';

export default defineConfig({
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src')
    }
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: path.resolve(__dirname, 'index-v2.html')
      }
    },
    chunkSizeWarningLimit: 1000
  },
  server: {
    port: 5173,
    open: true
  }
});
