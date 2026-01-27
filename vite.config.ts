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
        main: path.resolve(__dirname, 'index.html')
      }
    },
    chunkSizeWarningLimit: 1000
  },
  server: {
    port: 5173,
    open: true,
    proxy: {
      // Proxy API requests to the Express server
      '/api': {
        target: 'http://localhost:9002',
        changeOrigin: true
      },
      // Proxy language algorithm directories for logos
      '/Algorithms': {
        target: 'http://localhost:9002',
        changeOrigin: true
      }
    }
  }
});
