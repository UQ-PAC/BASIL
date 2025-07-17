import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    proxy: {
      '/api': { // Catch all requests starting with /api
        target: 'http://localhost:8080', // Your Scala backend server
        changeOrigin: true, // Changes the origin of the host header to the target URL
        rewrite: (path) => path.replace(/^\/api/, ''), // Remove the /api prefix when forwarding
      },
    }
  }
})
