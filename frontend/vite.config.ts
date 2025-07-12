import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      '/ir-before': 'http://localhost:8080',
      '/ir-after': 'http://localhost:8080' // TODO: Add more endpoints for procedures and CFG code
    }
  }
})
