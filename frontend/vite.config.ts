import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import svgr from 'vite-plugin-svgr';

const SERVER_HOST = process.env.VITE_SERVER_HOST || '127.0.0.1';
const SERVER_PORT = process.env.VITE_SERVER_PORT || 8080;
const SERVER_TARGET = `http://${SERVER_HOST}:${SERVER_PORT}`;

// https://vite.dev/config/
export default defineConfig({
  server: {
    port: 5173,
    proxy: {
      '/api': {
        target: SERVER_TARGET,
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ''),
      },
    },
  },
  plugins: [
    react(),
    svgr({
      // svgr options: https://react-svgr.com/docs/options/
      svgrOptions: {
        exportType: 'default',
        ref: true,
        svgo: false,
        titleProp: true,
      },
      include: '**/*.svg',
    }),
  ],
});
