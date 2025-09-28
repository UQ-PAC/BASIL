// src/hooks/useGraphvizWASM.ts
import { useState, useEffect } from 'react';
import { Graphviz } from '@hpcc-js/wasm-graphviz'; // TODO: Can this be done offline

export function useGraphvizWASM() {
  const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);
  const [graphvizWasmError, setGraphvizWasmError] = useState<string | null>(
    null
  );

  useEffect(() => {
    Graphviz.load()
      .then(() => {
        console.log('Graphviz WASM initialized successfully.');
        setIsGraphvizWasmReady(true);
      })
      .catch((err: any) => {
        console.error('Failed to initialize Graphviz WASM:', err);
        setGraphvizWasmError(`Graphviz WASM failed to load: ${err.message}`);
      });
  }, []);

  return { isGraphvizWasmReady, graphvizWasmError };
}
