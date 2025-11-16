// src/hooks/useGraphvizWASM.ts
import { useState, useEffect } from 'react';
import { Graphviz } from '@hpcc-js/wasm-graphviz';

export function useGraphvizWASM() {
  const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);
  const [graphvizWasmError, setGraphvizWasmError] = useState<string | null>(
    null
  );

  useEffect(() => {
    Graphviz.load()
      .then(() => {
        console.log('Graphviz WASM initialised successfully.');
        setIsGraphvizWasmReady(true);
      })
      .catch((err: any) => {
        console.error('Failed to initialise Graphviz WASM:', err);
        setGraphvizWasmError(`Graphviz WASM failed to load: ${err.message}`);
      });
  }, []);

  return { isGraphvizWasmReady, graphvizWasmError };
}
