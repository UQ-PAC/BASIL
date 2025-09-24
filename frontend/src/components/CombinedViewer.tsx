// src/components/CombinedViewer.tsx
import React, { useState, useEffect, useRef } from 'react';
import {
  type Node,
  type Edge,
  ReactFlowProvider,
  useNodesState,
  useEdgesState,
} from '@xyflow/react';
import { API_BASE_URL } from '../api';
import { Graphviz } from '@hpcc-js/wasm-graphviz';
import { getLayoutedElements } from '../utils/graphLayout.ts';
import { type CustomNodeData } from './CustomNode';
import GraphPanel from './GraphPanel';

import '../styles/combined-viewer.css';
import '../styles/button-selection.css';
import '../styles/graph.css';
import { FIT_VIEW_OPTIONS, ZOOM_CONFIGS } from '../constants';

declare const Prism: any;

import '../lib/prism-ir.ts';
import 'prismjs/plugins/line-numbers/prism-line-numbers.js';
import 'prismjs/plugins/line-numbers/prism-line-numbers.css';

interface DotGraphResponse {
  [procedureName: string]: string;
}

interface CombinedViewerProps {
  selectedStartEpoch: string | null;
  selectedEndEpoch: string | null;
  selectedProcedureName: string | null;
  setSelectedProcedureName: (name: string | null) => void;
  procedureNames: string[];
  loadingProcedures: boolean;
  procedureError: string | null;
}

// TODO: Remove the dup
const CombinedViewer: React.FC<CombinedViewerProps> = ({
  selectedStartEpoch,
  selectedEndEpoch,
  selectedProcedureName,
  setSelectedProcedureName,
  procedureNames,
  loadingProcedures,
  procedureError,
}) => {
  const [irCode, setIrCode] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [graphError, setGraphError] = useState<string | null>(null);
  const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);

  // TODO: Do I need to State for storing both before and after graph data (after coloring)
  const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<
    Node<CustomNodeData>
  >([]);
  const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] =
    useEdgesState<Edge>([]);
  const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<
    Node<CustomNodeData>
  >([]);
  const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>(
    []
  );

  // State to control which CFG is currently displayed
  const [displayCfgType, setDisplayCfgType] = useState<'before' | 'after'>(
    'before'
  );
  const [graphRenderKey, setGraphRenderKey] = useState(0);
  const irCodeRef = useRef<HTMLDivElement>(null);

  const [isSticky, setIsSticky] = useState(false);
  const [panelTopOffset, setPanelTopOffset] = useState(0);
  const graphWrapperRef = useRef<HTMLDivElement>(null);
  const stickyTopOffset = 105; /* The distance the headers above it take up */

  useEffect(() => {
    const handleScroll = () => {
      if (window.scrollY > panelTopOffset + stickyTopOffset) {
        setIsSticky(true);
      } else {
        setIsSticky(false);
      }
    };

    const setInitialTop = () => {
      if (graphWrapperRef.current) {
        setPanelTopOffset(graphWrapperRef.current.offsetTop);
      }
    };

    const timeoutId = setTimeout(setInitialTop, 100);

    window.addEventListener('scroll', handleScroll);

    return () => {
      window.removeEventListener('scroll', handleScroll);
      clearTimeout(timeoutId);
    };
  }, [panelTopOffset]);

  useEffect(() => {
    // TODO: Code dup here
    Graphviz.load()
      .then(() => {
        console.log(
          'Graphviz WASM initialized successfully in CombinedViewer.'
        );
        setIsGraphvizWasmReady(true);
      })
      .catch((err: any) => {
        console.error(
          'Failed to initialize Graphviz WASM in CombinedViewer:',
          err
        );
        setGraphError(
          (prev) =>
            (prev ? prev + '\n' : '') +
            `Graphviz WASM failed to load: ${err.message}`
        );
      });
  }, []);

  useEffect(() => {
    if (irCodeRef.current && irCode) {
      if (typeof Prism !== 'undefined' && Prism.languages.ir) {
        try {
          Prism.highlightElement(irCodeRef.current);
        } catch (e) {
          console.error('Prism.highlight failed:', e);
        }
      } else {
        console.warn(
          "Prism.js or 'ir' language not fully loaded. Highlighting may not apply."
        );
      }
    }
  }, [irCode, irCodeRef.current]);

  useEffect(() => {
    if (!selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName) {
      setIrCode(null);
      return;
    }

    const fetchIrCode = async () => {
      setLoading(true);
      try {
        const irResponse = await fetch(
          `${API_BASE_URL}/ir/${
            displayCfgType === 'before' ? selectedStartEpoch : selectedEndEpoch
          }/${selectedProcedureName}/${displayCfgType}`
        );
        if (!irResponse.ok) {
          throw new Error(
            `HTTP error! status: ${irResponse.status} fetching IR`
          );
        }
        const code: string = await irResponse.text();
        setIrCode(code);
      } catch (error) {
        console.error('Failed to fetch IR code:', error);
        setIrCode(null);
      } finally {
        setLoading(false);
      }
    };

    fetchIrCode();
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    displayCfgType,
  ]);

  useEffect(() => {
    if (
      !isGraphvizWasmReady ||
      !selectedStartEpoch ||
      !selectedEndEpoch ||
      !selectedProcedureName
    ) {
      setBeforeNodes([]);
      setBeforeEdges([]);
      setAfterNodes([]);
      setAfterEdges([]);
      setGraphRenderKey((prev) => prev + 1);
      return;
    }

    const fetchCfgData = async () => {
      setLoading(true);
      try {
        const fetchDotString = async (
          epoch: string,
          type: 'before' | 'after'
        ): Promise<string | undefined> => {
          const response = await fetch(`${API_BASE_URL}/cfg/${epoch}/${type}`);
          if (!response.ok) {
            console.error(
              `HTTP error! status: ${response.status} for ${type} CFG`
            );
            return undefined;
          }
          const data: DotGraphResponse = await response.json();
          const lowerSelectedProcedure = selectedProcedureName!.toLowerCase();
          const matchingProcedureKey = Object.keys(data).find((key) =>
            key.toLowerCase().includes(lowerSelectedProcedure)
          );
          return matchingProcedureKey ? data[matchingProcedureKey] : undefined;
        };

        const beforeDotString = await fetchDotString(
          selectedStartEpoch!,
          'before'
        );
        const afterDotString = await fetchDotString(selectedEndEpoch!, 'after');

        if (beforeDotString) {
          const { nodes, edges } = await getLayoutedElements(
            beforeDotString,
            'before-'
          );
          setBeforeNodes(nodes);
          setBeforeEdges(edges);
        } else {
          console.warn(
            `No 'before' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedStartEpoch}'.`
          );
        }

        if (afterDotString) {
          const { nodes, edges } = await getLayoutedElements(
            afterDotString,
            'after-'
          );
          setAfterNodes(nodes);
          setAfterEdges(edges);
        } else {
          console.warn(
            `No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEndEpoch}'.`
          );
        }
      } catch (e: any) {
        console.error('Error fetching data in CombinedViewer:', e);
        setGraphError(`Failed to load data: ${e.message}`);
        setBeforeNodes([]);
        setBeforeEdges([]);
        setAfterNodes([]);
        setAfterEdges([]);
      } finally {
        setLoading(false);
        setGraphRenderKey((prev) => prev + 1);
      }
    };

    fetchCfgData();
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    displayCfgType,
    isGraphvizWasmReady,
    setBeforeNodes,
    setBeforeEdges,
    setAfterNodes,
    setAfterEdges,
  ]);

  const currentCfgNodes =
    displayCfgType === 'before' ? beforeNodes : afterNodes;
  const currentCfgEdges =
    displayCfgType === 'before' ? beforeEdges : afterEdges;
  const currentOnNodesChange =
    displayCfgType === 'before' ? onBeforeNodesChange : onAfterNodesChange;
  const currentOnEdgesChange =
    displayCfgType === 'before' ? onBeforeEdgesChange : onAfterEdgesChange;

  const currentCfgTitle = `${displayCfgType === 'before' ? 'Before' : 'After'} Transform: ${selectedProcedureName || 'N/A'}`;

  if (loadingProcedures || loading) {
    return <div className="combined-viewer-message">Loading data...</div>;
  }

  if (procedureError || graphError) {
    return (
      <div className="combined-viewer-error">
        Error: {procedureError || graphError}
      </div>
    );
  }

  const showInitialMessage =
    !selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName;
  if (showInitialMessage && procedureNames.length === 0) {
    return (
      <div className="combined-viewer-message">
        Please select an epoch from the sidebar to view IR and CFG.
      </div>
    );
  }
  if (
    showInitialMessage &&
    procedureNames.length > 0 &&
    !selectedProcedureName
  ) {
    return (
      <div className="combined-viewer-message">
        Please select a procedure from the dropdown.
      </div>
    );
  }

  return (
    <div className="combined-viewer-container">
      <div className="combined-viewer-header">
        {selectedStartEpoch &&
          selectedEndEpoch &&
          procedureNames.length > 0 && (
            <div className="header-controls">
              <div className="flex-spacer"></div>
              <div className="procedure-select-wrapper">
                <label id="procedure-select">Select Procedure: </label>
                <select
                  id="combined-procedure-select"
                  className="procedure-dropdown"
                  value={selectedProcedureName || ''}
                  onChange={(e) => setSelectedProcedureName(e.target.value)}
                  disabled={loadingProcedures || loading}
                >
                  {!selectedProcedureName && (
                    <option value="">-- Choose a Procedure --</option>
                  )}
                  {procedureNames.map((name) => (
                    <option key={name} value={name}>
                      {name}
                    </option>
                  ))}
                </select>
              </div>
              <div className="flex-spacer"></div>
              <div className="toggle-group">
                <button
                  className={`toggle-button ${displayCfgType === 'before' ? 'active' : ''}`}
                  onClick={() => setDisplayCfgType('before')}
                  disabled={loadingProcedures || loading}
                >
                  {displayCfgType === 'before' && (
                    <span className="tick">✓</span>
                  )}
                  Show Before CFG
                </button>
                <button
                  className={`toggle-button ${displayCfgType === 'after' ? 'active' : ''}`}
                  onClick={() => setDisplayCfgType('after')}
                  disabled={loadingProcedures || loading}
                >
                  {displayCfgType === 'after' && (
                    <span className="tick">✓</span>
                  )}
                  Show After CFG
                </button>
              </div>
            </div>
          )}
        {selectedStartEpoch &&
          selectedEndEpoch &&
          procedureNames.length === 0 &&
          !loadingProcedures &&
          !procedureError && (
            <p className="no-procedures-message">
              No procedures found for epoch: {selectedStartEpoch},{' '}
              {selectedEndEpoch}
            </p>
          )}
      </div>

      <div className="combined-content-area">
        <div className="ir-code-panel">
          <h3>Intermediate Representation (IR)</h3>
          {irCode ? (
            <pre className="ir-code-display line-numbers">
              <code ref={irCodeRef} className="language-ir">
                {' '}
                {irCode}{' '}
              </code>
            </pre>
          ) : (
            <div className="ir-code-message">
              No IR code available for this procedure.
            </div>
          )}
        </div>
        <div
          className={`cfg-panel ${isSticky ? 'is-sticky-and-expanded' : ''}`}
        >
          {/* Render only one GraphPanel */}
          {currentCfgNodes.length > 0 || currentCfgEdges.length > 0 ? (
            <ReactFlowProvider>
              <GraphPanel
                nodes={currentCfgNodes}
                edges={currentCfgEdges}
                onNodesChange={currentOnNodesChange}
                onEdgesChange={currentOnEdgesChange}
                title={currentCfgTitle}
                fitViewOptions={FIT_VIEW_OPTIONS}
                minZoom={ZOOM_CONFIGS.min}
                maxZoom={ZOOM_CONFIGS.max}
                graphRenderKey={graphRenderKey}
              />
            </ReactFlowProvider>
          ) : (
            <div className="cfg-panel-message">
              No CFG data available for this view.
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default CombinedViewer;
