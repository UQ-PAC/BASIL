// src/components/CombinedViewer.tsx
import React, { useState, useEffect, useRef } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import GraphPanel from './graph/GraphPanel.tsx';
import { useCombinedData } from '../../hooks/useCombinedData.ts';

import '../../styles/components/viewers/combined-viewer.css';
import '../../styles/components/button-selection.css';
import '../../styles/components/viewers/graph/graph.css';
import { FIT_VIEW_OPTIONS, ZOOM_CONFIGS } from '../../constants.ts';

import '../../lib/prism-ir.ts';
import 'prismjs/plugins/line-numbers/prism-line-numbers.js';
import 'prismjs/plugins/line-numbers/prism-line-numbers.css';
declare const Prism: any;

interface CombinedViewerProps {
  selectedStartEpoch: string | null;
  selectedEndEpoch: string | null;
  selectedProcedureName: string | null;
  setSelectedProcedureName: (name: string | null) => void;
  procedureNames: string[];
  loadingProcedures: boolean;
  procedureError: string | null;
}

const CombinedViewer: React.FC<CombinedViewerProps> = ({
  selectedStartEpoch,
  selectedEndEpoch,
  selectedProcedureName,
  setSelectedProcedureName,
  procedureNames,
  loadingProcedures,
  procedureError,
}) => {
  const [displayCfgType, setDisplayCfgType] = useState<'before' | 'after'>(
    'before'
  );

  const {
    irCode,
    isLoading,
    graphError,
    graphRenderKey,
    beforeNodes,
    beforeEdges,
    afterNodes,
    afterEdges,
    onBeforeNodesChange,
    onBeforeEdgesChange,
    onAfterNodesChange,
    onAfterEdgesChange,
  } = useCombinedData(
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    displayCfgType
  );

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

  const currentCfgNodes =
    displayCfgType === 'before' ? beforeNodes : afterNodes;
  const currentCfgEdges =
    displayCfgType === 'before' ? beforeEdges : afterEdges;
  const currentOnNodesChange =
    displayCfgType === 'before' ? onBeforeNodesChange : onAfterNodesChange;
  const currentOnEdgesChange =
    displayCfgType === 'before' ? onBeforeEdgesChange : onAfterEdgesChange;

  const currentCfgTitle = `${displayCfgType === 'before' ? 'Before' : 'After'} Transform: ${selectedProcedureName || 'N/A'}`;

  if (loadingProcedures || isLoading) {
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
                  disabled={loadingProcedures || isLoading}
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
                  disabled={loadingProcedures || isLoading}
                >
                  {displayCfgType === 'before' && (
                    <span className="tick">✓</span>
                  )}
                  Show Before CFG
                </button>
                <button
                  className={`toggle-button ${displayCfgType === 'after' ? 'active' : ''}`}
                  onClick={() => setDisplayCfgType('after')}
                  disabled={loadingProcedures || isLoading}
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
