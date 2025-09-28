// src/components/CfgViewer.tsx
import React from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import '@xyflow/react/dist/style.css';

import { useGraphvizWASM } from '../../../hooks/useGraphvizWASM';
import { useCfgData } from '../../../hooks/useCfgData';
import { FIT_VIEW_OPTIONS, ZOOM_CONFIGS } from '../../../constants.ts';
import { compareAndColourElements } from '../../../utils/cfgColouring.ts';

import GraphPanel from './GraphPanel.tsx';

import '../../../styles/components/viewers/cfg-viewer.css';
import '../../../styles/components/viewers/graph/graph.css';

interface CfgViewerProps {
  selectedStartEpoch: string | null;
  selectedEndEpoch: string | null;
  selectedProcedureName: string | null;
  setSelectedProcedureName: (name: string | null) => void;
  procedureNames: string[];
  loadingProcedures: boolean;
  procedureError: string | null;
}

const CfgViewer: React.FC<CfgViewerProps> = ({
  selectedStartEpoch,
  selectedEndEpoch,
  selectedProcedureName,
  setSelectedProcedureName,
  procedureNames,
  loadingProcedures,
  procedureError,
}) => {
  const { isGraphvizWasmReady, graphvizWasmError } = useGraphvizWASM();

  const {
    beforeNodes,
    beforeEdges,
    afterNodes,
    afterEdges,
    loadingGraphs,
    graphError: dataFetchError,
    graphRenderKey,
    onBeforeNodesChange,
    onBeforeEdgesChange,
    onAfterNodesChange,
    onAfterEdgesChange,
  } = useCfgData(
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    isGraphvizWasmReady,
    compareAndColourElements
  );

  const combinedGraphError = graphvizWasmError || dataFetchError;

  if (loadingProcedures || loadingGraphs) {
    return <div className="cfg-viewer-message">Loading CFG data...</div>;
  }

  if (procedureError || combinedGraphError) {
    return (
      <div className="cfg-viewer-error">
        Error: {procedureError || combinedGraphError}
      </div>
    );
  }

  const showInitialMessage =
    !selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName;
  if (showInitialMessage && procedureNames.length === 0) {
    return (
      <div className="cfg-viewer-message">
        Please select an epoch from the sidebar to view CFGs.
      </div>
    );
  }
  if (
    showInitialMessage &&
    procedureNames.length > 0 &&
    !selectedProcedureName
  ) {
    return (
      <div className="cfg-viewer-message">
        Please select a procedure from the dropdown.
      </div>
    );
  }

  // if ((!beforeNodes.length && !beforeEdges.length) && (!afterNodes.length && !afterEdges.length) && !loading && !error) {
  //     return <div className="cfg-viewer-message">No CFG data available for the selected procedure.</div>;
  // } // TODO: Maybe here just have a pop up notification instead...

  return (
    <div className="cfg-comparison-container-wrapper">
      <div className="cfg-viewer-header">
        {selectedStartEpoch &&
          selectedEndEpoch &&
          procedureNames.length > 0 && (
            <>
              <div className="flex-spacer"></div>
              <div className="procedure-select-wrapper">
                <label id="procedure-select">Select Procedure: </label>
                <select
                  id="procedure-select"
                  className="procedure-dropdown"
                  value={selectedProcedureName || ''}
                  onChange={(e) => setSelectedProcedureName(e.target.value)}
                  disabled={loadingGraphs}
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
              <div className="flex-spacer"></div>{' '}
              {/* TODO: Maybe add a bit more padding here? */}
            </>
          )}
        {selectedStartEpoch &&
          selectedEndEpoch &&
          procedureNames.length === 0 &&
          !loadingGraphs &&
          !combinedGraphError && (
            <p className="no-procedures-message">
              No procedures found for epoch: '{selectedStartEpoch}' and '
              {selectedEndEpoch}'
            </p>
          )}
      </div>
      <div className="cfg-comparison-container">
        <div className="cfg-panel">
          {beforeNodes.length > 0 || beforeEdges.length > 0 ? (
            <>
              {graphRenderKey > 0 && (
                <ReactFlowProvider>
                  <GraphPanel
                    nodes={beforeNodes}
                    edges={beforeEdges}
                    onNodesChange={onBeforeNodesChange}
                    onEdgesChange={onBeforeEdgesChange}
                    title={`Before Transform: ${selectedProcedureName}`}
                    fitViewOptions={FIT_VIEW_OPTIONS}
                    minZoom={ZOOM_CONFIGS.min}
                    maxZoom={ZOOM_CONFIGS.max}
                    graphRenderKey={graphRenderKey}
                  />
                </ReactFlowProvider>
              )}
            </>
          ) : (
            <div className="cfg-panel-message">
              No CFG data available for the Before Nodes view.
            </div>
          )}
        </div>
        <div className="cfg-panel">
          {beforeNodes.length > 0 || beforeEdges.length > 0 ? (
            <>
              {graphRenderKey > 0 && (
                <ReactFlowProvider>
                  <GraphPanel
                    nodes={afterNodes}
                    edges={afterEdges}
                    onNodesChange={onAfterNodesChange}
                    onEdgesChange={onAfterEdgesChange}
                    title={`After Transform: ${selectedProcedureName}`}
                    fitViewOptions={FIT_VIEW_OPTIONS}
                    minZoom={ZOOM_CONFIGS.min}
                    maxZoom={ZOOM_CONFIGS.max}
                    graphRenderKey={graphRenderKey}
                  />
                </ReactFlowProvider>
              )}
            </>
          ) : (
            <div className="cfg-panel-message">
              No CFG data available for the Before Nodes view.
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default CfgViewer;
