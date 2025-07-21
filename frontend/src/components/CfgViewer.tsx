// src/components/CfgViewer.tsx
import React, { useState, useEffect } from 'react';
import {
    ReactFlow,
    Controls,
    Background,
    MiniMap,
    useNodesState,
    useEdgesState,
    ReactFlowProvider,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions, BackgroundVariant } from '@xyflow/react';

import '@xyflow/react/dist/style.css';
import '../styles/CfgViewer.css';

import { API_BASE_URL } from '../api';
import { Graphviz } from "@hpcc-js/wasm-graphviz";
import { getLayoutedElements } from '../utils/GraphLayout';

import CustomNode from './CustomNode';
import { type CustomNodeData } from './CustomNode';

const FIT_VIEW_OPTIONS: FitViewOptions = {
    padding: 0.2,
};

const ZOOM_CONFIGS = {
    min: 0.3,
    max: 3,
};

interface DotGraphResponse {
    [procedureName: string]: string;
}

interface CfgViewerProps {
    selectedEpochName: string | null;
    selectedProcedureName: string | null;
}

const nodeTypes = { customNode: CustomNode };

const CfgViewer: React.FC<CfgViewerProps> = ({ selectedEpochName, selectedProcedureName }) => {
    const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] = useEdgesState<Edge>([]);
    const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>([]);

    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);

    const [graphRenderKey, setGraphRenderKey] = useState(0);

    // --- Graphviz WASM Initialization ---
    useEffect(() => {
        Graphviz.load().then(() => {
            console.log("Graphviz WASM initialized successfully.");
            setIsGraphvizWasmReady(true);
        }).catch((err: any) => {
            console.error("Failed to initialize Graphviz WASM:", err);
            setError((prev) => (prev ? prev + "\n" : "") + `Graphviz WASM failed to load: ${err.message}`);
        });
    }, []);


    useEffect(() => {
        const fetchAndRenderCfgs = async () => {
            if (!isGraphvizWasmReady) { // TODO: Clean up this
                setError("Graphviz WebAssembly is still loading or failed to initialize.");
                setLoading(false);
                return;
            }

            if (!selectedEpochName || !selectedProcedureName) {
                setBeforeNodes([]);
                setBeforeEdges([]);
                setAfterNodes([]);
                setAfterEdges([]);
                setLoading(false);
                setError(null);
                return;
            }

            setLoading(true);
            setError(null);
            setBeforeNodes([]);
            setBeforeEdges([]);
            setAfterNodes([]);
            setAfterEdges([]);

            try {
                let beforeDotString: string | undefined;
                let afterDotString: string | undefined;


                const fetchDotString = async (epoch: string, type: 'before' | 'after') => {
                    const response = await fetch(`${API_BASE_URL}/cfg/${epoch}/${type}`);
                    if (!response.ok) {
                        throw new Error(`HTTP error! status: ${response.status} for ${type} CFG`);
                    }
                    const data: DotGraphResponse = await response.json();
                    return selectedProcedureName ? data[selectedProcedureName] : undefined;
                };

                beforeDotString = await fetchDotString(selectedEpochName!, 'before');
                afterDotString = await fetchDotString(selectedEpochName!, 'after');

                if (beforeDotString) {
                    const { nodes, edges } = await getLayoutedElements(beforeDotString, 'before-');
                    setBeforeNodes(nodes);
                    setBeforeEdges(edges);
                } else {
                    console.warn(`No 'before' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'before' CFG data for '${selectedProcedureName}'.`);
                }

                if (afterDotString) {
                    const { nodes, edges } = await getLayoutedElements(afterDotString, 'after-');
                    setAfterNodes(nodes);
                    setAfterEdges(edges);
                } else {
                    console.warn(`No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'after' CFG data for '${selectedProcedureName}'.`);
                }

            } catch (e: any) {
                console.error("Error fetching or processing CFG data:", e);
                setError(`Failed to load or render CFG data: ${e.message}`);
            } finally {
                setLoading(false);
                setGraphRenderKey(prev => prev + 1);
            }
        };

        if (isGraphvizWasmReady) {
            fetchAndRenderCfgs();
        }
    }, [selectedEpochName, selectedProcedureName, isGraphvizWasmReady]); // Dependencies for re-running effect


    if (loading) {
        return <div className="cfg-viewer-message">Loading CFG data...</div>;
    }

    if (error) {
        return <div className="cfg-viewer-error">Error: {error}</div>;
    }

    if (!selectedEpochName || !selectedProcedureName) {
        return <div className="cfg-viewer-message">Please select an epoch and a procedure from the sidebar to view CFGs.</div>;
    }

    if ((!beforeNodes.length && !beforeEdges.length) && (!afterNodes.length && !afterEdges.length)) {
        return <div className="cfg-viewer-message">No CFG data available for the selected procedure.</div>;
    }

    return (
        <div className="cfg-comparison-container">
            {/* Render 'Before' Graph */}
            {graphRenderKey > 0 && (
                <div key={`before-graph-${graphRenderKey}`} className="graph-wrapper">
                    <h3>Before Transform: {selectedProcedureName}</h3>
                    <div className="react-flow-instance">
                        <ReactFlowProvider>
                            <ReactFlow
                                nodes={beforeNodes}
                                edges={beforeEdges}
                                onNodesChange={onBeforeNodesChange}
                                onEdgesChange={onBeforeEdgesChange}
                                fitView
                                fitViewOptions={FIT_VIEW_OPTIONS}
                                proOptions={{ hideAttribution: true }}
                                minZoom={ZOOM_CONFIGS.min} // TODO: Convert to const
                                maxZoom={ZOOM_CONFIGS.max}
                                nodeTypes={nodeTypes}
                            >
                                <MiniMap />
                                <Controls />
                                <Background variant={"dots" as BackgroundVariant} gap={12} size={1} />
                            </ReactFlow>
                        </ReactFlowProvider>
                    </div>
                </div>
            )}

            {/* Render 'After' Graph */}
            {graphRenderKey > 0 && (
                <div key={`after-graph-${graphRenderKey}`} className="graph-wrapper">
                    <h3>After Transform: {selectedProcedureName}</h3>
                    <div className="react-flow-instance">
                        <ReactFlowProvider>
                            <ReactFlow
                                nodes={afterNodes}
                                edges={afterEdges}
                                onNodesChange={onAfterNodesChange}
                                onEdgesChange={onAfterEdgesChange}
                                fitView
                                fitViewOptions={FIT_VIEW_OPTIONS}
                                proOptions={{ hideAttribution: true }}
                                minZoom={ZOOM_CONFIGS.min} // TODO: Convert to const
                                maxZoom={ZOOM_CONFIGS.max}
                                nodeTypes={nodeTypes}
                            >
                                <MiniMap />
                                <Controls />
                                <Background variant={"dots" as BackgroundVariant} gap={12} size={1} />
                            </ReactFlow>
                        </ReactFlowProvider>
                    </div>
                </div>
            )}
        </div>
    );
};

export default CfgViewer;