// src/components/CfgViewer.tsx
import React, { useState, useEffect, useCallback } from 'react';
import {
    useNodesState,
    useEdgesState,
    ReactFlowProvider,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions } from '@xyflow/react';

import '@xyflow/react/dist/style.css';
import '../styles/CfgViewer.css';

import { API_BASE_URL } from '../api';
import { Graphviz } from "@hpcc-js/wasm-graphviz";
import { getLayoutedElements } from '../utils/GraphLayout';

import { type CustomNodeData } from './CustomNode';
import GraphPanel from './GraphPanel';

const FIT_VIEW_OPTIONS: FitViewOptions = {
    padding: 0.2,
};

const ZOOM_CONFIGS = {
    min: 0.3,
    max: 3,
};

const NODE_COLORS = {
    RED: '#FF4D4D',
    LIGHT_RED: '#FFCCCC',
    GREEN: '#90EE90',
    LIGHT_GREEN: '#b9f4b9',
    DEFAULT: '#FFFFFF',
};

interface DotGraphResponse {
    [procedureName: string]: string;
}

interface CfgViewerProps {
    selectedEpochName: string | null;
    selectedProcedureName: string | null;
}

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

    const compareAndColorNodes = useCallback((
        beforeGraphNodes: Node<CustomNodeData>[],
        afterGraphNodes: Node<CustomNodeData>[]
    ): { coloredBeforeNodes: Node<CustomNodeData>[], coloredAfterNodes: Node<CustomNodeData>[] } => {
        const coloredBeforeNodes: Node<CustomNodeData>[] = [];
        const coloredAfterNodes: Node<CustomNodeData>[] = [];

        const beforeMap = new Map<string, Node<CustomNodeData>>();
        beforeGraphNodes.forEach(node => {
            const originalId = node.id.replace('before-', '');
            beforeMap.set(originalId, node);
        });

        const afterMap = new Map<string, Node<CustomNodeData>>();
        afterGraphNodes.forEach(node => {
            const originalId = node.id.replace('after-', '');
            afterMap.set(originalId, node);
        });

        const allOriginalIds = new Set([...beforeMap.keys(), ...afterMap.keys()]);

        allOriginalIds.forEach(originalId => {
            const beforeNode = beforeMap.get(originalId);
            const afterNode = afterMap.get(originalId);

            let beforeColor = NODE_COLORS.DEFAULT;
            let afterColor = NODE_COLORS.DEFAULT;

            if (beforeNode && afterNode) {
                const headerEquivalent = beforeNode.data.header === afterNode.data.header;
                const fullContentEquivalent = beforeNode.data.fullContent === afterNode.data.fullContent;

                if (!headerEquivalent) {
                    beforeColor = NODE_COLORS.LIGHT_RED;
                    afterColor = NODE_COLORS.LIGHT_GREEN;
                } else if (!fullContentEquivalent) {
                    beforeColor = NODE_COLORS.LIGHT_RED;
                    afterColor = NODE_COLORS.LIGHT_GREEN;
                } else {
                    beforeColor = NODE_COLORS.DEFAULT; // Both headers and full content equivalent
                    afterColor = NODE_COLORS.DEFAULT;
                }
            } else if (beforeNode) {
                // Node exists only in 'before' graph
                beforeColor = NODE_COLORS.RED;
            } else if (afterNode) {
                // Node exists only in 'after' graph
                afterColor = NODE_COLORS.GREEN
            }

            if (beforeNode) {
                coloredBeforeNodes.push({
                    ...beforeNode,
                    data: { ...beforeNode.data, nodeBackgroundColor: beforeColor },
                });
            }
            if (afterNode) {
                coloredAfterNodes.push({
                    ...afterNode,
                    data: { ...afterNode.data, nodeBackgroundColor: afterColor },
                });
            }
        });

        return { coloredBeforeNodes, coloredAfterNodes };
    }, []);

    useEffect(() => {
        const fetchAndRenderCfgs = async () => {
            if (!isGraphvizWasmReady) {
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

                let processedBeforeNodes: Node<CustomNodeData>[] = [];
                let processedBeforeEdges: Edge[] = [];
                let processedAfterNodes: Node<CustomNodeData>[] = [];
                let processedAfterEdges: Edge[] = [];

                if (beforeDotString) {
                    const { nodes, edges } = await getLayoutedElements(beforeDotString, 'before-');
                    processedBeforeNodes = nodes;
                    processedBeforeEdges = edges;
                    console.log('Final Before Nodes Count:', nodes.length);
                } else {
                    console.warn(`No 'before' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'before' CFG data for '${selectedProcedureName}'.`);
                }

                if (afterDotString) {
                    const { nodes, edges } = await getLayoutedElements(afterDotString, 'after-');
                    processedAfterNodes = nodes;
                    processedAfterEdges = edges;
                    console.log('Final After Nodes Count:', nodes.length);
                } else {
                    console.warn(`No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'after' CFG data for '${selectedProcedureName}'.`);
                }

                const { coloredBeforeNodes, coloredAfterNodes } = compareAndColorNodes(processedBeforeNodes, processedAfterNodes);

                setBeforeNodes(coloredBeforeNodes);
                setBeforeEdges(processedBeforeEdges);
                setAfterNodes(coloredAfterNodes);
                setAfterEdges(processedAfterEdges);

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

            {/* Render 'After' Graph */}
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
        </div>
    );
};

export default CfgViewer;