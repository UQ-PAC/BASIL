// src/components/CfgViewer.tsx
import React, { useState, useEffect, useCallback } from 'react';
import {
    useNodesState,
    useEdgesState,
    ReactFlowProvider,
    MarkerType
} from '@xyflow/react';
import type { Node, Edge } from '@xyflow/react';

import '@xyflow/react/dist/style.css';
import '../styles/cfg-viewer.css';
import '../styles/graph.css';

import { API_BASE_URL } from '../api';
import { Graphviz } from "@hpcc-js/wasm-graphviz";
import { getLayoutedElements } from '../utils/graphLayout.ts';

import { type CustomNodeData } from './CustomNode';
import GraphPanel from './GraphPanel';
import { FIT_VIEW_OPTIONS, ZOOM_CONFIGS } from '../constants';

const NODE_COLORS = {
    RED: '#FF4D4D',
    LIGHT_RED: '#FFCCCC', // TODO: Maybe make it yellow?
    GREEN: '#04d104',
    LIGHT_GREEN: '#b9f4b9',
    DEFAULT: '#777',
};

const EDGE_COLORS = {
    RED: '#c52222',
    GREEN: '#0f800f',
    DEFAULT: '#70e1ed',
};

interface DotGraphResponse {
    [procedureName: string]: string;
}

interface CfgViewerProps {
    selectedEpochName: string | null;
    selectedProcedureName: string | null;
    setSelectedProcedureName: (name: string | null) => void;
    procedureNames: string[];
    loadingProcedures: boolean;
    procedureError: string | null;
}

const CfgViewer: React.FC<CfgViewerProps> = ({
                                                 selectedEpochName,
                                                 selectedProcedureName,
                                                 setSelectedProcedureName,
                                                 procedureNames,
                                                 loadingProcedures,
                                                 procedureError
                                             }) => {
    const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] = useEdgesState<Edge>([]);
    const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>([]);

    const [loadingGraphs, setLoadingGraphs] = useState(false);
    const [graphError, setGraphError] = useState<string | null>(null);
    const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);

    const [graphRenderKey, setGraphRenderKey] = useState(0);

    // --- Graphviz WASM Initialization ---
    useEffect(() => {
        Graphviz.load().then(() => {
            console.log("Graphviz WASM initialized successfully.");
            setIsGraphvizWasmReady(true);
        }).catch((err: any) => {
            console.error("Failed to initialize Graphviz WASM:", err);
            setGraphError((prev) => (prev ? prev + "\n" : "") + `Graphviz WASM failed to load: ${err.message}`);
        });
    }, []);

    const compareAndColorElements = useCallback((
        beforeGraphNodes: Node<CustomNodeData>[],
        beforeGraphEdges: Edge[],
        afterGraphNodes: Node<CustomNodeData>[],
        afterGraphEdges: Edge[]
    ): {
        coloredBeforeNodes: Node<CustomNodeData>[],
        coloredAfterNodes: Node<CustomNodeData>[],
        coloredBeforeEdges: Edge[],
        coloredAfterEdges: Edge[]
    } => {
        const coloredBeforeNodes: Node<CustomNodeData>[] = [];
        const coloredAfterNodes: Node<CustomNodeData>[] = [];
        const coloredBeforeEdges: Edge[] = [];
        const coloredAfterEdges: Edge[] = [];

        const beforeNodeMap = new Map<string, Node<CustomNodeData>>();
        beforeGraphNodes.forEach(node => {
            const originalId = node.id.replace('before-', '');
            beforeNodeMap.set(originalId, node);
        });

        const afterNodeMap = new Map<string, Node<CustomNodeData>>();
        afterGraphNodes.forEach(node => {
            const originalId = node.id.replace('after-', '');
            afterNodeMap.set(originalId, node);
        });

        const allOriginalNodeIds = new Set([...beforeNodeMap.keys(), ...afterNodeMap.keys()]);

        allOriginalNodeIds.forEach(originalId => {
            const beforeNode = beforeNodeMap.get(originalId);
            const afterNode = afterNodeMap.get(originalId);

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
                    data: { ...beforeNode.data, nodeBorderColor: beforeColor },
                });
            }
            if (afterNode) {
                coloredAfterNodes.push({
                    ...afterNode,
                    data: { ...afterNode.data, nodeBorderColor: afterColor },
                });
            }
        });

        const beforeEdgeMap = new Map<string, Edge>();
        beforeGraphEdges.forEach(edge => {
            const originalSource = edge.source.replace('before-', '');
            const originalTarget = edge.target.replace('before-', '');
            beforeEdgeMap.set(`${originalSource}-${originalTarget}`, edge);
        });

        const afterEdgeMap = new Map<string, Edge>();
        afterGraphEdges.forEach(edge => {
            const originalSource = edge.source.replace('after-', '');
            const originalTarget = edge.target.replace('after-', '');
            afterEdgeMap.set(`${originalSource}-${originalTarget}`, edge);
        });

        const allOriginalEdgeKeys = new Set([...beforeEdgeMap.keys(), ...afterEdgeMap.keys()]);

        allOriginalEdgeKeys.forEach(edgeKey => {
            const beforeEdge = beforeEdgeMap.get(edgeKey);
            const afterEdge = afterEdgeMap.get(edgeKey);

            let beforeEdgeColor = EDGE_COLORS.DEFAULT;
            let afterEdgeColor = EDGE_COLORS.DEFAULT;

            if (beforeEdge && afterEdge) {
                // Edge exists in both, it's a matched edge
                beforeEdgeColor = EDGE_COLORS.DEFAULT;
                afterEdgeColor = EDGE_COLORS.DEFAULT;
            } else if (beforeEdge) {
                // Edge exists only in 'before' graph (deleted)
                beforeEdgeColor = EDGE_COLORS.RED;
            } else if (afterEdge) {
                // Edge exists only in 'after' graph (added)
                afterEdgeColor = EDGE_COLORS.GREEN;
            }

            if (beforeEdge) {
                coloredBeforeEdges.push({
                    ...beforeEdge,
                    style: { ...beforeEdge.style, stroke: beforeEdgeColor },
                    markerEnd: { type: MarkerType.ArrowClosed, color: beforeEdgeColor },
                });
            }
            if (afterEdge) {
                coloredAfterEdges.push({
                    ...afterEdge,
                    style: { ...afterEdge.style, stroke: afterEdgeColor },
                    markerEnd: { type: MarkerType.ArrowClosed, color: afterEdgeColor },
                });
            }
        });

        return { coloredBeforeNodes, coloredAfterNodes, coloredBeforeEdges, coloredAfterEdges };
    }, []);

    useEffect(() => {
        const fetchAndRenderCfgs = async () => {
            if (!isGraphvizWasmReady || !selectedEpochName || !selectedProcedureName) {
                setGraphError("Graphviz WebAssembly is still loading or failed to initialize.");
                setLoadingGraphs(false);
                return;
            }

            setLoadingGraphs(true);
            setGraphError(null);
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
                    const data: DotGraphResponse = await response.json(); // TODO: Save this so that I don't run this every time - calc time saved as well maybe? For Thesis

                    if (!selectedProcedureName) {
                        return undefined;
                    }

                    const lowerSelectedProcedure = selectedProcedureName.toLowerCase();

                    const matchingProcedureKey = Object.keys(data).find(key =>
                        key.toLowerCase().includes(lowerSelectedProcedure)
                    );

                    return matchingProcedureKey ? data[matchingProcedureKey] : undefined;
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
                    setGraphError((prev) => (prev ? prev + "\n" : "") + `No 'before' CFG data for '${selectedProcedureName}'.`);
                }

                if (afterDotString) {
                    const { nodes, edges } = await getLayoutedElements(afterDotString, 'after-');
                    processedAfterNodes = nodes;
                    processedAfterEdges = edges;
                    console.log('Final After Nodes Count:', nodes.length);
                } else {
                    console.warn(`No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setGraphError((prev) => (prev ? prev + "\n" : "") + `No 'after' CFG data for '${selectedProcedureName}'.`);
                }

                const {
                    coloredBeforeNodes,
                    coloredAfterNodes,
                    coloredBeforeEdges,
                    coloredAfterEdges
                } = compareAndColorElements(
                    processedBeforeNodes,
                    processedBeforeEdges,
                    processedAfterNodes,
                    processedAfterEdges
                );
                setBeforeNodes(coloredBeforeNodes);
                setBeforeEdges(coloredBeforeEdges);
                setAfterNodes(coloredAfterNodes);
                setAfterEdges(coloredAfterEdges);

            } catch (e: any) {
                console.error("Error fetching or processing CFG data:", e);
                setGraphError(`Failed to load or render CFG data: ${e.message}`);
            } finally {
                setLoadingGraphs(false);
                setGraphRenderKey(prev => prev + 1);
            }
        };

        if (isGraphvizWasmReady) {
            fetchAndRenderCfgs().catch(error => console.error("Unhandled promise rejected from 'fetchAndRenderCfgs: ", error));
        } else if (!selectedEpochName || !selectedProcedureName) {
            setBeforeNodes([]);
            setBeforeEdges([]);
            setAfterNodes([]);
            setAfterEdges([]);
            setLoadingGraphs(false);
            setGraphError(null);
            setGraphRenderKey(prev => prev + 1);
        }
    }, [selectedEpochName, selectedProcedureName, isGraphvizWasmReady, compareAndColorElements]);

    if (loadingProcedures || loadingGraphs) {
        return <div className="cfg-viewer-message">Loading CFG data...</div>;
    }

    if (procedureError || graphError) {
        return <div className="cfg-viewer-error">Error: {procedureError || graphError}</div>;
    }

    const showInitialMessage = !selectedEpochName || !selectedProcedureName;
    if (showInitialMessage && procedureNames.length === 0) {
        return <div className="cfg-viewer-message">Please select an epoch from the sidebar to view CFGs.</div>;
    }
    if (showInitialMessage && procedureNames.length > 0 && !selectedProcedureName) {
        return <div className="cfg-viewer-message">Please select a procedure from the dropdown.</div>;
    }

    // if ((!beforeNodes.length && !beforeEdges.length) && (!afterNodes.length && !afterEdges.length) && !loading && !error) {
    //     return <div className="cfg-viewer-message">No CFG data available for the selected procedure.</div>;
    // } // TODO: Maybe here just have a pop up notification instead...

    return (
        <div className="cfg-comparison-container-wrapper">
            <div className="cfg-viewer-header">
                {selectedEpochName && procedureNames.length > 0 && (
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
                                {!selectedProcedureName && <option value="">-- Choose a Procedure --</option>}
                                {procedureNames.map((name) => (
                                    <option key={name} value={name}>
                                        {name}
                                    </option>
                                ))}
                            </select>
                        </div>
                        <div className="flex-spacer"></div> { /* TODO: Maybe add a bit more padding here? */ }
                    </>
                )}
                {selectedEpochName && procedureNames.length === 0 && !loadingGraphs && !graphError && (
                    <p className="no-procedures-message">No procedures found for epoch: {selectedEpochName}</p>
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
                        <div className="cfg-panel-message">No CFG data available for the Before Nodes view.</div>
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
                        <div className="cfg-panel-message">No CFG data available for the Before Nodes view.</div>
                    )}
                </div>
            </div>
        </div>
    );
};

export default CfgViewer;
