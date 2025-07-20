
// src/components/CfgViewer.tsx
import React, { useState, useEffect, useCallback } from 'react';
import {
    ReactFlow,
    Controls,
    Background,
    MiniMap,
    useNodesState,
    useEdgesState,
    ReactFlowProvider,
    MarkerType,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions, BackgroundVariant } from '@xyflow/react';

import '@xyflow/react/dist/style.css';
import '../styles/CfgViewer.css'; // Ensure your CSS is still applied correctly

import { API_BASE_URL } from '../api';
import { Graphviz } from "@hpcc-js/wasm-graphviz";
import ELK from 'elkjs/lib/elk.bundled.js';

interface DotGraphResponse {
    [procedureName: string]: string;
}

interface CfgViewerProps {
    selectedEpochName: string | null;
    selectedProcedureName: string | null;
}

const fitViewOptions: FitViewOptions = {
    padding: 0.2,
};

const elk = new ELK();

const elkLayoutOptions = {
    'elk.algorithm': 'layered',
    'elk.direction': 'DOWN',
    'elk.spacing.nodeNode': '75',
    'elk.spacing.nodeNodeBetweenLayers': '75',
    'elk.padding': '[top=50,left=50,bottom=50,right=50]',
    'elk.edgeRouting': 'ORTHOGONAL',
    'org.eclipse.elk.layered.unnecessaryBendpoints': 'true',
    'org.eclipse.elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
};

const simpleDotString = `
digraph SimpleCFG {
    nodeA [label="Start Node"];
    nodeB [label="Middle Node"];
    nodeC [label="End Node"];
    nodeA -> nodeB;
    nodeB -> nodeC;
}
`;


const CfgViewer: React.FC<CfgViewerProps> = ({ selectedEpochName, selectedProcedureName }) => {
    const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<Node>([]);
    const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] = useEdgesState<Edge>([]);
    const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<Node>([]);
    const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>([]);

    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);

    const [useMockData] = useState(false); // Keep this toggle for mock data - TODO: Chnage if I want, otherwise delete

    const [graphRenderKey, setGraphRenderKey] = useState(0); // Add this new state

    useEffect(() => {
        Graphviz.load().then(() => {
            console.log("Graphviz WASM initialized successfully.");
            setIsGraphvizWasmReady(true);
        }).catch((err: any) => {
            console.error("Failed to initialize Graphviz WASM:", err);
            setError((prev) => (prev ? prev + "\n" : "") + `Graphviz WASM failed to load: ${err.message}`);
        });
    }, []);


    // --- FIXED: getLayoutedElements now ensures unique node IDs using a Map ---
    const getLayoutedElements = useCallback(async (dotString: string, prefix: string): Promise<{ nodes: Node[]; edges: Edge[] }> => {
        console.log(`DEBUG: Input DOT String for ELK layout (prefix: ${prefix}):`, dotString);

        // --- Use a Map to store unique nodes by their ID ---
        const rawNodesMap = new Map<string, { id: string, label: string, width: number, height: number, shape: string }>();
        const rawEdgesDataForELK: { id: string, source: string, target: string, label?: string }[] = [];


        try {
            // --- Node Parsing ---
            const nodeRegex = /(?:\"([^"]+)\"|(\w+))\s*\[label="((?:[^"\\]|\\.)*?)"(?:, ([^\]]*))?\]/g;
            const nodeMatches = dotString.matchAll(nodeRegex);
            let uniqueNodeCount = 0; // Count of unique nodes added to the map

            for (const match of nodeMatches) {
                const originalId = match[1] || match[2];
                const id = `${prefix}${originalId}`; // Apply prefix to node ID

                // If this ID (with prefix) is already in the map, skip it
                if (rawNodesMap.has(id)) {
                    // console.warn(`Duplicate node ID '${originalId}' found in DOT string, ignoring additional definition for graph '${prefix}'.`);
                    continue; // Skip this duplicate entry
                }

                const label = match[3].replace(/\\"/g, '"').replace(/\\l/g, '\n');
                const attributes = match[4] || '';
                const shape = attributes.includes('shape="box"') ? 'box' : 'default';

                const lines = label.split('\n');
                const longestLineLength = Math.max(...lines.map(line => line.length));
                const estimatedWidth = Math.max(120, longestLineLength * 7 + 20);
                const estimatedHeight = Math.max(40, lines.length * 20 + 10);

                rawNodesMap.set(id, { id, label, width: estimatedWidth, height: estimatedHeight, shape }); // Add to map
                uniqueNodeCount++; // Increment count only for unique additions
            }
            // Convert map values to an array for ELK and React Flow
            const rawNodesData = Array.from(rawNodesMap.values());
            console.log(`DEBUG: Parsed ${uniqueNodeCount} unique raw nodes for ELK. Total unique nodes: ${rawNodesData.length}`);
            console.log(`DEBUG: Final rawNodesData after parsing (unique IDs):`, rawNodesData.map(n => n.id));


            // --- Edge Parsing ---
            const edgeRegex = /(?:\"([^"]+)\"|(\w+))\s*->\s*(?:\"([^"]+)\"|(\w+))(?:\[label="((?:[^"\\]|\\.)*?)"(?:, ([^\]]*))?\])?/g;
            const edgeMatches = dotString.matchAll(edgeRegex);

            let edgeCount = 0;
            for (const match of edgeMatches) {
                const originalSource = match[1] || match[2];
                const originalTarget = match[3] || match[4];
                const edgeLabel = match[5] ? match[5].replace(/\\"/g, '"') : undefined;

                // Apply prefix to edge source/target and ID
                const source = `${prefix}${originalSource}`;
                const target = `${prefix}${originalTarget}`;
                const id = `e-${source}-${target}-${edgeCount}`; // Keep unique ID for each edge

                // Note: Edge IDs do not need to be unique based on source/target,
                // but for React Flow, overall edge IDs should be unique in the array.
                // The current `e-${source}-${target}-${edgeCount}` ensures this.

                rawEdgesDataForELK.push({ id, source, target, label: edgeLabel });
                edgeCount++;
            }
            console.log(`DEBUG: Parsed ${edgeCount} raw edges for ELK.`);


            // --- Prepare graph data for ELK.js ---
            const elkNodes = rawNodesData.map(node => ({
                id: node.id,
                width: node.width,
                height: node.height,
            }));

            const elkEdges = rawEdgesDataForELK.map(edge => ({
                id: edge.id,
                sources: [edge.source],
                targets: [edge.target],
            }));

            const elkGraph = {
                id: 'root',
                layoutOptions: elkLayoutOptions,
                children: elkNodes,
                edges: elkEdges,
            };

            // --- Perform layout with ELK.js ---
            console.log('DEBUG: Attempting ELK layout...');
            const layoutedGraph = await elk.layout(elkGraph);
            console.log('DEBUG: ELK layout completed successfully.');

            // --- Convert ELK.js output back to React Flow format ---
            const reactFlowNodes: Node[] = layoutedGraph.children
                ? layoutedGraph.children
                    // Ensure we only map ELK nodes that correspond to our unique rawNodesData
                    .filter(elkNode => rawNodesData.some(n => n.id === elkNode.id))
                    .map(elkNode => {
                        const originalNodeData = rawNodesData.find(n => n.id === elkNode.id)!;

                        return {
                            id: elkNode.id, // ID already has prefix
                            position: { x: elkNode.x || 0, y: elkNode.y || 0 },
                            data: { label: originalNodeData.label },
                            style: { width: originalNodeData.width, height: originalNodeData.height },
                            type: originalNodeData.shape === 'box' ? 'default' : 'default', // Your shape logic
                        };
                    })
                : [];

            const reactFlowEdges: Edge[] = layoutedGraph.edges ? layoutedGraph.edges.map(elkEdge => {
                // Find original edge data using the prefixed source/target
                const originalEdgeData = rawEdgesDataForELK.find(e => e.source === elkEdge.sources[0] && e.target === elkEdge.targets[0]);
                return {
                    id: elkEdge.id,
                    source: elkEdge.sources[0],
                    target: elkEdge.targets[0],
                    label: originalEdgeData?.label, // Use label from parsed data
                    markerEnd: {
                        type: MarkerType.ArrowClosed,
                    },
                };
            }) : [];

            return { nodes: reactFlowNodes, edges: reactFlowEdges };

        } catch (e: any) {
            console.error("DEBUG: Error during DOT parsing or ELK layout process:", e);
            throw new Error(`Failed to parse and layout graph: ${e.message}`);
        }
    }, []);


    useEffect(() => {
        const fetchAndRenderCfgs = async () => {
            if (!isGraphvizWasmReady && !useMockData) {
                setError("Graphviz WebAssembly is still loading or failed to initialize.");
                setLoading(false);
                setBeforeNodes([]);
                setBeforeEdges([]);
                setAfterNodes([]);
                setAfterEdges([]);
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

                if (useMockData) {
                    console.log("Using mock data for CFG viewer.");
                    beforeDotString = simpleDotString;
                    afterDotString = simpleDotString;
                } else {
                    if (!selectedEpochName) {
                        setError("Please select an epoch to view CFGs.");
                        setLoading(false);
                        return;
                    }
                    if (!selectedProcedureName) {
                        setError("Please select a procedure to view CFGs.");
                        setLoading(false);
                        return;
                    }

                    const beforeResponse = await fetch(`${API_BASE_URL}/cfg/${selectedEpochName}/before`);
                    if (!beforeResponse.ok) {
                        throw new Error(`HTTP error! status: ${beforeResponse.status} for before CFG`);
                    }
                    const beforeData: DotGraphResponse = await beforeResponse.json();
                    beforeDotString = selectedProcedureName ? beforeData[selectedProcedureName] : undefined;

                    const afterResponse = await fetch(`${API_BASE_URL}/cfg/${selectedEpochName}/after`);
                    if (!afterResponse.ok) {
                        throw new Error(`HTTP error! status: ${afterResponse.status} for after CFG`);
                    }
                    const afterData: DotGraphResponse = await afterResponse.json();
                    afterDotString = selectedProcedureName ? afterData[selectedProcedureName] : undefined;
                }

                // Call getLayoutedElements with unique prefixes
                if (beforeDotString) {
                    const { nodes, edges } = await getLayoutedElements(beforeDotString, 'before-'); // Prefix for left graph
                    setBeforeNodes(nodes); // TODO: Could the problem be here?
                    setBeforeEdges(edges);
                } else if (!useMockData) {
                    console.warn(`No 'before' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'before' CFG data for '${selectedProcedureName}'.`);
                }

                if (afterDotString) {
                    const { nodes, edges } = await getLayoutedElements(afterDotString, 'after-'); // Prefix for right graph
                    setAfterNodes(nodes);
                    setAfterEdges(edges);
                } else if (!useMockData) {
                    console.warn(`No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEpochName}'.`);
                    setError((prev) => (prev ? prev + "\n" : "") + `No 'after' CFG data for '${selectedProcedureName}'.`);
                }

            } catch (e: any) {
                console.error("Error fetching or processing CFG data:", e);
                setError(`Failed to load or render CFG data: ${e.message}`);
            } finally {
                setLoading(false);
            }
            setGraphRenderKey(prev => prev + 1); // Increment key to force remount of ReactFlow components
        };

        if (isGraphvizWasmReady || useMockData) {
            fetchAndRenderCfgs();
        }
    }, [selectedEpochName, selectedProcedureName, getLayoutedElements, useMockData, isGraphvizWasmReady]);


    if (loading) {
        return <div className="cfg-viewer-message">Loading CFG data...</div>;
    }

    if (error) {
        return <div className="cfg-viewer-error">Error: {error}</div>;
    }

    if (!useMockData && !selectedEpochName && !selectedProcedureName) {
        return <div className="cfg-viewer-message">Please select an epoch and a procedure from the sidebar.</div>;
    }
    if (!useMockData && !selectedEpochName) {
        return <div className="cfg-viewer-message">Please select an epoch from the sidebar.</div>;
    }
    if (!useMockData && !selectedProcedureName) {
        return <div className="cfg-viewer-message">Please select a procedure from the sidebar.</div>;
    }

    if ((!beforeNodes.length && !beforeEdges.length) && (!afterNodes.length && !afterEdges.length)) {
        return <div className="cfg-viewer-message">No CFG data available for the selected procedure, or mock data failed.</div>;
    }

    return (
        <div className="cfg-comparison-container">
            {/* Apply key to force remount */}
            {graphRenderKey > 0 && ( // Only render if key is > 0 (after first data load)
                <div key={`before-graph-${graphRenderKey}`} className="graph-wrapper">
                    <h3>Before Transform: {useMockData ? "Mock Data (ELK Layout)" : selectedProcedureName}</h3>
                    <div className="react-flow-instance">
                        <ReactFlowProvider>
                            <ReactFlow
                                nodes={beforeNodes}
                                edges={beforeEdges}
                                onNodesChange={onBeforeNodesChange}
                                onEdgesChange={onBeforeEdgesChange}
                                fitView
                                fitViewOptions={fitViewOptions}
                                proOptions={{ hideAttribution: true }}
                            >
                                <MiniMap />
                                <Controls />
                                <Background variant={"dots" as BackgroundVariant} gap={12} size={1} />
                            </ReactFlow>
                        </ReactFlowProvider>
                    </div>
                </div>
            )}

            {graphRenderKey > 0 && ( // Only render if key is > 0
                <div key={`after-graph-${graphRenderKey}`} className="graph-wrapper">
                    <h3>After Transform: {useMockData ? "Mock Data (ELK Layout)" : selectedProcedureName}</h3>
                    <div className="react-flow-instance">
                        <ReactFlowProvider>
                            <ReactFlow
                                nodes={afterNodes}
                                edges={afterEdges}
                                onNodesChange={onAfterNodesChange}
                                onEdgesChange={onAfterEdgesChange}
                                fitView
                                fitViewOptions={fitViewOptions}
                                proOptions={{ hideAttribution: true }}
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