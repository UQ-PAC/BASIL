// src/components/CombinedViewer.tsx
import React, {useState, useEffect, useRef,} from 'react';
import {
    type Node, type Edge, ReactFlowProvider, useNodesState, useEdgesState
} from '@xyflow/react';
import { API_BASE_URL } from '../api';
import { Graphviz } from "@hpcc-js/wasm-graphviz";
import { getLayoutedElements } from '../utils/graphLayout.ts';
import { type CustomNodeData } from './CustomNode';
import GraphPanel from './GraphPanel';

import '../styles/combined-viewer.css';
import '../styles/button-selection.css'
import '../styles/graph.css'
import { FIT_VIEW_OPTIONS, ZOOM_CONFIGS } from '../constants';

declare const Prism: any;

import '../lib/prism-ir.ts';

interface DotGraphResponse {
    [procedureName: string]: string;
}

interface CombinedViewerProps {
    selectedEpochName: string | null;
}

// TODO: Remove the dup
const CombinedViewer: React.FC<CombinedViewerProps> = ({ selectedEpochName }) => {
    const [procedureNames, setProcedureNames] = useState<string[]>([]);
    const [selectedProcedureFromDropdown, setSelectedProcedureFromDropdown] = useState<string | null>(null);
    const [irCode, setIrCode] = useState<string | null>(null);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [isGraphvizWasmReady, setIsGraphvizWasmReady] = useState(false);

    // TODO: Do I need to State for storing both before and after graph data (after coloring)
    const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] = useEdgesState<Edge>([]);
    const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<Node<CustomNodeData>>([]);
    const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>([]);

    // State to control which CFG is currently displayed
    const [displayCfgType, setDisplayCfgType] = useState<'before' | 'after'>('before');
    const [graphRenderKey, setGraphRenderKey] = useState(0);
    const irCodeRef = useRef<HTMLDivElement>(null);

    useEffect(() => { // TODO: Code dup here
        Graphviz.load().then(() => {
            console.log("Graphviz WASM initialized successfully in CombinedViewer.");
            setIsGraphvizWasmReady(true);
        }).catch((err: any) => {
            console.error("Failed to initialize Graphviz WASM in CombinedViewer:", err);
            setError((prev) => (prev ? prev + "\n" : "") + `Graphviz WASM failed to load: ${err.message}`);
        });
    }, []);

    useEffect(() => {
        if (irCodeRef.current) {
            if (irCode && typeof Prism !== 'undefined' && Prism.languages && Prism.languages.ir) {
                try {
                    // Use Prism.highlight() to get the HTML string directly
                    const highlightedHtml = Prism.highlight(irCode, Prism.languages.ir, 'ir');
                    irCodeRef.current.innerHTML = highlightedHtml;
                } catch (e) {
                    console.error("Prism.highlight failed:", e);
                    irCodeRef.current.textContent = irCode;
                }
            } else {
                irCodeRef.current.textContent = irCode || '';
                if (irCode) {
                    console.warn("Prism.js or 'ir' language not fully loaded. Highlighting may not apply.");
                }
            }
        }
    }, [irCode, irCodeRef.current]);

    // TODO: This is also a lot of dup...
    useEffect(() => {
        const fetchAllData = async () => {
            if (!selectedEpochName) {
                setProcedureNames([]);
                setSelectedProcedureFromDropdown(null);
                setIrCode(null);
                setBeforeNodes([]);
                setBeforeEdges([]);
                setAfterNodes([]);
                setAfterEdges([]);
                return;
            }

            setLoading(true);
            setError(null);
            setIrCode(null); // TODO: Dupe
            setBeforeNodes([]);
            setBeforeEdges([]);
            setAfterNodes([]);
            setAfterEdges([]);

            try {
                const namesResponse = await fetch(`${API_BASE_URL}/procedures/${selectedEpochName}`);
                if (!namesResponse.ok) {
                    throw new Error(`HTTP error! status: ${namesResponse.status} fetching procedures for ${selectedEpochName}`);
                }
                const names: string[] = await namesResponse.json();
                setProcedureNames(names);

                let currentProcedure = selectedProcedureFromDropdown;
                if (!currentProcedure || !names.includes(currentProcedure)) {
                    currentProcedure = names.length > 0 ? names[0] : null;
                    setSelectedProcedureFromDropdown(currentProcedure);
                }

                if (!currentProcedure) {
                    setLoading(false);
                    return;
                }

                const irResponse = await fetch(`${API_BASE_URL}/ir/${selectedEpochName}/${currentProcedure}/${displayCfgType}`);
                if (!irResponse.ok) {
                    throw new Error(`HTTP error! status: ${irResponse.status} fetching IR for ${currentProcedure} (${displayCfgType})`);
                }
                const code: string = await irResponse.text();
                setIrCode(code);

                const fetchDotString = async (epoch: string, type: 'before' | 'after'): Promise<string | undefined> => {
                    const response = await fetch(`${API_BASE_URL}/cfg/${epoch}/${type}`);
                    if (!response.ok) {
                        console.error(`HTTP error! status: ${response.status} for ${type} CFG`);
                        return undefined;
                    }
                    const data: DotGraphResponse = await response.json();
                    const lowerSelectedProcedure = currentProcedure!.toLowerCase();
                    const matchingProcedureKey = Object.keys(data).find(key =>
                        key.toLowerCase().includes(lowerSelectedProcedure)
                    );
                    return matchingProcedureKey ? data[matchingProcedureKey] : undefined;
                };

                const beforeDotString = await fetchDotString(selectedEpochName!, 'before');
                const afterDotString = await fetchDotString(selectedEpochName!, 'after');

                if (beforeDotString) {
                    const { nodes, edges } = await getLayoutedElements(beforeDotString, 'before-');
                    setBeforeNodes(nodes);
                    setBeforeEdges(edges);
                } else {
                    console.warn(`No 'before' CFG data (DOT) for procedure '${currentProcedure}' in epoch '${selectedEpochName}'.`);
                }

                if (afterDotString) {
                    const { nodes, edges } = await getLayoutedElements(afterDotString, 'after-');
                    setAfterNodes(nodes);
                    setAfterEdges(edges);
                } else {
                    console.warn(`No 'after' CFG data (DOT) for procedure '${currentProcedure}' in epoch '${selectedEpochName}'.`);
                }

            } catch (e: any) {
                console.error("Error fetching data in CombinedViewer:", e);
                setError(`Failed to load data: ${e.message}`);
                setIrCode(null);
                setProcedureNames([]);
                setSelectedProcedureFromDropdown(null);
            } finally {
                setLoading(false);
                setGraphRenderKey(prev => prev + 1);
            }
        };

        if (isGraphvizWasmReady) {
            fetchAllData();
        }
    }, [selectedEpochName, selectedProcedureFromDropdown, displayCfgType, isGraphvizWasmReady, setBeforeNodes, setBeforeEdges, setAfterNodes, setAfterEdges]); // Re-run when epoch or procedure changes or after/before


    const currentCfgNodes = displayCfgType === 'before' ? beforeNodes : afterNodes;
    const currentCfgEdges = displayCfgType === 'before' ? beforeEdges : afterEdges;
    const currentOnNodesChange = displayCfgType === 'before' ? onBeforeNodesChange : onAfterNodesChange;
    const currentOnEdgesChange = displayCfgType === 'before' ? onBeforeEdgesChange : onAfterEdgesChange;

    const currentCfgTitle = `${displayCfgType === 'before' ? 'Before' : 'After'} Transform: ${selectedProcedureFromDropdown || 'N/A'}`;

    if (loading) {
        return <div className="combined-viewer-message">Loading data...</div>;
    }

    if (error) {
        return <div className="combined-viewer-error">Error: {error}</div>;
    }

    const showInitialMessage = !selectedEpochName || !selectedProcedureFromDropdown;
    if (showInitialMessage && procedureNames.length === 0) {
        return <div className="combined-viewer-message">Please select an epoch from the sidebar to view IR and CFG.</div>;
    }
    if (showInitialMessage && procedureNames.length > 0 && !selectedProcedureFromDropdown) {
        return <div className="combined-viewer-message">Please select a procedure from the dropdown.</div>;
    }

    return (
        <div className="combined-viewer-container">
            <div className="combined-viewer-header">
                {selectedEpochName && procedureNames.length > 0 && (
                    <div className="header-controls">
                        <div className="flex-spacer"></div>
                        <div className="procedure-select">
                            <label htmlFor="combined-procedure-select">Select Procedure:</label> { /* Change this to match the last screen */ }
                            <select
                                id="combined-procedure-select"
                                className="procedure-dropdown"
                                value={selectedProcedureFromDropdown || ''}
                                onChange={(e) => setSelectedProcedureFromDropdown(e.target.value)}
                                disabled={loading}
                            >
                                {!selectedProcedureFromDropdown && <option value="">-- Choose a Procedure --</option>}
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
                                disabled={loading}
                            >
                                {displayCfgType === 'before' && <span className="tick">✓</span>}
                                Show Before CFG
                            </button>
                            <button
                                className={`toggle-button ${displayCfgType === 'after' ? 'active' : ''}`}
                                onClick={() => setDisplayCfgType('after')}
                                disabled={loading}
                            >
                                {displayCfgType === 'after' && <span className="tick">✓</span>}
                                Show After CFG
                            </button>
                        </div>
                    </div>
                )}
                {selectedEpochName && procedureNames.length === 0 && !loading && !error && (
                    <p className="no-procedures-message">No procedures found for epoch: {selectedEpochName}</p>
                )}
            </div>

            <div className="combined-content-area">
                <div className="ir-code-panel">
                    <h3>Intermediate Representation (IR)</h3>
                    {irCode ? (
                        // <pre className="ir-code-display"><code ref={irCodeRef}>{irCode}</code></pre>
                        <pre className="ir-code-display">
                            <div ref={irCodeRef} className="language-ir">{irCode}</div>
                        </pre>
                    ) : (
                        <div className="ir-code-message">No IR code available for this procedure.</div>
                    )}
                </div>
                <div className="cfg-panel">
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
                                graphRenderKey={graphRenderKey} // Use the same key for both to trigger remount
                            />
                        </ReactFlowProvider>
                    ) : (
                        <div className="cfg-panel-message">No CFG data available for this view.</div>
                    )}
                </div>
            </div>
        </div>
    );
};

export default CombinedViewer;
