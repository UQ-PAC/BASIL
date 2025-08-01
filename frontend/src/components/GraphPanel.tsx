// src/components/GraphPanel.tsx
import React, {useCallback, useEffect, useRef, useState} from 'react';
import {
    ReactFlow,
    Controls,
    MiniMap,
    type NodeChange,
    type EdgeChange,
    type Node as ReactFlowNode,
    type ReactFlowInstance, useReactFlow,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions } from '@xyflow/react';

import CustomNode from './CustomNode';
import { type CustomNodeData } from './CustomNode';
import CustomBackground from './CustomBackground';

interface GraphPanelProps {
    nodes: Node<CustomNodeData>[];
    edges: Edge[];
    onNodesChange: (changes: NodeChange<Node<CustomNodeData>>[]) => void;
    onEdgesChange: (changes: EdgeChange<Edge>[]) => void;
    title: string;
    fitViewOptions: FitViewOptions;
    minZoom: number;
    maxZoom: number;
    graphRenderKey: number;
}

const nodeTypes = { customNode: CustomNode };

const GraphPanel: React.FC<GraphPanelProps> = ({
                                                   nodes,
                                                   edges,
                                                   onNodesChange,
                                                   onEdgesChange,
                                                   title,
                                                   fitViewOptions,
                                                   minZoom,
                                                   maxZoom,
                                                   graphRenderKey,
                                               }) => {
    const { fitView } = useReactFlow();
    const [reactFlowInstanceReady, setReactFlowInstanceReady] = useState(false);
    const fitViewExecutedRef = useRef<number | null>(null);

    const onReactFlowInit = useCallback((instance: ReactFlowInstance<Node<CustomNodeData>, Edge>) => {
        console.log(`ReactFlow instance for "${title}" initialized.`);
        setReactFlowInstanceReady(true);
        instance.fitView(fitViewOptions as FitViewOptions<Node<CustomNodeData>>);
    }, [fitViewOptions, title]);

    useEffect(() => {
        setReactFlowInstanceReady(false);
    }, [graphRenderKey]);

    useEffect(() => {
        if (nodes.length > 0 && reactFlowInstanceReady && graphRenderKey > 0 && fitViewExecutedRef.current !== graphRenderKey) {
            const timeoutId = setTimeout(() => {
                fitView(fitViewOptions as FitViewOptions<Node<CustomNodeData>>);
                fitViewExecutedRef.current = graphRenderKey;
                console.log(`fitView executed for "${title}" (key: ${graphRenderKey}).`);
            }, 50);
            return () => clearTimeout(timeoutId);
        }
    }, [nodes.length, fitView, fitViewOptions, graphRenderKey, reactFlowInstanceReady, title]);

    const getMiniMapNodeColor = (node: ReactFlowNode<CustomNodeData>): string => {
        return node.data.nodeBorderColor || '#E0E0E0';
    };

    return (
        <div key={`graph-panel-${graphRenderKey}`} className="graph-wrapper">
            <h3>{title}</h3>
            <div className="react-flow-instance">
                {reactFlowInstanceReady && <CustomBackground />}
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    fitViewOptions={fitViewOptions}
                    proOptions={{ hideAttribution: true }}
                    minZoom={minZoom}
                    maxZoom={maxZoom}
                    nodeTypes={nodeTypes}
                    nodesConnectable={false}
                    onInit={onReactFlowInit}
                >
                    {reactFlowInstanceReady && (
                        <>
                            <MiniMap
                                nodeColor={getMiniMapNodeColor}
                                nodeStrokeWidth={1}
                                nodeBorderRadius={2}
                                pannable={true}
                            />
                            <Controls />
                        </>
                    )}
                </ReactFlow>
            </div>
        </div>
    );
};

export default GraphPanel;
