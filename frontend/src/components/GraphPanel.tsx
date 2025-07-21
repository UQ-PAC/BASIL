// src/components/GraphPanel.tsx
import React, { useEffect, useRef } from 'react';
import {
    ReactFlow,
    Controls,
    Background,
    MiniMap,
    useReactFlow,
    type NodeChange,
    type EdgeChange,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions, BackgroundVariant } from '@xyflow/react';

import CustomNode from './CustomNode';
import { type CustomNodeData } from './CustomNode';

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
    const fitViewExecutedRef = useRef<number | null>(null);

    useEffect(() => {
        if (nodes.length > 0 && graphRenderKey > 0 && fitViewExecutedRef.current !== graphRenderKey) {
            const timeoutId = setTimeout(() => {
                fitView(fitViewOptions);
                fitViewExecutedRef.current = graphRenderKey;
            }, 50);
            return () => clearTimeout(timeoutId);
        }
    }, [nodes, fitView, fitViewOptions, graphRenderKey]);

    return (
        <div key={`graph-panel-${graphRenderKey}`} className="graph-wrapper">
            <h3>{title}</h3>
            <div className="react-flow-instance">
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
                >
                    <MiniMap />
                    <Controls />
                    <Background variant={"dots" as BackgroundVariant} gap={12} size={1} />
                </ReactFlow>
            </div>
        </div>
    );
};

export default GraphPanel;
