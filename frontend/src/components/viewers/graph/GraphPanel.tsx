// src/components/GraphPanel.tsx
import React, { useCallback, useEffect, useRef, useState } from 'react';
import {
  ReactFlow,
  Controls,
  MiniMap,
  type NodeChange,
  type EdgeChange,
  type Node as ReactFlowNode,
  type ReactFlowInstance,
  useReactFlow,
} from '@xyflow/react';
import type { Node, Edge, FitViewOptions } from '@xyflow/react';

import CustomNode from './CustomNode.tsx';
import { type CustomNodeData } from './CustomNode.tsx';
import CustomBackground from './CustomBackground.tsx';

import ExpandIcon from '../../../assets/expand-icon.svg';
import CollapseIcon from '../../../assets/collapse-icon.svg';

interface GraphPanelProps {
  nodes: Node<CustomNodeData>[];
  edges: Edge[];
  onNodesChange: (changes: NodeChange<Node<CustomNodeData>>[]) => void;
  onEdgesChange: (changes: EdgeChange<Edge>[]) => void;
  onNodeDoubleClick?: (nodeId: string) => void;
  onExpandAll?: () => Promise<void>;
  allNodesExpanded?: boolean;
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
  onNodeDoubleClick,
  onExpandAll,
  allNodesExpanded,
  title,
  fitViewOptions,
  minZoom,
  maxZoom,
  graphRenderKey,
}) => {
  const { fitView } = useReactFlow();
  const [reactFlowInstanceReady, setReactFlowInstanceReady] = useState(false);
  const fitViewExecutedRef = useRef<number | null>(null);
  const [expandTriggered, setExpandTriggered] = useState(false);

  const handleExpandAll = useCallback(async () => {
    if (!onExpandAll || !reactFlowInstanceReady) return;

    await onExpandAll();
    setExpandTriggered(true);
  }, [onExpandAll, reactFlowInstanceReady]);

  useEffect(() => {
    if (!reactFlowInstanceReady || !expandTriggered) return;

    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        fitView(fitViewOptions as FitViewOptions<Node<CustomNodeData>>);
        setExpandTriggered(false);
      });
    });
  }, [nodes, reactFlowInstanceReady, expandTriggered, fitView, fitViewOptions]);

  const getMiniMapNodeColor = (node: ReactFlowNode<CustomNodeData>): string => {
    return node.data.nodeBorderColour || '#E0E0E0';
  };

  const onReactFlowInit = useCallback(
    (instance: ReactFlowInstance<Node<CustomNodeData>, Edge>) => {
      console.log(`ReactFlow instance for "${title}" initialized.`);
      setReactFlowInstanceReady(true);
      instance.fitView(fitViewOptions as FitViewOptions<Node<CustomNodeData>>);
    },
    [fitViewOptions, title]
  );

  useEffect(() => {
    if (
      nodes.length > 0 &&
      reactFlowInstanceReady &&
      graphRenderKey > 0 &&
      fitViewExecutedRef.current !== graphRenderKey
    ) {
      const timeoutId = setTimeout(() => {
        fitView(fitViewOptions as FitViewOptions<Node<CustomNodeData>>);
        fitViewExecutedRef.current = graphRenderKey;
        console.log(
          `fitView executed for "${title}" (key: ${graphRenderKey}).`
        );
      }, 50);
      return () => clearTimeout(timeoutId);
    }
  }, [
    nodes.length,
    fitView,
    fitViewOptions,
    graphRenderKey,
    reactFlowInstanceReady,
    title,
  ]);

  const handleNodeDoubleClick = useCallback(
    (_event: React.MouseEvent, node: ReactFlowNode<CustomNodeData>) => {
      if (onNodeDoubleClick) {
        onNodeDoubleClick(node.id);
      }
    },
    [onNodeDoubleClick]
  );

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
          onNodeDoubleClick={handleNodeDoubleClick}
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
              <div
                className="react-flow__panel react-flow__controls vertical bottom left"
                style={{ bottom: '104px' }}
              >
                <button
                  type="button"
                  onClick={handleExpandAll}
                  className="react-flow__controls-button"
                  title={
                    allNodesExpanded ? 'Collapse All Nodes' : 'Expand All Nodes'
                  }
                  aria-label={
                    allNodesExpanded ? 'Collapse All Nodes' : 'Expand All Nodes'
                  }
                >
                  {allNodesExpanded ? (
                    <CollapseIcon className="node-size-icon" />
                  ) : (
                    <ExpandIcon className="node-size-icon" />
                  )}
                </button>
              </div>
            </>
          )}
        </ReactFlow>
      </div>
    </div>
  );
};

export default GraphPanel;
