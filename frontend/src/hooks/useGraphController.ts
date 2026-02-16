import { useCallback, useState } from 'react';
import {
  useNodesState,
  useEdgesState,
  type Node,
  type Edge,
} from '@xyflow/react';
import { applyLayout } from '../utils/graphLayout';
import type { CustomNodeData } from '../components/viewers/graph/CustomNode';

export function useGraphController(
  initialNodes: Node<CustomNodeData>[],
  initialEdges: Edge[]
) {
  const [nodes, setNodes, onNodesChange] =
    useNodesState<Node<CustomNodeData>>(initialNodes);

  const [edges, setEdges, onEdgesChange] = useEdgesState<Edge>(initialEdges);

  const [allNodesExpanded, setAllNodesExpanded] = useState(false);

  const toggleNodeExpand = useCallback(
    (nodeId: string) => {
      setNodes((nds) =>
        nds.map((n) =>
          n.id === nodeId
            ? { ...n, data: { ...n.data, isExpanded: !n.data.isExpanded } }
            : n
        )
      );
    },
    [setNodes]
  );

  const toggleAllExpand = useCallback(
    async (expand: boolean) => {
      const updatedNodes = nodes.map((n) => ({
        ...n,
        data: {
          ...n.data,
          isExpanded: expand ? (n.data.originalExpanded ?? true) : false,
          originalExpanded: expand
            ? n.data.originalExpanded
            : n.data.isExpanded,
        },
      }));

      const layouted = await applyLayout(updatedNodes, edges, expand);

      setNodes(layouted.nodes);
      setAllNodesExpanded(expand);
    },
    [nodes, edges, setNodes]
  );

  return {
    nodes,
    edges,
    onNodesChange,
    onEdgesChange,
    toggleNodeExpand,
    toggleAllExpand,
    allNodesExpanded,
    setNodes,
    setEdges,
  };
}
