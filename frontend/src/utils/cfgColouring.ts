// src/utils/cfgColouring.ts
import { type Node, type Edge, MarkerType } from '@xyflow/react';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode';

const NODE_colourS = {
  RED: '#FF4D4D',
  LIGHT_RED: '#FFCCCC',
  GREEN: '#04d104',
  LIGHT_GREEN: '#b9f4b9',
  DEFAULT: '#777',
};

const EDGE_colourS = {
  RED: '#c52222',
  GREEN: '#0f800f',
  DEFAULT: '#70e1ed',
};

export function compareAndColourElements(
  beforeGraphNodes: Node<CustomNodeData>[],
  beforeGraphEdges: Edge[],
  afterGraphNodes: Node<CustomNodeData>[],
  afterGraphEdges: Edge[]
): {
  colouredBeforeNodes: Node<CustomNodeData>[];
  colouredAfterNodes: Node<CustomNodeData>[];
  colouredBeforeEdges: Edge[];
  colouredAfterEdges: Edge[];
} {
  const colouredBeforeNodes: Node<CustomNodeData>[] = [];
  const colouredAfterNodes: Node<CustomNodeData>[] = [];
  const colouredBeforeEdges: Edge[] = [];
  const colouredAfterEdges: Edge[] = [];

  const beforeNodeMap = new Map<string, Node<CustomNodeData>>();
  beforeGraphNodes.forEach((node) => {
    const originalId = node.id.replace('before-', '');
    beforeNodeMap.set(originalId, node);
  });

  const afterNodeMap = new Map<string, Node<CustomNodeData>>();
  afterGraphNodes.forEach((node) => {
    const originalId = node.id.replace('after-', '');
    afterNodeMap.set(originalId, node);
  });

  const allOriginalNodeIds = new Set([
    ...beforeNodeMap.keys(),
    ...afterNodeMap.keys(),
  ]);

  allOriginalNodeIds.forEach((originalId) => {
    const beforeNode = beforeNodeMap.get(originalId);
    const afterNode = afterNodeMap.get(originalId);

    let beforeColour = NODE_colourS.DEFAULT;
    let afterColour = NODE_colourS.DEFAULT;

    if (beforeNode && afterNode) {
      const headerEquivalent = beforeNode.data.header === afterNode.data.header;
      const fullContentEquivalent =
        beforeNode.data.fullContent === afterNode.data.fullContent;

      if (!headerEquivalent || !fullContentEquivalent) {
        beforeColour = NODE_colourS.LIGHT_RED;
        afterColour = NODE_colourS.LIGHT_GREEN;
      } else {
        beforeColour = NODE_colourS.DEFAULT;
        afterColour = NODE_colourS.DEFAULT;
      }
    } else if (beforeNode) {
      // Node exists only in 'before' graph
      beforeColour = NODE_colourS.RED;
    } else if (afterNode) {
      // Node exists only in 'after' graph
      afterColour = NODE_colourS.GREEN;
    }

    if (beforeNode) {
      colouredBeforeNodes.push({
        ...beforeNode,
        data: { ...beforeNode.data, nodeBorderColour: beforeColour },
      });
    }
    if (afterNode) {
      colouredAfterNodes.push({
        ...afterNode,
        data: { ...afterNode.data, nodeBorderColour: afterColour },
      });
    }
  });

  const beforeEdgeMap = new Map<string, Edge>();
  beforeGraphEdges.forEach((edge) => {
    const originalSource = edge.source.replace('before-', '');
    const originalTarget = edge.target.replace('before-', '');
    beforeEdgeMap.set(`${originalSource}-${originalTarget}`, edge);
  });

  const afterEdgeMap = new Map<string, Edge>();
  afterGraphEdges.forEach((edge) => {
    const originalSource = edge.source.replace('after-', '');
    const originalTarget = edge.target.replace('after-', '');
    afterEdgeMap.set(`${originalSource}-${originalTarget}`, edge);
  });

  const allOriginalEdgeKeys = new Set([
    ...beforeEdgeMap.keys(),
    ...afterEdgeMap.keys(),
  ]);

  allOriginalEdgeKeys.forEach((edgeKey) => {
    const beforeEdge = beforeEdgeMap.get(edgeKey);
    const afterEdge = afterEdgeMap.get(edgeKey);

    let beforeEdgecolour = EDGE_colourS.DEFAULT;
    let afterEdgecolour = EDGE_colourS.DEFAULT;

    if (beforeEdge && afterEdge) {
      // Edge exists in both, it's a matched edge
      beforeEdgecolour = EDGE_colourS.DEFAULT;
      afterEdgecolour = EDGE_colourS.DEFAULT;
    } else if (beforeEdge) {
      // Edge exists only in 'before' graph (deleted)
      beforeEdgecolour = EDGE_colourS.RED;
    } else if (afterEdge) {
      // Edge exists only in 'after' graph (added)
      afterEdgecolour = EDGE_colourS.GREEN;
    }

    if (beforeEdge) {
      colouredBeforeEdges.push({
        ...beforeEdge,
        style: { ...beforeEdge.style, stroke: beforeEdgecolour },
        markerEnd: { type: MarkerType.ArrowClosed, color: beforeEdgecolour },
      });
    }
    if (afterEdge) {
      colouredAfterEdges.push({
        ...afterEdge,
        style: { ...afterEdge.style, stroke: afterEdgecolour },
        markerEnd: { type: MarkerType.ArrowClosed, color: afterEdgecolour },
      });
    }
  });

  return {
    colouredBeforeNodes,
    colouredAfterNodes,
    colouredBeforeEdges,
    colouredAfterEdges,
  };
}
