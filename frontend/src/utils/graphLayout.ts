// src/utils/graphLayout.ts

import ELK from 'elkjs/lib/elk.bundled.js';
import { type Node, type Edge, MarkerType } from '@xyflow/react';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode';

const elk = new ELK();

const elkLayoutOptions = {
  'elk.algorithm': 'layered',
  'elk.direction': 'DOWN',
  'elk.spacing.nodeNode': '75',
  'elk.spacing.nodeNodeBetweenLayers': '75',
  'elk.padding': '[top=50,left=50,bottom=50,right=50]',
  'elk.edgeRouting': 'ORTHOGONAL',
  // 'org.eclipse.elk.layered.unnecessaryBendpoints': 'true', // Potentially uncomment for very complex graphs
  // 'org.eclipse.elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
};

export interface GraphJSON {
  nodes: {
    id: string;
    label: string;
    shape?: string;
    nodeBackgroundColor?: string;
  }[];
  edges: {
    source: string;
    target: string;
    label?: string;
  }[];
}

export async function getLayoutedElementsFromJSON(
  graph: GraphJSON,
  prefix = ''
): Promise<{ nodes: Node<CustomNodeData>[]; edges: Edge[] }> {
  const ASSUMED_FONT_SIZE_PX = 14;
  const LINE_HEIGHT = 1.5;
  const H_PADDING = 12; // px
  const V_PADDING = 10; // px

  const rawNodes = graph.nodes.map((node) => {
    const id = `${prefix}${node.id}`;
    const lines = node.label.split('\n');
    const headerLine = lines[0] ?? '';

    const longestLine = Math.max(...lines.map((l) => l.length));

    const headerWidth = Math.max(
      120,
      headerLine.length * ASSUMED_FONT_SIZE_PX * 0.65 + H_PADDING
    );
    const headerHeight = Math.max(
      40,
      ASSUMED_FONT_SIZE_PX * LINE_HEIGHT + V_PADDING
    );

    const fullContentWidth = Math.max(
      120,
      longestLine * ASSUMED_FONT_SIZE_PX * 0.65 + H_PADDING
    );
    const fullContentHeight = Math.max(
      40,
      lines.length * ASSUMED_FONT_SIZE_PX * LINE_HEIGHT + V_PADDING
    );

    return {
      id,
      header: headerLine,
      fullLabelContent: node.label,
      headerWidth,
      headerHeight,
      fullContentWidth,
      fullContentHeight,
      nodeBackgroundColor: node.nodeBackgroundColor,
      shape: node.shape ?? 'default',
    };
  });

  const elkNodes = rawNodes.map((n) => ({
    id: n.id,
    width: n.headerWidth,
    height: n.headerHeight,
  }));

  const elkEdges = graph.edges.map((edge, index) => ({
    id: `e-${prefix}${edge.source}-${prefix}${edge.target}-${index}`,
    sources: [`${prefix}${edge.source}`],
    targets: [`${prefix}${edge.target}`],
  }));

  const elkGraph = {
    id: 'root',
    layoutOptions: elkLayoutOptions,
    children: elkNodes,
    edges: elkEdges,
  };

  const layoutedGraph = await elk.layout(elkGraph);

  if (!layoutedGraph.children) {
    return { nodes: [], edges: [] };
  }

  const reactFlowNodes: Node<CustomNodeData>[] = layoutedGraph.children.map(
    (elkNode) => {
      const raw = rawNodes.find((n) => n.id === elkNode.id)!;

      return {
        id: elkNode.id,
        type: 'customNode',
        position: {
          x: elkNode.x ?? 0,
          y: elkNode.y ?? 0,
        },
        data: {
          header: raw.header,
          fullContent: raw.fullLabelContent,
          headerWidth: raw.headerWidth,
          headerHeight: raw.headerHeight,
          fullContentWidth: raw.fullContentWidth,
          fullContentHeight: raw.fullContentHeight,
          isExpanded: false,
        },
        style: {
          width: raw.headerWidth,
          height: raw.headerHeight,
        },
      };
    }
  );

  const reactFlowEdges: Edge[] = elkEdges.map((elkEdge) => {
    return {
      id: elkEdge.id,
      source: elkEdge.sources[0],
      target: elkEdge.targets[0],
      label: undefined,
      markerEnd: {
        type: MarkerType.ArrowClosed,
      },
      style: {
        strokeWidth: 2,
        stroke: '#ab6392',
      },
    };
  });

  return { nodes: reactFlowNodes, edges: reactFlowEdges };
}
