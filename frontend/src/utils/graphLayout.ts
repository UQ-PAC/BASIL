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

function calculateNodeSizes(label: string) {
  const ASSUMED_FONT_SIZE_PX = 14;
  const LINE_HEIGHT = 1.5;
  const H_PADDING = 12;
  const V_PADDING = 10;

  const lines = label.split('\n');
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

  return { headerWidth, headerHeight, fullContentWidth, fullContentHeight };
}

function mapNodesForELK(rawNodes: any[], expanded: boolean) {
  return rawNodes.map((n) => ({
    id: n.id,
    width: expanded ? n.fullContentWidth : n.headerWidth,
    height: expanded ? n.fullContentHeight : n.headerHeight,
  }));
}

function mapELKToReactFlow(
  rawNodes: any[],
  elkNodes: any[],
  expanded: boolean
): Node<CustomNodeData>[] {
  return elkNodes.map((elkNode) => {
    const raw = rawNodes.find((n) => n.id === elkNode.id)!;
    const label = raw.label;
    const header = label.split('\n')[0] ?? '';

    return {
      id: elkNode.id,
      type: 'customNode',
      position: { x: elkNode.x ?? 0, y: elkNode.y ?? 0 },
      data: {
        header,
        fullContent: label,
        headerWidth: raw.headerWidth,
        headerHeight: raw.headerHeight,
        fullContentWidth: raw.fullContentWidth,
        fullContentHeight: raw.fullContentHeight,
        isExpanded: expanded,
        nodeBackgroundColor: raw.nodeBackgroundColor,
      },
      style: {
        width: expanded ? raw.fullContentWidth : raw.headerWidth,
        height: expanded ? raw.fullContentHeight : raw.headerHeight,
      },
    };
  });
}

function mapEdges<T extends { source: string; target: string; label?: any }>(
  edges: T[],
  prefix = ''
): { elkEdges: any[]; reactFlowEdges: Edge[] } {
  const elkEdges = edges.map((edge, index) => ({
    id: `e-${prefix}${edge.source}-${prefix}${edge.target}-${index}`,
    sources: [`${prefix}${edge.source}`],
    targets: [`${prefix}${edge.target}`],
  }));

  const reactFlowEdges: Edge[] = elkEdges.map((elkEdge) => ({
    id: elkEdge.id,
    source: elkEdge.sources[0],
    target: elkEdge.targets[0],
    label: undefined,
    markerEnd: { type: MarkerType.ArrowClosed },
    style: { strokeWidth: 2, stroke: '#ab6392' },
  }));

  return { elkEdges, reactFlowEdges };
}

export async function applyLayout(
  nodes: Node<CustomNodeData>[],
  edges: Edge[],
  expanded = false,
  prefix = ''
): Promise<{ nodes: Node<CustomNodeData>[]; edges: Edge[] }> {
  const rawNodes = nodes.map((n) => {
    const id = `${prefix}${n.id}`;
    const label = expanded ? n.data.fullContent : n.data.header;
    const sizes = calculateNodeSizes(label);
    return {
      id,
      label,
      ...sizes,
      nodeBackgroundColor: n.data.nodeBackgroundColor,
      shape: n.type ?? 'default',
    };
  });

  const { elkEdges, reactFlowEdges } = mapEdges(edges, prefix);
  const elkNodes = mapNodesForELK(rawNodes, expanded);

  const elkGraph = {
    id: 'root',
    layoutOptions: elkLayoutOptions,
    children: elkNodes,
    edges: elkEdges,
  };
  const layoutedGraph = await elk.layout(elkGraph);
  if (!layoutedGraph.children) return { nodes: [], edges: [] };

  const layoutedNodes = mapELKToReactFlow(
    rawNodes,
    layoutedGraph.children,
    expanded
  );
  return { nodes: layoutedNodes, edges: reactFlowEdges };
}

export async function getLayoutedElementsFromJSON(
  graph: GraphJSON,
  prefix = ''
): Promise<{ nodes: Node<CustomNodeData>[]; edges: Edge[] }> {
  const rawNodes = graph.nodes.map((node) => {
    const id = `${prefix}${node.id}`;
    const label = node.label;
    const sizes = calculateNodeSizes(label);
    return {
      id,
      label,
      ...sizes,
      nodeBackgroundColor: node.nodeBackgroundColor,
      shape: node.shape ?? 'default',
    };
  });

  const { elkEdges, reactFlowEdges } = mapEdges(graph.edges, prefix);
  const elkNodes = rawNodes.map((n) => ({
    id: n.id,
    width: n.headerWidth,
    height: n.headerHeight,
  }));

  const elkGraph = {
    id: 'root',
    layoutOptions: elkLayoutOptions,
    children: elkNodes,
    edges: elkEdges,
  };
  const layoutedGraph = await elk.layout(elkGraph);
  if (!layoutedGraph.children) return { nodes: [], edges: [] };

  const layoutedNodes = mapELKToReactFlow(
    rawNodes,
    layoutedGraph.children,
    false
  );
  return { nodes: layoutedNodes, edges: reactFlowEdges };
}
