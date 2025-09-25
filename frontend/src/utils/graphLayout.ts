// src/utils/graphLayout.ts
import ELK from 'elkjs/lib/elk.bundled.js';
import { type Node, type Edge, MarkerType } from '@xyflow/react';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode.tsx';

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

export async function applyLayout(
  nodes: Node<CustomNodeData>[],
  edges: Edge[]
): Promise<Node<CustomNodeData>[]> {
  const elkNodes = nodes.map((node) => {
    const width = node.data.isExpanded
      ? node.data.fullContentWidth
      : node.data.headerWidth;
    const height = node.data.isExpanded
      ? node.data.fullContentHeight
      : node.data.headerHeight;

    return {
      id: node.id,
      width,
      height,
    };
  });

  const elkEdges = edges.map((edge) => ({
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

  try {
    const layoutedGraph = await elk.layout(elkGraph);

    if (!layoutedGraph.children) {
      console.warn('ELK layout returned no children.');
      return nodes;
    }

    const updatedNodes = nodes.map((node) => {
      const elkNode = layoutedGraph.children?.find((n) => n.id === node.id);
      if (elkNode) {
        return {
          ...node,
          position: { x: elkNode.x || 0, y: elkNode.y || 0 },
          style: {
            width: elkNode.width,
            height: elkNode.height,
          },
        };
      }
      return node;
    });

    return updatedNodes;
  } catch (e) {
    console.error('Failed to re-apply ELK layout:', e);
    return nodes;
  }
}

export async function getLayoutedElements(
  dotString: string,
  prefix: string
): Promise<{ nodes: Node<CustomNodeData>[]; edges: Edge[] }> {
  console.log(
    `DEBUG: Input DOT String for ELK layout (prefix: ${prefix}):`,
    dotString
  );

  let cleanedDotString = dotString.replace(/graph\s*\[\];/g, '');

  const rawNodesMap = new Map<
    string,
    {
      id: string;
      fullLabelContent: string;
      headerWidth: number;
      headerHeight: number;
      fullContentWidth: number;
      fullContentHeight: number;
      nodeBackgroundColor?: string;
      shape: string;
    }
  >();
  const rawEdgesDataForELK: {
    id: string;
    source: string;
    target: string;
    label?: string;
  }[] = [];

  try {
    const nodeDefinitionRegex =
      /(?:\"([^"]+)\"|(\w+))\s*\[((?:[^\]"]|\"(?:\\.|[^"\\])*\")*)\]/g;

    const labelRegex = /label="((?:[^"\\]|\\.)*?)"/;
    const shapeRegex = /shape="(\w+)"/;
    let uniqueNodeCount = 0;

    const nodeMatches = cleanedDotString.matchAll(nodeDefinitionRegex);
    for (const match of nodeMatches) {
      const originalId = match[1] || match[2];
      const id = `${prefix}${originalId}`;

      if (rawNodesMap.has(id)) {
        continue;
      }
      const attributesString = match[3] || '';
      const labelMatch = attributesString.match(labelRegex);
      const originalLabelContent = labelMatch ? labelMatch[1] : '';
      const fullLabelContent = originalLabelContent
        .replace(/\\"/g, '"')
        .replace(/\\l/g, '\n')
        .trim();

      const shapeMatch = attributesString.match(shapeRegex);
      const shape = shapeMatch ? shapeMatch[1] : 'default';

      const headerLine = fullLabelContent.split('\n')[0] || '';

      const ASSUMED_FONT_SIZE_PX = 14;
      const ASSUMED_LINE_HEIGHT_MULTIPLIER = 1.5;
      const HORIZONTAL_PADDING_PX = 5; // px
      const VERTICAL_PADDING_PX = 5; // px

      const headerTextWidth = headerLine.length * (ASSUMED_FONT_SIZE_PX * 0.65);
      const headerWidth = Math.max(
        120,
        headerTextWidth + HORIZONTAL_PADDING_PX
      );
      const headerHeight = Math.max(
        40,
        ASSUMED_FONT_SIZE_PX * ASSUMED_LINE_HEIGHT_MULTIPLIER +
          VERTICAL_PADDING_PX
      );

      const fullContentLines = fullLabelContent.split('\n');
      const longestFullContentLineLength = Math.max(
        ...fullContentLines.map((line) => line.length)
      );

      const fullContentTextWidth =
        longestFullContentLineLength * (ASSUMED_FONT_SIZE_PX * 0.65);
      const fullContentWidth = Math.max(
        120,
        fullContentTextWidth + HORIZONTAL_PADDING_PX
      );
      const fullContentHeight = Math.max(
        40,
        fullContentLines.length *
          ASSUMED_FONT_SIZE_PX *
          ASSUMED_LINE_HEIGHT_MULTIPLIER +
          VERTICAL_PADDING_PX
      );

      // console.log(`\nDEBUG: match: ${match}, \n\n attributesString: ${attributesString}, \n\nfullLabelContent: ${fullLabelContent}`)
      rawNodesMap.set(id, {
        id,
        fullLabelContent,
        headerWidth,
        headerHeight,
        fullContentWidth,
        fullContentHeight,
        shape,
      });
      uniqueNodeCount++;
    }
    const rawNodesData = Array.from(rawNodesMap.values());
    console.log(
      `DEBUG: Parsed ${uniqueNodeCount} unique raw nodes for ELK. Total unique nodes: ${rawNodesData.length}`
    );
    console.log(
      `DEBUG: Final rawNodesData after parsing (unique IDs):`,
      rawNodesData.map((n) => n.id)
    );

    // --- Edge Parsing ---
    const edgeRegex =
      /(?:\"([^"]+)\"|(\w+))\s*->\s*(?:\"([^"]+)\"|(\w+))(?:\[label="((?:[^"\\]|\\.)*?)"(?:, ([^\]]*))?\])?/g;
    const edgeMatches = dotString.matchAll(edgeRegex);

    let edgeCount = 0;
    for (const match of edgeMatches) {
      const originalSource = match[1] || match[2];
      const originalTarget = match[3] || match[4];
      const edgeLabel = match[5] ? match[5].replace(/\\"/g, '"') : undefined;

      const source = `${prefix}${originalSource}`;
      const target = `${prefix}${originalTarget}`;
      const id = `e-${source}-${target}-${edgeCount}`;

      rawEdgesDataForELK.push({ id, source, target, label: edgeLabel });
      edgeCount++;
    }
    console.log(`DEBUG: Parsed ${edgeCount} raw edges for ELK.`);

    const elkNodes = rawNodesData.map((node) => ({
      id: node.id,
      width: node.headerWidth,
      height: node.headerHeight,
    }));

    const elkEdges = rawEdgesDataForELK.map((edge) => ({
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

    console.log('DEBUG: Attempting ELK layout...');
    const layoutedGraph = await elk.layout(elkGraph);
    console.log('DEBUG: ELK layout completed successfully.');

    const reactFlowNodes: Node<CustomNodeData>[] = layoutedGraph.children
      ? layoutedGraph.children
          .filter((elkNode) => rawNodesData.some((n) => n.id === elkNode.id))
          .map((elkNode) => {
            const originalNodeData = rawNodesMap.get(elkNode.id)!;
            // TODO: Do I need this, if so, make it earlier so it doesn't affect padding
            const headerLine =
              originalNodeData.fullLabelContent
                .split('\n')[0]
                .replace(/ -1$/, '') || '';

            return {
              id: elkNode.id,
              position: { x: elkNode.x || 0, y: elkNode.y || 0 },
              data: {
                header: headerLine,
                fullContent: originalNodeData.fullLabelContent,
                headerWidth: originalNodeData.headerWidth,
                headerHeight: originalNodeData.headerHeight,
                fullContentWidth: originalNodeData.fullContentWidth,
                fullContentHeight: originalNodeData.fullContentHeight,
                isExpanded: false,
              },
              style: {
                width: originalNodeData.headerWidth,
                height: originalNodeData.headerHeight,
              },
              type: 'customNode',
            };
          })
      : [];

    const reactFlowEdges: Edge[] = layoutedGraph.edges
      ? layoutedGraph.edges.map((elkEdge) => {
          const originalEdgeData = rawEdgesDataForELK.find(
            (e) =>
              e.source === elkEdge.sources[0] && e.target === elkEdge.targets[0]
          );
          return {
            id: elkEdge.id,
            source: elkEdge.sources[0],
            target: elkEdge.targets[0],
            label: originalEdgeData?.label,
            markerEnd: {
              type: MarkerType.ArrowClosed,
            },
            style: {
              strokeWidth: 2,
              stroke: '#ab6392',
            },
          };
        })
      : [];

    return { nodes: reactFlowNodes, edges: reactFlowEdges };
  } catch (e: any) {
    console.error('DEBUG: Error during DOT parsing or ELK layout process:', e);
    throw new Error(`Failed to parse and layout graph: ${e.message}`);
  }
}
