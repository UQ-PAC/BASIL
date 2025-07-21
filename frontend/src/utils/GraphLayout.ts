// src/utils/graphLayout.ts
import ELK from 'elkjs/lib/elk.bundled.js';
import { type Node, type Edge, MarkerType } from '@xyflow/react';
import { type CustomNodeData } from '../components/CustomNode';

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

/**
 * Parses a DOT string, applies ELK layout, and returns React Flow nodes and edges.
 * @param dotString The DOT graph representation.
 * @param prefix A unique prefix to apply to all node and edge IDs.
 */
export async function getLayoutedElements(dotString: string, prefix: string): Promise<{ nodes: Node<CustomNodeData>[]; edges: Edge[] }> {
    console.log(`DEBUG: Input DOT String for ELK layout (prefix: ${prefix}):`, dotString);

    const rawNodesMap = new Map<string, { id: string, fullLabelContent: string, width: number, height: number, shape: string }>();
    const rawEdgesDataForELK: { id: string, source: string, target: string, label?: string }[] = [];

    try {
        const nodeRegex = /(?:\"([^"]+)\"|(\w+))\s*\[label="((?:[^"\\]|\\.)*?)"(?:, ([^\]]*))?\]/g;
        const nodeMatches = dotString.matchAll(nodeRegex);
        let uniqueNodeCount = 0;

        for (const match of nodeMatches) {
            const originalId = match[1] || match[2];
            const id = `${prefix}${originalId}`;

            if (rawNodesMap.has(id)) {
                continue;
            }

            const fullLabelContent = match[3].replace(/\\"/g, '"').replace(/\\l/g, '\n');
            const attributes = match[4] || '';
            const shape = attributes.includes('shape="box"') ? 'box' : 'default';

            const headerLine = fullLabelContent.split('\n')[0] || '';

            const ASSUMED_FONT_SIZE_PX = 14;
            const ASSUMED_LINE_HEIGHT_MULTIPLIER = 1.5;
            const HORIZONTAL_PADDING_PX = 10; // px
            const VERTICAL_PADDING_PX = 5; // px

            // const lines = fullLabelContent.split('\n'); TODO: Use later on when attaching data
            // const longestLineLength = Math.max(...lines.map(line => line.length)); TODO: Use later on when attaching data

            const estimatedTextWidth = headerLine.length * (ASSUMED_FONT_SIZE_PX * 0.65);
            const estimatedWidth = Math.max(
                120, // Minimum width for a node
                estimatedTextWidth + HORIZONTAL_PADDING_PX
            );

            const effectiveLineHeight = ASSUMED_FONT_SIZE_PX * ASSUMED_LINE_HEIGHT_MULTIPLIER;
            // const estimatedTextHeight = lines.length * effectiveLineHeight; TODO: Use later on when attaching data
            const estimatedHeight = Math.max(
                40, // Minimum height for a node
                effectiveLineHeight + VERTICAL_PADDING_PX
            );

            rawNodesMap.set(id, { id, fullLabelContent, width: estimatedWidth, height: estimatedHeight, shape });
            uniqueNodeCount++;
        }
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

            const source = `${prefix}${originalSource}`;
            const target = `${prefix}${originalTarget}`;
            const id = `e-${source}-${target}-${edgeCount}`;

            rawEdgesDataForELK.push({ id, source, target, label: edgeLabel });
            edgeCount++;
        }
        console.log(`DEBUG: Parsed ${edgeCount} raw edges for ELK.`);


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

        console.log('DEBUG: Attempting ELK layout...');
        const layoutedGraph = await elk.layout(elkGraph);
        console.log('DEBUG: ELK layout completed successfully.');

        const reactFlowNodes: Node<CustomNodeData>[] = layoutedGraph.children
            ? layoutedGraph.children
                .filter(elkNode => rawNodesData.some(n => n.id === elkNode.id))
                .map(elkNode => {
                    const originalNodeData = rawNodesMap.get(elkNode.id)!;
                    const headerLine = originalNodeData.fullLabelContent.split('\n')[0] || '';

                    return {
                        id: elkNode.id,
                        position: { x: elkNode.x || 0, y: elkNode.y || 0 },
                        // data: { label: originalNodeData.label },
                        data: {
                            header: headerLine,
                            fullContent: originalNodeData.fullLabelContent,
                        },
                        style: { width: originalNodeData.width, height: originalNodeData.height },
                        // type: originalNodeData.shape === 'box' ? 'default' : 'default',
                        type: 'customNode',
                    };
                })
            : [];

        const reactFlowEdges: Edge[] = layoutedGraph.edges ? layoutedGraph.edges.map(elkEdge => {

            const originalEdgeData = rawEdgesDataForELK.find(e => e.source === elkEdge.sources[0] && e.target === elkEdge.targets[0]);
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
        }) : [];

        return { nodes: reactFlowNodes, edges: reactFlowEdges };

    } catch (e: any) {
        console.error("DEBUG: Error during DOT parsing or ELK layout process:", e);
        throw new Error(`Failed to parse and layout graph: ${e.message}`);
    }
}