// src/utils/graphUtils.ts
import { type GraphJSON } from './graphLayout';

export const extractProcedureGraph = (
  graphs: Record<string, GraphJSON> | undefined,
  procedureName: string
): GraphJSON => {
  if (!graphs) return { nodes: [], edges: [] };

  const lowerProcedure = procedureName.toLowerCase();
  const matchingKey = Object.keys(graphs).find((key) =>
    key.toLowerCase().includes(lowerProcedure)
  );

  if (!matchingKey) return { nodes: [], edges: [] };

  const procGraph = graphs[matchingKey];

  const nodes: GraphJSON['nodes'] = [];
  const edges: GraphJSON['edges'] = [];

  if (procGraph?.nodes) nodes.push(...procGraph.nodes);
  if (procGraph?.edges) edges.push(...procGraph.edges);

  return { nodes, edges };
};