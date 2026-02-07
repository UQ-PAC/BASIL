// src/hooks/useCfgData.ts
import { useState, useEffect } from 'react';
import {
  type Node,
  type Edge,
  useNodesState,
  useEdgesState,
} from '@xyflow/react';
import {
  getLayoutedElementsFromJSON,
  type GraphJSON,
} from '../utils/graphLayout.ts';
import { fetchGraphJson } from '../api/viewer.ts';
import { compareAndColourElements } from '../utils/cfgColouring.ts';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode.tsx';

interface CfgDataHookResult {
  beforeNodes: Node<CustomNodeData>[];
  beforeEdges: Edge[];
  afterNodes: Node<CustomNodeData>[];
  afterEdges: Edge[];
  isLoadingGraphs: boolean;
  graphError: string | null;
  graphRenderKey: number;
  onBeforeNodesChange: (changes: any) => void;
  onBeforeEdgesChange: (changes: any) => void;
  onAfterNodesChange: (changes: any) => void;
  onAfterEdgesChange: (changes: any) => void;
}

export function useCfgData(
  selectedStartEpoch: string | null,
  selectedEndEpoch: string | null,
  selectedProcedureName: string | null
): CfgDataHookResult {
  const [beforeNodes, setBeforeNodes, onBeforeNodesChange] = useNodesState<
    Node<CustomNodeData>
  >([]);
  const [beforeEdges, setBeforeEdges, onBeforeEdgesChange] =
    useEdgesState<Edge>([]);
  const [afterNodes, setAfterNodes, onAfterNodesChange] = useNodesState<
    Node<CustomNodeData>
  >([]);
  const [afterEdges, setAfterEdges, onAfterEdgesChange] = useEdgesState<Edge>(
    []
  );

  const [isLoadingGraphs, setIsLoadingGraphs] = useState(false);
  const [graphError, setGraphError] = useState<string | null>(null);
  const [graphRenderKey, setGraphRenderKey] = useState(0);

  useEffect(() => {
    if (!selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName) {
      setIsLoadingGraphs(false);
      setGraphError(null);
      setBeforeNodes([]);
      setBeforeEdges([]);
      setAfterNodes([]);
      setAfterEdges([]);
      setGraphRenderKey((prev) => prev + 1);
      return;
    }

    const extractProcedureGraph = (
      // TODO: Duplicated in useCfgData.ts
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

      // Flatten in case of nested graphs (optional)
      const nodes: GraphJSON['nodes'] = [];
      const edges: GraphJSON['edges'] = [];

      if (procGraph?.nodes) nodes.push(...procGraph.nodes);
      if (procGraph?.edges) edges.push(...procGraph.edges);

      return { nodes, edges };
    };

    const fetchAndRenderCfgs = async () => {
      setIsLoadingGraphs(true);
      setGraphError(null);

      try {
        const [beforeResponse, afterResponse] = await Promise.all([
          fetchGraphJson(selectedStartEpoch!, 'before'),
          fetchGraphJson(selectedEndEpoch!, 'after'),
        ]);

        const beforeDotString = beforeResponse; // TODO: A lot of renaming to be done now
        const afterDotString = afterResponse;

        let processedBeforeNodes: Node<CustomNodeData>[] = [];
        let processedBeforeEdges: Edge[] = [];
        let processedAfterNodes: Node<CustomNodeData>[] = [];
        let processedAfterEdges: Edge[] = [];

        if (beforeDotString) {
          const singleProcGraph = extractProcedureGraph(
            beforeResponse,
            selectedProcedureName! // TODO: Is there a better way then !
          );
          ({ nodes: processedBeforeNodes, edges: processedBeforeEdges } =
            await getLayoutedElementsFromJSON(singleProcGraph, 'before-'));
        }
        if (afterDotString) {
          const singleProcGraph = extractProcedureGraph(
            afterResponse,
            selectedProcedureName! // TODO: Is there a better way then !
          );
          ({ nodes: processedAfterNodes, edges: processedAfterEdges } =
            await getLayoutedElementsFromJSON(singleProcGraph, 'after-'));
        }

        const {
          colouredBeforeNodes,
          colouredAfterNodes,
          colouredBeforeEdges,
          colouredAfterEdges,
        } = compareAndColourElements(
          processedBeforeNodes,
          processedBeforeEdges,
          processedAfterNodes,
          processedAfterEdges
        );

        setBeforeNodes(colouredBeforeNodes);
        setBeforeEdges(colouredBeforeEdges);
        setAfterNodes(colouredAfterNodes);
        setAfterEdges(colouredAfterEdges);
      } catch (e: any) {
        console.error('Error fetching or processing CFG data:', e);
        setGraphError(`Failed to load or render CFG data: ${e.message}`);
        setBeforeNodes([]);
        setBeforeEdges([]);
        setAfterNodes([]);
        setAfterEdges([]);
      } finally {
        setIsLoadingGraphs(false);
        setGraphRenderKey((prev) => prev + 1);
      }
    };

    fetchAndRenderCfgs().catch((e) => {
      console.error('Uncaught error during CFG fetching promise:', e);
      setGraphError(`Uncaught error during CFG fetching promise: ${e.message}`);
    });
  }, [selectedStartEpoch, selectedEndEpoch, selectedProcedureName]);

  return {
    beforeNodes,
    beforeEdges,
    afterNodes,
    afterEdges,
    isLoadingGraphs,
    graphError,
    graphRenderKey,
    onBeforeNodesChange,
    onBeforeEdgesChange,
    onAfterNodesChange,
    onAfterEdgesChange,
  };
}
