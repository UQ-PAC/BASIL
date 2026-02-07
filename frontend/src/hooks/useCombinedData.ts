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
import { fetchGraphJson, fetchIrCode } from '../api/viewer.ts';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode.tsx';

interface CombinedDataHookResult {
  irCode: string | null;
  isLoading: boolean;
  graphError: string | null;
  graphRenderKey: number;
  beforeNodes: Node<CustomNodeData>[];
  beforeEdges: Edge[];
  afterNodes: Node<CustomNodeData>[];
  afterEdges: Edge[];
  onBeforeNodesChange: (changes: any) => void;
  onBeforeEdgesChange: (changes: any) => void;
  onAfterNodesChange: (changes: any) => void;
  onAfterEdgesChange: (changes: any) => void;
}

export function useCombinedData(
  selectedStartEpoch: string | null,
  selectedEndEpoch: string | null,
  selectedProcedureName: string | null,
  displayCfgType: 'before' | 'after'
): CombinedDataHookResult {
  const [irCode, setIrCode] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [graphError, setGraphError] = useState<string | null>(null);
  const [graphRenderKey, setGraphRenderKey] = useState(0);

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

  useEffect(() => {
    if (!selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName) {
      setIrCode(null);
      return;
    }

    const fetchIrCodeData = async () => {
      setIsLoading(true);
      try {
        const epoch =
          displayCfgType === 'before' ? selectedStartEpoch : selectedEndEpoch;

        const code = await fetchIrCode(
          epoch!,
          selectedProcedureName!,
          displayCfgType
        );
        setIrCode(code);
      } catch (error) {
        console.error('Failed to fetch IR code:', error);
        setIrCode(null);
      } finally {
        setIsLoading(false);
      }
    };

    fetchIrCodeData().catch((e) => {
      console.error('Uncaught error during IR fetching:', e);
      setGraphError(`Uncaught error during IR fetching: ${e.message}`);
    });
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    displayCfgType,
  ]);

  useEffect(() => {
    if (!selectedStartEpoch || !selectedEndEpoch || !selectedProcedureName) {
      setBeforeNodes([]);
      setBeforeEdges([]);
      setAfterNodes([]);
      setAfterEdges([]);
      setGraphRenderKey((prev) => prev + 1);
      return;
    }

    const extractProcedureGraph = ( // TODO: Duplicated in useCfgData.ts
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

    const fetchCfgData = async () => {
      setIsLoading(true);
      setGraphError(null);
      try {
        const [beforeResponse, afterResponse] = await Promise.all([
          fetchGraphJson(selectedStartEpoch!, 'before'), // TODO: Are these correct
          fetchGraphJson(selectedEndEpoch!, 'after'),
        ]);

        const beforeDotString = beforeResponse;
        const afterDotString = afterResponse;

        if (beforeDotString) {
          const singleProcGraph = extractProcedureGraph(
            beforeResponse,
            selectedProcedureName!  // TODO: Is there a better way then !
          );
          const { nodes, edges } = await getLayoutedElementsFromJSON(
            singleProcGraph,
            'before-'
          );
          setBeforeNodes(nodes);
          setBeforeEdges(edges);
        } else {
          setBeforeNodes([]);
          setBeforeEdges([]);
          console.warn(
            `No 'before' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedStartEpoch}'.`
          );
        }

        if (afterDotString) {
          const singleProcGraph = extractProcedureGraph(
            afterResponse,
            selectedProcedureName!  // TODO: Is there a better way then !
          );
          const { nodes, edges } = await getLayoutedElementsFromJSON(
            singleProcGraph,
            'after-'
          );
          setAfterNodes(nodes);
          setAfterEdges(edges);
        } else {
          setAfterNodes([]);
          setAfterEdges([]);
          console.warn(
            `No 'after' CFG data (DOT) for procedure '${selectedProcedureName}' in epoch '${selectedEndEpoch}'.`
          );
        }
      } catch (e: any) {
        console.error('Error fetching data in useCombinedData:', e);
        setGraphError(
          `Failed to load or render CFG data in useCombinedData: ${e.message}`
        );
        setBeforeNodes([]);
        setBeforeEdges([]);
        setAfterNodes([]);
        setAfterEdges([]);
      } finally {
        setIsLoading(false);
        setGraphRenderKey((prev) => prev + 1);
      }
    };

    fetchCfgData().catch((e) => {
      console.error('Uncaught error during CFG fetching promise:', e);
      setGraphError(`Uncaught error during CFG fetching promise: ${e.message}`);
    });
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    setBeforeNodes,
    setBeforeEdges,
    setAfterNodes,
    setAfterEdges,
  ]);

  return {
    irCode,
    isLoading,
    graphError,
    graphRenderKey,
    beforeNodes,
    beforeEdges,
    afterNodes,
    afterEdges,
    onBeforeNodesChange,
    onBeforeEdgesChange,
    onAfterNodesChange,
    onAfterEdgesChange,
  };
}
