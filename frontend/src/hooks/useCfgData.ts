// src/hooks/useCfgData.ts
import { useState, useEffect } from 'react';
import { type Node, type Edge, useNodesState, useEdgesState } from '@xyflow/react';
import { getLayoutedElements } from '../utils/graphLayout.ts';
import { fetchDotString } from '../api/viewer.ts';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode.tsx';

interface CfgDataHookResult {
  beforeNodes: Node<CustomNodeData>[];
  beforeEdges: Edge[];
  afterNodes: Node<CustomNodeData>[];
  afterEdges: Edge[];
  loadingGraphs: boolean;
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
  selectedProcedureName: string | null,
  isGraphvizWasmReady: boolean,
  compareAndcolourElements: (
    beforeNodes: Node<CustomNodeData>[],
    beforeEdges: Edge[],
    afterNodes: Node<CustomNodeData>[],
    afterEdges: Edge[]
  ) => {
    colouredBeforeNodes: Node<CustomNodeData>[];
    colouredAfterNodes: Node<CustomNodeData>[];
    colouredBeforeEdges: Edge[];
    colouredAfterEdges: Edge[];
  }
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

  const [loadingGraphs, setLoadingGraphs] = useState(false);
  const [graphError, setGraphError] = useState<string | null>(null);
  const [graphRenderKey, setGraphRenderKey] = useState(0);

  useEffect(() => {
    if (
      !isGraphvizWasmReady ||
      !selectedStartEpoch ||
      !selectedEndEpoch ||
      !selectedProcedureName
    ) {
      setLoadingGraphs(false);
      setGraphError(null);
      setBeforeNodes([]);
      setBeforeEdges([]);
      setAfterNodes([]);
      setAfterEdges([]);
      setGraphRenderKey((prev) => prev + 1);
      return;
    }

    const fetchAndRenderCfgs = async () => {
      setLoadingGraphs(true);
      setGraphError(null);

      try {
        const [beforeResponse, afterResponse] = await Promise.all([
          fetchDotString(selectedStartEpoch!, 'before'),
          fetchDotString(selectedEndEpoch!, 'after'),
        ]);

        const lowerSelectedProcedure = selectedProcedureName!.toLowerCase();

        const extractDot = (response: Record<string, string>) => {
          const matchingKey = Object.keys(response).find((key) =>
            key.toLowerCase().includes(lowerSelectedProcedure)
          );
          return matchingKey ? response[matchingKey] : undefined;
        };

        const beforeDotString = extractDot(beforeResponse);
        const afterDotString = extractDot(afterResponse);

        let processedBeforeNodes: Node<CustomNodeData>[] = [];
        let processedBeforeEdges: Edge[] = [];
        let processedAfterNodes: Node<CustomNodeData>[] = [];
        let processedAfterEdges: Edge[] = [];

        if (beforeDotString) {
          ({ nodes: processedBeforeNodes, edges: processedBeforeEdges } =
            await getLayoutedElements(beforeDotString, 'before-'));
        }
        if (afterDotString) {
          ({ nodes: processedAfterNodes, edges: processedAfterEdges } =
            await getLayoutedElements(afterDotString, 'after-'));
        }

        const {
          colouredBeforeNodes,
          colouredAfterNodes,
          colouredBeforeEdges,
          colouredAfterEdges,
        } = compareAndcolourElements(
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
        setLoadingGraphs(false);
        setGraphRenderKey((prev) => prev + 1);
      }
    };

    fetchAndRenderCfgs();
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    isGraphvizWasmReady,
    compareAndcolourElements,
  ]);

  return {
    beforeNodes,
    beforeEdges,
    afterNodes,
    afterEdges,
    loadingGraphs,
    graphError,
    graphRenderKey,
    onBeforeNodesChange,
    onBeforeEdgesChange,
    onAfterNodesChange,
    onAfterEdgesChange,
  };
}
