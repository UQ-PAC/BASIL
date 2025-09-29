// src/hooks/useCfgData.ts
import { useState, useEffect } from 'react';
import {
  type Node,
  type Edge,
  useNodesState,
  useEdgesState,
} from '@xyflow/react';
import { getLayoutedElements } from '../utils/graphLayout.ts';
import { fetchDotString, fetchIrCode } from '../api/viewer.ts';
import { type CustomNodeData } from '../components/viewers/graph/CustomNode.tsx';
import { useGraphvizWASM } from './useGraphvizWASM';

interface DotGraphResponse {
  [procedureName: string]: string;
}

interface CombinedDataHookResult {
  irCode: string | null;
  loading: boolean;
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
  const [loading, setLoading] = useState(false);
  const [graphError, setGraphError] = useState<string | null>(null);
  const [graphRenderKey, setGraphRenderKey] = useState(0);
  const { isGraphvizWasmReady, graphvizWasmError } = useGraphvizWASM();

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
      setLoading(true);
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
        setLoading(false);
      }
    };

    fetchIrCodeData();
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    displayCfgType,
  ]);

  useEffect(() => {
    if (
      !isGraphvizWasmReady ||
      !selectedStartEpoch ||
      !selectedEndEpoch ||
      !selectedProcedureName
    ) {
      setBeforeNodes([]);
      setBeforeEdges([]);
      setAfterNodes([]);
      setAfterEdges([]);
      setGraphRenderKey((prev) => prev + 1);
      return;
    }

    const fetchCfgData = async () => {
      setLoading(true);
      setGraphError(null);
      try {
        const [beforeDotResponse, afterDotResponse] = await Promise.all([
          fetchDotString(selectedStartEpoch!, 'before'),
          fetchDotString(selectedEndEpoch!, 'after'),
        ]);

        const lowerSelectedProcedure = selectedProcedureName!.toLowerCase();

        const extractDot = (response: DotGraphResponse | undefined) => {
          if (!response) return undefined;
          const matchingProcedureKey = Object.keys(response).find((key) =>
            key.toLowerCase().includes(lowerSelectedProcedure)
          );
          return matchingProcedureKey
            ? response[matchingProcedureKey]
            : undefined;
        };

        const beforeDotString = extractDot(beforeDotResponse);
        const afterDotString = extractDot(afterDotResponse);

        if (beforeDotString) {
          const { nodes, edges } = await getLayoutedElements(
            beforeDotString,
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
          const { nodes, edges } = await getLayoutedElements(
            afterDotString,
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
          (graphvizWasmError ? graphvizWasmError + '\n' : '') +
            `Failed to load data: ${e.message}`
        );
        setBeforeNodes([]);
        setBeforeEdges([]);
        setAfterNodes([]);
        setAfterEdges([]);
      } finally {
        setLoading(false);
        setGraphRenderKey((prev) => prev + 1);
      }
    };

    fetchCfgData();
  }, [
    selectedStartEpoch,
    selectedEndEpoch,
    selectedProcedureName,
    isGraphvizWasmReady,
    graphvizWasmError,
    setBeforeNodes,
    setBeforeEdges,
    setAfterNodes,
    setAfterEdges,
  ]);

  const combinedGraphError = graphvizWasmError || graphError;

  return {
    irCode,
    loading,
    graphError: combinedGraphError,
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
