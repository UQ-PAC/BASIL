import { useEffect, useState } from 'react';
import { useHierarchicalIrDiff } from '../../../hooks/useHierarchicalIrDiff';
import { useProcedureDiffViewer } from '../../../hooks/useProcedureDiffViewer';
import { DiffControls } from './DiffControls';
import '../../../styles/components/viewers/diff-viewer.css';

export function DiffViewer({
  startEpoch,
  endEpoch,
  theme,
}: {
  startEpoch: string | null;
  endEpoch: string | null;
  theme: string | null;
}) {
  const { procedures, loading, error, fetchProcedureBody } =
    useHierarchicalIrDiff(startEpoch, endEpoch);
  const [selectedName, setSelectedName] = useState<string | null>(null);
  const selectedProcedure =
    procedures.find((p) => p.metadata.name === selectedName) ?? null;
  const [contextLines, setContextLines] = useState(5);
  const [outputFormat, setOutputFormat] = useState<
    'side-by-side' | 'line-by-line'
  >('side-by-side');

  useEffect(() => {
    if (!selectedName && procedures.length > 0) {
      const firstName = procedures[0].metadata.name;
      fetchProcedureBody(firstName).then(() => setSelectedName(firstName));
    }
  }, [procedures, selectedName, fetchProcedureBody]);

  const { diffContainerRef, headerElement } = useProcedureDiffViewer(
    selectedProcedure,
    theme,
    contextLines,
    outputFormat
  );

  const handleSelectProcedure = async (procName: string) => {
    const proc = procedures.find((p) => p.metadata.name === procName);
    if (!proc) return;

    await fetchProcedureBody(procName);
    setSelectedName(procName);
  };

  if (loading)
    return <div className="p-4 text-center">Loading procedures...</div>;
  if (error) return <div className="p-4 text-center text-red-500">{error}</div>;

  return (
    <div className="diff-viewer-root">
      <div className="procedure-bar">
        Procedures:{' '}
        {procedures.map((proc) => (
          <button
            key={proc.metadata.name}
            className={`proc-btn
          ${proc.status !== 'unchanged' ? 'changed' : 'unchanged'}
          ${selectedName === proc.metadata.name ? 'active' : ''}`}
            onClick={() => handleSelectProcedure(proc.metadata.name)}
          >
            {proc.metadata.name}
          </button>
        ))}
      </div>

      <div ref={diffContainerRef} id="diff-container" />
      <DiffControls
        headerElement={headerElement}
        outputFormat={outputFormat}
        setOutputFormat={setOutputFormat}
        contextLines={contextLines}
        setContextLines={setContextLines}
      />
    </div>
  );
}
