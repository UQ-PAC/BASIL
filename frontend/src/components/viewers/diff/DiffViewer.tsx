// src/components/viewers/diff/DiffViewer.tsx

import 'diff2html/bundles/css/diff2html.min.css';
import '../../../styles/components/viewers/diff-viewer.css';
import '../../../styles/components/viewers/code-theme.css';
import '../../../styles/components/button-selection.css';
import { DiffControls } from './DiffControls';
import { useDiffViewer } from '../../../hooks/useDiffViewerLogic';

interface DiffViewerProps {
  selectedStartEpoch: string | null;
  selectedEndEpoch: string | null;
  theme: string | null;
}

export function DiffViewer({
  selectedStartEpoch,
  selectedEndEpoch,
  theme,
}: DiffViewerProps) {
  const {
    diffContainerRef,
    headerElement,
    outputFormat,
    setOutputFormat,
    contextLines,
    setContextLines,
    irData,
    isLoading,
    error,
    scrollToLine,
  } = useDiffViewer(selectedStartEpoch, selectedEndEpoch, theme);

  if (isLoading) return <div className="p-4 text-center">Loading diff...</div>;
  if (error)
    return <div className="p-4 text-center text-red-500">Error: {error}</div>;
  if (!selectedStartEpoch || !selectedEndEpoch)
    return (
      <div className="flex-1 p-4 text-center">
        Please select an epoch from the sidebar.
      </div>
    );

  return (
    <>
      <div id="diff" ref={diffContainerRef} />
      <DiffControls
        headerElement={headerElement}
        outputFormat={outputFormat}
        setOutputFormat={setOutputFormat}
        contextLines={contextLines}
        setContextLines={setContextLines}
        procedureList={irData?.procedures || []}
        onSelectProcedure={scrollToLine}
      />
    </>
  );
}
