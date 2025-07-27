import ReactDOM from 'react-dom';
import * as React from "react";

interface ProcedureLocation {
    name: string;
    startLine: number;
    approxEndLine: number; // You might not need this one in controls, but keeping for consistency
}

type Props = {
    headerElement: HTMLElement | null;
    outputFormat: 'side-by-side' | 'line-by-line';
    setOutputFormat: (val: 'side-by-side' | 'line-by-line') => void;
    contextLines: number;
    setContextLines: (val: number) => void;
    procedureList: ProcedureLocation[];
    onSelectProcedure: (lineNumber: number) => void
};

export function DiffControls({
                                 headerElement,
                                 outputFormat,
                                 setOutputFormat,
                                 contextLines,
                                 setContextLines,
                                 procedureList,
                                 onSelectProcedure
                             }: Props) {
    if (!headerElement) return null;


    // Handler for the new procedure dropdown
    const handleProcedureChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
        const selectedLine = parseInt(event.target.value, 10);
        if (!isNaN(selectedLine) && selectedLine > 0) {
            onSelectProcedure(selectedLine);
        }
    };

    return ReactDOM.createPortal(
        <div className="custom-controls">
            <div className="toggle-group">
                <button
                    className={`toggle-button ${outputFormat === 'side-by-side' ? 'active' : ''}`}
                    onClick={() => setOutputFormat('side-by-side')}
                >
                    {outputFormat === 'side-by-side' && <span className="tick">✓</span>}
                    Side-by-Side
                </button>
                <button
                    className={`toggle-button ${outputFormat === 'line-by-line' ? 'active' : ''}`}
                    onClick={() => setOutputFormat('line-by-line')}
                >
                    {outputFormat === 'line-by-line' && <span className="tick">✓</span>}
                    Line-by-Line
                </button>
            </div>

            <div className="display-type-container">
                <div className="procedure-select-wrapper">
                    <label htmlFor="procedure-select" className="procedure-label">
                        Go to Procedure:
                    </label>
                    <select
                        id="procedure-select"
                        className="procedure-dropdown"
                        onChange={handleProcedureChange}
                        defaultValue="" // Default to an empty option
                    >
                        <option value="" disabled>Select a procedure</option>
                        {procedureList.map((proc) => (
                            <option key={proc.name} value={proc.startLine}>
                                {proc.name} (Line {proc.startLine})
                            </option>
                        ))}
                    </select>
                </div>
            </div>

            <div className="context-wrapper">
                <label htmlFor="context-dropdown" className="context-label">
                    Context Lines:
                </label>
                <select
                    id="context-dropdown"
                    value={contextLines === Number.MAX_SAFE_INTEGER ? 'all' : contextLines}
                    onChange={(e) => {
                        const val = e.target.value;
                        setContextLines(val === 'all' ? Number.MAX_SAFE_INTEGER : Number(val));
                    }}
                    className="context-dropdown"
                >
                    {Array.from({ length: 11 }, (_, i) => (
                        <option key={i} value={i}>{i}</option>
                    ))}
                    <option value="all">All</option>
                </select>
            </div>
        </div>,
        headerElement
    );
}