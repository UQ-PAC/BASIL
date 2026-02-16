import ReactDOM from 'react-dom';

type Props = {
  headerElement: HTMLElement | null;
  outputFormat: 'side-by-side' | 'line-by-line';
  setOutputFormat: (val: 'side-by-side' | 'line-by-line') => void;
  contextLines: number;
  setContextLines: (val: number) => void;
};

export function DiffControls({
  headerElement,
  outputFormat,
  setOutputFormat,
  contextLines,
  setContextLines,
}: Props) {
  const controls = (
    <div className="custom-controls">
      {/* Output Format Toggle */}
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

      {/* Context Lines */}
      <div className="context-wrapper">
        <label htmlFor="context-dropdown" className="context-label">
          Context Lines:
        </label>
        <select
          id="context-dropdown"
          value={
            contextLines === Number.MAX_SAFE_INTEGER ? 'all' : contextLines
          }
          onChange={(e) => {
            const val = e.target.value;
            setContextLines(
              val === 'all' ? Number.MAX_SAFE_INTEGER : Number(val)
            );
          }}
          className="context-dropdown"
        >
          {Array.from({ length: 11 }, (_, i) => (
            <option key={i} value={i}>
              {i}
            </option>
          ))}
          <option value="all">All</option>
        </select>
      </div>
    </div>
  );

  return headerElement
    ? ReactDOM.createPortal(controls, headerElement)
    : controls;
}
