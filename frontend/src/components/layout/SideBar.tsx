// src/components/SideBar.tsx
import '../../styles/components/layout/sidebar.css';
import React from 'react';

interface SidebarProps {
  epochNames: string[];
  selectedEpochs: Set<string>;
  onEpochSelect: (epochName: string, event: React.MouseEvent) => void;
  loading: boolean;
  selectedDataset: string | null;
  onDirectorySelect: () => void;
  datasetLoading: boolean;
}

export function Sidebar({
  epochNames,
  selectedEpochs,
  onEpochSelect,
  loading,
  selectedDataset,
  onDirectorySelect,
  datasetLoading,
}: SidebarProps) {
  const displayPath = selectedDataset
    ? selectedDataset
    : 'No directory selected';

  const fullPathTitle = selectedDataset
    ? `Full Path: ${selectedDataset}`
    : undefined;

  return (
    <aside className="sidebar">
      <h2 className="sidebar-header">Analysis Epochs</h2>

      <div className="dataset-select-container">
        <label className="dataset-label">Selected Config Directory</label>

        <div className="relative">
          {selectedDataset ? (
            <button
              className={`selected-path-button ${datasetLoading ? 'loading' : ''}`}
              onClick={onDirectorySelect}
              title={fullPathTitle} // full path shown upon hover
              disabled={datasetLoading}
            >
              <span className="path-prefix">Path:</span>
              <span className="path-value">{displayPath}</span>
            </button>
          ) : (
            // TODO: OR an error
            <button
              className="selected-path-button"
              onClick={onDirectorySelect}
              disabled={datasetLoading}
            >
              {datasetLoading ? 'Loading...' : 'Select Directory'}
            </button>
          )}
        </div>
      </div>

      {!loading && epochNames.length > 0 && (
        <nav className="epoch-list">
          {epochNames.map((name) => (
            <button
              key={name}
              className={`epoch-button ${selectedEpochs.has(name) ? 'selected' : ''}`}
              onClick={(event) => onEpochSelect(name, event)}
              disabled={loading}
            >
              {name}
            </button>
          ))}
        </nav>
      )}
    </aside>
  );
}
