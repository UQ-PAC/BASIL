// src/components/SideBar.tsx
import '../styles/sidebar.css';
import React from "react";

// import DownArrow from '../assets/arrow-down-icon.svg';

interface SidebarProps {
    epochNames: string[];
    selectedEpochs: Set<string>;
    onEpochSelect: (epochName: string, event: React.MouseEvent) => void;
    loading: boolean;
    error: string | null;
    // datasets: DatasetConfig[];
    selectedDataset: string | null;
    // onDatasetChange: (name: string) => void;
    onDirectorySelect: () => Promise<void>;
    datasetLoading: boolean;
    // datasetError: string | null;
}

export function Sidebar({ epochNames, selectedEpochs, onEpochSelect, loading, error, selectedDataset, onDirectorySelect, datasetLoading }: SidebarProps) {
    return (
        <aside className="sidebar">
            <h2 className="sidebar-header">Analysis Epochs</h2>

            <div className="dataset-select-container">
                <label className="dataset-label">Selected Config Directory</label>

                {/* Display loading/error messages */}
                {datasetLoading && <p className="sidebar-message">Processing directory...</p>} // TODO: Remove this

                <div className="relative">
                    {/* Button to trigger the directory selection */}
                    <button
                        className="dataset-select-button"
                        onClick={onDirectorySelect}
                        disabled={datasetLoading}
                    >
                        Select Directory
                    </button>

                    {/* Display the selected directory path (assuming selectedDataset now holds the path) */}
                    {selectedDataset && (
                        <p className="selected-path-display">
                            Path: {selectedDataset}
                        </p>
                    )}
                </div>
            </div>

            {loading && <p className="sidebar-message">Loading epochs...</p>}
            {error && <p className="sidebar-error">Error: {error}</p>} {/* Display error */}
            {!loading && !error && epochNames.length === 0 && (
                <p className="sidebar-message">No epochs available.</p>
            )}
            {!loading && !error && epochNames.length > 0 && (
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