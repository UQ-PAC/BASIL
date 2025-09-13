// src/components/SideBar.tsx
import '../styles/sidebar.css';
import type {DatasetConfig} from '../utils/types';
import React from "react";

import DownArrow from '../assets/arrow-down-icon.svg';

const getDatasetName = (path: string): string => {
    return path.replace('.adt', '');
};

interface SidebarProps {
    epochNames: string[];
    selectedEpochs: Set<string>;
    onEpochSelect: (epochName: string, event: React.MouseEvent) => void;
    loading: boolean;
    error: string | null;
    datasets: DatasetConfig[];
    selectedDataset: string | null;
    onDatasetChange: React.Dispatch<React.SetStateAction<string>>;
    datasetLoading: boolean;
    datasetError: string | null;
}

export function Sidebar({ epochNames, selectedEpochs, onEpochSelect, loading, error, datasets, selectedDataset, onDatasetChange, datasetLoading, datasetError }: SidebarProps) {
    return (
        <aside className="sidebar">
            <h2 className="sidebar-header">Analysis Epochs</h2>
            <div className="dataset-select-container">
                <label className="dataset-label" >Selected Config File</label>
                {datasetLoading && <p className="sidebar-message">Loading datasets...</p>}
                {datasetError && <p className="sidebar-error">Error: {datasetError}</p>}
                {!datasetLoading && !datasetError && datasets.length > 0 && (
                    <div className="relative">
                        <select
                            className="dataset-select"
                            value={selectedDataset || ''}
                            onChange={(e) => onDatasetChange(e.target.value)}
                        >
                            {datasets.map((dataset) => (
                                <option key={getDatasetName(dataset.adt)} value={getDatasetName(dataset.adt)}>{getDatasetName(dataset.adt)}</option>
                            ))}
                        </select>
                        <DownArrow className="arrow-down-icon" />
                    </div>
                )}
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