// src/components/SideBar.tsx
import '../styles/sidebar.css';
import React from "react";

interface SidebarProps {
    epochNames: string[];
    selectedEpochs: Set<string>;
    onEpochSelect: (epochName: string, event: React.MouseEvent) => void;
    loading: boolean;
    error: string | null;
}

export function Sidebar({ epochNames, selectedEpochs, onEpochSelect, loading, error }: SidebarProps) {
    return (
        <aside className="sidebar">
            <h2 className="sidebar-header">Analysis Epochs</h2>
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