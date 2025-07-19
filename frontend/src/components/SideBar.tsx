
import '../styles/sidebar.css';

interface SidebarProps {
    epochNames: string[];
    selectedEpochName: string | null;
    onEpochSelect: (epochName: string) => void;
    loading: boolean;
    error: string | null;
}

export function Sidebar({ epochNames, selectedEpochName, onEpochSelect, loading, error }: SidebarProps) {
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
                            className={`epoch-button ${name === selectedEpochName ? 'selected' : ''}`}
                            onClick={() => onEpochSelect(name)}
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