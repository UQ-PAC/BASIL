import {useEffect, useState, useCallback} from 'react'
import './App.css'
import { DiffViewer } from './components/DiffViewer';
import { Header } from './components/Header';
import { Sidebar } from './components/SideBar';
import CfgViewer from './components/CfgViewer';
import { API_BASE_URL } from './api';

function App() {
    const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>('IR'); { /* TODO: Add the IR/CFG later */}
    const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
    const [selectedEpochName, setSelectedEpochName] = useState<string | null>(null);
    const [loadingEpochs, setLoadingEpochs] = useState(true);
    const [epochError, setEpochError] = useState<string | null>(null);
    const [selectedProcedureName, setSelectedProcedureName] = useState<string | null>(null); // New state

    useEffect(() => {
        const fetchEpochNames = async () => {
            try {
                setLoadingEpochs(true);
                setEpochError(null);
                const namesResponse = await fetch(`${API_BASE_URL}/epochs`);
                if (!namesResponse.ok) {
                    throw new Error(`HTTP error fetching epoch names! status: ${namesResponse.status}`);
                }
                const names: string[] = await namesResponse.json();
                setAllEpochNames(names);

                // Automatically select the first epoch if available
                if (names.length > 0) {
                    setSelectedEpochName(names[0]);
                } else {
                    setEpochError("No analysis epochs found.");
                }
            } catch (err: any) {
                console.error("Error fetching epoch names:", err);
                setEpochError(`Error fetching epochs: ${err.message}`);
                setAllEpochNames([]);
                setSelectedEpochName(null);
            } finally {
                setLoadingEpochs(false);
            }
        };

        fetchEpochNames();
    }, []); // Empty dependency array: runs only once on mount

    // --- Callback for when an epoch is selected from the sidebar ---
    const handleEpochSelect = useCallback((epochName: string) => {
        if (selectedEpochName !== epochName) { // Only update if it's a different epoch
            setSelectedEpochName(epochName);
        }
    }, [selectedEpochName]);

    useEffect(() => {
        const fetchProcedureNames = async () => {
            if (!selectedEpochName) {
                setSelectedProcedureName(null);
                return;
            }
            // TODO: I have my specific code here to do that. Ensure there is a button to select them here. Or maybe do this on CfgViewer
            // setSelectedProcedureName("SimpleCfg")
            setSelectedProcedureName("register_tm_clones_1664")
            // ... fetch procedure names for selectedEpochName ...
            // And then set selectedProcedureName, e.g., to the first one
        };
        fetchProcedureNames();
    }, [selectedEpochName]);

    return (
        <div className="app-layout">
            <Header setViewMode={setViewMode} viewMode={viewMode} />
            <main className="main-layout">
              <Sidebar
                  epochNames={allEpochNames}
                  selectedEpochName={selectedEpochName}
                  onEpochSelect={handleEpochSelect}
                  loading={loadingEpochs}
                  error={epochError}
              />
              {viewMode === 'IR' ? (
                  <DiffViewer selectedEpochName={selectedEpochName} />
              ) : (
                  <CfgViewer
                      selectedEpochName={selectedEpochName}
                      selectedProcedureName={selectedProcedureName || ''}
                  />
              )}
            </main>
        </div>
    )
}

export default App
