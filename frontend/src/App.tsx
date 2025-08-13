// App.tsx
import {useEffect, useState, useCallback} from 'react'
import './App.css'
import { DiffViewer } from './components/DiffViewer';
import { Header } from './components/Header';
import { Sidebar } from './components/SideBar';
import { ResizableSidebar } from './components/ResizableSidebar';
import CfgViewer from './components/CfgViewer';
import CombinedViewer from './components/CombinedViewer';
import SettingsModal from './components/SettingsModal'
import { API_BASE_URL } from './api';

const LOCAL_STORAGE_PROCEDURE_KEY = 'cfgViewerSelectedProcedure';

function App() {
    const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>('IR');
    const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
    const [selectedEpochName, setSelectedEpochName] = useState<string | null>(null);
    const [loadingEpochs, setLoadingEpochs] = useState(true);
    const [epochError, setEpochError] = useState<string | null>(null);
    const [isSidebarMinimized, setIsSidebarMinimized] = useState(false);
    const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
    const [theme, setTheme] = useState<'light' | 'dark' | 'system'>(
        (localStorage.getItem('theme') as 'light' | 'dark' | 'system') || 'system'
    );

    const [procedureNames, setProcedureNames] = useState<string[]>([]);
    const [loadingProcedures, setLoadingProcedures] = useState(false);
    const [procedureError, setProcedureError] = useState<string | null>(null);
    const [selectedProcedureName, setSelectedProcedureName] = useState<string | null>(() => {
        try {
            const storedProcedure = localStorage.getItem(LOCAL_STORAGE_PROCEDURE_KEY);
            return storedProcedure ? storedProcedure : null;
        } catch (e) {
            console.error("Failed to read from localStorage:", e);
            return null;
        }
    });

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
    }, []);

    // Fetch procedures for the currently selected epoch
    useEffect(() => {
        const fetchProcedureNames = async () => {
            if (!selectedEpochName) {
                setProcedureNames([]);
                setSelectedProcedureName(null);
                setProcedureError(null);
                return;
            }

            setLoadingProcedures(true);
            setProcedureError(null);
            try {
                const response = await fetch(`${API_BASE_URL}/procedures/${selectedEpochName}`);
                if (!response.ok) {
                    const errorMessage = `HTTP error! status: ${response.status} fetching procedures for ${selectedEpochName}`;
                    console.error(errorMessage);
                    setProcedureError(errorMessage);
                    setProcedureNames([]);
                    setSelectedProcedureName(null);
                    setLoadingProcedures(false);
                    return;
                }
                const names: string[] = await response.json();
                setProcedureNames(names);

                // Check if the previously selected procedure is still valid
                if (selectedProcedureName === null || !names.includes(selectedProcedureName)) {
                    if (names.length > 0) {
                        setSelectedProcedureName(names[0]);
                    } else {
                        setSelectedProcedureName(null);
                    }
                } else {
                    console.log("Retaining previously selected procedure: " + selectedProcedureName);
                }
            } catch (e: any) {
                console.error("Error fetching procedure names:", e);
                setProcedureError(`Failed to load procedure names: ${e.message}`);
                setProcedureNames([]);
                setSelectedProcedureName(null);
            } finally {
                setLoadingProcedures(false);
            }
        };

        fetchProcedureNames().catch(error => console.error("Unhandled promise rejected from 'fetchProcedureNames': ", error));
    }, [selectedEpochName, selectedProcedureName]); // Added selectedProcedureName to dependencies to handle case where it's reset

    // Save the selected procedure to local storage whenever it changes
    useEffect(() => {
        if (selectedProcedureName) {
            localStorage.setItem(LOCAL_STORAGE_PROCEDURE_KEY, selectedProcedureName);
        } else {
            localStorage.removeItem(LOCAL_STORAGE_PROCEDURE_KEY);
        }
    }, [selectedProcedureName]);

    const handleEpochSelect = useCallback((epochName: string) => {
        if (selectedEpochName !== epochName) {
            setSelectedEpochName(epochName);
        }
    }, [selectedEpochName]);

    const toggleSidebar = () => {
        setIsSidebarMinimized(!isSidebarMinimized);
    };

    const toggleSettings = () => {
        setIsSettingsOpen(!isSettingsOpen);
    };

    useEffect(() => {
        if (theme === 'system') {
            document.documentElement.removeAttribute('data-theme');
        } else {
            document.documentElement.setAttribute('data-theme', theme);
        }

        localStorage.setItem('theme', theme);
    }, [theme]);

    return (
        <div className="app-container">

            <div className="app-layout">
                <Header setViewMode={setViewMode} viewMode={viewMode} toggleSettings={toggleSettings} />
                <main className="main-layout">
                    <ResizableSidebar
                        isSidebarMinimised={isSidebarMinimized}
                        toggleSidebar={toggleSidebar}
                    >
                        <Sidebar
                          epochNames={allEpochNames}
                          selectedEpochName={selectedEpochName}
                          onEpochSelect={handleEpochSelect}
                          loading={loadingEpochs}
                          error={epochError}
                        />
                    </ResizableSidebar>
                    {viewMode === 'IR' ? ( // TODO: Change to enums and a switch
                      <DiffViewer selectedEpochName={selectedEpochName} theme={theme} />
                  ) : viewMode === 'CFG' ? (
                        <CfgViewer
                            selectedEpochName={selectedEpochName}
                            selectedProcedureName={selectedProcedureName}
                            setSelectedProcedureName={setSelectedProcedureName}
                            procedureNames={procedureNames}
                            loadingProcedures={loadingProcedures}
                            procedureError={procedureError}
                        />
                    ) : ( // 'IR/CFG'
                      <CombinedViewer
                          selectedEpochName={selectedEpochName}
                          selectedProcedureName={selectedProcedureName}
                          setSelectedProcedureName={setSelectedProcedureName}
                          procedureNames={procedureNames}
                          loadingProcedures={loadingProcedures}
                          procedureError={procedureError}                      />
                  )}
                </main>
            </div>
            <SettingsModal
                isOpen={isSettingsOpen}
                onClose={toggleSettings}
                theme={theme}
                setTheme={setTheme}
            />
        </div>
    )
}

export default App
