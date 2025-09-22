// App.tsx
import React, {useEffect, useState, useCallback} from 'react'
import './App.css'
import { DiffViewer } from './components/DiffViewer';
import { Header } from './components/Header';
import { Sidebar } from './components/SideBar';
import { ResizableSidebar } from './components/ResizableSidebar';
import CfgViewer from './components/CfgViewer';
import CombinedViewer from './components/CombinedViewer';
import SettingsModal from './components/SettingsModal'
import { API_BASE_URL } from './api';
import LoadingModal from "./components/LoadingModal.tsx";
import ErrorModal from "./components/ErrorModal.tsx";

const LOCAL_STORAGE_PROCEDURE_KEY = 'cfgViewerSelectedProcedure';
const LOCAL_STORAGE_THEME_KEY = 'theme';
const LOCAL_STORAGE_DATASET_KEY = 'selectedDataset';
const LOCAL_STORAGE_VIEW_MODE_KEY = 'selectedViewMode';

const ViewMode = {
    IR: 'IR',
    CFG: 'CFG',
    IR_CFG: 'IR/CFG'
}

function App() {
    const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>(() => {
        return (localStorage.getItem(LOCAL_STORAGE_VIEW_MODE_KEY) as 'IR' | 'CFG' | 'IR/CFG') || 'IR';
    });
    const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
    const [selectedEpochs, setSelectedEpochs] = useState<Set<string>>(new Set());
    const [lastClickedEpoch, setLastClickedEpoch] = useState<string | null>(null);
    const [loadingEpochs, setLoadingEpochs] = useState(true);
    const [epochError, setEpochError] = useState<string | null>(null);
    const [isSidebarMinimized, setIsSidebarMinimized] = useState(false);
    const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
    const [theme, setTheme] = useState<'light' | 'dark' | 'system'>(() => {
        const savedTheme = localStorage.getItem(LOCAL_STORAGE_THEME_KEY);
        return (savedTheme as  'light' | 'dark' | 'system') || 'system';
    });

    const [selectedDataset, setSelectedDataset] = useState<string>(() => {
        const savedDataset = localStorage.getItem(LOCAL_STORAGE_DATASET_KEY);
        return savedDataset || 'dataBaseNameExample'; // TODO: Change to default data config file
    });
    const [postStatus, setPostStatus] = useState({ message: '', type: '' });
    const [datasetLoading, setDatasetLoading] = useState(true);
    const [datasetError, setDatasetError] = useState<string | null>(null);

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
    const [isAnalysisRunning, setIsAnalysisRunning] = useState(false);

    useEffect(() => {
        setDatasetLoading(false);
    }, []);

    useEffect(() => {
        localStorage.setItem(LOCAL_STORAGE_VIEW_MODE_KEY, viewMode);
    }, [viewMode]);

    const fetchEpochNames = useCallback(async () => {
        try {
            setLoadingEpochs(true);
            setEpochError(null);
            const namesResponse = await fetch(`${API_BASE_URL}/epochs`);
            if (!namesResponse.ok) {
                throw new Error(`HTTP error fetching epoch names! status: ${namesResponse.status}`);
            }
            const names: string[] = await namesResponse.json();
            setAllEpochNames(names);

            if (names.length > 0) {
                setSelectedEpochs(new Set([names[0]]));
                setLastClickedEpoch(names[0]);
            } else {
                setEpochError("No analysis epochs found.");
                setSelectedEpochs(new Set());
            }
        } catch (err: any) {
            console.error("Error fetching epoch names:", err);
            setEpochError(`Error fetching epochs: ${err.message}`);
            setAllEpochNames([]);
            setSelectedEpochs(new Set());
        } finally {
            setLoadingEpochs(false);
        }
    }, []);

    useEffect(() => {
        fetchEpochNames();
    }, []);

    useEffect(() => {
        let intervalId: number | null = null;
        if (isAnalysisRunning) {
            let isPolling = false;
            intervalId = setInterval(async () => {
                if (isPolling) return;
                try {
                    const statusResponse = await fetch(`${API_BASE_URL}/status`);
                    const statusData = await statusResponse.json();
                    if (statusData.status === 'completed') {
                        setIsAnalysisRunning(false);

                        // await fetchEpochNames();
                        window.location.reload() // TODO: Maybe there is a smoother approach? But don't worry about it for now

                        if (intervalId) {
                            clearInterval(intervalId);
                        }
                    }
                } catch (error) {
                    console.error("Polling for analysis status failed:", error);
                    // setIsAnalysisRunning(false);
                    setPostStatus({ message: 'Analysis status check failed. Please refresh manually.', type: 'error' });
                    if (intervalId) {
                        clearInterval(intervalId);
                    }
                } finally {
                    isPolling = false;
                }
            }, 1000); // Poll every 1 second
        }

        return () => {
            if (intervalId) {
                clearInterval(intervalId);
            }
        };
    }, [isAnalysisRunning, fetchEpochNames]);

    // Add this function inside your App component, near your other handlers
    const onDirectorySelect = async () => {
        // TODO: set the {isAnalysisRunning} variable - I need this to make it load

        setDatasetError(null);
        setDatasetLoading(true);

        try {
            const directoryIdentifier = prompt(
                "Please enter the FULL, absolute path to the directory (e.g., /Users/user/folder/BASIL/src/test/correct/arrays_simple/clang/arrays_simple.gts:"
            );

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const response = await fetch(`${API_BASE_URL}/config/select-directory`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    // Send the full path string to the Scala backend
                    directoryPath: directoryIdentifier,
                }),
            });

            if (!response.ok) {
                const errorText = await response.text();
                throw new Error(errorText || `Backend failed with status: ${response.status}`);
            }

            setSelectedDataset(directoryIdentifier);
            localStorage.setItem(LOCAL_STORAGE_DATASET_KEY, directoryIdentifier); // TODO: Ensure this is run on the first opening with the previously selected path... Or always redo the old one...?

            setDatasetError(null);
            setPostStatus({ message: `Successfully processed directory: ${directoryIdentifier}`, type: 'success' });
            setIsAnalysisRunning(true); // This calls the other useEffect that waits until analysis running is false

        } catch (err: any) {
            setDatasetError(`Error processing directory: ${err.message}`); // Add thus into the post status instead, and keep it open.
            console.error("Error processing directory:", err);
        } finally {
            setDatasetLoading(false);
        }
    };

    const singleSelectedStartEpoch = selectedEpochs.size > 0 ? Array.from(selectedEpochs)[0] : null;
    const singleSelectedEndEpoch = selectedEpochs.size > 0 ? Array.from(selectedEpochs)[selectedEpochs.size - 1] : null;

    useEffect(() => {
        const fetchProcedureNames = async () => {
            if (!singleSelectedStartEpoch && !singleSelectedEndEpoch) {
                setProcedureNames([]);
                setSelectedProcedureName(null);
                setProcedureError(null);
                return;
            }

            setLoadingProcedures(true);
            setProcedureError(null);
            try {
                // Assume that after procedures always have the same name as before procedures
                const response = await fetch(`${API_BASE_URL}/procedures/${singleSelectedEndEpoch}`);
                if (!response.ok) {
                    const errorMessage = `HTTP error! status: ${response.status} fetching procedures for ${singleSelectedEndEpoch}`;
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
    }, [singleSelectedEndEpoch, selectedProcedureName, selectedEpochs]);

    // Save the selected procedure to local storage whenever it changes
    useEffect(() => {
        if (selectedProcedureName) {
            localStorage.setItem(LOCAL_STORAGE_PROCEDURE_KEY, selectedProcedureName);
        } else {
            localStorage.removeItem(LOCAL_STORAGE_PROCEDURE_KEY);
        }
    }, [selectedProcedureName]);

    const handleEpochSelect = useCallback((name: string, event: React.MouseEvent) => {
        if (!event.shiftKey) {
            setSelectedEpochs(new Set([name]));
            setLastClickedEpoch(name);
        } else if (lastClickedEpoch !== null) {
            const lastIndex = allEpochNames.indexOf(lastClickedEpoch);
            const currentIndex = allEpochNames.indexOf(name);

            const startIndex = Math.min(lastIndex, currentIndex);
            const endIndex = Math.max(lastIndex, currentIndex);

            const newSelectedEpochs = new Set<string>();
            for (let i = startIndex; i <= endIndex; i++) {
                newSelectedEpochs.add(allEpochNames[i]);
            }

            setSelectedEpochs(newSelectedEpochs);
        }
    }, [allEpochNames, lastClickedEpoch]);

    const toggleSidebar = () => {
        setIsSidebarMinimized(!isSidebarMinimized);
    };

    const toggleSettings = () => {
        setIsSettingsOpen(!isSettingsOpen);
    };

    const handleCloseErrorModal = () => {
        setDatasetError(null);
    };

    useEffect(() => {
        if (theme === 'system') {
            document.documentElement.removeAttribute('data-theme');
        } else {
            document.documentElement.setAttribute('data-theme', theme);
        }

        localStorage.setItem('theme', theme);
    }, [theme]);

    const renderViewer = () => {
        switch (viewMode) {
            case ViewMode.IR:
                return (
                    <DiffViewer
                        selectedStartEpoch={singleSelectedStartEpoch}
                        selectedEndEpoch={singleSelectedEndEpoch}
                        theme={theme}
                    />
                );
            case ViewMode.CFG:
                return (
                    <CfgViewer
                        selectedStartEpoch={singleSelectedStartEpoch}
                        selectedEndEpoch={singleSelectedEndEpoch}
                        selectedProcedureName={selectedProcedureName}
                        setSelectedProcedureName={setSelectedProcedureName}
                        procedureNames={procedureNames}
                        loadingProcedures={loadingProcedures}
                        procedureError={procedureError}
                    />
                );
            case ViewMode.IR_CFG:
                return (
                    <CombinedViewer
                        selectedStartEpoch={singleSelectedStartEpoch}
                        selectedEndEpoch={singleSelectedEndEpoch}
                        selectedProcedureName={selectedProcedureName}
                        setSelectedProcedureName={setSelectedProcedureName}
                        procedureNames={procedureNames}
                        loadingProcedures={loadingProcedures}
                        procedureError={procedureError}
                    />
                );
            default:
                return <div>Select a valid view mode.</div>;
        }
    };

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
                          selectedEpochs={selectedEpochs}
                          onEpochSelect={handleEpochSelect}
                          loading={loadingEpochs}
                          // error={epochError} // TODO: Turn this into a pop up error before I use this
                          selectedDataset={selectedDataset}
                          onDirectorySelect={onDirectorySelect}
                          datasetLoading={datasetLoading}
                        />
                    </ResizableSidebar>
                    {renderViewer()}
                </main>
            </div>
            <SettingsModal
                isOpen={isSettingsOpen}
                onClose={toggleSettings}
                theme={theme}
                setTheme={setTheme}
            />
            <LoadingModal
                isOpen={isAnalysisRunning}
                message="Running Analysis..."
                postStatus={postStatus}
            />
            <ErrorModal
                isOpen={!!datasetError} // Open if datasetError is not null
                errorMessage={datasetError} // TODO: OR should it take postStatus as well
                onClose={handleCloseErrorModal} // Function to clear the error state
            />
        </div>
    )
}

export default App
