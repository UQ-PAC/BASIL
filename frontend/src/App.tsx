
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

function App() {
    const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>('IR');
    const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
    const [selectedEpochName, setSelectedEpochName] = useState<string | null>(null);
    const [loadingEpochs, setLoadingEpochs] = useState(true);
    const [epochError, setEpochError] = useState<string | null>(null);
    const [isSidebarMinimized, setIsSidebarMinimized] = useState(false);
    const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
    const [theme, setTheme] = useState<'light' | 'dark' | 'system'>(
        (localStorage.getItem('theme') as 'light' | 'dark' | 'system') || 'system' // TODO: Save into a file instead
    );

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
                      <DiffViewer selectedEpochName={selectedEpochName} />
                  ) : viewMode === 'CFG' ? (
                      <CfgViewer selectedEpochName={selectedEpochName} />
                  ) : ( // 'IR/CFG'
                      <CombinedViewer
                          selectedEpochName={selectedEpochName}
                      />
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
