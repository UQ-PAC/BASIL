// App.tsx
import React, { useEffect, useState, useCallback, useRef } from 'react';
import './App.css';
import { DiffViewer } from './components/viewers/diff/DiffViewer.tsx';
import { Header } from './components/layout/Header.tsx';
import { Sidebar } from './components/layout/SideBar.tsx';
import { ResizableSidebar } from './components/layout/ResizableSidebar.tsx';
import CfgViewer from './components/viewers/graph/CfgViewer.tsx';
import CombinedViewer from './components/viewers/CombinedViewer.tsx';
import SettingsModal from './components/modals/SettingsModal.tsx';
import LoadingModal from './components/modals/LoadingModal.tsx';
import ErrorModal from './components/modals/ErrorModal.tsx';
import ConfigurationModal from './components/modals/ConfigurationModal.tsx';
import { getAnalysisStatus } from './api/analysis.ts';
import { getEpochNames, getProcedureNames } from './api/data.ts';
import { useDirectory } from './hooks/useDirectory.ts';

const LOCAL_STORAGE_PROCEDURE_KEY = 'cfgViewerSelectedProcedure';
const LOCAL_STORAGE_THEME_KEY = 'theme';
const LOCAL_STORAGE_DATASET_KEY = 'selectedDataset';
const LOCAL_STORAGE_VIEW_MODE_KEY = 'selectedViewMode';
const DEFAULT_DATASET_PLACEHOLDER =
  'src/test/correct/secret_write/gcc/secret_write';
const DATA_BASE_LOADED = 'dataBaseLoaded';

const ViewMode = {
  IR: 'IR',
  CFG: 'CFG',
  IR_CFG: 'IR/CFG',
};

const initialiseState = () => {
  try {
    const storedValue = localStorage.getItem(DATA_BASE_LOADED);
    return storedValue === 'true';
  } catch (error) {
    console.error('Error reading from localStorage:', error);
    return true; // TODO: Throw an error instead - atm soft pass
  }
};

function App() {
  const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>(() => {
    return (
      (localStorage.getItem(LOCAL_STORAGE_VIEW_MODE_KEY) as
        | 'IR'
        | 'CFG'
        | 'IR/CFG') || 'IR'
    );
  });
  const [isDatabaseLoaded, setIsDatabaseLoaded] =
    useState<boolean>(initialiseState);
  const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
  const [selectedEpochs, setSelectedEpochs] = useState<Set<string>>(new Set());
  const [lastClickedEpoch, setLastClickedEpoch] = useState<string | null>(null);
  const [loadingEpochs, setLoadingEpochs] = useState(true);
  const [isSidebarMinimized, setIsSidebarMinimized] = useState(false);
  const [isConfigModalOpen, setIsConfigModalOpen] = useState<boolean>(false);
  const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
  const [theme, setTheme] = useState<'light' | 'dark'>(() => {
    const savedTheme = localStorage.getItem(LOCAL_STORAGE_THEME_KEY);
    return (savedTheme as 'light' | 'dark') || 'light';
  });

  const [selectedDataset] = useState<string>(() => {
    const savedDataset = localStorage.getItem(LOCAL_STORAGE_DATASET_KEY);
    return savedDataset || DEFAULT_DATASET_PLACEHOLDER;
  });
  const [postStatus, setPostStatus] = useState({ message: '', type: '' });
  const [datasetError, setDatasetError] = useState<boolean>(false);
  const isAutoReloadingRef = useRef(false);

  const [procedureNames, setProcedureNames] = useState<string[]>([]);
  const [loadingProcedures, setLoadingProcedures] = useState(false);
  const [procedureError, setProcedureError] = useState<string | null>(null);
  const [selectedProcedureName, setSelectedProcedureName] = useState<
    string | null
  >(() => {
    try {
      const storedProcedure = localStorage.getItem(LOCAL_STORAGE_PROCEDURE_KEY);
      return storedProcedure ? storedProcedure : null;
    } catch (e) {
      console.error('Failed to read from localStorage:', e);
      return null;
    }
  });
  const [isAnalysisRunning, setIsAnalysisRunning] = useState(false);

  const { datasetLoading, submitDirectoryPath } = useDirectory({
    setIsAnalysisRunning,
    setDatasetError,
    setPostStatus,
    setIsDatabaseLoaded,
  });

  useEffect(() => {
    localStorage.setItem(LOCAL_STORAGE_VIEW_MODE_KEY, viewMode);
  }, [viewMode]);

  const fetchEpochNames = useCallback(async () => {
    if (!isDatabaseLoaded && selectedEpochs.size === 0) {
      console.log('fetchEpochNames skipped: isDatabaseLoaded is false.');
      return;
    }

    if (isAutoReloadingRef.current) {
      console.log(
        'fetchEpochNames skipped: Auto-reload process already active.'
      );
      return;
    }

    setLoadingEpochs(true);
    let names: string[];
    let errorOccurred = false;

    try {
      names = await getEpochNames();

      setAllEpochNames(names);

      if (names.length > 0) {
        setSelectedEpochs(new Set([names[0]]));
        setLastClickedEpoch(names[0]);
      } else {
        setPostStatus({
          message: 'No analysis epochs found.',
          type: 'warning',
        });
        setDatasetError(true);
      }
    } catch (err: any) {
      if (err.message === 'No config file loaded.') {
        // reload the database
        if (!isAutoReloadingRef.current) {
          console.log(
            'Error: "No config file loaded." Triggering auto-reload.'
          );
          await submitDirectoryPath(selectedDataset);
        }
      } else {
        console.error('Error fetching epoch names:', err);
        setPostStatus({
          message: `Error fetching epochs: \n${err.message}`,
          type: 'error',
        });
        setDatasetError(true);
        errorOccurred = true;
      }
    } finally {
      setLoadingEpochs(false);
    }

    if (errorOccurred) {
      setAllEpochNames([]);
      setSelectedEpochs(new Set());
    }
  }, [isDatabaseLoaded]);

  useEffect(() => {
    if (isDatabaseLoaded && !isAutoReloadingRef.current) {
      fetchEpochNames()
        .then(() => {
          console.log('fetchEpochNames run (triggered by isDatabaseLoaded)');
        })
        .catch((error) => {
          console.error('Error from promise chain of fetchEpochNames', error);
        });
    }
  }, [isDatabaseLoaded]);

  useEffect(() => {
    let intervalId: number | null = null;
    if (isAnalysisRunning) {
      let isPolling = false;
      intervalId = setInterval(async () => {
        if (isPolling) return;
        try {
          const statusData = await getAnalysisStatus();

          if (statusData.status === 'completed') {
            // await fetchEpochNames();
            window.location.reload(); // TODO: Maybe there is a smoother approach? But don't worry about it for now
            setIsAnalysisRunning(false);
            isAutoReloadingRef.current = false;
            console.log('Analysis completed. Lock released.');
            if (intervalId) {
              clearInterval(intervalId);
            }
          }
        } catch (error) {
          console.error('Polling for analysis status failed:', error);
          setIsAnalysisRunning(false);
          setPostStatus({
            message: 'Analysis status check failed. Please refresh manually.',
            type: 'error',
          });
          setDatasetError(true);
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
  }, [isAnalysisRunning]);

  useEffect(() => {
    if (!isDatabaseLoaded && allEpochNames.length === 0) {
      setIsConfigModalOpen(true);
    }
  }, [isDatabaseLoaded, allEpochNames.length]);

  const openConfigModal = () => {
    setIsConfigModalOpen(true);
  };

  const singleSelectedStartEpoch =
    selectedEpochs.size > 0 ? Array.from(selectedEpochs)[0] : null;
  const singleSelectedEndEpoch =
    selectedEpochs.size > 0
      ? Array.from(selectedEpochs)[selectedEpochs.size - 1]
      : null;

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
        const names: string[] = await getProcedureNames(
          singleSelectedStartEpoch as string
        );
        setProcedureNames(names);

        // Check if the previously selected procedure is still valid
        if (
          selectedProcedureName === null ||
          !names.includes(selectedProcedureName)
        ) {
          if (names.length > 0) {
            setSelectedProcedureName(names[0]);
          } else {
            setSelectedProcedureName(null);
          }
        } else {
          console.log(
            'Retaining previously selected procedure: ' + selectedProcedureName
          );
        }
      } catch (e: any) {
        console.error('Error fetching procedure names:', e);
        setProcedureError(`Failed to load procedure names: ${e.message}`);
        setProcedureNames([]);
        setSelectedProcedureName(null);
      } finally {
        setLoadingProcedures(false);
      }
    };

    fetchProcedureNames().catch(
      (error) =>
        console.error(
          "Unhandled promise rejected from 'fetchProcedureNames': ",
          error
        )
      // TODO: Also post an Error. DatabaseError
    );
  }, [singleSelectedEndEpoch, selectedProcedureName, selectedEpochs]);

  // Save the selected procedure to local storage whenever it changes
  useEffect(() => {
    if (selectedProcedureName) {
      localStorage.setItem(LOCAL_STORAGE_PROCEDURE_KEY, selectedProcedureName);
    } else {
      localStorage.removeItem(LOCAL_STORAGE_PROCEDURE_KEY);
    }
  }, [selectedProcedureName]);

  const handleEpochSelect = useCallback(
    (name: string, event: React.MouseEvent) => {
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
    },
    [allEpochNames, lastClickedEpoch]
  );

  const toggleSidebar = () => {
    setIsSidebarMinimized(!isSidebarMinimized);
  };

  const toggleSettings = () => {
    setIsSettingsOpen(!isSettingsOpen);
  };

  const handleCloseErrorModal = () => {
    setDatasetError(false);
  };

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);

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
        <Header
          setViewMode={setViewMode}
          viewMode={viewMode}
          toggleSettings={toggleSettings}
        />
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
              selectedDataset={selectedDataset}
              onDirectorySelect={openConfigModal}
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
        isOpen={datasetError}
        postStatus={postStatus}
        onClose={handleCloseErrorModal}
      />
      <ConfigurationModal
        isOpen={isConfigModalOpen}
        onClose={() => setIsConfigModalOpen(false)}
        onSubmit={submitDirectoryPath}
        initialPath={selectedDataset}
        hasDatabaseLoaded={isDatabaseLoaded}
      />
    </div>
  );
}

export default App;
