// App.tsx
import React, { useEffect, useState, useCallback } from 'react';
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
import { getAnalysisStatus, selectDirectory } from './api/analysis.ts';
import { getEpochNames, getProcedureNames } from './api/data.ts';

const LOCAL_STORAGE_PROCEDURE_KEY = 'cfgViewerSelectedProcedure';
const LOCAL_STORAGE_THEME_KEY = 'theme';
const LOCAL_STORAGE_DATASET_KEY = 'selectedDataset';
const LOCAL_STORAGE_VIEW_MODE_KEY = 'selectedViewMode';

const ViewMode = {
  IR: 'IR',
  CFG: 'CFG',
  IR_CFG: 'IR/CFG',
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
  const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
  const [selectedEpochs, setSelectedEpochs] = useState<Set<string>>(new Set());
  const [lastClickedEpoch, setLastClickedEpoch] = useState<string | null>(null);
  const [loadingEpochs, setLoadingEpochs] = useState(true);
  const [isSidebarMinimized, setIsSidebarMinimized] = useState(false);
  const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
  const [theme, setTheme] = useState<'light' | 'dark' | 'system'>(() => {
    const savedTheme = localStorage.getItem(LOCAL_STORAGE_THEME_KEY);
    return (savedTheme as 'light' | 'dark' | 'system') || 'system';
  });

  const [selectedDataset, setSelectedDataset] = useState<string>(() => {
    const savedDataset = localStorage.getItem(LOCAL_STORAGE_DATASET_KEY);
    return savedDataset || 'dataBaseNameExample'; // TODO: Change to default data config file
  });
  const [postStatus, setPostStatus] = useState({ message: '', type: '' });
  const [datasetLoading, setDatasetLoading] = useState(true);
  const [datasetError, setDatasetError] = useState<boolean>(false);

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

  useEffect(() => {
    setDatasetLoading(false);
  }, []);

  useEffect(() => {
    localStorage.setItem(LOCAL_STORAGE_VIEW_MODE_KEY, viewMode);
  }, [viewMode]);

  const fetchEpochNames = useCallback(async () => {
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
      console.error('Error fetching epoch names:', err);
      setPostStatus({
        message: `Error fetching epochs: \n${err.message}`,
        type: 'error',
      });
      setDatasetError(true);
      errorOccurred = true;
    } finally {
      setLoadingEpochs(false);
    }

    if (errorOccurred) {
      setAllEpochNames([]);
      setSelectedEpochs(new Set());
    }
  }, []);

  useEffect(() => {
    fetchEpochNames()
      .then(() => {
        console.log('fetchEpochNames run');
      })
      .catch((error) => {
        console.error('Error from promise chain of fetchEpochNames', error);
      });
  }, []);

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
  }, [isAnalysisRunning, fetchEpochNames]);

  // Add this function inside your App component, near your other handlers
  const onDirectorySelect = async () => {
    setDatasetError(false);
    setDatasetLoading(true);

    try {
      const directoryIdentifier = prompt(
        // TODO: Potentially custom make this for better error handling and info hints
        'Please enter the FULL, absolute path to the directory (e.g., /Users/user/folder/BASIL/src/test/correct/arrays_simple/clang/arrays_simple.gts:'
      );

      if (!directoryIdentifier) {
        console.log('Directory path input cancelled or empty.');
        setDatasetLoading(false);
        return;
      }

      await selectDirectory(directoryIdentifier);

      setSelectedDataset(directoryIdentifier);
      localStorage.setItem(LOCAL_STORAGE_DATASET_KEY, directoryIdentifier); // TODO: Ensure this is run on the first opening with the previously selected path... Or always redo the old one...?

      console.info(`Successfully processed directory: ${directoryIdentifier}`);
      setIsAnalysisRunning(true); // This calls the loading modal
    } catch (err: any) {
      setPostStatus({
        message: `Error processing directory: \n${err.message}`,
        type: 'error',
      });
      setDatasetError(true);
      console.error('Error processing directory:', err);
    } finally {
      setDatasetLoading(false);
    }
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
        isOpen={datasetError}
        postStatus={postStatus}
        onClose={handleCloseErrorModal}
      />
    </div>
  );
}

export default App;
