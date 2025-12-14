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
import { useDirectory } from './hooks/useDirectory.ts';
import { useProcedures } from './hooks/useProcedures.ts';
import { useAnalysisStatus } from './hooks/useAnalysisStatus.ts';
import { useEpochs } from './hooks/useEpochs.ts';
import { useAppContext } from './context/AppContext.tsx';
import { useCustomLogo } from './hooks/logo/useCustomLogo.ts';

const LOCAL_STORAGE_PROCEDURE_KEY = 'cfgViewerSelectedProcedure';
const LOCAL_STORAGE_VIEW_MODE_KEY = 'selectedViewMode';

const ViewMode = {
  IR: 'IR',
  CFG: 'CFG',
  IR_CFG: 'IR/CFG',
};

function App() {
  const {
    postStatus,
    setPostStatus,
    datasetError,
    setDatasetError,
    isAnalysisRunning,
    setIsAnalysisRunning,
    isDatabaseLoaded,
    setIsDatabaseLoaded,
    selectedDataset,
    viewMode,
    setViewMode,
    theme,
    setTheme,
    selectedProcedureName,
    setSelectedProcedureName,
    isSidebarMinimised,
    setIsSidebarMinimised,
    isConfigModalOpen,
    setIsConfigModalOpen,
  } = useAppContext();

  const [isSettingsOpen, setIsSettingsOpen] = useState<boolean>(false);
  const isAutoReloadingRef = useRef(false);

  useAnalysisStatus({
    isRunning: isAnalysisRunning,
    setIsRunning: setIsAnalysisRunning,
    setPostStatus,
    setDatasetError,
  });

  const { datasetLoading, submitDirectoryPath } = useDirectory({
    setIsAnalysisRunning,
    setDatasetError,
    setPostStatus,
    setIsDatabaseLoaded,
  });

  const {
    allEpochNames,
    selectedEpochs,
    lastClickedEpoch,
    loadingEpochs,
    setSelectedEpochs,
    setLastClickedEpoch,
    fetchEpochNames,
  } = useEpochs({
    isDatabaseLoaded,
    selectedDataset,
    setPostStatus,
    setDatasetError,
    submitDirectoryPath,
  });

  const { customLogoFile, handleSetCustomLogoFile } = useCustomLogo({
    setPostStatus,
    setDatasetError,
  });

  useEffect(() => {
    localStorage.setItem(LOCAL_STORAGE_VIEW_MODE_KEY, viewMode);
  }, [viewMode]);

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

  const { procedureNames, loadingProcedures, procedureError } = useProcedures({
    selectedStartEpoch: singleSelectedStartEpoch,
    selectedProcedureName,
    setSelectedProcedureName,
    setPostStatus,
  });

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
    setIsSidebarMinimised(!isSidebarMinimised);
  };

  const toggleSettings = () => {
    setIsSettingsOpen(!isSettingsOpen);
    setPostStatus({ message: '', type: '' });
  };

  const handleCloseErrorModal = () => {
    setDatasetError(false);
    setPostStatus({ message: '', type: '' });
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
          customLogoFile={customLogoFile}
        />
        <main className="main-layout">
          <ResizableSidebar
            isSidebarMinimised={isSidebarMinimised}
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
        customLogoFile={customLogoFile}
        setCustomLogoFile={handleSetCustomLogoFile}
        setPostStatus={setPostStatus}
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
