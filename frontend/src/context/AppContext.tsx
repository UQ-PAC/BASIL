// src/context/AppContext.tsx
import React, {
  createContext,
  useState,
  useContext,
  type ReactNode,
  useRef,
} from 'react';

export interface PostStatus {
  message: string;
  type: 'error' | 'warning' | 'info' | '';
}

interface AppContextProps {
  isDatabaseLoaded: boolean;
  setIsDatabaseLoaded: (val: boolean) => void;
  datasetError: boolean;
  setDatasetError: (val: boolean) => void;
  postStatus: PostStatus;
  setPostStatus: (status: PostStatus) => void;
  isAnalysisRunning: boolean;
  setIsAnalysisRunning: (val: boolean) => void;
  selectedDataset: string;
  selectedProcedureName: string | null;
  setSelectedProcedureName: (val: string | null) => void;
  viewMode: 'IR' | 'CFG' | 'IR/CFG';
  setViewMode: (val: 'IR' | 'CFG' | 'IR/CFG') => void;
  theme: 'light' | 'dark';
  setTheme: (val: 'light' | 'dark') => void;
  isSidebarMinimised: boolean;
  setIsSidebarMinimised: (val: boolean) => void;
  isConfigModalOpen: boolean;
  setIsConfigModalOpen: (val: boolean) => void;
  isSettingsOpen: boolean;
  setIsSettingsOpen: (val: boolean) => void;
  isAutoReloadingRef: React.RefObject<boolean>;
}

const AppContext = createContext<AppContextProps | undefined>(undefined);

export const AppProvider = ({ children }: { children: ReactNode }) => {
  const [isDatabaseLoaded, setIsDatabaseLoaded] = useState<boolean>(() => {
    try {
      return localStorage.getItem('dataBaseLoaded') === 'true';
    } catch {
      return true;
    }
  });
  const [datasetError, setDatasetError] = useState(false);
  const [postStatus, setPostStatus] = useState<PostStatus>({
    message: '',
    type: '',
  });
  const [isAnalysisRunning, setIsAnalysisRunning] = useState(false);
  const [selectedDataset] = useState<string>(() => {
    const saved = localStorage.getItem('selectedDataset');
    return saved || 'src/test/correct/secret_write/gcc/secret_write';
  });
  const [selectedProcedureName, setSelectedProcedureName] = useState<
    string | null
  >(null);

  const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>(() => {
    const saved = localStorage.getItem('selectedViewMode') as
      | 'IR'
      | 'CFG'
      | 'IR/CFG';
    return saved || 'IR';
  });

  const [theme, setTheme] = useState<'light' | 'dark'>(() => {
    const saved = localStorage.getItem('theme') as 'light' | 'dark';
    return saved || 'light';
  });

  const [isSidebarMinimised, setIsSidebarMinimised] = useState(false);
  const [isConfigModalOpen, setIsConfigModalOpen] = useState(false);
  const [isSettingsOpen, setIsSettingsOpen] = useState(false);
  const isAutoReloadingRef = useRef(false);

  return (
    <AppContext.Provider
      value={{
        isDatabaseLoaded,
        setIsDatabaseLoaded,
        datasetError,
        setDatasetError,
        postStatus,
        setPostStatus,
        isAnalysisRunning,
        setIsAnalysisRunning,
        selectedDataset,
        selectedProcedureName,
        setSelectedProcedureName,
        viewMode,
        setViewMode,
        theme,
        setTheme,
        isSidebarMinimised,
        setIsSidebarMinimised,
        isConfigModalOpen,
        setIsConfigModalOpen,
        isSettingsOpen,
        setIsSettingsOpen,
        isAutoReloadingRef,
      }}
    >
      {children}
    </AppContext.Provider>
  );
};

export const useAppContext = () => {
  const context = useContext(AppContext);
  if (!context)
    throw new Error('useAppContext must be used within AppProvider');
  return context;
};
