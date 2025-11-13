import {
  createContext,
  useState,
  useContext,
  type ReactNode,
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
  const [postStatus, setPostStatus] = useState<PostStatus>({ message: '', type: '' });
  const [isAnalysisRunning, setIsAnalysisRunning] = useState(false);
  const [selectedDataset] = useState<string>(() => {
    const saved = localStorage.getItem('selectedDataset');
    return saved || 'src/test/correct/secret_write/gcc/secret_write';
  });

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
      }}
    >
      {children}
    </AppContext.Provider>
  );
};

export const useAppContext = () => {
  const context = useContext(AppContext);
  if (!context) throw new Error('useAppContext must be used within AppProvider');
  return context;
};
