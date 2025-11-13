// src/hooks/useDirectory.ts
import { useState } from 'react';
import { selectDirectory } from '../api/analysis';

export const DATA_BASE_LOADED = 'dataBaseLoaded';
export const LOCAL_STORAGE_DATASET_KEY = 'selectedDataset';

interface UseDirectoryProps {
  setIsAnalysisRunning: (running: boolean) => void;
  setDatasetError: (val: boolean) => void;
  setPostStatus: (status: { message: string; type: string }) => void;
  setIsDatabaseLoaded: (loaded: boolean) => void;
}

export function useDirectory({
                               setIsAnalysisRunning,
                               setDatasetError,
                               setPostStatus,
                               setIsDatabaseLoaded,
                             }: UseDirectoryProps) {
  const [datasetLoading, setDatasetLoading] = useState(false);

  const submitDirectoryPath = async (directoryIdentifier: string) => {
    if (!directoryIdentifier || !directoryIdentifier.trim()) {
      console.log('Directory path input empty.');
      return;
    }

    setDatasetError(false);
    setDatasetLoading(true);

    try {
      await selectDirectory(directoryIdentifier);
      console.info(`Successfully processed directory: ${directoryIdentifier}`);

      localStorage.setItem(LOCAL_STORAGE_DATASET_KEY, directoryIdentifier);
      setIsDatabaseLoaded(true);
      localStorage.setItem(DATA_BASE_LOADED, 'true');
      setIsAnalysisRunning(true);
    } catch (err: any) {
      console.error('Error processing directory:', err);
      setPostStatus({
        message: `Error processing directory:\n${err.message}`,
        type: 'error',
      });
      setDatasetError(true);
    } finally {
      setDatasetLoading(false);
    }
  };

  return { datasetLoading, submitDirectoryPath };
}
