// src/hooks/useDirectory.ts
import { useState } from 'react';
import { selectDirectory } from '../api/analysis';
import type { PostStatus } from '../context/AppContext.tsx';

export const DATA_BASE_LOADED = 'dataBaseLoaded';
export const LOCAL_STORAGE_DATASET_KEY = 'selectedDataset';

interface UseDirectoryProps {
  setIsAnalysisRunning: (running: boolean) => void;
  setDatasetError: (val: boolean) => void;
  setPostStatus: (status: PostStatus) => void;
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
    const cleanDirectoryIdentifier = directoryIdentifier.trim();
    if (!cleanDirectoryIdentifier) {
      console.log('Directory path input empty.');
      return;
    }

    setDatasetError(false);
    setDatasetLoading(true);

    try {
      await selectDirectory(cleanDirectoryIdentifier);
      console.info(
        `Successfully processed directory: ${cleanDirectoryIdentifier}`
      );

      localStorage.setItem(LOCAL_STORAGE_DATASET_KEY, cleanDirectoryIdentifier);
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
