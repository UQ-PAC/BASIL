// src/hooks/useEpochs.ts
import { useState, useEffect, useCallback, useRef } from 'react';
import { getEpochNames } from '../api/data';

interface UseEpochsProps {
  isDatabaseLoaded: boolean;
  selectedDataset: string;
  setPostStatus: (status: { message: string; type: string }) => void;
  setDatasetError: (val: boolean) => void;
  submitDirectoryPath: (path: string) => Promise<void>;
}

export const useEpochs = ({
  isDatabaseLoaded,
  selectedDataset,
  setPostStatus,
  setDatasetError,
  submitDirectoryPath,
}: UseEpochsProps) => {
  const [allEpochNames, setAllEpochNames] = useState<string[]>([]);
  const [selectedEpochs, setSelectedEpochs] = useState<Set<string>>(new Set());
  const [lastClickedEpoch, setLastClickedEpoch] = useState<string | null>(null);
  const [loadingEpochs, setLoadingEpochs] = useState(true);

  const isAutoReloadingRef = useRef(false);
  const didFetchRef = useRef(false);

  const fetchEpochNames = useCallback(async () => {
    if (!isDatabaseLoaded) {
      setPostStatus({
        message: 'Database not loaded yet. Fetch skipped.',
        type: 'info',
      });
      return;
    }
    if (isAutoReloadingRef.current) {
      setPostStatus({
        message: 'Auto-reload already in progress. Fetch skipped.',
        type: 'info',
      });
      return;
    }

    setLoadingEpochs(true);
    let errorOccurred = false;

    try {
      const names = await getEpochNames();
      setAllEpochNames(names);

      if (names.length > 0) {
        if (!selectedEpochs.has(names[0])) {
          setSelectedEpochs(new Set([names[0]]));
          setLastClickedEpoch(names[0]);
        }
        setPostStatus({
          message: `Loaded ${names.length} epoch(s).`,
          type: 'success',
        });
      } else {
        setPostStatus({
          message: 'No analysis epochs found.',
          type: 'warning',
        });
        setDatasetError(true);
      }
    } catch (err: any) {
      if (
        err.message === 'No config file loaded.' &&
        !isAutoReloadingRef.current
      ) {
        setPostStatus({
          message: 'No config file loaded. Triggering auto-reload...',
          type: 'info',
        });
        console.log('No config file loaded. Triggering auto-reload.');
        isAutoReloadingRef.current = true;
        await submitDirectoryPath(selectedDataset);
        isAutoReloadingRef.current = false;
        await fetchEpochNames();
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
      setLastClickedEpoch(null);
    }
  }, [
    isDatabaseLoaded,
    selectedDataset,
    setPostStatus,
    setDatasetError,
    submitDirectoryPath,
    selectedEpochs,
  ]);

  useEffect(() => {
    if (isDatabaseLoaded && !didFetchRef.current) {
      didFetchRef.current = true;
      fetchEpochNames().catch((err) =>
        console.error('Error fetching epochs:', err)
      );
    }
  }, [isDatabaseLoaded, fetchEpochNames]);

  return {
    allEpochNames,
    selectedEpochs,
    lastClickedEpoch,
    loadingEpochs,
    setSelectedEpochs,
    setLastClickedEpoch,
    fetchEpochNames,
  };
};
