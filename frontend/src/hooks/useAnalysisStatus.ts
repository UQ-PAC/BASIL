import { useEffect, useRef } from 'react';
import { getAnalysisStatus } from '../api/analysis';
import type { PostStatus } from '../context/AppContext.tsx';

interface UseAnalysisStatusProps {
  isRunning: boolean;
  setIsRunning: (running: boolean) => void;
  setPostStatus: (status: PostStatus) => void;
  setDatasetError: (val: boolean) => void;
}

export const useAnalysisStatus = ({
                                    isRunning,
                                    setIsRunning,
                                    setPostStatus,
                                    setDatasetError,
                                  }: UseAnalysisStatusProps) => {
  const isAutoReloadingRef = useRef(false);

  useEffect(() => {
    let intervalId: number | null = null;

    if (!isRunning) return;

    let isPolling = false;

    const poll = async () => {
      if (isPolling) return;
      isPolling = true;

      try {
        const statusData = await getAnalysisStatus();

        if (statusData.status === 'completed') {
          window.location.reload();
          setIsRunning(false);
          isAutoReloadingRef.current = false;
          if (intervalId !== null) clearInterval(intervalId);
        }
      } catch (error: any) {
        console.error('Polling for analysis status failed:', error);
        setIsRunning(false);
        setPostStatus({
          message: 'Analysis status check failed. Please refresh manually.',
          type: 'error',
        });
        setDatasetError(true);
        if (intervalId !== null) clearInterval(intervalId);
      } finally {
        isPolling = false;
      }
    };

    intervalId = window.setInterval(poll, 1000);

    return () => {
      if (intervalId !== null) clearInterval(intervalId);
    };
  }, [isRunning, setIsRunning, setPostStatus, setDatasetError]);
};
