// src/hooks/useIrDiffData.ts
import { useState, useEffect } from 'react';
import { fetchDiffIRData, type IREpochData } from '../api/viewer.ts';

interface UseIrDiffDataResult {
  irData: IREpochData | null;
  isLoading: boolean;
  error: string | null;
}

export function useIrDiffData(
  selectedStartEpoch: string | null,
  selectedEndEpoch: string | null
): UseIrDiffDataResult {
  const [irData, setIrData] = useState<IREpochData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!selectedStartEpoch || !selectedEndEpoch) {
      setIrData(null);
      setLoading(false);
      setError(null);
      return;
    }

    const loadData = async () => {
      try {
        setLoading(true);
        setError(null);

        const data = await fetchDiffIRData(
          selectedStartEpoch,
          selectedEndEpoch
        );

        console.log(
          `Fetched data for epoch: ${selectedStartEpoch} and ${selectedEndEpoch}`
        );

        setIrData(data);
      } catch (err: any) {
        console.error(
          `Error fetching analysis data for ${selectedStartEpoch} and ${selectedEndEpoch}:`,
          err
        );
        setError(
          `Error fetching data for ${selectedStartEpoch} and ${selectedEndEpoch}: ${err.message}`
        );
        setIrData(null);
      } finally {
        setLoading(false);
      }
    };

    loadData();
  }, [selectedStartEpoch, selectedEndEpoch]);

  return { irData, isLoading: loading, error };
}
