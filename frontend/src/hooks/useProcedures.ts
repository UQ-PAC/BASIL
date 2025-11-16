// src/hooks/useProcedures.ts
import { useState, useEffect } from 'react';
import { getProcedureNames } from '../api/data';
import type { PostStatus } from '../context/AppContext.tsx';

interface UseProceduresProps {
  selectedStartEpoch: string | null;
  selectedProcedureName: string | null;
  setSelectedProcedureName: (name: string | null) => void;
  setPostStatus: (status: PostStatus) => void;
}

export function useProcedures({
  selectedStartEpoch,
  selectedProcedureName,
  setSelectedProcedureName,
  setPostStatus,
}: UseProceduresProps) {
  const [procedureNames, setProcedureNames] = useState<string[]>([]);
  const [loadingProcedures, setLoadingProcedures] = useState(false);
  const [procedureError, setProcedureError] = useState<string | null>(null);

  useEffect(() => {
    const startEpoch = selectedStartEpoch;
    if (!startEpoch) {
      setProcedureNames([]);
      setSelectedProcedureName(null);
      setProcedureError(null);
      return;
    }

    setLoadingProcedures(true);
    setProcedureError(null);

    getProcedureNames(startEpoch)
      .then((names) => {
        setProcedureNames(names);

        if (!names.includes(selectedProcedureName || '')) {
          setSelectedProcedureName(names[0] || null);
        }
      })
      .catch((err: any) => {
        console.error('Error fetching procedure names:', err);
        setProcedureError(`Failed to load procedure names: ${err.message}`);
        setProcedureNames([]);
        setSelectedProcedureName(null);
        setPostStatus({
          message: `Error fetching procedure names: ${err.message}`,
          type: 'error',
        });
      })
      .finally(() => setLoadingProcedures(false));
  }, [selectedStartEpoch]);

  return { procedureNames, loadingProcedures, procedureError };
}
