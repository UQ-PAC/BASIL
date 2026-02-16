// src/hooks/useHierarchicalIrDiff.ts
import { useEffect, useState } from 'react';
import {
  fetchProcedureIndex,
  fetchIrCode,
  type ProcedureMetadata,
} from '../api/viewer';

export type ProcedureStatus = 'unchanged' | 'added' | 'removed' | 'modified';

export interface ProcedureDiffItem {
  metadata: ProcedureMetadata;
  status: ProcedureStatus;
  before?: string; // lazily fetched
  after?: string; // lazily fetched
}

export function useHierarchicalIrDiff(
  startEpoch: string | null,
  endEpoch: string | null
) {
  const [procedures, setProcedures] = useState<ProcedureDiffItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!startEpoch || !endEpoch) return;

    const loadMetadata = async () => {
      try {
        setLoading(true);

        const [beforeMeta, afterMeta] = await Promise.all([
          fetchProcedureIndex(startEpoch),
          fetchProcedureIndex(endEpoch),
        ]);

        const beforeMap = Object.fromEntries(
          beforeMeta.map((p) => [p.name, p])
        );
        const afterMap = Object.fromEntries(afterMeta.map((p) => [p.name, p]));

        const merged: ProcedureDiffItem[] = [];

        const allNames = new Set([
          ...beforeMeta.map((p) => p.name),
          ...afterMeta.map((p) => p.name),
        ]);
        allNames.forEach((name) => {
          const beforeProc = beforeMap[name];
          const afterProc = afterMap[name];

          let status: ProcedureStatus;
          if (beforeProc && afterProc) {
            status =
              beforeProc.beforeHash === afterProc.afterHash
                ? 'unchanged'
                : 'modified';
          } else if (beforeProc) {
            status = 'removed';
          } else {
            status = 'added';
          }

          merged.push({
            metadata: afterProc ?? beforeProc!,
            status,
          });
        });

        setProcedures(merged);
      } catch (err: any) {
        console.error(err);
        setError(err.message);
      } finally {
        setLoading(false);
      }
    };

    loadMetadata();
  }, [startEpoch, endEpoch]);

  // Lazy load procedure body
  const fetchProcedureBody = async (procName: string) => {
    const proc = procedures.find((p) => p.metadata.name === procName);
    if (!proc) return;

    if (!proc.before && proc.status !== 'added') {
      proc.before = await fetchIrCode(startEpoch!, procName, 'before');
    }
    if (!proc.after && proc.status !== 'removed') {
      proc.after = await fetchIrCode(endEpoch!, procName, 'after');
    }

    setProcedures([...procedures]); // trigger re-render
  };

  return { procedures, loading, error, fetchProcedureBody };
}
