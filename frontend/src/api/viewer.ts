// src/api/viewer.ts
import { API_BASE_URL } from './index';
import type { GraphJSON } from '../utils/graphLayout.ts'; // TODO: Or maybe move this here

interface ProcedureLocation {
  name: string;
  startLine: number;
  approxEndLine: number;
}

export interface IREpochData {
  before: string;
  after: string;
  procedures: ProcedureLocation[];
  epochName: string;
}

export async function fetchGraphJson(
  epoch: string,
  type: 'before' | 'after'
): Promise<Record<string, GraphJSON>> {
  const response = await fetch(`${API_BASE_URL}/cfg/${epoch}/${type}`);

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(
      errorText ||
        `HTTP error fetching ${type} CFG for epoch ${epoch}. Status: ${response.status}`
    );
  }

  return response.json();
}

export async function fetchIrCode(
  epoch: string,
  procedureName: string,
  type: 'before' | 'after'
): Promise<string> {
  const irResponse = await fetch(
    `${API_BASE_URL}/ir/${epoch}/${procedureName}/${type}`
  );

  if (!irResponse.ok) {
    const errorText = await irResponse.text();
    throw new Error(
      errorText || `HTTP error fetching IR code. Status: ${irResponse.status}`
    );
  }

  return irResponse.text();
}

export async function fetchDiffIRData(
  epochBeforeName: string,
  epochAfterName: string
): Promise<IREpochData> {
  const [beforeResponse, afterResponse, proceduresResponse] = await Promise.all(
    [
      fetch(`${API_BASE_URL}/ir/${epochBeforeName}/before`),
      fetch(`${API_BASE_URL}/ir/${epochAfterName}/after`),
      fetch(`${API_BASE_URL}/ir/${epochAfterName}/procedures_with_lines`),
    ]
  );

  if (!beforeResponse.ok)
    throw new Error(
      `HTTP error fetching before IR for ${epochBeforeName}! status: ${beforeResponse.status}`
    );
  if (!afterResponse.ok)
    throw new Error(
      `HTTP error fetching after IR for ${epochAfterName}! status: ${afterResponse.status}`
    );
  if (!proceduresResponse.ok)
    throw new Error(
      `HTTP error fetching procedures! status: ${proceduresResponse.status}`
    );

  const beforeText: string = await beforeResponse.text();
  const afterText: string = await afterResponse.text();
  const proceduresData: ProcedureLocation[] = await proceduresResponse.json();

  return {
    before: beforeText,
    after: afterText,
    procedures: proceduresData,
    epochName:
      epochBeforeName === epochAfterName
        ? epochBeforeName
        : `${epochBeforeName} to ${epochAfterName}`,
  };
}
