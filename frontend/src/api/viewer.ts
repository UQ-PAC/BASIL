// src/api/viewer.ts
import { API_BASE_URL } from './index';

interface DotGraphResponse {
  [procedureName: string]: string;
}

export async function fetchDotString(
  epoch: string,
  type: 'before' | 'after'
): Promise<DotGraphResponse> {
  const response = await fetch(`${API_BASE_URL}/cfg/${epoch}/${type}`);

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(errorText || `HTTP error fetching ${type} CFG for epoch ${epoch}. Status: ${response.status}`);
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
    throw new Error(errorText || `HTTP error fetching IR code. Status: ${irResponse.status}`);
  }

  return irResponse.text();
}