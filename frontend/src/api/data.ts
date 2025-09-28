// src/api/data.ts
import { API_BASE_URL } from './index';

export async function getEpochNames(): Promise<string[]> {
  const namesResponse = await fetch(`${API_BASE_URL}/epochs`);

  if (!namesResponse.ok) {
    const errorText = await namesResponse.text();
    throw new Error(errorText || `HTTP error fetching epoch names! Status: ${namesResponse.status}`);
  }

  return namesResponse.json();
}

export async function getProcedureNames(epochName: string): Promise<string[]> {
  const response = await fetch(`${API_BASE_URL}/procedures/${epochName}`);

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(errorText || `HTTP error fetching procedures for ${epochName}! Status: ${response.status}`);
  }

  return response.json();
}
