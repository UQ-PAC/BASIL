// src/api/analysis.ts
import { API_BASE_URL } from './index';

export async function getAnalysisStatus() {
  const statusResponse = await fetch(`${API_BASE_URL}/status`);

  if (!statusResponse.ok) {
    throw new Error('Failed to fetch analysis status.');
  }

  return statusResponse.json();
}

export async function selectDirectory(directoryPath: string): Promise<void> {
  const response = await fetch(`${API_BASE_URL}/config/select-directory`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ directoryPath }),
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(
      errorText || `Backend failed with status: ${response.status}`
    );
  }
}
