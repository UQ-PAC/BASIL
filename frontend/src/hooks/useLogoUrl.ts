// src/hooks/useLogoUrl.ts
import { useState, useEffect } from 'react';

export function useLogoUrl(file: File | null): string | null {
  const [logoPreviewUrl, setLogoPreviewUrl] = useState<string | null>(null);

  useEffect(() => {
    if (file) {
      const url = URL.createObjectURL(file);
      setLogoPreviewUrl(url);

      return () => {
        URL.revokeObjectURL(url);
      };
    } else {
      setLogoPreviewUrl(null);
    }
  }, [file]);

  return logoPreviewUrl;
}
