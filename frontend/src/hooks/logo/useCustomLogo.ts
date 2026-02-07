// src/hooks/logo/useCustomLogo.ts

import { useState, useEffect } from 'react';
import { base64ToFile } from '../../utils/base64ToFile.ts';
import type { PostStatus } from '../../context/AppContext.tsx';

const LOCAL_STORAGE_LOGO_KEY = 'customAppLogo';

interface CustomLogoHookProps {
  setPostStatus: (status: PostStatus) => void;
  setDatasetError: (error: boolean) => void;
}

export function useCustomLogo({
  setPostStatus,
  setDatasetError,
}: CustomLogoHookProps) {
  const [customLogoFile, setCustomLogoFile] = useState<File | null>(null);

  useEffect(() => {
    const savedBase64 = localStorage.getItem(LOCAL_STORAGE_LOGO_KEY);

    // Extract MIME type from the data URL (e.g., data:image/png;...)
    // MIME (Multipurpose Internet Mail Extensions) defines the file type
    if (savedBase64) {
      const mimeMatch = savedBase64.match(/^data:(.*?);/);
      const mimeType = mimeMatch ? mimeMatch[1] : 'image/png';

      try {
        const restoredFile = base64ToFile(
          savedBase64,
          'restored-logo',
          mimeType
        );
        setCustomLogoFile(restoredFile);
      } catch (e) {
        setPostStatus({
          message:
            'Error restoring logo from local storage: ' +
            (e instanceof Error ? e.message : String(e)),
          type: 'error',
        });
        setDatasetError(true);
        console.error('Error restoring logo from local storage:', e);
        localStorage.removeItem(LOCAL_STORAGE_LOGO_KEY);
      }
    }
  }, [setDatasetError, setPostStatus]);

  const handleSetCustomLogoFile = (file: File | null) => {
    setCustomLogoFile(file);

    if (file) {
      const reader = new FileReader();
      reader.onloadend = () => {
        const base64String = reader.result as string;
        localStorage.setItem(LOCAL_STORAGE_LOGO_KEY, base64String);
        setPostStatus({
          message: `Logo saved and persisted: ${file.name}`,
          type: 'success',
        });
      };
      reader.readAsDataURL(file);
    } else {
      localStorage.removeItem(LOCAL_STORAGE_LOGO_KEY);
      setPostStatus({
        message: 'Logo reset to default icon.',
        type: 'info',
      });
    }
  };

  return {
    customLogoFile,
    handleSetCustomLogoFile,
  };
}
