// src/hooks/useLogoSettingsForm.ts

import React, { useState, useEffect } from 'react';
import type { PostStatus } from '../context/AppContext.tsx';
import { useLogoUrl } from './useLogoUrl.ts';

interface LogoSettingsHookProps {
  initialFile: File | null;
  setCustomLogoFile: (file: File | null) => void;
  setPostStatus: (status: PostStatus) => void;
}

interface LogoSettingsHookReturn {
  stagedLogoFile: File | null;
  previewUrl: string | null;
  isLogoChangePending: boolean;
  handleFileChange: (event: React.ChangeEvent<HTMLInputElement>) => void;
  handleResetLogoAndApply: () => void;
  handleApplyLogo: () => void;
}

export function useLogoSettingsForm({
  initialFile,
  setCustomLogoFile,
  setPostStatus,
}: LogoSettingsHookProps): LogoSettingsHookReturn {
  const [stagedLogoFile, setStagedLogoFile] = useState(initialFile);

  useEffect(() => {
    setStagedLogoFile(initialFile);
  }, [initialFile]);

  const previewUrl = useLogoUrl(stagedLogoFile);

  const isLogoChangePending = stagedLogoFile !== initialFile;

  const applyLogoCommit = (fileToApply: File | null) => {
    setCustomLogoFile(fileToApply);

    if (!fileToApply) {
      const fileInput = document.getElementById(
        'logo-file-input'
      ) as HTMLInputElement;
      if (fileInput) {
        fileInput.value = '';
      }
    }
  };

  const handleApplyLogo = () => {
    applyLogoCommit(stagedLogoFile);
  };

  const handleResetLogoAndApply = () => {
    setStagedLogoFile(null);
    applyLogoCommit(null);
  };

  const handleFileChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0] || null;

    const isAllowedType =
      file?.type === 'image/png' || file?.type === 'image/svg+xml';

    if (file && isAllowedType) {
      setStagedLogoFile(file);
    } else {
      setStagedLogoFile(null);
      if (file) {
        setPostStatus({
          message: 'Error: Please select a valid PNG or SVG image file.',
          type: 'error',
        });
      }
    }
  };

  return {
    stagedLogoFile,
    previewUrl,
    isLogoChangePending,
    handleFileChange,
    handleResetLogoAndApply,
    handleApplyLogo: () => handleApplyLogo(),
  };
}
