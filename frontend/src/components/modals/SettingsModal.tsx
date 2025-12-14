// src/components/modals/SettingsModal.tsx

import React, { useEffect, useState } from 'react';
import '../../styles/components/modals/modal-base.css';
import type { PostStatus } from '../../context/AppContext.tsx';

interface SettingsModalProps {
  isOpen: boolean;
  onClose: () => void;
  theme: 'light' | 'dark';
  setTheme: (theme: 'light' | 'dark') => void;
  customLogoFile: File | null;
  setCustomLogoFile: (file: File | null) => void;
  setPostStatus: (status: PostStatus) => void;
}

function SettingsModal({
  isOpen,
  onClose,
  theme,
  setTheme,
  customLogoFile,
  setCustomLogoFile,
  setPostStatus,
}: SettingsModalProps) {
  if (!isOpen) return null;

  const [stagedLogoFile, setStagedLogoFile] = useState(customLogoFile);

  useEffect(() => {
    setStagedLogoFile(customLogoFile);
  }, [isOpen, customLogoFile]);

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

  const handleResetLogoAndApply = () => {
    setStagedLogoFile(null);
    // immediately apply reset, no need for 'confirmation button'
    handleApplyLogo(null);
  };

  const handleApplyLogo = (fileToApply: File | null) => {
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

  const getPreviewUrl = (file: File | null): string | null => {
    if (file) {
      return URL.createObjectURL(file);
    }
    return null;
  };
  const previewUrl = getPreviewUrl(stagedLogoFile);

  useEffect(() => {
    return () => {
      if (previewUrl) {
        URL.revokeObjectURL(previewUrl);
      }
    };
  }, [previewUrl]);

  const isLogoChangePending = stagedLogoFile !== customLogoFile;

  return (
    <div className="modal-overlay">
      <div className="modal-content-base">
        <div className="modal-header">
          <h2 className="modal-title">Settings</h2>
          <button
            className="modal-close-button"
            aria-label="Close settings modal"
            onClick={onClose}
          >
            &times; {/* The image of an X */}
          </button>
        </div>
        <div className="selection-options">
          <div className="toggle-group">
            <label className="theme-select">Theme:</label>
            <button
              className={`toggle-button ${theme === 'light' ? 'active' : ''}`}
              onClick={() => setTheme('light')}
            >
              {theme === 'light' && <span className="tick">✓</span>}
              Light
            </button>
            <button
              className={`toggle-button ${theme === 'dark' ? 'active' : ''}`}
              onClick={() => setTheme('dark')}
            >
              {theme === 'dark' && <span className="tick">✓</span>}
              Dark
            </button>
          </div>

          <div className="input-group" style={{ marginTop: '15px' }}>
            <label htmlFor="logo-file-input" className="logo-file-label">
              Upload Custom Logo (.png only):
            </label>
            <input
              id="logo-file-input"
              type="file"
              accept="image/png, image/svg+xml"
              onChange={handleFileChange}
              style={{ padding: '5px 0' }}
            />

            {customLogoFile && (
              <button
                onClick={handleResetLogoAndApply}
                className="reset-button"
                style={{
                  marginLeft: '10px',
                  padding: '5px 10px',
                  backgroundColor: '#f44336',
                  color: 'white',
                  border: 'none',
                  borderRadius: '4px',
                  cursor: 'pointer',
                }}
              >
                Reset to Default
              </button>
            )}
          </div>

          {stagedLogoFile && previewUrl && (
            <div
              className="logo-preview"
              style={{
                marginTop: '10px',
                padding: '10px',
                border: '1px solid #eee',
                borderRadius: '4px',
                display: 'flex',
                justifyContent: 'space-between',
                alignItems: 'center',
              }}
            >
              <div style={{ display: 'flex', alignItems: 'center' }}>
                <img
                  src={previewUrl}
                  alt="Custom Logo Preview"
                  style={{
                    maxWidth: '50px',
                    maxHeight: '50px',
                    objectFit: 'contain',
                    marginRight: '10px',
                  }}
                />
                <p style={{ margin: 0 }}>
                  **{stagedLogoFile.name}** is staged.
                </p>
              </div>

              {isLogoChangePending && (
                <button
                  onClick={() => handleApplyLogo(stagedLogoFile)}
                  className="apply-logo-button"
                  style={{
                    padding: '8px 15px',
                    backgroundColor: '#007bff',
                    color: 'white',
                    border: 'none',
                    borderRadius: '4px',
                    cursor: 'pointer',
                  }}
                >
                  Apply Logo
                </button>
              )}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

export default SettingsModal;
