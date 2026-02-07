// src/components/modals/SettingsModal.tsx

import '../../styles/components/modals/modal-base.css';
import '../../styles/components/modals/modal-settings.css';
import type { PostStatus } from '../../context/AppContext.tsx';
import { useLogoSettingsForm } from '../../hooks/logo/useLogoSettingsForm.ts';

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

  const {
    stagedLogoFile,
    previewUrl,
    isLogoChangePending,
    handleFileChange,
    handleResetLogoAndApply,
    handleApplyLogo,
  } = useLogoSettingsForm({
    initialFile: customLogoFile,
    setCustomLogoFile,
    setPostStatus,
  });

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
            <label className="selection-text">Theme:</label>
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
            <label htmlFor="logo-file-input" className="selection-text">
              Upload Custom Logo (.png or .svg only):
            </label>
            <input
              id="logo-file-input"
              type="file"
              accept="image/png, image/svg+xml"
              onChange={handleFileChange}
            />

            {customLogoFile && (
              <button
                onClick={handleResetLogoAndApply}
                className="reset-button"
              >
                Reset to Default
              </button>
            )}
          </div>

          {stagedLogoFile && previewUrl && (
            <div className="logo-preview">
              <div className="logo-preview-info">
                <img src={previewUrl} alt="Custom Logo Preview" />
                <p>
                  <strong>{stagedLogoFile.name}</strong> is staged.
                </p>
              </div>

              {isLogoChangePending && (
                <button onClick={handleApplyLogo} className="apply-logo-button">
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
