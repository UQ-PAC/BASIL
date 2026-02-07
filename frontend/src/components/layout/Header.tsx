//src/components/layout/Header.tsx

import '../../styles/components/layout/header.css';
import { useLogoUrl } from '../../hooks/logo/useLogoUrl.ts';

import SettingsIcon from '../../assets/icon-settings.svg';
import DefaultBasilIcon from '../../assets/basil-logo.svg';

interface HeaderProps {
  setViewMode: (mode: 'IR' | 'CFG' | 'IR/CFG') => void;
  viewMode: 'IR' | 'CFG' | 'IR/CFG';
  toggleSettings: () => void;
  customLogoFile: File | null;
}

export function Header({
  setViewMode,
  viewMode,
  toggleSettings,
  customLogoFile,
}: HeaderProps) {
  const logoPreviewUrl = useLogoUrl(customLogoFile);

  return (
    <header className="app-header">
      <div className="header-information">
        {logoPreviewUrl ? (
          <img
            src={logoPreviewUrl}
            alt="Custom Basil Logo"
            className="header-logo"
          />
        ) : (
          <DefaultBasilIcon className="header-logo" />
        )}

        <h1 className="header-title">
          BASIL IR
          <br />
          Comparison
        </h1>
      </div>

      <div className="display-type-container">
        <h1 className="header-selection">Display Type</h1>

        <div className="toggle-group" style={{ transform: 'translateY(-50%)' }}>
          <button
            className={`toggle-button ${viewMode === 'IR' ? 'active' : ''}`}
            onClick={() => setViewMode('IR')}
          >
            {viewMode === 'IR' && <span className="tick">✓</span>}
            IR-IR
          </button>
          <button
            className={`toggle-button ${viewMode === 'CFG' ? 'active' : ''}`}
            onClick={() => setViewMode('CFG')}
          >
            {viewMode === 'CFG' && <span className="tick">✓</span>}
            CFG-CFG
          </button>
          <button
            className={`toggle-button ${viewMode === 'IR/CFG' ? 'active' : ''}`}
            onClick={() => setViewMode('IR/CFG')}
          >
            {viewMode === 'IR/CFG' && <span className="tick">✓</span>}
            IR-CFG
          </button>
        </div>
      </div>

      <button
        className="settings-button"
        aria-label="Settings "
        onClick={toggleSettings}
      >
        <SettingsIcon className="settings-icon" />
      </button>
    </header>
  );
}
