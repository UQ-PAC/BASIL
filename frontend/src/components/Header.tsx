import '../styles/header.css';

import SettingsIcon from '../assets/icon-settings.svg';

interface HeaderProps {
    setViewMode: (mode: 'IR' | 'CFG' | 'IR/CFG') => void;
    viewMode: 'IR' | 'CFG' | 'IR/CFG';
}

export function Header({ setViewMode, viewMode }: HeaderProps) {
    return (
        <header className="app-header">
            <div className="header-information">
                <img className="header-logo" src="/BASIL_logo.png" alt="Basil" />
                <h1 className="header-title">
                    BASIL IR
                    <br />
                    Comparison
                </h1>
            </div>

            <div className="display-type-container">
                <h1 className="header-selection">Display Type</h1>

                <div
                    className="toggle-group"
                    style={{ transform: 'translateY(-50%)' }}
                    >
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

            <button className="settings-button" aria-label="Settings">
                 <SettingsIcon className="settings-icon" />
            </button>
        </header>
    );
}