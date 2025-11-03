// src/components/modals/SettingsModal.tsx
import '../../styles/components/modals/modal-base.css';

interface SettingsModalProps {
  isOpen: boolean;
  onClose: () => void;
  theme: 'light' | 'dark';
  setTheme: (theme: 'light' | 'dark') => void;
}

function SettingsModal({
  isOpen,
  onClose,
  theme,
  setTheme,
}: SettingsModalProps) {
  if (!isOpen) return null;

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
        </div>
      </div>
    </div>
  );
}

export default SettingsModal;
