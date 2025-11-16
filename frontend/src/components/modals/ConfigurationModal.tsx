// src/components/modals/ConfigurationModal.tsx
import React, { useEffect, useState } from 'react';
import { Info } from 'lucide-react';
import Tooltip from '../../ui/Tooltip.tsx';

interface ConfigurationModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (path: string) => Promise<void>;
  initialPath: string;
  hasDatabaseLoaded: boolean;
}

function ConfigurationModal({
  isOpen,
  onClose,
  onSubmit,
  initialPath,
  hasDatabaseLoaded,
}: ConfigurationModalProps) {
  const [directoryPath, setDirectoryPath] = useState<string>(initialPath);

  useEffect(() => {
    if (!isOpen) {
      setDirectoryPath(initialPath);
    }
  }, [initialPath, isOpen]);

  if (!isOpen) return null;

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    await onSubmit(directoryPath);
    onClose();
  };

  const isCloseLocked = !hasDatabaseLoaded;

  if (!isOpen) return null;

  return (
    <div className="modal-overlay">
      <div className="modal-content-base">
        <div className="modal-header">
          <h2 className="modal-title">Configuration File Selection</h2>
          <button
            className="modal-close-button"
            aria-label="Close configeration modal"
            onClick={onClose}
            disabled={isCloseLocked}
          >
            <Tooltip
              content={
                isCloseLocked
                  ? 'Configuration is mandatory before continuing.'
                  : 'Close configuration modal'
              }
            >
              &times; {/* The image of an X */}
            </Tooltip>
          </button>
        </div>

        <form onSubmit={handleSubmit} className="modal-body directory-form">
          <div className="directory-label-icon-group">
            <label htmlFor="path-input" className="directory-input-label">
              Configuration Directory Path
            </label>
            <Tooltip content="Enter the FULL, absolute path to the directory or file. This action requires a restart of the analysis process.">
              <Info />
            </Tooltip>
          </div>
          <div>
            <textarea
              id="path-input"
              rows={3}
              value={directoryPath}
              onChange={(e) => setDirectoryPath(e.target.value)}
              className="directory-input-field"
              placeholder="/Users/user/project/analysis/clang/file.gts"
            />
          </div>

          <div className="action-buttons-group">
            <button
              type="submit"
              className="submit-button"
              disabled={!directoryPath.trim()}
            >
              Set Path & Start Analysis
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}

export default ConfigurationModal;
