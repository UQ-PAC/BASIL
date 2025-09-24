// src/components/ErrorModal.tsx
import React from 'react';
import '../styles/modal-base.css';

interface ErrorModalProps {
  isOpen: boolean;
  postStatus: { message: string | null; type: string };
  onClose: () => void;
}

const ErrorModal: React.FC<ErrorModalProps> = ({
  isOpen,
  postStatus,
  onClose,
}) => {
  if (!isOpen || !postStatus.message) return null;

  const messageClass = `${postStatus.type}-text`;

  return (
    <div className="modal-overlay">
      <div className="modal-content-base">
        <div className="modal-header">
          <h2 className="modal-title">Analysis Error</h2>
          <button
            className="modal-close-button"
            aria-label="Close settings modal"
            onClick={onClose}
          >
            &times; {/* The image of an X */}
          </button>
        </div>
        <div className="error-message-body">
          <p className={`mb-4 ${messageClass}`}>{postStatus.message}</p>
        </div>
      </div>
    </div>
  );
};

export default ErrorModal;
