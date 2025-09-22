// src/components/ErrorModal.tsx
import React from 'react';
import '../styles/modal-base.css'

interface ErrorModalProps {
    isOpen: boolean;
    errorMessage: string | null;
    onClose: () => void;
}

const ErrorModal: React.FC<ErrorModalProps> = ({ isOpen, errorMessage, onClose }) => {
    if (!isOpen || !errorMessage) return null;

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
                        &times; { /* The image of an X */ }
                    </button>
                </div>
                <div className="error-message-body">
                    <p className="mb-4">{errorMessage}</p>
                </div>
            </div>
        </div>
    );
};

export default ErrorModal;