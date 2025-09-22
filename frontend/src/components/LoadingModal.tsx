// src/components/LoadingModal.tsx

import React from 'react';

import '../styles/modal-base.css'

import Spinner from '../assets/spinner-loading-icon.svg';

interface LoadingModalProps {
    isOpen: boolean;
    message?: string;
    postStatus: { message: string; type: string };
}

const LoadingModal: React.FC<LoadingModalProps> = ({ isOpen, message, postStatus }) => {
    if (!isOpen) return null;
    return (
        <div className="modal-overlay">
            <div className="modal-content-base">
                <header className="modal-header">
                    <h2 className="modal-title">{message || "Running Analysis..."}</h2>
                    {/* No close button on a non-interruptible loading modal */}
                </header>
                <div className="modal-body">
                    <Spinner className="spinner-icon" />
                    {postStatus.message && (
                        <div className={`status-message status-${postStatus.type}`}>
                            {postStatus.message}
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
};

export default LoadingModal;