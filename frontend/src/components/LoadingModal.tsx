// src/components/LoadingModal.tsx

import React from 'react';

import '../styles/loading-modal.css'

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
            <div className="modal-content p-6 text-center">
                <h2 className="text-xl font-bold mb-4">{message || "Running Analysis..."}</h2>
                <Spinner className="spinner-icon" />
                {postStatus.message && ( // TODO: Maybe make this appear even upon failure
                    <div className={`status-message status-${postStatus.type}`}>
                        {postStatus.message}
                    </div>
                )}
            </div>
        </div>
    );
};

export default LoadingModal;