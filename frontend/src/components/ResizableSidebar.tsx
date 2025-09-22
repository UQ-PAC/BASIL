import { useState, useCallback, useRef, type ReactNode } from 'react';
import '../styles/sidebar.css';

import RightArrow from '../assets/arrow-right-icon.svg';

const INITIAL_WIDTH = 250;
const MAX_WIDTH = 500;
const MIN_WIDTH = 100;
const COLLAPSED_WIDTH = 20;

interface ResizableSidebarProps {
    children: ReactNode;
    isSidebarMinimised: boolean;
    toggleSidebar: () => void;
    initialWidth?: number;
    minWidth?: number;
    maxWidth?: number;
}

export function ResizableSidebar({
                                     children,
                                     isSidebarMinimised,
                                     toggleSidebar,
                                     initialWidth = INITIAL_WIDTH,
                                     minWidth = MIN_WIDTH,
                                     maxWidth = MAX_WIDTH
                                 }: ResizableSidebarProps) {
    const [width, setWidth] = useState(initialWidth);
    const sidebarRef = useRef<HTMLDivElement>(null);
    const isResizing = useRef(false);

    const resize = useCallback((event: MouseEvent) => {
        if (sidebarRef.current && !isSidebarMinimised) {
            const newWidth = event.clientX;

            if (newWidth < minWidth) {
                toggleSidebar();

                setWidth(minWidth);

                document.removeEventListener('mousemove', resize);
                document.removeEventListener('mouseup', stopResizing);
                isResizing.current = false;
                return;
            }

            const clampedWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
            setWidth(clampedWidth);
        }
    }, [minWidth, maxWidth, isSidebarMinimised, toggleSidebar]);

    const stopResizing = useCallback(() => {
        isResizing.current = false;
        document.removeEventListener('mousemove', resize);
        document.removeEventListener('mouseup', stopResizing);
    }, [resize]);

    const startResizing = useCallback(() => {
        if (isSidebarMinimised) {
            toggleSidebar();
            setWidth(width > minWidth ? width : minWidth);
        }

        isResizing.current = true;
        document.addEventListener('mousemove', resize);
        document.addEventListener('mouseup', stopResizing);
    }, [resize, stopResizing, isSidebarMinimised, toggleSidebar, width, minWidth]);

    const handleContainerClick = useCallback(() => {
        if (isSidebarMinimised) {
            toggleSidebar();
            setWidth(width > minWidth ? width : MIN_WIDTH + 10);
        }
    }, [isSidebarMinimised, toggleSidebar, width, minWidth]);

    return (
        <div
            className={`resizable-sidebar-container ${isSidebarMinimised ? 'sidebar-minimized' : ''}`}
            style={{ width: isSidebarMinimised ? `${COLLAPSED_WIDTH}px` : `${width}px`, cursor: isSidebarMinimised ? 'pointer' : 'default'  }}
            ref={sidebarRef}
            onClick={isSidebarMinimised ? handleContainerClick : undefined}
        >
            {isSidebarMinimised && (
                <RightArrow className="arrow-right-icon" ></RightArrow>
            )}

            {!isSidebarMinimised && (
                <>
                    {children}
                    <div className="resize-handle" onMouseDown={startResizing}></div>
                </>
            )}

        </div>
    );
}