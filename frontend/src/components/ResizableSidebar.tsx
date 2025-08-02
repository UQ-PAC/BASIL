import { useState, useCallback, useRef, type ReactNode } from 'react';

const INITIAL_WIDTH = 250;
const MAX_WIDTH = 500;
const MIN_WIDTH = 80;

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
            const clampedWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
            setWidth(clampedWidth);
        }
    }, [minWidth, maxWidth, isSidebarMinimised]);

    const stopResizing = useCallback(() => {
        isResizing.current = false;
        document.removeEventListener('mousemove', resize);
        document.removeEventListener('mouseup', stopResizing);
    }, [resize]);

    const startResizing = useCallback(() => {
        isResizing.current = true;
        document.addEventListener('mousemove', resize);
        document.addEventListener('mouseup', stopResizing);
    }, [resize, stopResizing]);

    return (
        <div
            className={`resizable-sidebar-container ${isSidebarMinimised ? 'sidebar-minimized' : ''}`}
            style={{ width: isSidebarMinimised ? '0px' : `${width}px` }}
            ref={sidebarRef}
        >
            <button onClick={toggleSidebar} className="collapse-button">
                {isSidebarMinimised ? (
                    <img
                        src="/icon-right-50.png"
                        alt="Expand Sidebar"
                        className="collapse-icon"
                    />
                ) : (
                    <img
                        src="/icon-left-50.png"
                        alt="Collapse Sidebar"
                        className="collapse-icon"
                    />
                )}
            </button>
            {children}
            <div className="resize-handle" onMouseDown={startResizing}></div>
        </div>
    );
}