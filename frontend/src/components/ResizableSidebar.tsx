import { useState, useCallback, useRef, type ReactNode } from 'react';

interface ResizableSidebarProps {
    children: ReactNode;
    initialWidth?: number;
    minWidth?: number;
    maxWidth?: number;
}

const INITIAL_WIDTH = 250;
const MAX_WIDTH = 500;
const MIN_WIDTH = 80;

export function ResizableSidebar({
                                     children,
                                     initialWidth = INITIAL_WIDTH,
                                     minWidth = MIN_WIDTH,
                                     maxWidth = MAX_WIDTH
                                 }: ResizableSidebarProps) {
    const [width, setWidth] = useState(initialWidth);
    const sidebarRef = useRef<HTMLDivElement>(null);

    const resize = useCallback((event: MouseEvent) => {
        if (sidebarRef.current) {
            const newWidth = event.clientX;
            const clampedWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
            setWidth(clampedWidth);
        }
    }, [minWidth, maxWidth]);

    const stopResizing = useCallback(() => {
        document.removeEventListener('mousemove', resize);
        document.removeEventListener('mouseup', stopResizing);
    }, [resize]);

    const startResizing = useCallback(() => {
        document.addEventListener('mousemove', resize);
        document.addEventListener('mouseup', stopResizing);
    }, [resize, stopResizing]);

    return (
        <div
            className="resizable-sidebar-container"
            style={{ width: `${width}px` }}
            ref={sidebarRef}
        >
            {children}
            <div className="resize-handle" onMouseDown={startResizing}></div>
        </div>
    );
}