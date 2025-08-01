// src/components/CustomBackground.tsx
import React, { type FC, useMemo } from 'react';
import { useReactFlow, useOnViewportChange } from '@xyflow/react';

interface CustomBackgroundProps {
    gap?: number;
    size?: number;
    dotsColor?: string;
}

const CustomBackground: FC<CustomBackgroundProps> = ({ gap = 12, size = 1, dotsColor = '#91919a' }) => {
    const { getViewport } = useReactFlow();

    const [viewport, setViewport] = React.useState(getViewport());

    useOnViewportChange({
        onChange: (viewport) => {
            setViewport(viewport);
        },
    });

    const backgroundStyle = useMemo(() => {
        const dotGap = gap;
        const dotSize = size;

        const scaledX = viewport.x * viewport.zoom;
        const scaledY = viewport.y * viewport.zoom;
        return {
            backgroundImage: `radial-gradient(${dotsColor} ${dotSize}px, transparent ${dotSize}px)`,
            backgroundSize: `${dotGap}px ${dotGap}px`,
            backgroundPosition: `${scaledX}px ${scaledY}px`,
        };
    }, [viewport, gap, size, dotsColor]);

    return (
        <div
            className="custom-background"
            style={{
                ...backgroundStyle,
                position: 'absolute',
                top: 0,
                left: 0,
                right: 0,
                bottom: 0,
                zIndex: 0,
                pointerEvents: 'none',
            }}
        />
    );
};

export default CustomBackground;