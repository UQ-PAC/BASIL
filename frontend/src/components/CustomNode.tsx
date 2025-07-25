// src/components/CustomNode.tsx
import React, {memo, useEffect, useState, useRef} from 'react';
import { Handle, Position, useUpdateNodeInternals, type Node, type NodeProps } from '@xyflow/react';

declare const Prism: any;

export interface CustomNodeData {
    header: string;
    fullContent: string;
    headerWidth: number;
    headerHeight: number;
    fullContentWidth: number;
    fullContentHeight: number;
    nodeBackgroundColor?: string;
    [key: string]: unknown; // Index signature to satisfy Record<string, unknown> constraint
}

type MyNodeType = Node<CustomNodeData>;

const CustomNode: React.FC<NodeProps<MyNodeType>> = memo(({ id, data, selected }) => {
    const [isExpanded, setIsExpanded] = useState(false);
    const updateNodeInternals = useUpdateNodeInternals();
    const contentRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        updateNodeInternals(id);
    }, [isExpanded, id, updateNodeInternals]);

    useEffect(() => {
        if (isExpanded && contentRef.current && Prism) {
            const highlightedHtml = Prism.highlight(data.fullContent, Prism.languages.ir, 'ir');
            contentRef.current.innerHTML = highlightedHtml;
        } else if (contentRef.current && !isExpanded) {
            contentRef.current.textContent = data.header;
        }
    }, [isExpanded, data.fullContent, data.header]);

    const handleDoubleClick = () => {
        setIsExpanded(prev => !prev);
    };

    const currentWidth = isExpanded ? data.fullContentWidth : data.headerWidth;
    const currentHeight = isExpanded ? data.fullContentHeight : data.headerHeight;

    const nodeStyle: React.CSSProperties = {
        width: currentWidth,
        height: currentHeight,
        border: `2px solid ${data.nodeBackgroundColor || '#777'}`,
        backgroundColor: selected ? '#e3e3e3' : '#FFF',
        whiteSpace: isExpanded ? 'pre-wrap' : 'nowrap',
        flexGrow: 1,
        overflow: isExpanded ? 'auto' : 'hidden',
        textOverflow: isExpanded ? 'clip' : 'ellipsis',
    };

    const nodeClassName = `custom-flow-node ${isExpanded ? 'custom-flow-node--expanded' : ''}`;

    return (
        <div className={nodeClassName} style={nodeStyle} onDoubleClick={handleDoubleClick}>
            <Handle type="target" position={Position.Top} />
            <div className="custom-node-header-text" ref={contentRef}>
                {data.header}
            </div>
            <Handle type="source" position={Position.Bottom} />
        </div>
    );
});

CustomNode.displayName = 'CustomNode';

export default CustomNode;
