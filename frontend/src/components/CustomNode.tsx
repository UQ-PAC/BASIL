// src/components/CustomNode.tsx
import React, {memo, useEffect, useState, useRef} from 'react';
import { Handle, Position, useUpdateNodeInternals, type Node, type NodeProps } from '@xyflow/react';

import '../styles/custom-node.css';

declare const Prism: any;

export interface CustomNodeData {
    header: string;
    fullContent: string;
    headerWidth: number;
    headerHeight: number;
    fullContentWidth: number;
    fullContentHeight: number;
    nodeBorderColor?: string;
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
        if (!contentRef.current) return;

        contentRef.current.textContent = isExpanded ? data.fullContent : data.header;

        if (isExpanded && data.fullContent && typeof Prism !== 'undefined' && Prism.languages && Prism.languages.ir) {
            try {
                requestAnimationFrame(() => {
                    const highlightedHtml = Prism.highlight(data.fullContent, Prism.languages.ir, 'ir');
                    contentRef.current!.innerHTML = highlightedHtml;
                })
            } catch (e) {
                console.error("Prism.highlight failed in CustomNode: ", e);
                contentRef.current.textContent = data.fullContent;
            }
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
        border: `2px solid ${data.nodeBorderColor || '#777'}`,
        backgroundColor: selected ? '#fbfbfb' : '#FFF',
        boxShadow: selected ? '0 0 0 3px rgba(0, 123, 255, 0.4)' : '0 2px 5px rgba(0,0,0,0.1)', // TODO: Maybe just use this as selected
        whiteSpace: isExpanded ? 'pre-wrap' : 'nowrap',
        overflow: isExpanded ? 'auto' : 'hidden',
        textOverflow: isExpanded ? 'clip' : 'ellipsis',
    };

    const textDivStyle: React.CSSProperties = {
        textAlign: isExpanded ? 'left' : 'center',
    };


    return (
        <div className="custom-flow-node" style={nodeStyle} onDoubleClick={handleDoubleClick}>
            <Handle type="target" position={Position.Top} />
            <div className="custom-node-header-text" ref={contentRef} style={textDivStyle} >
                {data.header}
            </div>
            <Handle type="source" position={Position.Bottom} />
        </div>
    );
});

CustomNode.displayName = 'CustomNode';

export default CustomNode;
