// src/components/CustomNode.tsx
import '../../../styles/prism-ir-theme.css';
import React, { memo, useEffect, useRef } from 'react';
import {
  Handle,
  Position,
  useUpdateNodeInternals,
  type Node,
  type NodeProps,
} from '@xyflow/react';

import '../../../styles/components/viewers/graph/custom-node.css';

declare const Prism: any;

export interface CustomNodeData {
  header: string;
  fullContent: string;
  headerWidth: number;
  headerHeight: number;
  fullContentWidth: number;
  fullContentHeight: number;
  nodeBorderColour?: string;
  isExpanded: boolean;
  originalExpanded?: boolean;
  [key: string]: unknown; // Index signature to satisfy Record<string, unknown> constraint
}

type MyNodeType = Node<CustomNodeData>;

const CustomNode: React.FC<NodeProps<MyNodeType>> = memo(
  ({ id, data, selected }) => {
    const updateNodeInternals = useUpdateNodeInternals();
    const codeRef = useRef<HTMLElement>(null);

    const isExpanded = data.isExpanded;

    useEffect(() => {
      updateNodeInternals(id);
    }, [isExpanded, id, updateNodeInternals]);

    useEffect(() => {
      if (!isExpanded || !data.fullContent || !codeRef.current) return;

      try {
        const highlighted = Prism.highlight(
          data.fullContent,
          Prism.languages.ir,
          'ir'
        );
        codeRef.current.innerHTML = highlighted;
      } catch (e) {
        console.error('Prism.highlight failed in CustomNode: ', e);
        codeRef.current.textContent = data.fullContent;
      }
    }, [isExpanded, data.fullContent]);

    const width = isExpanded ? data.fullContentWidth : data.headerWidth;
    const height = isExpanded ? data.fullContentHeight : data.headerHeight;

    const nodeStyle: React.CSSProperties = {
      width,
      height,
      border: `3px solid ${data.nodeBorderColour || '#777'}`,
      backgroundColor: selected ? '#fbfbfb' : '#FFF',
      boxShadow: selected
        ? '0 0 0 3px rgba(0, 123, 255, 0.4)'
        : '0 2px 5px rgba(0,0,0,0.1)',
      overflow: isExpanded ? 'auto' : 'hidden',
      whiteSpace: isExpanded ? 'pre-wrap' : 'nowrap',
      textOverflow: isExpanded ? 'clip' : 'ellipsis',
    };

    const contentStyle: React.CSSProperties = {
      textAlign: isExpanded ? 'left' : 'center',
    };

    return (
      <div className="custom-flow-node" style={nodeStyle}>
        <Handle type="target" position={Position.Top} />

        <div className="custom-node-header-text" style={contentStyle}>
          {isExpanded ? (
            <pre className="language-ir" style={{ margin: 0 }}>
              <code ref={codeRef} className="language-ir" />
            </pre>
          ) : (
            <span>{data.header}</span>
          )}
        </div>

        <Handle type="source" position={Position.Bottom} />
      </div>
    );
  }
);

CustomNode.displayName = 'CustomNode';

export default CustomNode;
