// src/components/CustomNode.tsx
import React, { memo } from 'react';
import { Handle, Position, type Node, type NodeProps } from '@xyflow/react';

// Export the CustomNodeData interface so CfgViewer.tsx can import it
export interface CustomNodeData {
    header: string;
    fullContent: string;
    [key: string]: unknown; // Index signature to satisfy Record<string, unknown> constraint
}

// Define the full Node type that uses your CustomNodeData
type MyNodeType = Node<CustomNodeData>;

// The component name should be CustomNode, matching the filename
const CustomNode: React.FC<NodeProps<MyNodeType>> = memo(({ data }) => {
    return (
        <div>
            {/* Handles for connecting edges */}
            <Handle type="target" position={Position.Top} />
            <div>{data.header}</div> {/* Display only the header line */}
            <Handle type="source" position={Position.Bottom} />
        </div>
    );
});

// The displayName should also match the component name
CustomNode.displayName = 'CustomNode';

export default CustomNode;