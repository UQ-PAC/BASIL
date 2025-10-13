import React, { useState } from 'react';
import '../styles/tooltip.css';

interface TooltipProps {
  children: React.ReactNode;
  content: string;
}

const Tooltip: React.FC<TooltipProps> = ({ children, content }) => {
  const [isVisible, setIsVisible] = useState(false);

  return (
    <div
      className="tooltip-container"
      onMouseEnter={() => setIsVisible(true)}
      onMouseLeave={() => setIsVisible(false)}
    >
      {children}
      <div className={`tooltip-content ${isVisible ? 'visible' : ''}`}>
        {content}
      </div>
    </div>
  );
};

export default Tooltip;
