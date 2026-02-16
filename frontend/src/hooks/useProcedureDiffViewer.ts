// src/hooks/useProcedureDiffViewer.ts
import { useState, useRef, useLayoutEffect } from 'react';
import * as Diff from 'diff';
import { Diff2HtmlUI } from 'diff2html/lib/ui/js/diff2html-ui-slim';
import 'diff2html/bundles/css/diff2html.min.css';
import { ColorSchemeType } from 'diff2html/lib/types';
import '../styles/prism-ir-theme.css';
import 'prismjs/components/prism-core';
import Prism from 'prismjs';
import type { ProcedureDiffItem } from './useHierarchicalIrDiff.ts';

export function useProcedureDiffViewer(
  selectedProcedure: ProcedureDiffItem | null,
  theme: string | null,
  contextLines: number,
  outputFormat: 'side-by-side' | 'line-by-line'
) {
  const diffContainerRef = useRef<HTMLDivElement>(null);
  const [headerElement, setHeaderElement] = useState<HTMLElement | null>(null);

  function highlightNodeWithPrism(node: ChildNode) {
    if (node.nodeType === Node.TEXT_NODE) {
      const span = document.createElement('span');
      span.innerHTML = Prism.highlight(
        node.textContent ?? '',
        Prism.languages.ir,
        'ir'
      );
      node.replaceWith(span);
    } else if (node.nodeType === Node.ELEMENT_NODE) {
      const el = node as HTMLElement;
      el.childNodes.forEach(highlightNodeWithPrism);
    }
  }

  useLayoutEffect(() => {
    if (!selectedProcedure) return;
    if (!diffContainerRef.current) return;

    const container = diffContainerRef.current;

    const beforeText = selectedProcedure.before ?? '';
    const afterText = selectedProcedure.after ?? '';

    const diffText = Diff.createTwoFilesPatch(
      'Before',
      'After',
      beforeText,
      afterText,
      selectedProcedure.metadata.name + ' (Before)',
      selectedProcedure.metadata.name + ' (After)',
      { context: contextLines }
    );

    const scheme =
      theme === 'dark'
        ? ColorSchemeType.DARK
        : theme === 'light'
          ? ColorSchemeType.LIGHT
          : ColorSchemeType.AUTO;

    container.innerHTML = '';
    const ui = new Diff2HtmlUI(container, diffText, {
      drawFileList: false,
      outputFormat: outputFormat,
      matching: 'lines',
      highlight: false,
      colorScheme: scheme,
    });

    requestAnimationFrame(() => {
      try {
        ui.draw();

        const header = container.querySelector('.d2h-file-header');
        if (header instanceof HTMLElement) setHeaderElement(header);

        const lines = container.querySelectorAll('.d2h-code-line-ctn');
        lines.forEach((line) => {
          line.childNodes.forEach(highlightNodeWithPrism);
        });
        console.log('selectedProcedure:', selectedProcedure);
      } catch (err) {
        console.error('Error rendering procedure diff:', err);
      }
    });
  }, [selectedProcedure, contextLines, outputFormat, theme]);

  return { diffContainerRef, headerElement };
}
