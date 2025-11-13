// src/hooks/useDiffViewerLogic.ts
import { useLayoutEffect, useRef, useState } from 'react';
import * as Diff from 'diff';
import { Diff2HtmlUI } from 'diff2html/lib/ui/js/diff2html-ui-slim';
import { ColorSchemeType } from 'diff2html/lib/types';
import Prism from 'prismjs';
import { useIrDiffData } from './useIrDiffData';

export function useDiffViewer(
  selectedStartEpoch: string | null,
  selectedEndEpoch: string | null,
  theme: string | null
) {
  const [contextLines, setContextLines] = useState(5);
  const [outputFormat, setOutputFormat] = useState<
    'side-by-side' | 'line-by-line'
  >('side-by-side');
  const [headerElement, setHeaderElement] = useState<HTMLElement | null>(null);
  const diffContainerRef = useRef<HTMLDivElement>(null);

  const { irData, isLoading, error } = useIrDiffData(
    selectedStartEpoch,
    selectedEndEpoch
  );

  const scrollToLine = (lineNumber: number) => {
    const container = diffContainerRef.current;
    if (!container) return;

    const lineNumbers = container.querySelectorAll(
      '.d2h-code-side-linenumber, .d2h-code-linenumber .line-num2'
    );
    let targetLine: Element | null = null;

    lineNumbers.forEach((td) => {
      if (td.textContent?.trim() === String(lineNumber)) {
        targetLine = td;
      }
    });

    const scrollTo = (el: Element | null) => {
      const row = el?.closest('tr');
      (row ?? el)?.scrollIntoView({ behavior: 'smooth', block: 'center' });
    };

    if (targetLine) scrollTo(targetLine);
    else console.warn(`Line ${lineNumber} not found in diff.`);
  };

  useLayoutEffect(() => {
    const diffContainer = diffContainerRef.current;
    if (!irData?.before || !irData?.after || !diffContainer) return;

    diffContainer.innerHTML = '';

    const diffText = Diff.createTwoFilesPatch(
      'IR-Before',
      'IR-After',
      irData.before,
      irData.after,
      `IR Before Transform (${irData.epochName || 'N/A'})`,
      `IR After Transform (${irData.epochName || 'N/A'})`,
      { context: contextLines }
    );

    const scheme =
      theme === 'dark'
        ? ColorSchemeType.DARK
        : theme === 'light'
          ? ColorSchemeType.LIGHT
          : ColorSchemeType.AUTO;

    diffContainer.innerHTML = '';
    const ui = new Diff2HtmlUI(diffContainer, diffText, {
      drawFileList: false,
      outputFormat,
      matching: 'lines',
      highlight: false,
      colorScheme: scheme,
    });

    requestAnimationFrame(() => {
      try {
        ui.draw();

        // Header
        const header = diffContainer.querySelector('.d2h-file-header');
        if (header instanceof HTMLElement && header !== headerElement) {
          setHeaderElement(header);
        }

        // Syntax highlighting
        const lines = diffContainer.querySelectorAll('.d2h-code-line-ctn');
        lines.forEach((line) => {
          if (line.querySelector('.token')) return; // skip if already highlighted

          const text = line.textContent ?? '';
          line.innerHTML = Prism.highlight(text, Prism.languages.ir, 'ir');
        });
      } catch (err) {
        console.error('Error rendering diff:', err);
      }
    });
  }, [irData, contextLines, outputFormat, theme]);

  return {
    diffContainerRef,
    headerElement,
    outputFormat,
    setOutputFormat,
    contextLines,
    setContextLines,
    irData,
    isLoading,
    error,
    scrollToLine,
  };
}
