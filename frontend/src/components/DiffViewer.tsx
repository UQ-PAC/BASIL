import {useEffect, useLayoutEffect, useRef, useState} from 'react';
import {Diff2HtmlUI} from 'diff2html/lib/ui/js/diff2html-ui-slim';
import 'diff2html/bundles/css/diff2html.min.css';
import * as Diff from 'diff';
import Prism from 'prismjs';
import 'prismjs/themes/prism.css';
import '../styles/code-theme.css';
import '../styles/diff-viewer.css';
import '../lib/prism-ir';
import '../styles/button-selection.css';
import {ColorSchemeType} from "diff2html/lib/types";
import {DiffControls} from "./DiffControls.tsx";
import {API_BASE_URL} from '../api';

interface ProcedureLocation {
    name: string;
    startLine: number;
    approxEndLine: number;
}

interface IREpochData {
    before: string;
    after: string;
    procedures: ProcedureLocation[];
    epochName: string;
}

interface DiffViewerProps {
    selectedEpochName: string | null;
    theme: string | null;
}

export function DiffViewer({ selectedEpochName, theme }: DiffViewerProps) {
    const [irData, setIrData] = useState<IREpochData | null>(null);
    const [contextLines, setContextLines] = useState(5);
    const [outputFormat, setOutputFormat] = useState<'side-by-side' | 'line-by-line'>('side-by-side');
    const [headerElement, setHeaderElement] = useState<HTMLElement | null>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const diffContainerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (!selectedEpochName) {
            setIrData(null);
            setLoading(false);
            setError(null);
            return;
        }

        const fetchIRDataForEpoch = async (epochName: string) => {
            try {
                setLoading(true);
                setError(null);

                const [beforeResponse, afterResponse, proceduresResponse] = await Promise.all([
                    fetch(`${API_BASE_URL}/ir/${epochName}/before`),
                    fetch(`${API_BASE_URL}/ir/${epochName}/after`),
                    fetch(`${API_BASE_URL}/ir/${epochName}/procedures_with_lines`)
                ]);

                if (!beforeResponse.ok) throw new Error(`HTTP error fetching before IR for ${epochName}! status: ${beforeResponse.status}`);
                if (!afterResponse.ok) throw new Error(`HTTP error fetching after IR for ${epochName}! status: ${afterResponse.status}`);
                if (!proceduresResponse.ok) throw new Error(`HTTP error fetching procedures! status: ${proceduresResponse.status}`);

                const beforeText: string = await beforeResponse.text();
                const afterText: string = await afterResponse.text();
                const proceduresData: ProcedureLocation[] = await proceduresResponse.json();

                console.log(`Fetched data for epoch: ${epochName}`);

                setIrData({
                    before: beforeText,
                    after: afterText,
                    procedures: proceduresData,
                    epochName: epochName
                });

            } catch (err: any) {
                console.error(`Error fetching analysis data for ${epochName}:`, err);
                setError(`Error fetching data for ${epochName}: ${err.message}`);
                setIrData(null);
            } finally {
                setLoading(false);
            }
        };

        fetchIRDataForEpoch(selectedEpochName); // Call the fetch function
    }, [selectedEpochName]);


    const scrollToLine = (lineNumber: number) => {

        if (diffContainerRef.current) {
            const lineNumbers = diffContainerRef.current.querySelectorAll(
                '.d2h-code-side-linenumber, .d2h-code-linenumber .line-num2'
            );
            let targetLineElement = null;
            lineNumbers.forEach(td => {
                // @ts-ignore --> // TODO: Might be null
                if (td.textContent.trim() === String(lineNumber)) {
                    targetLineElement = td;
                }
            });

            if (targetLineElement) {
                // @ts-ignore
                const row = targetLineElement.closest('tr');
                if (row) {
                    row.scrollIntoView({ behavior: 'smooth', block: 'center' }); // center the line in the viewport
                } else {
                    // Fallback if the closest 'tr' isn't found for some reason
                    // @ts-ignore
                    targetLineElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
                }
            } else {
                console.warn(`Line number ${lineNumber} not found in the diff view.`);
                // --- Start of logic for "go to the line above it that exists" ---
                // Find the closest available line number less than the target
                let closestExistingLine = -1;
                lineNumbers.forEach(td => {
                    const currentTdNumber = parseInt(td.textContent?.trim() || '0', 10);
                    if (!isNaN(currentTdNumber) && currentTdNumber < lineNumber && currentTdNumber > closestExistingLine) {
                        closestExistingLine = currentTdNumber;
                    }
                });

                if (closestExistingLine !== -1) {
                    console.log(`Scrolling to closest existing line: ${closestExistingLine}`);
                    // Now find the actual DOM element for this closest existing line
                    let fallbackTargetElement: Element | null = null;
                    for (let i = 0; i < lineNumbers.length; i++) {
                        const td = lineNumbers[i];
                        if (td.textContent?.trim() === String(closestExistingLine)) {
                            fallbackTargetElement = td;
                            break;
                        }
                    }

                    if (fallbackTargetElement) {
                        const row = fallbackTargetElement.closest('tr');
                        if (row) {
                            row.scrollIntoView({ behavior: 'smooth', block: 'center' });
                        } else {
                            fallbackTargetElement.scrollIntoView({ behavior: 'smooth', block: 'center' });
                        }
                        // TODO: Add notification here that the exact line was hidden/not found
                        // Example: alert(`Line ${lineNumber} is hidden. Showing line ${closestExistingLine} instead.`);
                    }
                } else {
                    console.warn(`No suitable line found to scroll to, neither exact nor a previous one.`);
                }
            }
        }
    };

    useEffect(() => {
        console.log(`Context lines changed to: ${contextLines}`);
    }, [contextLines]);

    useEffect(() => {
        console.log(`Output format switched to: ${outputFormat}`);
    }, [outputFormat]);

    useLayoutEffect(() => {
        const diffContainer = diffContainerRef.current; // Use the ref directly

        // Only render diff if data is available and not in loading/error state
        if (!irData || !irData.before || !irData.after || !diffContainer) {
            const targetElement = document.getElementById('diff-container');
            if (targetElement) targetElement.innerHTML = '';
            return;
        }

        const diffText = Diff.createTwoFilesPatch(
            'IR-Before',
            'IR-After',
            irData?.before,
            irData?.after,
            `IR Before Transform (${irData?.epochName || 'N/A'})`,
            `IR After Transform (${irData?.epochName || 'N/A'})`,
            {
                context: contextLines,
            }
        );
        // console.log("Generated diffText:", diffText);

        let diffColorScheme = ColorSchemeType.AUTO;
        if (theme == 'dark') {
            diffColorScheme = ColorSchemeType.DARK;
        } if (theme == 'light') {
            diffColorScheme = ColorSchemeType.LIGHT;
        }

        diffContainer.innerHTML = '';
        const ui = new Diff2HtmlUI(diffContainer, diffText, {
            drawFileList: false,
            outputFormat,
            matching: 'lines',
            highlight: false,
            colorScheme: diffColorScheme,
        });

        requestAnimationFrame(() => {
            try {

                ui.draw();
                console.log("useLayoutEffect: Diff2HtmlUI.draw() executed.");

                // Grab header for portal
                const currentHeader = diffContainer.querySelector('.d2h-file-header');
                if (currentHeader instanceof HTMLElement && currentHeader !== headerElement) {
                    setHeaderElement(currentHeader);
                    console.log("useLayoutEffect: Header element found and set.");
                } else if (!currentHeader && headerElement) {
                    setHeaderElement(null);
                    console.log("useLayoutEffect: Header element cleared.");
                }

                requestAnimationFrame(() => { // TODO: Nested good?
                    const lines = diffContainer.querySelectorAll('.d2h-code-line-ctn');
                    lines.forEach(line => {
                        const ins = line.querySelector('ins');
                        const del = line.querySelector('del');

                        const applyHighlight = (el: HTMLElement | null) => {
                            if (!el) return;
                            const raw = el.textContent ?? '';
                            // Highlight raw text and replace content inline
                            el.innerHTML = Prism.highlight(raw, Prism.languages.ir, 'ir');
                        };

                        if (ins || del) {
                            applyHighlight(ins as HTMLElement);
                            applyHighlight(del as HTMLElement);
                        } else {
                            applyHighlight(line as HTMLElement);
                        }
                    });
                });
            } catch (error) {
                console.error("Error during diff rendering or highlighting:", error);
            }
        });
    }, [irData, contextLines, outputFormat, theme]);

    // --- Conditional rendering: these checks must come AFTER all hooks ---
    if (loading) {
        return <div className="p-4 text-center">Loading diff...</div>;
    }

    if (error) {
        return <div className="p-4 text-center text-red-500">Error: {error}</div>;
    }

    if (!selectedEpochName) {
        return <div className="flex-1 p-4 text-center">Please select an epoch from the sidebar.</div>;
    }

    return (
        <>
            <div id="diff" ref={diffContainerRef} />
            <DiffControls
                headerElement={headerElement}
                outputFormat={outputFormat}
                setOutputFormat={setOutputFormat}
                contextLines={contextLines}
                setContextLines={setContextLines}
                procedureList={irData?.procedures || []}
                onSelectProcedure={scrollToLine}
            />
        </>
    );
}