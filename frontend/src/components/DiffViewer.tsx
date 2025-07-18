import {useEffect, useLayoutEffect, useRef, useState} from 'react';
import {Diff2HtmlUI} from 'diff2html/lib/ui/js/diff2html-ui-slim';
import 'diff2html/bundles/css/diff2html.min.css';
import * as Diff from 'diff';
import Prism from 'prismjs';
import 'prismjs/themes/prism.css';
import '../styles/code-theme.css';
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

export function DiffViewer() {
    const [irBefore, setIrBefore] = useState('');
    const [irAfter, setIrAfter] = useState('');
    const [contextLines, setContextLines] = useState(5);
    const [outputFormat, setOutputFormat] = useState<'side-by-side' | 'line-by-line'>('side-by-side');
    const [headerElement, setHeaderElement] = useState<HTMLElement | null>(null);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [currentEpochName, setCurrentEpochName] = useState<string | null>(null);
    const [procedureList, setProcedureList] = useState<ProcedureLocation[]>([]);
    const diffContainerRef = useRef<HTMLDivElement>(null);

    // TODO: Maybe move elsewhere
    const normalizeText = (text: string) => {
        let normalized = text;
        normalized = normalized.replace(/\r\n|\r/g, '\n');
        normalized = normalized.split('\n').map(line => line.trimEnd()).join('\n');
        return normalized;
    };

    // --- EFFECT 1: Fetch all epoch names on initial mount ---
    useEffect(() => {
        const fetchIRData = async () => {
            try {
                setLoading(true);
                setError(null);
                setIrBefore(''); // Clear previous state
                setIrAfter('');
                setCurrentEpochName(null);

                // step 1: Fetch all epoch names
                const namesResponse = await fetch(`${API_BASE_URL}/epochs`); // TODO: Do I need these checks? Or should I remove
                if (!namesResponse.ok) throw new Error(`HTTP error fetching epoch names! status: ${namesResponse.status}`);
                const names: string[] = await namesResponse.json();

                if (names.length === 0) {
                    setError("No analysis epochs found.");
                    setLoading(false);
                    return; // Exit if no epochs
                }

                // Step 2: Set the first epoch name and fetch its IR data
                const firstEpoch = names[0]; // TODO: to be changed to just epoch name...
                setCurrentEpochName(firstEpoch); // This will trigger the useLayoutEffect later

                const [beforeResponse, afterResponse, proceduresResponse] = await Promise.all([
                    fetch(`${API_BASE_URL}/ir/${firstEpoch}/before`),
                    fetch(`${API_BASE_URL}/ir/${firstEpoch}/after`),
                    fetch(`${API_BASE_URL}/ir/${firstEpoch}/procedures_with_lines`)
                ]);

                if (!beforeResponse.ok) throw new Error(`HTTP error fetching before IR for ${firstEpoch}! status: ${beforeResponse.status}`);
                if (!afterResponse.ok) throw new Error(`HTTP error fetching after IR for ${firstEpoch}! status: ${afterResponse.status}`);
                if (!proceduresResponse.ok) throw new Error(`HTTP error fetching procedures! status: ${proceduresResponse.status}`);

                const beforeText: string = await beforeResponse.text();
                const afterText: string = await afterResponse.text();
                const proceduresData: ProcedureLocation[] = await proceduresResponse.json();

                console.log("Before IR:", beforeText);
                console.log("After IR:", afterText);
                console.log("Procedures Data:", proceduresData);

                const beforeTextSerialised = normalizeText(beforeText);
                const afterTextSerialised = normalizeText(afterText);

                // TODO: Could I just call the Diff code right here, make it cleaner...?

                setIrBefore(beforeTextSerialised);
                setIrAfter(afterTextSerialised);
                setProcedureList(proceduresData)

            } catch (err: any) { // Type 'any' for simpler error handling, refine as needed
                console.error("Error fetching analysis data:", err);
                setError(`Error fetching data: ${err.message}`);
                setIrBefore('');
                setIrAfter('');
                setCurrentEpochName(null);
            } finally {
                setLoading(false);
            }
        };

        fetchIRData().catch(err => { // TODO: Maybe I don't need this - Should chaneg this later
            console.error("Unhandled promise rejection in fetchIRData:", err);
        });
    }, []);

    useEffect(() => {
        console.log(`Context lines changed to: ${contextLines}`);
    }, [contextLines]);

    useEffect(() => {
        console.log(`Output format switched to: ${outputFormat}`);
    }, [outputFormat]);


    const scrollToLine = (lineNumber: number) => {

        if (diffContainerRef.current) {
            const lineNumbers = diffContainerRef.current.querySelectorAll('.d2h-code-side-linenumber');
            let targetLineElement = null;
            lineNumbers.forEach(td => {
                // Trim to remove leading/trailing spaces from the text content
                // @ts-ignore --> // TODO: Might be null
                if (td.textContent.trim() === String(lineNumber)) {
                    targetLineElement = td;
                    // You might need more logic here if line numbers can repeat or if
                    // you need to distinguish between left/right side or added/deleted
                    // based on other classes.
                } // TODO: This works, below doesn't
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
                // --- End of logic for "go to the line above it that exists" ---
            }
        }
    };

    // --- useLayoutEffect for rendering the diff ---
    useLayoutEffect(() => {
        const diffContainer = diffContainerRef.current; // Use the ref directly

        // Only render diff if data is available and not in loading/error state
        if (!currentEpochName || !irBefore || !irAfter || !diffContainer) {
            const targetElement = document.getElementById('diff-container');
            if (targetElement) targetElement.innerHTML = '';
            return;
        }

        const diffText = Diff.createTwoFilesPatch(
            'IR-Before',
            'IR-After',
            irBefore,
            irAfter,
            `IR Before Transform (${currentEpochName || 'N/A'})`, // Use currentEpochName here
            `IR After Transform (${currentEpochName || 'N/A'})`,
            {
                context: contextLines,
            }
        );

        diffContainer.innerHTML = '';
        const ui = new Diff2HtmlUI(diffContainer, diffText, {
            drawFileList: false,
            outputFormat,
            matching: 'lines',
            highlight: false,
            colorScheme: ColorSchemeType.AUTO, // TODO: End result I want auto
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
                } else if (!currentHeader && headerElement) { // If header disappears, set to null
                    setHeaderElement(null);
                    console.log("useLayoutEffect: Header element cleared.");
                }

                // Prism syntax custom highlighting
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
    }, [irBefore, irAfter, contextLines, outputFormat, currentEpochName]); // TODO: Header can't be here??? Why?

    // --- Conditional rendering: these checks must come AFTER all hooks ---
    if (loading) {
        return <div className="p-4 text-center">Loading diff...</div>;
    }

    if (error) {
        return <div className="p-4 text-center text-red-500">Error: {error}</div>;
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
                procedureList={procedureList} // Pass procedure list to controls
                onSelectProcedure={scrollToLine} // Pass the scroll function
            />
        </>
    );
}