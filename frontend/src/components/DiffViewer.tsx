import {useEffect, useLayoutEffect, useState} from 'react';
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

export function DiffViewer() {
    const [irBefore, setIrBefore] = useState('');
    const [irAfter, setIrAfter] = useState('');
    const [contextLines, setContextLines] = useState(5);
    const [outputFormat, setOutputFormat] = useState<'side-by-side' | 'line-by-line'>('side-by-side');
    const [headerElement, setHeaderElement] = useState<HTMLElement | null>(null);

    useEffect(() => {
        Promise.all([
            fetch('/ir-before').then(res => res.text()),
            fetch('/ir-after').then(res => res.text()),
        ]).then(([before, after]) => {
            setIrBefore(before.replace(/\\n/g, '\n'));
            setIrAfter(after.replace(/\\n/g, '\n'));
        });
    }, []);

    useLayoutEffect(() => { // TODO: Why is useLayoutEffect better?
        if (!irBefore || !irAfter) return;

        const diffText = Diff.createTwoFilesPatch(
            'IR-Before',
            'IR-After',
            irBefore,
            irAfter,
            '',
            '',
            { context: contextLines }
        );

        const diffContainer = document.getElementById('diff');
        if (!diffContainer) return;

        diffContainer.innerHTML = ''; // clear old diff
        const ui = new Diff2HtmlUI(diffContainer, diffText, {
            drawFileList: false,
            outputFormat,
            matching: 'lines',
            highlight: false,
            colorScheme: ColorSchemeType.AUTO, // TODO: End result I want auto
        });

        ui.draw();

        // Grab header for portal
        const header = diffContainer.querySelector('.d2h-file-header');
        if (header instanceof HTMLElement) {
            setHeaderElement(header);  // React state update triggers re-render
        }

        // Prism syntax custom highlighting
        setTimeout(() => {
            const lines = diffContainer.querySelectorAll('.d2h-code-line-ctn');
            lines.forEach(line => {
                const ins = line.querySelector('ins');
                const del = line.querySelector('del');

                if (ins || del) {
                    if (ins) {
                        const raw = ins.textContent ?? '';
                        ins.innerHTML = Prism.highlight(raw, Prism.languages.ir, 'ir');
                    }
                    if (del) {
                        const raw = del.textContent ?? '';
                        del.innerHTML = Prism.highlight(raw, Prism.languages.ir, 'ir');
                    }
                } else {
                    const raw = line.textContent ?? '';
                    line.innerHTML = Prism.highlight(raw, Prism.languages.ir, 'ir');
                }
            });
        }, 0);

    }, [irBefore, irAfter, contextLines, outputFormat]);

    return (
        <>
            <div id="diff" />
            <DiffControls
                headerElement={headerElement}
                outputFormat={outputFormat}
                setOutputFormat={setOutputFormat}
                contextLines={contextLines}
                setContextLines={setContextLines}
            />
        </>
    );
}