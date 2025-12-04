import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function patchSolvers() {
    const rootDir = path.resolve(__dirname, '..');
    console.log(`Searching for solvers in ${rootDir}...`);

    const manualSolvers = await glob(path.join(rootDir, 'Manual', '*/runMe_ai.sh'));
    const aiSolvers = await glob(path.join(rootDir, 'AI-2025', '*/runMe_ai.sh'));
    const allSolvers = [...manualSolvers, ...aiSolvers];

    console.log(`Found ${allSolvers.length} solvers.`);

    let patchedCount = 0;

    for (const scriptPath of allSolvers) {
        let content = await fs.readFile(scriptPath, 'utf-8');

        // Check if already patched
        if (content.includes('"output": $json_output')) {
            console.log(`Skipping ${scriptPath} (already patched)`);
            continue;
        }

        // Look for the status line
        const statusLine = 'echo "    \\"status\\": \\"$status\\"" >> "$METRICS_FILE"';

        if (!content.includes(statusLine)) {
            console.warn(`Could not find status line in ${scriptPath}`);
            continue;
        }

        const patch = `    echo "    \\"status\\": \\"$status\\"," >> "$METRICS_FILE"
    # Capture output safely
    json_output=$(cat "$tmp_out" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
    echo "    \\"output\\": $json_output" >> "$METRICS_FILE"`;

        const newContent = content.replace(statusLine, patch);

        await fs.writeFile(scriptPath, newContent);
        console.log(`Patched ${scriptPath}`);
        patchedCount++;
    }

    console.log(`Patched ${patchedCount} solvers.`);
}

patchSolvers().catch(console.error);
