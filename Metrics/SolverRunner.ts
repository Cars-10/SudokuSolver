import * as path from 'path';
import * as util from 'util';
import { exec } from 'child_process';
import * as fs from 'fs/promises';
import type { SolverMetrics } from './types.ts';

const execPromise = util.promisify(exec);

export async function runSolver(scriptPath: string, matrixPattern: string): Promise<SolverMetrics | null> {
    let solverDir: string;
    let langName: string;
    let parentDir: string;

    if (scriptPath.endsWith('.sh')) {
        solverDir = path.dirname(scriptPath);
        langName = path.basename(solverDir);
        parentDir = path.basename(path.dirname(solverDir));
    } else {
        // Assume it's a directory (CleanedUp/Languages/Lang)
        solverDir = scriptPath;
        langName = path.basename(solverDir);
        parentDir = path.basename(path.dirname(solverDir)); // Should be 'Languages'
    }

    let solverName = langName;
    let runType = 'Local'; // Default to Local for CleanedUp

    console.log(`Running solver: ${solverName} on ${matrixPattern}`);

    try {
        let command: string;
        let cwd: string;

        // matrixPattern is like "1.matrix", runMeGlobal expects "1"
        const matrixName = matrixPattern.replace('.matrix', '');
        const globalScript = path.resolve(solverDir, '../../runMeGlobal.sh');
        command = `${globalScript} ${langName} ${matrixName}`;
        cwd = path.dirname(globalScript); // Run from CleanedUp dir

        const { stdout, stderr } = await execPromise(command, {
            cwd: cwd,
            timeout: 180000 // 3 minutes
        });

        try {
            const results = JSON.parse(stdout);
            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: results
            };
        } catch (e) {
            console.error(`Failed to parse JSON output from ${solverName}:`, e);
            console.log("Stdout:", stdout);
            return null;
        }
    } catch (e: any) {
        if (e.killed || e.signal === 'SIGTERM' || e.code === 143) {
            console.error(`Solver ${solverName} timed out after 3 minutes.`);
            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: [{
                    matrix: matrixPattern,
                    time: 180, // Cap at timeout
                    iterations: 0,
                    memory: 0,
                    cpu_user: 0,
                    cpu_sys: 0,
                    status: "timeout",
                    output: "Solver timed out after 180s"
                }]
            };
        }
        console.error(`Failed to run solver ${solverName}:`, e);
        return null;
    }
}

export async function runDockerSolver(scriptPath: string, matrixPattern: string): Promise<SolverMetrics | null> {
    let solverDir: string;
    let langName: string;

    if (scriptPath.endsWith('.sh')) {
        solverDir = path.dirname(scriptPath);
        langName = path.basename(solverDir);
    } else {
        solverDir = scriptPath;
        langName = path.basename(solverDir);
    }

    const solverName = langName;
    const runType = 'Docker';
    console.log(`Running solver in Docker: ${solverName} on ${matrixPattern}`);

    const dockerFile = path.join(solverDir, 'Dockerfile');
    try {
        await fs.access(dockerFile);
    } catch {
        console.log(`Skipping ${langName}: No Dockerfile found.`);
        return null;
    }

    try {
        // 1. Build Image
        // Sanitize image name: lowercase, replace special chars with nothing or safe chars
        const safeLangName = langName.toLowerCase().replace(/[^a-z0-9]/g, '');
        const imageName = `sudoku-${safeLangName}`;
        console.log(`Building Docker image: ${imageName}...`);
        await execPromise(`docker build -t ${imageName} .`, { cwd: solverDir });

        // 2. Run Container
        const repoRoot = path.resolve(solverDir, '../../../');
        const matricesDir = path.join(repoRoot, 'Matrices');

        const matrixName = matrixPattern.replace('.matrix', '');
        const containerMatrixPath = `../Matrices/${matrixPattern}`;

        const command = `docker run --rm -v "${matricesDir}:/usr/src/Matrices" ${imageName} "${containerMatrixPath}"`;

        console.log(`Executing: ${command}`);
        const { stdout, stderr } = await execPromise(command);

        try {
            const jsonStart = stdout.indexOf('[');
            const jsonEnd = stdout.lastIndexOf(']');
            if (jsonStart === -1 || jsonEnd === -1) {
                throw new Error("No JSON array found in output");
            }

            const jsonStr = stdout.substring(jsonStart, jsonEnd + 1);
            const results = JSON.parse(jsonStr);

            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: results
            };

        } catch (e) {
            console.error(`Failed to parse JSON output from Docker ${solverName}:`, e);
            console.log("Stdout:", stdout);
            return null;
        }

    } catch (e: any) {
        console.error(`Failed to run Docker solver ${solverName}:`, e);
        return null;
    }
}
