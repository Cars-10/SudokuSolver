import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';

export async function readReferenceOutputs(rootDir: string): Promise<Record<string, string>> {
    const refPath = path.join(rootDir, 'Matrices', 'ReferenceForAllMatrixRun.txt');
    try {
        const content = await fs.readFile(refPath, 'utf-8');
        const refs: Record<string, string> = {};
        const parts = content.split('../Matrices/');
        for (const part of parts) {
            if (!part.trim()) continue;
            const lines = part.split('\n');
            const matrixName = lines[0].trim();
            const output = lines.slice(1).join('\n').trim();
            refs[matrixName] = output;
        }
        return refs;
    } catch (e) {
        console.error('Failed to read reference outputs:', e);
        return {};
    }
}

export async function findSolvers(rootDir: string): Promise<string[]> {
    // Find all runMe_ai.sh files in Manual and AI-2025
    console.log(`Searching for solvers in Manual and AI-2025...`);

    const manualSolvers = await glob(path.join(rootDir, 'Manual', '*/runMe_ai.sh'));
    const aiSolvers = await glob(path.join(rootDir, 'AI-2025', '*/runMe_ai.sh'));
    // For CleanedUp, we look for language directories directly
    const cleanedUpSolvers = await glob(path.join(rootDir, 'CleanedUp', 'Languages', '*', 'setupAndRunMe.sh'));

    const allSolvers = [...manualSolvers, ...aiSolvers, ...cleanedUpSolvers];
    const filteredSolvers = allSolvers.filter(s => !s.includes('/Racket/') && !s.includes('/Cobol/'));

    console.log(`Found ${filteredSolvers.length} solvers (excluding Racket and Cobol):`, filteredSolvers);
    return filteredSolvers;
}
