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
    const solvers = await glob(path.join(rootDir, 'Languages', '*', 'setupAndRunMe.sh'));

    const allSolvers = [...solvers];
    const filteredSolvers = allSolvers.filter(s => !s.includes('/Racket/') && !s.includes('/Cobol/'));

    console.log(`Found ${filteredSolvers.length} solvers (excluding Racket and Cobol):`, filteredSolvers);
    return filteredSolvers;
}
