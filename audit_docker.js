const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');
const util = require('util');
const execPromise = util.promisify(exec);

const languagesDir = path.join(__dirname, 'Languages');
const languages = fs.readdirSync(languagesDir).filter(f => {
    return fs.statSync(path.join(languagesDir, f)).isDirectory() && !f.startsWith('.');
});

async function audit() {
    console.log(`Auditing ${languages.length} languages in Docker...`);
    const results = [];
    const errors = [];

    // Use a concurrency limit to avoid overloading Docker
    const concurrency = 5;
    
    for (let i = 0; i < languages.length; i += concurrency) {
        const chunk = languages.slice(i, i + concurrency);
        await Promise.all(chunk.map(async (lang) => {
            const projectRoot = process.cwd();
            const dockerWorkDir = `/app/Languages/${lang}`;
            // Use 1.matrix as a quick smoke test
            const cmd = `docker run --rm -v "${projectRoot}:/app" -w "${dockerWorkDir}" sudoku-benchmark:latest ./runMe.sh ../../Matrices/1.matrix`;
            
            try {
                // Set a timeout of 20s
                await execPromise(cmd, { timeout: 20000 });
                process.stdout.write('.');
                results.push({ lang, status: 'OK' });
            } catch (e) {
                process.stdout.write('F');
                // Clean up error message
                let msg = e.stderr || e.stdout || e.message;
                if (msg.includes('Command failed')) {
                    // Extract the actual error from stderr if available
                    msg = e.stderr || e.message; 
                }
                
                // Truncate if too long
                if (msg && msg.length > 500) msg = msg.substring(0, 500) + '...';
                else if (!msg) msg = "Unknown error";
                
                errors.push({ lang, error: msg.trim() });
            }
        }));
    }
    
    console.log('\n\nAudit Complete.');
    console.log('--- Failures ---');
    if (errors.length === 0) {
        console.log("No failures! All languages ran successfully in Docker.");
    } else {
        console.log(`Found ${errors.length} failing languages.`);
        
        const reportPath = 'docker_audit_report.md';
        const lines = [
            '# Docker Compatibility Audit',
            '',
            `Date: ${new Date().toISOString()}`,
            '',
            '| Language | Error |',
            '|---|---|'
        ];
        
        errors.forEach(e => {
            // Escape pipes in error message
            const safeError = e.error.replace(/\|/g, '\\|').replace(/\n/g, '<br>');
            lines.push(`| **${e.lang}** | 
${safeError}
 |`);
        });
        
        fs.writeFileSync(reportPath, lines.join('\n'));
        console.log(`Report saved to ${reportPath}`);
    }
}

audit();