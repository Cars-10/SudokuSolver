import * as fs from 'fs/promises';
import * as path from 'path';

export async function captureScreenshot(htmlFilePath: string, outputPath?: string) {
    try {
        let puppeteer;
        try {
            puppeteer = (await import('puppeteer')).default;
        } catch (e) {
            console.log("Puppeteer not found, skipping screenshot.");
            return;
        }

        const executablePath = process.platform === 'darwin' 
            ? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
            : undefined;

        const browser = await puppeteer.launch({
            headless: true,
            executablePath: executablePath,
            args: ['--no-sandbox', '--disable-setuid-sandbox', '--window-size=1920,1080']
        });
        const page = await browser.newPage();
        await page.setViewport({ width: 1920, height: 1080 });

        // Use file:// protocol
        await page.goto(`file://${htmlFilePath}`, { waitUntil: 'domcontentloaded', timeout: 60000 });

        // Wait for D3 to load and chart to render
        await page.waitForSelector('#d3-chart-container svg', { timeout: 15000 }).catch(() => {
            console.log('Warning: Chart SVG not found within timeout');
        });

        // Additional wait for chart animations to complete
        await new Promise(r => setTimeout(r, 1000));

        // Scroll to top
        await page.evaluate(() => window.scrollTo(0, 0));
        await new Promise(r => setTimeout(r, 100));

        let screenshotPath = outputPath;
        if (!screenshotPath) {
            const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
            const htmlDir = path.dirname(htmlFilePath);
            const screenshotsDir = path.join(htmlDir, 'screenshots');
            await fs.mkdir(screenshotsDir, { recursive: true });
            screenshotPath = path.join(screenshotsDir, `benchmark_${timestamp}.png`);
        } else {
            await fs.mkdir(path.dirname(screenshotPath), { recursive: true });
        }

        await page.screenshot({ path: screenshotPath });

        await browser.close();
    } catch (e) {
        console.error("Failed to capture screenshot:", e);
    }
}
