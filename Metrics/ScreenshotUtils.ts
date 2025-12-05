import puppeteer from 'puppeteer';
import * as fs from 'fs/promises';
import * as path from 'path';

export async function captureScreenshot(htmlFilePath: string, outputPath?: string) {
    try {
        const browser = await puppeteer.launch({
            headless: true,
            args: ['--no-sandbox', '--disable-setuid-sandbox', '--window-size=1920,1080']
        });
        const page = await browser.newPage();
        await page.setViewport({ width: 1920, height: 1080 });

        // Use file:// protocol
        await page.goto(`file://${htmlFilePath}`, { waitUntil: 'networkidle0' });

        // Wait a bit for animations
        await new Promise(r => setTimeout(r, 2000));

        let screenshotPath = outputPath;
        if (!screenshotPath) {
            const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
            const htmlDir = path.dirname(htmlFilePath);
            const screenshotsDir = path.join(htmlDir, 'screenshots');
            await fs.mkdir(screenshotsDir, { recursive: true },); // Ensure dir exists
            screenshotPath = path.join(screenshotsDir, `benchmark_${timestamp}.png`);
        } else {
            await fs.mkdir(path.dirname(screenshotPath), { recursive: true });
        }

        await page.screenshot({ path: screenshotPath, fullPage: true });
        console.log(`Screenshot saved to ${screenshotPath}`);

        await browser.close();
    } catch (e) {
        console.error("Failed to capture screenshot:", e);
    }
}
