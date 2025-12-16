const puppeteer = require('puppeteer');
const path = require('path');

(async () => {
    const browser = await puppeteer.launch({ headless: true });
    const page = await browser.newPage();

    page.on('console', msg => console.log('PAGE LOG:', msg.text()));
    page.on('pageerror', error => console.log('PAGE ERROR:', error.message));
    page.on('requestfailed', request => console.log('REQUEST FAILED:', request.failure().errorText, request.url()));

    const reportPath = path.resolve(__dirname, 'benchmark_report.html');
    console.log(`Loading ${reportPath}`);

    try {
        await page.goto(`file://${reportPath}`, { waitUntil: 'networkidle0' });
    } catch (e) {
        console.error("Navigation error:", e);
    }

    // Check if table rows exist
    const rows = await page.$$('tbody tr');
    console.log(`Table row count: ${rows.length}`);

    for (const row of rows) {
        const text = await page.evaluate(el => el.innerText, row);
        console.log(`Row content: ${text.replace(/\n/g, ' ')}`);
        const display = await page.evaluate(el => getComputedStyle(el).display, row);
        console.log(`Row display: ${display}`);
        const color = await page.evaluate(el => getComputedStyle(el).color, row);
        console.log(`Row color: ${color}`);
    }

    await browser.close();
})();
