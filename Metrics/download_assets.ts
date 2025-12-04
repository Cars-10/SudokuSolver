
import { languageMetadata } from './gather_metrics.ts';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import * as https from 'https';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const LOGOS_DIR = path.join(__dirname, 'logos');

if (!fs.existsSync(LOGOS_DIR)) {
    fs.mkdirSync(LOGOS_DIR);
}

async function downloadFile(url: string, dest: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const file = fs.createWriteStream(dest);
        https.get(url, (response) => {
            if (response.statusCode !== 200) {
                reject(new Error(`Failed to download ${url}: ${response.statusCode}`));
                return;
            }
            response.pipe(file);
            file.on('finish', () => {
                file.close();
                resolve();
            });
        }).on('error', (err) => {
            fs.unlink(dest, () => { });
            reject(err);
        });
    });
}

async function main() {
    console.log("Starting asset download...");

    for (const [lang, meta] of Object.entries(languageMetadata)) {
        const langDir = path.join(LOGOS_DIR, lang);
        if (!fs.existsSync(langDir)) {
            fs.mkdirSync(langDir);
        }

        if (meta.logo) {
            const ext = path.extname(new URL(meta.logo).pathname) || '.png';
            const dest = path.join(langDir, `logo${ext}`);
            try {
                console.log(`Downloading logo for ${lang}...`);
                await downloadFile(meta.logo, dest);
                console.log(`Saved to ${dest}`);
            } catch (e) {
                console.error(`Error downloading logo for ${lang}:`, e);
            }
        }

        if (meta.image) {
            const ext = path.extname(new URL(meta.image).pathname) || '.jpg';
            const dest = path.join(langDir, `image${ext}`);
            try {
                console.log(`Downloading image for ${lang}...`);
                await downloadFile(meta.image, dest);
                console.log(`Saved to ${dest}`);
            } catch (e) {
                console.error(`Error downloading image for ${lang}:`, e);
            }
        }
    }
    console.log("Download complete.");
}

main().catch(console.error);
