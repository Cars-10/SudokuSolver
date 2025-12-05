import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import * as https from 'https';
import { languageMetadata } from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const mediaDir = path.join(__dirname, 'media');

async function downloadFile(url: string, dest: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const file = fs.createWriteStream(dest);
        https.get(url, (response) => {
            if (response.statusCode !== 200) {
                reject(new Error(`Failed to download ${url}: Status Code ${response.statusCode}`));
                return;
            }
            response.pipe(file);
            file.on('finish', () => {
                file.close();
                resolve();
            });
        }).on('error', (err) => {
            fs.unlink(dest).catch(() => { }); // Delete the file async. (But we don't check the result)
            reject(err);
        });
    });
}

async function main() {
    await fs.mkdir(mediaDir, { recursive: true });
    console.log(`Media directory: ${mediaDir}`);

    const languages = Object.keys(languageMetadata);
    const missingLogos: string[] = [];

    for (const lang of languages) {
        const meta = languageMetadata[lang];
        const safeLangName = lang.replace(/[^a-zA-Z0-9]/g, '_');

        // Handle Creator Image
        if (meta.image && meta.image.startsWith('http')) {
            const ext = path.extname(meta.image) || '.jpg'; // Default to jpg if no extension
            const filename = `${safeLangName}_creator${ext}`;
            const dest = path.join(mediaDir, filename);

            try {
                // Check if file exists
                await fs.access(dest);
                console.log(`[EXIST] ${lang} creator image: ${filename}`);
            } catch {
                console.log(`[DOWNLOADING] ${lang} creator image...`);
                try {
                    await downloadFile(meta.image, dest);
                    console.log(`[SAVED] ${filename}`);
                } catch (e) {
                    console.error(`[ERROR] Failed to download ${meta.image}:`, e);
                }
            }
        }

        // Handle Logo
        if (meta.logo && meta.logo.startsWith('http')) {
            const ext = path.extname(meta.logo) || '.png';
            const filename = `${safeLangName}_logo${ext}`;
            const dest = path.join(mediaDir, filename);

            try {
                await fs.access(dest);
                console.log(`[EXIST] ${lang} logo: ${filename}`);
            } catch {
                console.log(`[DOWNLOADING] ${lang} logo...`);
                try {
                    await downloadFile(meta.logo, dest);
                    console.log(`[SAVED] ${filename}`);
                } catch (e) {
                    console.error(`[ERROR] Failed to download ${meta.logo}:`, e);
                }
            }
        } else if (!meta.logo) {
            missingLogos.push(lang);
        }
    }

    console.log('\n--- Missing Logos ---');
    if (missingLogos.length > 0) {
        missingLogos.forEach(l => console.log(l));
    } else {
        console.log('None! All languages have logos.');
    }
}

main().catch(console.error);
