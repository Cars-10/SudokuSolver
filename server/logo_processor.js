const sharp = require('sharp');
const fs = require('fs');
const path = require('path');

const LOGOS_DIR = path.join(__dirname, '../logos');
const TAILORING_PATH = path.join(LOGOS_DIR, 'Tailoring.json');

/**
 * Load tailoring config from Tailoring.json
 * @returns {Object} Tailoring configuration object
 */
function loadTailoring() {
    try {
        if (!fs.existsSync(TAILORING_PATH)) {
            console.log('Tailoring.json not found, no transformations will be applied');
            return {};
        }
        const content = fs.readFileSync(TAILORING_PATH, 'utf8');
        return JSON.parse(content);
    } catch (error) {
        console.error('Error loading Tailoring.json:', error);
        return {};
    }
}

/**
 * Convert SVG buffer to PNG buffer using Sharp
 * @param {Buffer} svgBuffer - SVG file content
 * @returns {Promise<Buffer>} PNG buffer
 */
async function convertSvgToPng(svgBuffer) {
    try {
        // Sharp can handle SVG natively with librsvg
        const pngBuffer = await sharp(svgBuffer)
            .resize(512, 512, { fit: 'inside', background: { r: 0, g: 0, b: 0, alpha: 0 } })
            .png()
            .toBuffer();

        return pngBuffer;
    } catch (error) {
        console.error('SVG conversion error:', error);
        throw new Error(`Failed to convert SVG to PNG: ${error.message}`);
    }
}

/**
 * Apply tailoring transformations to image
 * @param {Buffer} imageBuffer - PNG image buffer
 * @param {string} language - Language name (e.g., "Awk", "C")
 * @returns {Promise<Buffer>} Transformed PNG buffer
 */
async function applyTailoring(imageBuffer, language) {
    const tailoring = loadTailoring();
    const languageTailoring = tailoring[language];

    if (!languageTailoring) {
        // No tailoring needed
        console.log(`No tailoring for ${language}, returning original image`);
        return imageBuffer;
    }

    console.log(`Applying tailoring for ${language}:`, languageTailoring);
    let image = sharp(imageBuffer);

    // Apply invert transformation
    if (languageTailoring.invert) {
        console.log(`  - Inverting colors for ${language}`);
        image = image.negate({ alpha: false });  // Invert colors, preserve alpha
    }

    // Apply transparent_white transformation
    if (languageTailoring.transparent_white) {
        console.log(`  - Making white transparent for ${language}`);
        // Strategy: threshold white pixels and make them transparent
        // This requires extracting RGB channels and creating an alpha mask
        const { data, info } = await image.ensureAlpha().raw().toBuffer({ resolveWithObject: true });

        // Process pixels: make white pixels (RGB > 240) transparent
        const threshold = 240;
        for (let i = 0; i < data.length; i += 4) {
            const r = data[i];
            const g = data[i + 1];
            const b = data[i + 2];

            // If pixel is white (all channels above threshold), make it transparent
            if (r >= threshold && g >= threshold && b >= threshold) {
                data[i + 3] = 0;  // Set alpha to 0 (transparent)
            }
        }

        // Create image from modified buffer
        image = sharp(data, {
            raw: {
                width: info.width,
                height: info.height,
                channels: 4
            }
        }).png();

        return await image.toBuffer();
    }

    return await image.toBuffer();
}

/**
 * Process uploaded logo file
 * @param {Buffer} fileBuffer - File content buffer
 * @param {string} filename - Original filename
 * @param {string} language - Language name
 * @returns {Promise<string>} Path to saved logo (e.g., "/logos/C.png")
 */
async function processUploadedLogo(fileBuffer, filename, language) {
    try {
        const ext = path.extname(filename).toLowerCase();
        let imageBuffer = fileBuffer;

        console.log(`Processing logo for ${language}: ${filename} (${ext})`);

        // Convert SVG to PNG if needed
        if (ext === '.svg') {
            console.log('  Converting SVG to PNG...');
            imageBuffer = await convertSvgToPng(fileBuffer);
        } else if (ext !== '.png' && ext !== '.jpg' && ext !== '.jpeg') {
            throw new Error(`Unsupported format: ${ext}. Only PNG, JPG, and SVG are supported.`);
        }

        // If JPG/JPEG, convert to PNG
        if (ext === '.jpg' || ext === '.jpeg') {
            console.log('  Converting JPEG to PNG...');
            imageBuffer = await sharp(imageBuffer).png().toBuffer();
        }

        // Apply tailoring transformations
        imageBuffer = await applyTailoring(imageBuffer, language);

        // Ensure logos directory exists
        if (!fs.existsSync(LOGOS_DIR)) {
            fs.mkdirSync(LOGOS_DIR, { recursive: true });
        }

        // Save to logos directory
        const outputFilename = `${language}.png`;
        const outputPath = path.join(LOGOS_DIR, outputFilename);
        fs.writeFileSync(outputPath, imageBuffer);

        console.log(`  Saved logo to: ${outputPath}`);
        return `/logos/${outputFilename}`;
    } catch (error) {
        console.error('Error processing uploaded logo:', error);
        throw error;
    }
}

/**
 * Fetch logo from URL and process it
 * @param {string} url - URL to fetch image from
 * @param {string} language - Language name
 * @returns {Promise<string>} Path to saved logo
 */
async function fetchLogoFromUrl(url, language) {
    try {
        console.log(`Fetching logo from URL for ${language}: ${url}`);

        // Dynamic import for ESM node-fetch
        const fetch = (await import('node-fetch')).default;
        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`Failed to fetch logo: ${response.status} ${response.statusText}`);
        }

        const buffer = Buffer.from(await response.arrayBuffer());

        // Determine extension from URL or Content-Type
        let ext = path.extname(new URL(url).pathname).toLowerCase();
        const contentType = response.headers.get('content-type') || '';

        if (!ext) {
            // Try to determine from content type
            if (contentType.includes('svg')) {
                ext = '.svg';
            } else if (contentType.includes('png')) {
                ext = '.png';
            } else if (contentType.includes('jpeg') || contentType.includes('jpg')) {
                ext = '.jpg';
            }
        }

        const filename = `downloaded${ext}`;
        return await processUploadedLogo(buffer, filename, language);
    } catch (error) {
        console.error('Error fetching logo from URL:', error);
        throw error;
    }
}

module.exports = {
    processUploadedLogo,
    fetchLogoFromUrl,
    convertSvgToPng,
    applyTailoring
};
