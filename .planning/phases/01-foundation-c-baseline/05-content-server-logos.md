# Plan: Content Server & Logo System

**Phase:** 1 - Foundation & C Baseline
**Plan:** 05 of 05
**Created:** 2025-12-16
**Status:** Ready for execution

## Objective

Fix Content Server UI issues (modal positioning, edit/update logic) and rebuild logo system using Sharp library. Enable upload, URL fetch, SVG→PNG conversion, and tailoring transformations (invert, transparent_white).

## Context

From 01-CONTEXT.md known issues:
- Modal positioning broken (appears top-left instead of near mouse click)
- Language Detail modal edit/update logic broken
- Logo upload/paste not working
- Logo tailoring system code lost (needs re-implementation)

Logo system requirements from PROJECT.md:
- PNG format only (convert SVG to PNG)
- Support upload and URL fetch
- Apply tailoring from Tailoring.json (invert, transparent_white only)
- Server-side processing with Sharp library

## Tasks

<task id="1" type="auto">
<description>Fix modal positioning in report_client.js</description>

<context>
Currently modals (Language Detail, Edit Language) appear at fixed position (likely top-left corner). Should appear near the mouse click location for better UX.

From 01-CONTEXT.md: "Calculate from mouse event" - modal position should be based on click coordinates with offset to avoid cursor overlap.
</context>

<files>
- /Metrics/report_client.js - Client-side modal interaction code
- /Metrics/HTMLGenerator.ts - May need template updates
</files>

<actions>
1. Read current modal positioning code:
   ```bash
   grep -n "modal" Metrics/report_client.js -A 10 -B 10
   ```

2. Identify modal show/hide functions and current positioning logic

3. Update modal positioning to use mouse event coordinates:
   ```javascript
   // In Language Detail modal open handler:
   function showLanguageModal(language, event) {
       const modal = document.getElementById('language-modal');

       // Get click coordinates
       const clickX = event.clientX || event.pageX;
       const clickY = event.clientY || event.pageY;

       // Add offset to avoid cursor overlap
       const offsetX = 20;
       const offsetY = 20;

       // Calculate position (with viewport bounds checking)
       let modalX = clickX + offsetX;
       let modalY = clickY + offsetY;

       // Get modal dimensions
       const modalWidth = modal.offsetWidth || 400;  // Fallback if not rendered
       const modalHeight = modal.offsetHeight || 300;

       // Check right edge
       if (modalX + modalWidth > window.innerWidth) {
           modalX = clickX - modalWidth - offsetX;  // Show on left instead
       }

       // Check bottom edge
       if (modalY + modalHeight > window.innerHeight) {
           modalY = window.innerHeight - modalHeight - 20;  // Push up
       }

       // Apply position
       modal.style.position = 'fixed';
       modal.style.left = `${modalX}px`;
       modal.style.top = `${modalY}px`;
       modal.style.display = 'block';

       // ... load language data ...
   }
   ```

4. Update event listeners to pass event object:
   ```javascript
   // Ensure click handlers receive event:
   languageElement.addEventListener('click', (event) => {
       showLanguageModal(languageName, event);
   });
   ```

5. Test modal positioning:
   - Click near top-left corner (modal should appear with offset)
   - Click near bottom-right corner (modal should flip to left/above)
   - Click in center (modal should appear near cursor)
   - Verify no modal overflow off screen

6. Apply same fix to Edit Language modal if separate

7. Update HTMLGenerator.ts if modal template needs CSS changes:
   ```css
   .modal {
       position: fixed;
       /* Remove any static positioning */
       z-index: 9999;
   }
   ```
</actions>

<verification>
- [ ] Modal positioning code located
- [ ] Mouse event coordinates captured
- [ ] Position calculated with offset
- [ ] Viewport bounds checking works
- [ ] Modal appears near cursor (not top-left)
- [ ] Modal doesn't overflow screen edges
- [ ] Both Language Detail and Edit modals fixed
</verification>

<acceptance>
Modal positioning fixed. Language Detail and Edit modals appear near mouse click with smart positioning to avoid screen edges. No more top-left corner placement.
</acceptance>
</task>

<task id="2" type="auto">
<description>Rebuild logo system with Sharp library</description>

<context>
Logo system code was lost. Need to rebuild from scratch using Sharp library for server-side image processing.

Requirements:
1. Upload local PNG/SVG files via multipart form
2. Fetch images from URL
3. Convert SVG to PNG (Sharp doesn't handle SVG natively - use svg2png or similar)
4. Apply tailoring transformations (invert, transparent_white)
5. Cache processed images to logos/ directory
6. Return path for use in metadata.json

From PROJECT.md: "Sharp-based server-side processing, PNG only, SVG→PNG conversion"
</context>

<files>
- /server/index.js - Add logo processing endpoints (may already exist)
- /server/logo_processor.js - New Sharp-based processing module
- /logos/Tailoring.json - Config for language-specific transformations
- /server/package.json - Add dependencies (sharp, svg2png-wasm or similar)
</files>

<actions>
1. Install dependencies:
   ```bash
   cd server
   npm install sharp svg2png-wasm
   ```

2. Create server/logo_processor.js:
   ```javascript
   import sharp from 'sharp';
   import { svg2png } from 'svg2png-wasm';
   import fs from 'fs';
   import path from 'path';

   const LOGOS_DIR = path.join(process.cwd(), 'logos');
   const TAILORING_PATH = path.join(LOGOS_DIR, 'Tailoring.json');

   // Load tailoring config
   function loadTailoring() {
       if (!fs.existsSync(TAILORING_PATH)) {
           return {};
       }
       return JSON.parse(fs.readFileSync(TAILORING_PATH, 'utf8'));
   }

   // Convert SVG to PNG
   export async function convertSvgToPng(svgBuffer) {
       try {
           const pngBuffer = await svg2png(svgBuffer, {
               width: 512,  // Max width
               height: 512  // Max height
           });
           return pngBuffer;
       } catch (error) {
           console.error('SVG conversion error:', error);
           throw new Error('Failed to convert SVG to PNG');
       }
   }

   // Apply tailoring transformations
   export async function applyTailoring(imageBuffer, language) {
       const tailoring = loadTailoring();
       const languageTailoring = tailoring[language];

       if (!languageTailoring) {
           // No tailoring needed
           return imageBuffer;
       }

       let image = sharp(imageBuffer);

       // Apply invert transformation
       if (languageTailoring.invert) {
           image = image.negate({ alpha: false });  // Invert colors, preserve alpha
       }

       // Apply transparent_white transformation
       if (languageTailoring.transparent_white) {
           // Make white pixels transparent
           image = image.removeAlpha()  // Remove existing alpha
               .threshold(240)  // White = 255, make 240+ transparent
               .toColorspace('rgba');
       }

       return await image.toBuffer();
   }

   // Process uploaded logo
   export async function processUploadedLogo(fileBuffer, filename, language) {
       const ext = path.extname(filename).toLowerCase();
       let imageBuffer = fileBuffer;

       // Convert SVG to PNG if needed
       if (ext === '.svg') {
           imageBuffer = await convertSvgToPng(fileBuffer);
       } else if (ext !== '.png') {
           throw new Error('Only PNG and SVG formats supported');
       }

       // Apply tailoring
       imageBuffer = await applyTailoring(imageBuffer, language);

       // Save to logos directory
       const outputPath = path.join(LOGOS_DIR, `${language}.png`);
       fs.writeFileSync(outputPath, imageBuffer);

       return `/logos/${language}.png`;
   }

   // Fetch logo from URL
   export async function fetchLogoFromUrl(url, language) {
       const response = await fetch(url);
       if (!response.ok) {
           throw new Error(`Failed to fetch logo: ${response.statusText}`);
       }

       const buffer = Buffer.from(await response.arrayBuffer());
       return await processUploadedLogo(buffer, url, language);
   }
   ```

3. Update server/index.js with logo endpoints:
   ```javascript
   import { processUploadedLogo, fetchLogoFromUrl } from './logo_processor.js';

   // Upload logo endpoint
   app.post('/api/upload-logo', upload.single('logo'), async (req, res) => {
       try {
           const language = req.body.language;
           const file = req.file;

           if (!file || !language) {
               return res.status(400).json({ error: 'Missing file or language' });
           }

           const logoPath = await processUploadedLogo(file.buffer, file.originalname, language);
           res.json({ success: true, path: logoPath });
       } catch (error) {
           res.status(500).json({ error: error.message });
       }
   });

   // Fetch logo from URL endpoint
   app.post('/api/fetch-logo', async (req, res) => {
       try {
           const { url, language } = req.body;

           if (!url || !language) {
               return res.status(400).json({ error: 'Missing url or language' });
           }

           const logoPath = await fetchLogoFromUrl(url, language);
           res.json({ success: true, path: logoPath });
       } catch (error) {
           res.status(500).json({ error: error.message });
       }
   });
   ```

4. Update Dockerfile to ensure Sharp native dependencies available:
   ```dockerfile
   # Add to Dockerfile if not present:
   RUN apt-get update && apt-get install -y \
       libvips-dev \
       && rm -rf /var/lib/apt/lists/*
   ```

5. Test logo processing:
   - Upload Awk.png → Should apply invert transformation
   - Upload C.svg → Should convert to PNG and apply transparent_white
   - Fetch from URL → Should download and process
   - Verify output saved to logos/LanguageName.png

6. Update HTMLGenerator.ts to use processed logo paths from metadata.json
</actions>

<verification>
- [ ] Sharp and svg2png-wasm installed
- [ ] logo_processor.js created with all functions
- [ ] SVG to PNG conversion works
- [ ] Invert transformation works (Awk)
- [ ] Transparent white transformation works (C)
- [ ] Upload endpoint functional
- [ ] URL fetch endpoint functional
- [ ] Processed logos saved to logos/ directory
- [ ] Tailoring.json read and applied correctly
</verification>

<acceptance>
Logo system operational. Can upload PNG/SVG files, fetch from URLs, convert SVG→PNG, and apply tailoring transformations. Processed logos cached to logos/ directory and usable in reports.
</acceptance>
</task>

<task id="3" type="checkpoint:ui_test">
<description>Test edit/update workflow end-to-end</description>

<context>
Validate that the complete UI workflow works:
1. View language detail modal (near cursor)
2. Click edit button
3. Upload/fetch logo
4. Edit metadata (creator, description)
5. Save changes
6. Verify changes persist in metadata.json
7. Regenerate report and see updated logo/metadata

This is the final Phase 1 checkpoint for UI functionality.
</context>

<files>
- /Languages/metadata.json - Updated by edit workflow
- /logos/ - Contains processed logo files
- /benchmark_report.html - Generated report shows changes
</files>

<actions>
1. Start Content Server:
   ```bash
   docker-compose up -d
   ```

2. Navigate to localhost:9001 in browser

3. Test modal positioning:
   - Click C language in report
   - Verify modal appears near cursor (not top-left)
   - Close modal
   - Click from different screen locations
   - Verify modal position follows cursor

4. Test edit workflow:
   - Click C language
   - Click "Edit" button in modal
   - Edit creator field: "Original Author"
   - Edit description: "Reference baseline implementation"
   - Click "Save"
   - Verify success message

5. Test logo upload:
   - Click "Edit" on language without logo (e.g., Python)
   - Choose local PNG file
   - Upload
   - Verify preview shows processed image
   - Verify logos/Python.png created

6. Test logo URL fetch:
   - Click "Edit" on another language
   - Enter URL: https://example.com/logo.png
   - Click "Fetch"
   - Verify image downloaded and processed
   - Verify logos/LanguageName.png created

7. Test tailoring application:
   - Upload logo for Awk (should invert colors per Tailoring.json)
   - Verify inverted colors in preview
   - Upload logo for C (should make white transparent per Tailoring.json)
   - Verify transparency applied

8. Verify metadata persistence:
   ```bash
   docker exec -it <container> cat /app/Languages/metadata.json | jq '.C'
   ```
   Should show updated creator/description

9. Regenerate report:
   ```bash
   docker exec -it <container> node /app/Metrics/generate_report_only.ts
   ```

10. Reload browser, verify:
    - Updated metadata displays
    - Logos show with correct tailoring
    - Modal still positions correctly

11. Test error handling:
    - Try uploading invalid file format (e.g., .txt)
    - Verify error message shown
    - Try invalid URL
    - Verify error message shown

12. Screenshot final state:
    ```bash
    docker exec -it <container> node /app/Metrics/capture_screenshot.js
    ```
    Verify screenshot shows updated report with logos
</actions>

<verification>
- [ ] Content Server accessible on port 9001
- [ ] Modal positioning works correctly
- [ ] Edit modal opens and displays current metadata
- [ ] Can edit creator, description fields
- [ ] Save button persists changes to metadata.json
- [ ] Logo upload works (PNG files)
- [ ] SVG upload converts to PNG
- [ ] URL fetch downloads and processes logos
- [ ] Invert tailoring applied to Awk
- [ ] Transparent white tailoring applied to C
- [ ] Report regenerates with updated data
- [ ] Changes persist across server restart
- [ ] Error handling works for invalid inputs
</verification>

<acceptance>
Content Server UI fully functional. Modal positioning fixed, edit/update workflow operational, logo system processes uploads and URLs with tailoring transformations. All changes persist and display correctly in regenerated reports.
</acceptance>
</task>

## Dependencies

**Blocks:**
- Phase 2-5 (Language implementation) - UI used for adding logos and metadata

**Blocked by:**
- Plan 01 (Docker & Database) - needs server running
- Plan 04 (C Baseline) - C logo and metadata used for testing

## Risks

- **Sharp native dependencies missing**: Docker image needs libvips-dev. Mitigation: Add to Dockerfile in Plan 01.
- **SVG conversion complex**: svg2png-wasm may have limitations. Mitigation: Test with real SVG files, fallback to manual conversion if needed.
- **Modal positioning edge cases**: Viewport size variations could cause issues. Mitigation: Test on different screen sizes, add robust bounds checking.
- **Tailoring transformations incorrect**: Visual verification needed. Mitigation: Compare with original Tailoring.json examples (Awk, C).

## Success Criteria

- [x] Modal positioning fixed (appears near cursor)
- [x] Logo system rebuilt with Sharp
- [x] Upload and URL fetch working
- [x] SVG→PNG conversion operational
- [x] Tailoring transformations applied correctly
- [x] Edit/update workflow tested end-to-end
- [x] Changes persist in metadata.json and logos/
- [x] Reports display updated logos and metadata

## Notes

This plan focuses on UI/UX polish and is not blocking for core validation (Plans 01-04). Can be executed in parallel with or after Plan 04 if needed.

Per 01-CONTEXT.md: "Just make it work - basic functionality (upload, URL fetch, SVG→PNG, tailoring), optimize later" - focus on functionality, not perfection.

Logo system is the final piece of Phase 1 infrastructure and enables easy logo management for the remaining 14 language implementations in Phases 2-5.
