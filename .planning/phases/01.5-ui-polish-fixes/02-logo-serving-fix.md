# Plan 02: Fix Logo Serving & Display

**Objective:** Make language logos display correctly in the benchmark report.

## Problem Analysis

### Current State
- 44 logo files exist in `/app/logos/` directory
- Report references logos like: `<img src="logos/C.png">`
- Express server does NOT serve `/logos` directory
- Server only serves: `/app/server` (static), `/Metrics`, `/js`, `/css`, `/runner`
- Result: All logo requests return 404 Not Found

### Logo References in Report
```html
<img src="logos/C.png" alt="C" class="lang-logo">
<img src="logos/Rust.png" alt="Rust" class="lang-logo">
<img src="logos/Go.png" alt="Go" class="lang-logo">
<!-- Some use external URLs -->
<img src="https://github.com/matz.png" alt="Ruby" class="lang-logo">
```

## Solution

### Task 1: Add Logos Route to Express Server

**File:** `/server/index.js`

Add after line 61 (after Metrics routes):

```javascript
// Serve Metrics directory for JS/CSS assets
app.use('/js', express.static(path.join(__dirname, '../Metrics')));
app.use('/css', express.static(path.join(__dirname, '../Metrics')));
app.use('/Metrics', express.static(path.join(__dirname, '../Metrics')));

// Serve logos directory  ← ADD THIS
app.use('/logos', express.static(path.join(__dirname, '../logos')));

const LANGUAGES_DIR = path.join(__dirname, '../Languages');
```

### Task 2: Verify Logo Files Exist

```bash
# Check logo files
docker exec sudokusolver-app-1 ls -la /app/logos/

# Should show 44 logo files including:
# C.png, Rust.png, Go.png, Python.png, JavaScript.png, etc.
```

### Task 3: Test Logo Loading

After adding route and restarting server:

```bash
# Test direct logo access
curl -I http://localhost:9001/logos/C.png
# Should return: 200 OK

curl -I http://localhost:9001/logos/Rust.png
# Should return: 200 OK

# Test in browser
open http://localhost:9001/logos/C.png
# Should display the C logo image
```

### Task 4: Add Fallback for Missing Logos

**File:** `/Metrics/HTMLGenerator.ts`

Add CSS for broken image fallback:

```css
/* Logo fallback styling */
.lang-logo {
    width: 40px;
    height: 40px;
    object-fit: contain;
    background: rgba(122, 162, 247, 0.1);
    border-radius: 6px;
    padding: 4px;
}

/* Show placeholder for broken images */
.lang-logo[src=""]:after,
.lang-logo:not([src]):after {
    content: "?";
    display: flex;
    align-items: center;
    justify-content: center;
    width: 100%;
    height: 100%;
    font-size: 24px;
    color: #565f89;
    background: #1a1b26;
}

/* Graceful degradation for 404 images */
.lang-logo {
    image-rendering: -webkit-optimize-contrast;
    image-rendering: crisp-edges;
}
```

Add JavaScript to handle missing images:

```javascript
// Handle broken logo images
document.addEventListener('DOMContentLoaded', function() {
    const logos = document.querySelectorAll('.lang-logo');
    logos.forEach(img => {
        img.addEventListener('error', function() {
            console.warn(`Failed to load logo: ${this.src}`);
            // Try fallback or show placeholder
            this.style.background = 'linear-gradient(135deg, #414868 0%, #24283b 100%)';
            this.alt = this.alt || '?';
            this.title = `${this.alt} (logo unavailable)`;
        });
    });
});
```

### Task 5: Optimize Logo Loading (Optional)

Add lazy loading for logos:

```html
<img src="logos/C.png"
     alt="C"
     class="lang-logo"
     loading="lazy"
     decoding="async">
```

### Task 6: Verify Logo Metadata Paths

Check that metadata.json has correct logo paths:

```bash
docker exec sudokusolver-app-1 cat /app/Languages/metadata.json | jq '.C.logo'
# Should return: "logos/C.png" (relative path)
```

If paths are wrong, update them:
```json
{
  "C": {
    "logo": "logos/C.png"
  }
}
```

## Testing Checklist

- [ ] `/logos` route added to server/index.js
- [ ] Server restart successful
- [ ] Direct logo access works (curl test)
- [ ] Logos display in browser at /logos/C.png
- [ ] Logos display in benchmark report
- [ ] External URL logos work (GitHub, Wikipedia)
- [ ] Missing logo fallback displays correctly
- [ ] No console errors for logo loading
- [ ] Lazy loading improves performance
- [ ] All 44 logos accessible

## Debugging Steps

If logos still don't display:

1. **Check server logs:**
   ```bash
   docker-compose logs app | grep logos
   ```

2. **Check Docker volumes:**
   ```bash
   docker exec sudokusolver-app-1 ls -la /app/logos/ | head
   ```

3. **Test route in browser:**
   - Open http://localhost:9001/logos/C.png
   - Should see image, not 404

4. **Check browser console:**
   - Open benchmark report
   - F12 → Console tab
   - Look for 404 errors on logo URLs

5. **Verify HTML paths:**
   ```bash
   docker exec sudokusolver-app-1 grep "logos/" /app/_report.html | head -5
   ```

## Files to Modify

1. `/server/index.js` - Add /logos route
2. `/Metrics/HTMLGenerator.ts` - Add logo fallback CSS/JS (optional)
3. Docker container restart required

## Acceptance Criteria

✅ All local logos display correctly in report
✅ External URL logos (GitHub, Wikipedia) display
✅ Missing logos show graceful fallback
✅ No 404 errors for logo requests
✅ Logo loading doesn't impact page performance
✅ Direct logo URLs accessible (/logos/C.png works)
