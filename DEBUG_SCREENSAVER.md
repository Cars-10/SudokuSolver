# Screensaver Debug Guide

## Issues Fixed

### 1. ✅ Duplicate ID Issue
**Problem**: There were two elements with `id="screensaver-pill"` causing getElementById() to fail
**Fix**: Removed duplicate from main header, kept only in fullscreen-header

### 2. ✅ Riddle Character Size
**Problem**: riddle-char elements had no font-size, making text too small
**Fix**: Added `font-size: 1.2em; color: #00ff9d;` to `.riddle-char` in `Metrics/index.css`

### 3. ✅ Debug Logging Added
**Purpose**: Help identify what's working and what's not

## Testing Instructions

1. **Open index.html** in your browser (preferably via the server at localhost:9001 or localhost:9002)

2. **Open browser console** (F12 or Cmd+Option+I on Mac)

3. **Look for initialization logs** when page loads:
   ```
   [RiddleSystem] Constructor - container found: true <div#riddle-container>
   [AlienStatusSystem] Constructor
   [AlienStatusSystem] Starting - will trigger every 10 seconds
   ```

4. **Click the red pill** (right half of the pill button)

5. **Watch for logs** in console:
   ```
   [startRiddleAnimation] Called - riddleSystem exists: true
   [RiddleSystem] start() called - container: true active: false
   [RiddleSystem] Starting riddle animation
   ```

6. **Wait 10 seconds** for alien glitch effect:
   ```
   [AlienStatusSystem] triggerEffect - fullscreen: true
   [AlienStatusSystem] Found targets: 5 [SPAN, DIV.mismatch-counter, SPAN#solver-text, DIV#screensaver-pill]
   ```

## What Should Be Visible

### In Red Pill Screensaver Mode:

1. **Matrix animation** - Falling green characters
2. **Fullscreen header** at top with:
   - "209 LANGUAGES" text (might be hidden by riddle)
   - **"OptionIsEscape"** text animating at the bottom (should scramble then reveal)
   - "MISMATCHES: 15" counter
   - Status indicators: ENV, TIMEOUT, ERROR, MISSING
   - **"Screensaver Active"** indicator with green dot

3. **Every 10 seconds**: Alien glitch effect
   - All text in the status area scrambles into katakana characters
   - Lasts for 3 seconds
   - Then reverts to original text

## Troubleshooting

### If "OptionIsEscape" is NOT visible:

Check console for these issues:

**Issue 1**: Container not found
```
[RiddleSystem] Constructor - container found: false undefined
```
**Solution**: The riddle-container element is missing from fullscreen-header

**Issue 2**: Start not being called
```
Missing: [startRiddleAnimation] Called - riddleSystem exists: true
```
**Solution**: The screensaver start function isn't calling startRiddleAnimation

**Issue 3**: Animation starting but not visible
```
[RiddleSystem] Starting riddle animation
(but still can't see it)
```
**Possible causes**:
- Z-index issue (check if matrix canvas is z-index 1000, fullscreen-header is 10000)
- CSS not loaded (check if Metrics/index.css is loaded)
- Element is positioned outside visible area

### If Alien Glitch is NOT working:

Check console for:

**Issue 1**: Not in fullscreen mode
```
[AlienStatusSystem] Not in fullscreen mode, skipping
```
**Solution**: Check if `document.body.classList.contains('fullscreen-active')` is true when red pill is active

**Issue 2**: No targets found
```
[AlienStatusSystem] Found targets: 0 []
```
**Solution**: The status elements are not in the DOM or have different IDs/classes

**Issue 3**: Effect not triggering
```
Missing all AlienStatusSystem logs after clicking red pill
```
**Solution**: AlienStatusSystem might not be initialized. Check for constructor log on page load.

## Manual CSS Verification

If riddle text is still too small, check computed styles in browser DevTools:

1. Click red pill to enter screensaver
2. Right-click in the header area → Inspect
3. Find `<div id="riddle-container" class="riddle-text">`
4. In Styles panel, verify:
   - `.riddle-text { font-size: 0.9em; }`
   - `.riddle-char { font-size: 1.2em; color: #00ff9d; }`

## Files Modified

1. **Metrics/index.css** - Added font-size to riddle-char
2. **Metrics/HTMLGenerator.ts** - Added console logging to RiddleSystem and AlienStatusSystem
3. **index.html** - Regenerated with fixes and logging

## Z-Index Stack (for reference)

- Matrix canvas (red pill): `z-index: 1000`
- Fullscreen header: `z-index: 10000` ✅ (above canvas)
- Riddle text: `z-index: 200` (within header)

## Next Steps

If issues persist after checking console logs:

1. Share the console logs here
2. Take a screenshot of the fullscreen header area
3. Use browser DevTools to inspect the riddle-container element and share computed styles
4. Check if index.css is actually being loaded (Network tab in DevTools)
