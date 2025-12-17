# Plan 03: Test & Polish Edit Workflow

**Objective:** Verify and fix the end-to-end language metadata edit workflow.

## Workflow Overview

```
User Flow:
1. Click language in report
2. Modal opens with language details
3. Click "Edit" button
4. Fields become editable
5. Modify creator, description, location, benefits, etc.
6. Upload new logo (optional)
7. Click "Save"
8. Changes persist to metadata.json
9. Regenerate report
10. Verify changes appear in new report
```

## Test Cases

### Test 1: Basic Edit Flow

**Steps:**
1. Open benchmark report: http://localhost:9001/
2. Click on "C" language
3. Verify modal opens (should float - from Plan 01 fix)
4. Click "Edit" button
5. Modify these fields:
   - Creator: "Dennis Ritchie (Updated)"
   - Description: "Systems programming language - tested"
   - Location: "Bell Labs, NJ, USA - verified"
6. Click "Save"

**Expected:**
- Success message appears
- Changes saved to `/Languages/metadata.json`
- Modal closes or returns to view mode

**Verify:**
```bash
docker exec sudokusolver-app-1 cat /app/Languages/metadata.json | jq '.C'
# Should show updated values
```

### Test 2: Logo Upload

**Steps:**
1. Open modal for language without logo (e.g., "Python_Test")
2. Click "Edit"
3. Click "Change" on logo area
4. Select a PNG file
5. Verify upload progress indicator
6. Click "Save"

**Expected:**
- Logo uploads to `/logos/Python_Test.png`
- Preview shows new logo immediately
- File saved successfully

**Verify:**
```bash
docker exec sudokusolver-app-1 ls -lh /app/logos/Python_Test.png
# Should exist with recent timestamp
```

### Test 3: Logo URL Fetch

**Steps:**
1. Open modal for "Go"
2. Click "Edit"
3. Enter logo URL: `https://raw.githubusercontent.com/devicons/devicon/master/icons/go/go-original.svg`
4. Click "Fetch Logo" button
5. Wait for processing
6. Click "Save"

**Expected:**
- SVG fetched and converted to PNG
- Logo saved to `/logos/Go.png`
- Preview shows processed logo
- Tailoring applied if configured

**Verify:**
```bash
docker exec sudokusolver-app-1 ls -lh /app/logos/Go.png
curl -X POST http://localhost:9001/api/fetch-logo \
  -H "Content-Type: application/json" \
  -d '{"url": "https://raw.githubusercontent.com/devicons/devicon/master/icons/go/go-original.svg", "language": "Go"}'
```

### Test 4: Metadata Persistence

**Steps:**
1. Make changes to C language metadata
2. Click "Save"
3. Regenerate report: `curl -X POST http://localhost:9001/api/generate-report`
4. Reload page
5. Click on C language again

**Expected:**
- Changes persist in modal view
- Report shows updated information
- No data loss after regeneration

### Test 5: Image Paste

**Steps:**
1. Open modal, click "Edit"
2. Copy an image to clipboard (Cmd+C / Ctrl+C)
3. Focus on modal
4. Paste (Cmd+V / Ctrl+V)
5. Verify image uploads

**Expected:**
- Paste event detected
- Image extracted from clipboard
- Upload triggered automatically
- Logo saved and displayed

**Note:** Check if paste listener exists in report_client.js (lines 514-543)

### Test 6: Cancel Edit

**Steps:**
1. Open modal, click "Edit"
2. Modify several fields
3. Click "Cancel" or close modal
4. Reopen modal

**Expected:**
- Changes discarded
- Original values restored
- No partial saves

### Test 7: Validation

**Steps:**
1. Try to save with empty required fields
2. Try invalid URLs
3. Try unsupported file formats (e.g., .txt)

**Expected:**
- Clear error messages
- Validation prevents save
- User can correct and retry

## Known Issues to Fix

### Issue 1: Edit Button State

Check if edit button toggles correctly:
```javascript
// In HTMLGenerator.ts or report_client.js
window.toggleEditMode = function () {
    const content = document.getElementById('modalContent');
    const btn = document.getElementById('editBtn');
    if (content.classList.contains('editing')) {
        content.classList.remove('editing');
        btn.innerText = "Edit";
        // Discard changes?
    } else {
        content.classList.add('editing');
        btn.innerText = "Cancel";
    }
};
```

### Issue 2: Save Function

Verify save function posts to correct endpoint:
```javascript
window.saveLanguageDetails = async function() {
    const updates = {
        creator: document.getElementById('editInputs-creator').value,
        description: document.getElementById('editInputs-desc').value,
        location: document.getElementById('editInputs-location').value,
        benefits: document.getElementById('editInputs-benefits').value,
        website: document.getElementById('editInputs-website').value,
        date: document.getElementById('editInputs-date').value
    };

    try {
        const response = await fetch(`/api/metadata/${currentEditingLang}`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(updates)
        });

        if (response.ok) {
            alert('✅ Changes saved successfully!');
            // Reload modal to show updated data
            await showLanguageDetails(currentEditingLang);
        } else {
            const error = await response.json();
            alert(`❌ Failed to save: ${error.error}`);
        }
    } catch (error) {
        alert(`❌ Error: ${error.message}`);
    }
};
```

### Issue 3: Server Endpoint

Verify `/api/metadata/:language` endpoint exists in server/index.js:

```javascript
// POST endpoint to update metadata
app.post('/api/metadata/:language', (req, res) => {
    const language = req.params.language;
    const updates = req.body;

    const metadataPath = path.join(__dirname, '../Languages/metadata.json');

    try {
        // Read existing metadata
        let metadata = {};
        if (fs.existsSync(metadataPath)) {
            metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
        }

        // Update language metadata
        metadata[language] = {
            ...(metadata[language] || {}),
            ...updates
        };

        // Write back
        fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, 2));

        res.json({ success: true, language, updates });
    } catch (error) {
        console.error('Error updating metadata:', error);
        res.status(500).json({ error: error.message });
    }
});
```

## Improvements to Add

### 1. Loading States
```javascript
// Show spinner while saving
const saveBtn = document.querySelector('#saveBtn');
saveBtn.disabled = true;
saveBtn.textContent = 'Saving...';
// ... save operation ...
saveBtn.disabled = false;
saveBtn.textContent = 'Save';
```

### 2. Better Error Messages
```javascript
if (!response.ok) {
    const error = await response.json();
    const errorMsg = error.details
        ? `Failed to save:\n${error.details.join('\n')}`
        : `Failed to save: ${error.error}`;
    alert(errorMsg);
}
```

### 3. Undo Changes
```javascript
let originalMetadata = null;

function startEdit() {
    originalMetadata = { ...currentMetadata };
    // enable edit mode
}

function cancelEdit() {
    currentMetadata = { ...originalMetadata };
    // restore form fields
    // exit edit mode
}
```

## Testing Checklist

- [ ] Modal opens correctly (from Plan 01)
- [ ] Edit button toggles edit mode
- [ ] All fields become editable
- [ ] Can modify text fields
- [ ] Logo upload works (file picker)
- [ ] Logo URL fetch works
- [ ] Paste image works
- [ ] Save button posts to API
- [ ] Metadata saves to file
- [ ] Changes persist after regeneration
- [ ] Cancel discards changes
- [ ] Validation shows errors
- [ ] Loading states show feedback
- [ ] Success/error messages clear

## Files to Check/Modify

1. `/Metrics/report_client.js` - Edit workflow logic (if exists)
2. `/Metrics/HTMLGenerator.ts` - Modal template and JS functions
3. `/server/index.js` - API endpoint for metadata updates
4. `/Languages/metadata.json` - Target file for updates

## Acceptance Criteria

✅ Can edit language metadata and save changes
✅ Logo upload/fetch functionality works
✅ Changes persist to metadata.json
✅ Regenerated report reflects changes
✅ Clear feedback for success/errors
✅ Validation prevents invalid data
✅ Cancel properly discards changes
