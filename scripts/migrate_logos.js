const fs = require('fs');
const path = require('path');

const LOGOS_DIR = path.join(__dirname, '../logos');
const ALGO_DIR = path.join(__dirname, '../Algorithms/BruteForce');

if (!fs.existsSync(LOGOS_DIR)) {
  console.log('Logos directory not found. Already migrated?');
  process.exit(0);
}

const files = fs.readdirSync(LOGOS_DIR);

files.forEach(file => {
  if (file === 'Tailoring.json' || file === '.DS_Store') return;

  const ext = path.extname(file);
  const name = path.basename(file, ext); // e.g., "C_Sharp", "Go_Test"

  let langName = name;
  
  // Custom mappings/cleanups
  if (langName.endsWith('_Test')) {
    langName = langName.replace('_Test', '');
  }
  
  // Specific overrides if needed (based on ls output)
  const mappings = {
    'VisualBasic': 'VisualBasic', 
    'BASH': 'BASH', // logos has Bash.png (case sensitive?)
  };
  
  // Attempt to find matching directory case-insensitively if direct match fails
  let targetDir = path.join(ALGO_DIR, langName);
  
  if (!fs.existsSync(targetDir)) {
      // Try case-insensitive lookup
      const dirs = fs.readdirSync(ALGO_DIR);
      const match = dirs.find(d => d.toLowerCase() === langName.toLowerCase());
      if (match) {
          targetDir = path.join(ALGO_DIR, match);
      } else {
          // Try mapping
           if (mappings[langName]) {
               targetDir = path.join(ALGO_DIR, mappings[langName]);
           } else {
               // Special case for Bash vs BASH
               if (langName === 'Bash') {
                   // Check for BASH
                   if (fs.existsSync(path.join(ALGO_DIR, 'BASH'))) {
                       targetDir = path.join(ALGO_DIR, 'BASH');
                   }
               }
           }
      }
  }

  if (fs.existsSync(targetDir)) {
    const mediaDir = path.join(targetDir, 'Media');
    if (!fs.existsSync(mediaDir)) {
      fs.mkdirSync(mediaDir);
    }

    const srcPath = path.join(LOGOS_DIR, file);
    const destPath = path.join(mediaDir, file);

    // Prevent overwriting if already exists (or overwrite? Plan says prefer existing media files if collision, but we are moving logos... actually logos might be better? 
    // Plan said: "Ensure no duplicates (prefer existing Media/ files if collision)."
    if (fs.existsSync(destPath)) {
        console.log(`Skipping ${file} - exists in ${mediaDir}`);
    } else {
        fs.renameSync(srcPath, destPath);
        console.log(`Moved ${file} to ${mediaDir}`);
    }
  } else {
    console.warn(`Could not find language directory for ${file} (Target: ${langName})`);
  }
});

// Check if logos dir is empty enough to delete
const remaining = fs.readdirSync(LOGOS_DIR).filter(f => f !== 'Tailoring.json' && f !== '.DS_Store');
if (remaining.length === 0) {
    fs.rmSync(LOGOS_DIR, { recursive: true, force: true });
    console.log('Removed logos directory.');
} else {
    console.log('Logos directory not empty:', remaining);
}
