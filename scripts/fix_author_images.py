#!/usr/bin/env python3
"""
Fix author image paths in static_data.js to match actual files on disk
"""

import os
import re
import json

STATIC_DATA_FILE = "public/static_data.js"
ALGORITHMS_DIR = "Algorithms/BruteForce"

def get_author_images(lang_dir):
    """Get all non-logo images in the Media folder"""
    media_dir = os.path.join(ALGORITHMS_DIR, lang_dir, "Media")
    if not os.path.exists(media_dir):
        return []

    images = []
    for f in os.listdir(media_dir):
        if f.lower().endswith(('.jpg', '.jpeg', '.png', '.gif', '.webp')):
            if 'logo' not in f.lower():
                images.append(f"Algorithms/BruteForce/{lang_dir}/Media/{f}")
    return images

def main():
    # Read static_data.js
    with open(STATIC_DATA_FILE, 'r') as f:
        content = f.read()

    # Extract languageMetadata
    match = re.search(r'const languageMetadata = ({.*?});', content, re.DOTALL)
    if not match:
        print("Could not find languageMetadata in static_data.js")
        return

    try:
        metadata = json.loads(match.group(1))
    except json.JSONDecodeError as e:
        print(f"Failed to parse JSON: {e}")
        return

    # Get list of language directories
    lang_dirs = [d for d in os.listdir(ALGORITHMS_DIR)
                 if os.path.isdir(os.path.join(ALGORITHMS_DIR, d))]

    updated = 0

    for lang_dir in lang_dirs:
        # Get actual images on disk
        actual_images = get_author_images(lang_dir)

        # Find matching metadata key
        meta_key = None
        if lang_dir in metadata:
            meta_key = lang_dir
        else:
            # Try common mappings
            mappings = {
                "C_Sharp": "C#",
                "F_Sharp": "F#",
                "BASH": "Bash",
            }
            if lang_dir in mappings and mappings[lang_dir] in metadata:
                meta_key = mappings[lang_dir]

        if not meta_key or meta_key not in metadata:
            continue

        lang_meta = metadata[meta_key]

        # Check if authors exist and need updating
        if 'authors' not in lang_meta or not lang_meta['authors']:
            continue

        # Update author images
        for i, author in enumerate(lang_meta['authors']):
            if not actual_images:
                # No images on disk, remove image field
                if 'image' in author:
                    del author['image']
                    updated += 1
            elif i < len(actual_images):
                # Assign images in order
                old_image = author.get('image', '')
                new_image = actual_images[i]
                if old_image != new_image:
                    author['image'] = new_image
                    updated += 1
                    print(f"Updated {meta_key} author {author.get('name', 'Unknown')}: {new_image}")

    # Rebuild the languageMetadata line
    new_metadata_json = json.dumps(metadata, ensure_ascii=False)
    new_line = f"const languageMetadata = {new_metadata_json};"

    # Replace in content
    content = re.sub(r'const languageMetadata = \{.*?\};', new_line, content, flags=re.DOTALL)

    # Write back
    with open(STATIC_DATA_FILE, 'w') as f:
        f.write(content)

    print(f"Updated {updated} author image paths")

if __name__ == "__main__":
    main()
