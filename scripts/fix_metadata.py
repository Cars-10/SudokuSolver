#!/usr/bin/env python3
"""
Fix syntax errors in LanguagesMetadata.ts introduced by fetch_metadata.py
"""
import re

def fix_metadata():
    file_path = 'Metrics/LanguagesMetadata.ts'

    with open(file_path, 'r') as f:
        content = f.read()

    # Split into before metadata, metadata section, and after metadata
    parts = content.split('export const languageMetadata')
    if len(parts) != 2:
        print("Could not find languageMetadata export")
        return

    before = parts[0] + 'export const languageMetadata'
    after_parts = parts[1].split('\n};', 1)
    metadata_section = after_parts[0]
    after = '\n};' + after_parts[1] if len(after_parts) > 1 else ''

    # Fix the metadata section
    # Pattern 1: Fix standalone commas and missing commas after "history"
    # Find: "related": "..."
    #     ,
    #         "history": "..."
    #
    #         "authors": [...]
    #         ],    "NextLang"

    # Replace standalone comma lines
    lines = metadata_section.split('\n')
    fixed_lines = []
    skip_next = False

    for i, line in enumerate(lines):
        if skip_next:
            skip_next = False
            continue

        stripped = line.strip()

        # If this is just a comma, skip it and ensure previous line has comma
        if stripped == ',':
            if fixed_lines and not fixed_lines[-1].rstrip().endswith(','):
                fixed_lines[-1] = fixed_lines[-1].rstrip() + ','
            continue

        # If line starts "history": and doesn't end with comma, add it
        if '"history":' in line and not line.rstrip().endswith(','):
            line = line.rstrip() + ','

        # If line is just whitespace before "authors", skip excessive blank lines
        if stripped == '' and i + 1 < len(lines) and '"authors":' in lines[i+1]:
            # Skip this blank line
            continue

        fixed_lines.append(line)

    metadata_section = '\n'.join(fixed_lines)

    # Fix ],}, -> ]
    metadata_section = metadata_section.replace('],},', ']')

    # Fix missing closing braces: "authors": [...] followed by new language
    # Pattern: ]    "LanguageName"
    # Should be: ]\n    },    "LanguageName"
    # Match any language name (capital letter followed by word characters)
    metadata_section = re.sub(
        r'\]\s+("(?:[A-Z][a-zA-Z0-9_+#]*)":\s*\{)',
        r']\n    },    \1',
        metadata_section
    )

    # Reassemble
    fixed_content = before + metadata_section + after

    # Write back
    with open(file_path, 'w') as f:
        f.write(fixed_content)

    print("Fixed LanguagesMetadata.ts")

if __name__ == '__main__':
    fix_metadata()
