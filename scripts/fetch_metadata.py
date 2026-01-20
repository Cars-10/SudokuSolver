import wikipedia
import json
import os
import requests
import re
import shutil
from bs4 import BeautifulSoup
from typing import Dict, List, Optional
import urllib3

# Suppress OpenSSL warning
urllib3.disable_warnings()

# Constants
LANGUAGES_DIR = "Algorithms/BruteForce"
MEDIA_DIR_NAME = "Media"
OUTPUT_FILE = "Metrics/LanguagesMetadata.ts"
BLACKLIST_IMAGES = [
    "commons-logo", "icon", "stub", "ambox", "text_document", "folder", "structure",
    "symbol", "question", "book", "edit", "dispute", "wiki", "padlock", "vote",
    "iso", "iec", "ansi", "logo", "flag" # We handle logos separately
]

# Master Language List
ORDERED_LANGUAGES = [
    "C", "C++", "Rust", "Zig", "Ada", "Fortran", "Pascal", "D", "Nim", "Crystal", "V", "Vala", "Go",
    "Java", "C_Sharp", "F_Sharp", "Scala", "Kotlin", "Swift", "Dart", "Julia", "R", "Haxe",
    "JavaScript", "TypeScript", "CoffeeScript", "Lua", "Python", "Ruby", "PHP", "Perl", "Raku", "Groovy", "Wren", "Red",
    "Erlang", "Elixir", "Haskell", "OCaml", "CommonLisp", "Scheme", "Racket", "Clojure", "EmacsLisp", "Vimscript",
    "Smalltalk", "Objective-C", "VisualBasic", "Cobol", "Prolog", "Rexx", "Tcl",
    "Bash", "Zsh", "Fish", "Ksh", "Tcsh", "Dash", "PowerShell", "AppleScript", "Make", "M4",
    "Awk", "Sed", "Bc", "Dc", "Jq",
    "SQLite", "XSLT", "Gnuplot", "PostScript",
    "Assembly", "Verilog", "BASIC", "Forth", "Jupyter", "Octave"
]

WIKI_QUERIES = {
    "C": "C (programming language)",
    "C++": "C++",
    "Rust": "Rust (programming language)",
    "Go": "Go (programming language)",
    "D": "D (programming language)",
    "R": "R (programming language)",
    "V": "V (programming language)",
    "Scheme": "Scheme (programming language)",
    "Assembly": "Assembly language",
    "VisualBasic": "Visual Basic",
    "C_Sharp": "C Sharp (programming language)",
    "F_Sharp": "F Sharp (programming language)",
    "CommonLisp": "Common Lisp",
    "EmacsLisp": "Emacs Lisp",
    "Objective-C": "Objective-C (programming language)",
    "PostScript": "PostScript",
    "CoffeeScript": "CoffeeScript",
    "TypeScript": "TypeScript",
    "JavaScript": "JavaScript",
    "AppleScript": "AppleScript",
    "PowerShell": "PowerShell",
    "Bash": "Bash (Unix shell)",
    "Fish": "Friendly interactive shell",
    "Tcsh": "Tcsh",
    "Ksh": "KornShell",
    "Zsh": "Z shell",
    "Make": "Make (software)",
    "Bc": "bc (programming language)",
    "Dc": "dc (computer program)",
    "Sed": "sed",
    "Awk": "AWK",
    "M4": "m4 (computer language)",
    "Jq": "jq (programming language)",
    "XSLT": "XSLT",
    "SQL": "SQL",
    "SQLite": "SQLite",
    "BASIC": "BASIC",
    "Forth": "Forth (programming language)",
    "Verilog": "Verilog",
    "VHDL": "VHDL",
    "WebAssembly": "WebAssembly",
    "SNOBOL": "SNOBOL",
    "Jupyter": "Project Jupyter",
    "Octave": "GNU Octave",
    "Haxe": "Haxe",
    "Wren": "Wren (programming language)",
    "Red": "Red (programming language)",
    "Clojure": "Clojure",
    "Kotlin": "Kotlin (programming language)",
    "Swift": "Swift (programming language)",
    "Dart": "Dart (programming language)",
    "Julia": "Julia (programming language)",
    "Lua": "Lua (programming language)",
    "Ruby": "Ruby (programming language)",
    "Perl": "Perl",
    "Raku": "Raku (programming language)",
    "Groovy": "Groovy (programming language)",
    "Erlang": "Erlang (programming language)",
    "Elixir": "Elixir (programming language)",
    "Haskell": "Haskell (programming language)",
    "OCaml": "OCaml",
    "Racket": "Racket (programming language)",
    "Vimscript": "Vim script",
    "Smalltalk": "Smalltalk",
    "Cobol": "COBOL",
    "Prolog": "Prolog",
    "Rexx": "Rexx",
    "Tcl": "Tool Command Language",
    "Gnuplot": "Gnuplot",
    "Crystal": "Crystal (programming language)",
    "Nim": "Nim (programming language)",
    "Zig": "Zig (programming language)",
    "Ada": "Ada (programming language)",
    "Fortran": "Fortran",
    "Pascal": "Pascal (programming language)",
    "Vala": "Vala (programming language)"
}

def clean_text(text):
    if not text: return ""
    # Remove [1], [a], etc.
    text = re.sub(r'\[.*?\]', '', text)
    return text.strip()

def sanitize_filename(name):
    return re.sub(r'[^a-zA-Z0-9_\-]', '', name)

def get_author_last_name(full_name):
    parts = full_name.strip().split()
    if not parts: return "Unknown"
    return parts[-1]

def download_image(url, save_path):
    try:
        response = requests.get(url, headers={'User-Agent': 'SudokuSolverBenchmark/1.0'}, timeout=10)
        if response.status_code == 200:
            with open(save_path, 'wb') as f:
                f.write(response.content)
            return True
    except Exception as e:
        print(f"Failed to download {url}: {e}")
    return False

def process_language(lang):
    query = WIKI_QUERIES.get(lang, f"{lang} (programming language)")
    print(f"Processing {lang} ({query})...")
    
    try:
        # Disable auto_suggest to prevent weird redirects
        page = wikipedia.page(query, auto_suggest=False)
        html = page.html()
        soup = BeautifulSoup(html, 'html.parser')
        
        # 1. Summary
        description = wikipedia.summary(query, sentences=3, auto_suggest=False)
        description = clean_text(description)
        
        # 2. Extract Authors
        authors = []
        infobox = soup.find('table', {'class': 'infobox'})
        if infobox:
            for row in infobox.find_all('tr'):
                text = row.get_text()
                # Normalize whitespace (including non-breaking spaces)
                text = text.replace('\xa0', ' ')
                if "Designed by" in text or "Created by" in text or "Developer" in text or "designed by" in text.lower():
                    # Look for list items or cell content
                    cell = row.find('td')
                    if cell:
                        # Remove sups
                        for sup in cell.find_all('sup'): sup.decompose()

                        # Check for list
                        items = cell.find_all('li')
                        if items:
                            for item in items:
                                authors.append(clean_text(item.get_text()))
                        else:
                            # Get text content and parse
                            content = clean_text(cell.get_text())
                            # Handle multi-line entries like "Ada 83: Jean Ichbiah\nAda 95: Tucker Taft"
                            lines = content.split('\n')
                            for line in lines:
                                line = line.strip()
                                if not line:
                                    continue
                                # Try to extract name after colon (e.g., "Ada 95: Tucker Taft")
                                if ':' in line:
                                    name = line.split(':', 1)[1].strip()
                                else:
                                    name = line
                                # Remove common prefixes
                                for prefix in ['and ', 'et al.', 'committee']:
                                    if name.lower().startswith(prefix):
                                        name = name[len(prefix):].strip()
                                # Split by comma or newline
                                for a in name.replace('\n', ',').split(','):
                                    a = a.strip()
                                    # Skip if too short or contains common non-name patterns
                                    if a and len(a) > 2 and not any(x in a.lower() for x in ['ada ', 'version', 'std', 'mil-']):
                                        authors.append(a)
                    break
        
        # Filter duplicates and bad data
        authors = [a for a in authors if len(a) < 50 and a not in ["", "Unknown"]]
        authors = list(set(authors))[:5] # Limit to top 5
        
        # 3. Find Images for Authors
        author_meta = []
        media_dir = os.path.join(LANGUAGES_DIR, lang, MEDIA_DIR_NAME)
        if not os.path.exists(media_dir):
            os.makedirs(media_dir)
            
        # Get existing files for cleanup
        existing_files = set(os.listdir(media_dir))
        kept_files = set()
        
        # Keep logo files
        for f in existing_files:
            if "logo" in f.lower():
                kept_files.add(f)
        
        for author in authors:
            last_name = get_author_last_name(author)
            found_image_url = None

            # Try to find author's Wikipedia page and get their profile image
            try:
                # Search for author's page
                author_page = wikipedia.page(author, auto_suggest=False)
                author_html = author_page.html()
                author_soup = BeautifulSoup(author_html, 'html.parser')

                # Look for infobox image
                infobox_img = author_soup.find('table', {'class': 'infobox'})
                if infobox_img:
                    img_tag = infobox_img.find('img')
                    if img_tag and img_tag.get('src'):
                        img_src = img_tag['src']
                        # Wikipedia image URLs are protocol-relative
                        if img_src.startswith('//'):
                            img_src = 'https:' + img_src

                        # Filter out common non-portrait images
                        filename = img_src.split('/')[-1]
                        if not any(bl in filename.lower() for bl in BLACKLIST_IMAGES):
                            found_image_url = img_src
                            print(f"  Found image for {author}: {filename}")
            except wikipedia.exceptions.DisambiguationError as e:
                # Try first option
                try:
                    author_page = wikipedia.page(e.options[0], auto_suggest=False)
                    author_html = author_page.html()
                    author_soup = BeautifulSoup(author_html, 'html.parser')
                    infobox_img = author_soup.find('table', {'class': 'infobox'})
                    if infobox_img:
                        img_tag = infobox_img.find('img')
                        if img_tag and img_tag.get('src'):
                            img_src = img_tag['src']
                            if img_src.startswith('//'):
                                img_src = 'https:' + img_src
                            filename = img_src.split('/')[-1]
                            if not any(bl in filename.lower() for bl in BLACKLIST_IMAGES):
                                found_image_url = img_src
                                print(f"  Found image for {author} (via {e.options[0]}): {filename}")
                except:
                    pass
            except wikipedia.exceptions.PageError:
                print(f"  No Wikipedia page found for {author}")
            except Exception as e:
                print(f"  Error searching for {author}: {e}")

            image_filename = None
            if found_image_url:
                ext = os.path.splitext(found_image_url)[1].split('?')[0]  # Remove query params
                if not ext or ext.lower() not in ['.jpg', '.jpeg', '.png', '.webp', '.gif']:
                    ext = '.jpg'  # Default
                image_filename = f"{sanitize_filename(author.replace(' ', '_'))}{ext}"
                save_path = os.path.join(media_dir, image_filename)

                # Download if not exists
                if not os.path.exists(save_path):
                    print(f"  Downloading image: {image_filename}")
                    if download_image(found_image_url, save_path):
                        kept_files.add(image_filename)
                    else:
                        image_filename = None # Failed
                else:
                    kept_files.add(image_filename)
                    print(f"  Using existing image: {image_filename}")

            author_data = {"name": author}
            if image_filename:
                # Use relative path from project root
                author_data["image"] = f"Algorithms/BruteForce/{lang}/Media/{image_filename}"

            author_meta.append(author_data)

        # 4. Cleanup
        for f in existing_files:
            if f not in kept_files:
                print(f"  Deleting unreferenced image: {f}")
                os.remove(os.path.join(media_dir, f))
                
        return {
            "creator": authors[0] if authors else "Unknown",
            "date": "Unknown", # Hard to parse reliable date from infobox consistently
            "description": description,
            "authors": author_meta,
            "website": page.url
        }

    except wikipedia.exceptions.DisambiguationError as e:
        print(f"Disambiguation error for {lang}: {e.options[:5]}")
        return None
    except wikipedia.exceptions.PageError:
        print(f"Page not found for {lang}")
        return None
    except Exception as e:
        print(f"Error processing {lang}: {e}")
        return None

if __name__ == "__main__":
    
    collected_data = {}
    
    # Process ALL languages
    langs_to_process = ORDERED_LANGUAGES 
    
    for lang in langs_to_process:
        data = process_language(lang)
        if data:
            collected_data[lang] = data
            
    # Update File
    if not os.path.exists(OUTPUT_FILE):
        print(f"Error: Output file {OUTPUT_FILE} not found.")
        exit(1)

    with open(OUTPUT_FILE, 'r') as f:
        content = f.read()
    
    for lang, meta in collected_data.items():
        # Build the author string
        auth_str = '''        "authors": [
'''
        for auth in meta["authors"]:
            auth_str += '            { ' 
            auth_str += f'"name": "{auth["name"]}"' 
            if "image" in auth:
                    auth_str += f', "image": "{auth["image"]}"'
            auth_str += ' },\n'
        auth_str = auth_str.rstrip(",\n") + "\n"
        auth_str += '        ],'
        
        # JSON dump description
        desc_str = f'        "description": {json.dumps(meta["description"])},'
        
        # Correct regex pattern with escaping for languages like C++
        lang_pattern = r'("' + re.escape(lang) + r'"\s*:\s*\{)([\s\S]*?)(\})'
        
        match = re.search(lang_pattern, content)
        if match:
            block_start = match.group(1)
            block_content = match.group(2)
            block_end = match.group(3)
            
            # Replace description
            if '"description":' in block_content:
                # Use lambda to avoid re processing backslashes in replacement
                block_content = re.sub(r'"description":\s*".*?",?', lambda m: desc_str, block_content, flags=re.DOTALL)
            else:
                block_content += f"\n{desc_str}"
                
            # Replace authors
            if '"authors":' in block_content:
                # Use lambda here too for safety
                block_content = re.sub(r'"authors":\s*(\[.*?\] .*?,?)', lambda m: auth_str, block_content, flags=re.DOTALL)
            else:
                block_content += f"\n{auth_str}"
                
            # Reassemble
            new_block = f"{block_start}{block_content}{block_end}"
            
            # Replace in main content (only the first occurrence of this specific block)
            content = content.replace(match.group(0), new_block)
        else:
            print(f"Warning: Could not find metadata block for {lang} in {OUTPUT_FILE}.")

    with open(OUTPUT_FILE, 'w') as f:
        f.write(content)
        
    print("Done.")
