#!/usr/bin/env python3
"""Helper script to add logos to all languages"""

LOGOS = {
    "Awk": "https://www.gnu.org/software/gawk/manual/gawk.png",
    "Bash": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Bash_Logo_Colored.svg/64px-Bash_Logo_Colored.svg.png",
    "Basic": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/BASIC_icon.svg/64px-BASIC_icon.svg.png",
    "#C++": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/ISO_C%2B%2B_Logo.svg/64px-ISO_C%2B%2B_Logo.svg.png",
    "C_Sharp": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Logo_C_sharp.svg/64px-Logo_C_sharp.svg.png",
    "Clojure": "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Clojure_logo.svg/64px-Clojure_logo.svg.png",
    "Cobol": "https://devicon-website.vercel.app/api/cobol/original.svg",
    "CoffeeScript": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/CoffeeScript_logo.svg/64px-CoffeeScript_logo.svg.png",
    "Crystal": "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/Crystal_logo.svg/64px-Crystal_logo.svg.png",
    "D": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/D_Programming_Language_logo.svg/64px-D_Programming_Language_logo.svg.png",
    "Dart": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Dart-logo.png/64px-Dart-logo.png",
    "Elixir": "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Official_Elixir_logo.png/64px-Official_Elixir_logo.png",
    "Erlang": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Erlang_logo.svg/64px-Erlang_logo.svg.png",
    "F_Sharp": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/F_Sharp_logo.svg/64px-F_Sharp_logo.svg.png",
    "Fortran": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b8/Fortran_logo.svg/64px-Fortran_logo.svg.png",
    "Go": "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Go_Logo_Blue.svg/64px-Go_Logo_Blue.svg.png",
    "Groovy": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/Groovy-logo.svg/64px-Groovy-logo.svg.png",
    "Haskell": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/64px-Haskell-Logo.svg.png",
    "Java": "https://upload.wikimedia.org/wikipedia/en/thumb/3/30/Java_programming_language_logo.svg/64px-Java_programming_language_logo.svg.png",
    "JavaScript": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/JavaScript-logo.png/64px-JavaScript-logo.png",
    "Julia": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1f/Julia_Programming_Language_Logo.svg/64px-Julia_Programming_Language_Logo.svg.png",
    "Kotlin": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Kotlin_Icon.png/64px-Kotlin_Icon.png",
    "Lua": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cf/Lua-Logo.svg/64px-Lua-Logo.svg.png",
    "Nim": "https://nim-lang.org/assets/img/logo.svg",
    "OCaml": "https://ocaml.org/logo/Colour/SVG/colour-logo.svg",
    "Octave": "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Gnu-octave-logo.svg/64px-Gnu-octave-logo.svg.png",
    "Pascal": "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Pascal_programming_language_logo.svg/64px-Pascal_programming_language_logo.svg.png",
    "Perl": "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Perl_language_logo.svg/64px-Perl_language_logo.svg.png",
    "PHP": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/64px-PHP-logo.svg.png",
    "PowerShell": "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/PowerShell_5.0_icon.png/64px-PowerShell_5.0_icon.png",
    #"Python": "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Python-logo-notext.svg/64px-Python-logo-notext.svg.png",
    "R": "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/64px-R_logo.svg.png",
    "Ruby": "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Ruby_logo.svg/64px-Ruby_logo.svg.png",
    "Rust": "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d5/Rust_programming_language_black_logo.svg/64px-Rust_programming_language_black_logo.svg.png",
    "Scala": "https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Scala-full-color.svg/64px-Scala-full-color.svg.png",
    "SQL": "https://www.svgrepo.com/show/255832/sql.svg",
    "TypeScript": "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Typescript_logo_2020.svg/64px-Typescript_logo_2020.svg.png",
}

import os
import urllib.request

# Assuming script is run from project root
TARGET_DIR = "CleanedUp/logos"
os.makedirs(TARGET_DIR, exist_ok=True)

for lang, url in LOGOS.items():
    if lang.startswith('#'):
        print(f"Skipping {lang}")
        continue
        
    try:
        # Determine extension from URL or default to .png
        ext = ".png"
        if url.endswith(".svg"):
            ext = ".svg"
        elif ".svg" in url:
             # Handle cases like .../image.svg/64px-...
             pass 
        
        # For simplicity, we'll try to keep the extension if it looks like an image, 
        # but the user's URLs are a mix. Most seem to be .png thumbnails of svgs.
        # Let's just use the name from the key + .png for consistency as requested by the user's previous code
        # But wait, some are .svg. 
        
        filename = f"{lang}.png"
        if url.endswith(".svg"):
            filename = f"{lang}.svg"
            
        filepath = os.path.join(TARGET_DIR, filename)
        
        print(f"Downloading {lang} from {url}...")
        
        # User agent to avoid 403s from some sites (like Wikipedia)
        req = urllib.request.Request(
            url, 
            data=None, 
            headers={
                'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
            }
        )
        
        with urllib.request.urlopen(req) as response, open(filepath, 'wb') as out_file:
            data = response.read()
            out_file.write(data)
            
    except Exception as e:
        print(f"Failed to download {lang}: {e}")
