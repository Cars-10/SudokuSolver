const fs = require('fs');
const path = require('path');

const metadataPath = path.join(__dirname, '../Algorithms/metadata.json');
let metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));

const updates = {
    "Nim": {
        history: "2008. Efficient, expressive, elegant.",
        description: "A statically typed, compiled systems programming language. It combines successful concepts from mature languages like Python, Ada and Modula.",
        paradigm: "Multi-paradigm, procedural, object-oriented",
        typeSystem: "Static, Strong, Inferred"
    },
    "Objective-C": {
        history: "1984. Smalltalk on top of C.",
        paradigm: "Object-oriented, reflective",
        typeSystem: "Static, Dynamic dispatch"
    },
    "OCaml": {
        history: "1996. Industrial strength functional programming.",
        paradigm: "Multi-paradigm, functional, object-oriented",
        typeSystem: "Static, Strong, Inferred"
    },
    "Octave": {
        history: "1988. Free alternative to MATLAB.",
        paradigm: "Numerical, scripting",
        typeSystem: "Dynamic"
    },
    "Pascal": {
        history: "1970. Structured programming for education.",
        paradigm: "Imperative, structured",
        typeSystem: "Static, Strong"
    },
    "Perl": {
        history: "1987. The Swiss Army chainsaw of scripting.",
        paradigm: "Multi-paradigm, scripting, procedural",
        typeSystem: "Dynamic, Weak"
    },
    "PHP": {
        history: "1995. Powering 80% of the web.",
        description: "A popular general-purpose scripting language that is especially suited to web development.",
        paradigm: "Imperative, functional, object-oriented",
        typeSystem: "Dynamic, Weak (optional static)"
    },
    "Pike": {
        history: "1994. C-like syntax, dynamic power.",
        paradigm: "Multi-paradigm, object-oriented",
        typeSystem: "Dynamic, Strong"
    },
    "PostScript": {
        history: "1982. The language of laser printers.",
        paradigm: "Concatenative, stack-oriented",
        typeSystem: "Dynamic, Strong"
    },
    "PowerShell": {
        history: "2006. Object-oriented shell for Windows.",
        description: "A cross-platform task automation and configuration management framework, consisting of a command-line shell and scripting language.",
        paradigm: "Imperative, object-oriented, pipeline",
        typeSystem: "Dynamic, Strong, .NET"
    },
    "Prolog": {
        history: "1972. Logic programming pioneer.",
        paradigm: "Logic, declarative",
        typeSystem: "Dynamic"
    },
    "Python": {
        history: "1991. Simple, readable, popular.",
        description: "A high-level, general-purpose programming language. Its design philosophy emphasizes code readability with the use of significant indentation.",
        paradigm: "Multi-paradigm, object-oriented, functional",
        typeSystem: "Dynamic, Strong"
    },
    "R": {
        history: "1993. Statistical computing and graphics.",
        description: "A programming language for statistical computing and graphics supported by the R Core Team and the R Foundation for Statistical Computing.",
        paradigm: "Multi-paradigm, functional, object-oriented",
        typeSystem: "Dynamic, Strong"
    },
    "Racket": {
        history: "1995. A programmable programming language.",
        description: "A general-purpose, multi-paradigm programming language based on the Scheme dialect of Lisp.",
        paradigm: "Multi-paradigm, functional, meta",
        typeSystem: "Dynamic, Strong"
    },
    "Raku": {
        history: "2015. The 100-year language (formerly Perl 6).",
        paradigm: "Multi-paradigm, functional, object-oriented",
        typeSystem: "Gradual, Strong"
    },
    "Red": {
        history: "2011. Full-stack Rebol evolution.",
        paradigm: "Multi-paradigm, symbolic, functional",
        typeSystem: "Dynamic, Strong"
    },
    "Rexx": {
        history: "1979. Mainframe scripting made easy.",
        paradigm: "Procedural, structured",
        typeSystem: "Dynamic, Weak"
    },
    "Ruby": {
        history: "1995. Optimized for developer happiness.",
        description: "A dynamic, open source programming language with a focus on simplicity and productivity. It has an elegant syntax that is natural to read and easy to write.",
        paradigm: "Multi-paradigm, object-oriented, functional",
        typeSystem: "Dynamic, Strong"
    },
    "Rust": {
        history: "2010. Performance without garbage collection.",
        paradigm: "Multi-paradigm, systems, functional",
        typeSystem: "Static, Strong, Safe, Affine"
    },
    "Scala": {
        history: "2004. Scalable language for the JVM.",
        paradigm: "Multi-paradigm, object-oriented, functional",
        typeSystem: "Static, Strong, Inferred"
    },
    "Scheme": {
        history: "1975. Minimalist Lisp dialect.",
        paradigm: "Multi-paradigm, functional",
        typeSystem: "Dynamic, Strong"
    },
    "Sed": {
        history: "1974. Stream Editor for text transformation.",
        paradigm: "Scripting, stream-editing",
        typeSystem: "Untyped"
    },
    "Smalltalk": {
        history: "1972. Everything is an object.",
        paradigm: "Object-oriented, reflective",
        typeSystem: "Dynamic, Strong"
    },
    "SQLite": {
        history: "2000. The world's most used database engine.",
        paradigm: "Relational, SQL",
        typeSystem: "Dynamic, Weak"
    },
    "Swift": {
        history: "2014. Modern, safe, fast Apple development.",
        paradigm: "Multi-paradigm, protocol-oriented",
        typeSystem: "Static, Strong, Inferred"
    },
    "Tcl": {
        history: "1988. Tool Command Language.",
        paradigm: "Scripting, procedural, event-driven",
        typeSystem: "Dynamic, String-based"
    },
    "Tcsh": {
        history: "1981. C shell with command completion.",
        paradigm: "Scripting, command line",
        typeSystem: "Dynamic"
    },
    "TypeScript": {
        history: "2012. JavaScript with syntax for types.",
        description: "A free and open source high-level programming language developed by Microsoft that adds static typing with optional type annotations to JavaScript.",
        paradigm: "Multi-paradigm, object-oriented, functional",
        typeSystem: "Static (gradual), Structural"
    },
    "V": {
        history: "2019. Simple, fast, safe, compiled.",
        paradigm: "Procedural, structured",
        typeSystem: "Static, Strong"
    },
    "Vala": {
        history: "2006. Modern features for GObject.",
        paradigm: "Object-oriented, structured",
        typeSystem: "Static, Strong"
    },
    "Verilog": {
        history: "1984. Hardware description standard.",
        paradigm: "Dataflow, hardware description",
        typeSystem: "Static"
    },
    "Vimscript": {
        history: "1991. The language of Vim.",
        paradigm: "Scripting, imperative",
        typeSystem: "Dynamic, Weak"
    },
    "VisualBasic": {
        history: "1991. Rapid Application Development pioneer.",
        paradigm: "Object-oriented, event-driven",
        typeSystem: "Static, Strong"
    },
    "Wren": {
        history: "2013. Small, fast, concurrent class-based.",
        paradigm: "Object-oriented, scripting",
        typeSystem: "Dynamic, Strong"
    },
    "XSLT": {
        history: "1998. XML transformations.",
        paradigm: "Declarative, functional",
        typeSystem: "Dynamic"
    },
    "Zig": {
        history: "2016. Robust, optimal systems programming.",
        paradigm: "Imperative, procedural, systems",
        typeSystem: "Static, Strong"
    },
    "Zsh": {
        history: "1990. Interactive shell with superpowers.",
        paradigm: "Scripting, command line",
        typeSystem: "Dynamic"
    }
};

Object.keys(updates).forEach(lang => {
    if (metadata.languageMetadata[lang]) {
        Object.assign(metadata.languageMetadata[lang], updates[lang]);
    } else {
        console.warn(`Language ${lang} not found in metadata`);
    }
});

fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, 4));
console.log("Metadata updated for batch N-Z.");
