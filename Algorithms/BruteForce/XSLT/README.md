# XSLT Sudoku Solver

## Requirements

```bash
apt install xsltproc
```

## Notes

XSLT (Extensible Stylesheet Language Transformations) is a language for transforming XML documents. This solver uses xsltproc with recursive template calls to implement the backtracking algorithm.

Due to XSLT's functional nature and deep recursion, performance is significantly slower than imperative languages.
