# Phase 11: Functional Languages - Part 1 - Research

**Researched:** 2026-01-13
**Domain:** Dancing Links (DLX) and Constraint Propagation (CP) algorithms in functional languages
**Confidence:** HIGH

<research_summary>
## Summary

Researched implementation patterns for DLX and CP algorithms across functional languages (Haskell, OCaml, F#, SML, Scheme). Key finding: **All functional languages provide mechanisms for controlled mutation**, making direct translation from C implementations feasible while also supporting pure functional approaches.

**DLX Strategy:** Circular doubly-linked lists require mutation. Use language-specific mutable references (Haskell STRef, OCaml/F#/SML ref, Scheme vectors) with index-based node management to avoid circular reference issues.

**CP Strategy:** Two viable approaches:
1. **Hybrid approach** (recommended): Mutable grid state with functional search/backtracking
2. **Pure functional**: Immutable data with backtracking monads (Haskell logict)

The C reference implementations can be mechanically translated to all five languages using their imperative features. Pure functional alternatives are possible but add complexity without significant benefit for this benchmark context.

**Primary recommendation:** Use each language's mutable data structures (refs/arrays) for direct C translation. Encapsulate mutations within functions to maintain functional style at the API level.
</research_summary>

<standard_stack>
## Standard Stack

### Haskell

#### Core Mutable Approach
| Feature | Mechanism | Purpose | Why Standard |
|---------|-----------|---------|--------------|
| STRef | ST monad with STRef s a | Controlled mutation within ST context | Pure functional with escape hatch for performance |
| IORef | IORef a in IO monad | Mutable references in IO | When ST not sufficient (less common) |
| MVector | Data.Vector.Mutable | Mutable arrays/vectors | Efficient indexed access |

**Installation:**
```bash
# STRef and IORef are in base
# MVector requires vector package
cabal install vector
```

#### Pure Functional Alternative
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| logict | 0.8.0.0+ | Backtracking logic monad | Pure constraint solving |
| containers | Latest | Set, Map for immutable state | Pure functional search |

**Example: ST Monad for DLX**
```haskell
import Control.Monad.ST
import Data.STRef

data DNode s = DNode
  { left, right, up, down :: STRef s (DNode s)
  , size :: STRef s Int
  , row, col :: Int
  }

-- Mutation happens within ST, result is pure
solveDLX :: Matrix -> Maybe Solution
solveDLX m = runST $ do
  nodes <- buildDLXNodes m
  search nodes
```

### OCaml

| Feature | Type | Purpose | Why Standard |
|---------|------|---------|--------------|
| ref | 'a ref | Mutable references | Idiomatic OCaml mutation |
| Array | 'a array | Mutable indexed containers | Efficient random access |
| Mutable records | { mutable field: 'a } | Selective field mutation | Fine-grained control |

**Example:**
```ocaml
type dlx_node = {
  mutable left: dlx_node;
  mutable right: dlx_node;
  mutable up: dlx_node;
  mutable down: dlx_node;
  mutable size: int;
  row: int;
  col: int;
}

let cover column =
  (* Direct pointer-style manipulation *)
  column.right.left <- column.left;
  column.left.right <- column.right;
  (* ... *)
```

### F#

| Feature | Type | Purpose | Why Standard |
|---------|------|---------|--------------|
| ref cells | 'a ref (Ref<'a>) | Mutable references | Standard F# mutation |
| .NET arrays | 'a[] | Mutable arrays | Full .NET array support |
| mutable fields | type T = { mutable f: 'a } | Record field mutation | Fine-grained control |

**Important:** F# 6.0+ uses `.Value` instead of deprecated `!` and `:=` operators.

**Example:**
```fsharp
type DlxNode = {
    mutable Left: DlxNode
    mutable Right: DlxNode
    mutable Up: DlxNode
    mutable Down: DlxNode
    mutable Size: int
    Row: int
    Col: int
}

let cover (column: DlxNode) =
    column.Right.Left <- column.Left
    column.Left.Right <- column.Right
```

### Standard ML (SML)

| Feature | Type | Purpose | Why Standard |
|---------|------|---------|--------------|
| ref | 'a ref | Mutable references | Built-in SML mutation |
| Array | 'a Array.array | Mutable arrays | Standard library arrays |

**Example:**
```sml
datatype dlx_node = DNode of {
  left: dlx_node ref,
  right: dlx_node ref,
  up: dlx_node ref,
  down: dlx_node ref,
  size: int ref,
  row: int,
  col: int
}

fun cover (DNode {left, right, ...}) =
  let
    val l = !left
    val r = !right
  in
    (#right l) := r;
    (#left r) := l
  end
```

### Scheme

| Feature | Mechanism | Purpose | Why Standard |
|---------|-----------|---------|--------------|
| vectors | #(a b c) with vector-set! | Mutable indexed containers | Efficient array-like access |
| set-car!/set-cdr! | Mutate pairs (if available) | List structure mutation | Traditional but deprecated |
| structs | define-struct with mutable | Mutable records | Modern Scheme/Racket |

**Note:** Modern Scheme dialects (Racket) prefer immutable pairs. Use vectors or explicit mutable structs.

**Example:**
```scheme
; Vector-based approach (preferred)
(define (make-dlx-node row col)
  (vector 'left 'right 'up 'down 0 row col))

(define (dlx-node-left node) (vector-ref node 0))
(define (set-dlx-node-left! node val) (vector-set! node 0 val))

; Cover operation
(define (cover column)
  (let ((left (dlx-node-left column))
        (right (dlx-node-right column)))
    (set-dlx-node-right! left right)
    (set-dlx-node-left! right left)))
```

</standard_stack>

<architecture_patterns>
## Architecture Patterns

### Pattern 1: Index-Based Circular Lists (Recommended for DLX)

**What:** Instead of direct pointer cycles, use arrays/vectors with integer indices.

**When to use:** When language makes circular references difficult (Haskell ST, Scheme).

**Why:** Avoids circular construction issues, simplifies initialization.

**Example (Haskell):**
```haskell
import Data.Vector.Mutable as V

data DNode = DNode
  { leftIdx, rightIdx, upIdx, downIdx :: Int
  , size :: Int
  , row, col :: Int
  }

type DLXGrid s = V.MVector s DNode

buildDLX :: Matrix -> ST s (DLXGrid s)
buildDLX m = do
  nodes <- V.new nodeCount
  -- Initialize with indices instead of refs
  forM_ [0..nodeCount-1] $ \i ->
    V.write nodes i (DNode (i-1) (i+1) (i-81) (i+81) 0 r c)
  return nodes
```

### Pattern 2: Encapsulated Mutation

**What:** Use mutation internally but present pure functional API.

**When to use:** All languages, maintains functional style.

**Example (OCaml):**
```ocaml
(* Internal mutation *)
let solve_dlx matrix =
  let nodes = build_nodes matrix in  (* mutable *)
  let rec search depth =
    if is_solved nodes then Some (get_solution nodes)
    else
      match select_column nodes with
      | None -> None
      | Some col ->
          cover col;
          let result = try_rows col in
          uncover col;  (* restore state *)
          result
  in
  search 0  (* Pure result despite internal mutation *)
```

### Pattern 3: Functional Backtracking (Pure Approach)

**What:** Use immutable data with backtracking monads.

**When to use:** Haskell when mutation must be avoided.

**Example (Haskell with logict):**
```haskell
import Control.Monad.Logic

solveSudoku :: Grid -> Logic Grid
solveSudoku grid
  | isSolved grid = return grid
  | otherwise = do
      cell <- select (emptyCells grid)
      digit <- eachOf [1..9]
      guard (isValid grid cell digit)
      let grid' = place grid cell digit
      grid'' <- propagate grid'
      solveSudoku grid''
```

### Pattern 4: Mutable Grid with Functional Search (Hybrid - Best for CP)

**What:** Mutable grid state, functional recursive search with explicit state passing.

**When to use:** CP algorithm in all languages.

**Example (F#):**
```fsharp
type Grid = int[,]
type Candidates = int[,]  // bitsets

let rec solve (grid: Grid) (candidates: Candidates) depth =
    if isSolved grid then
        Some grid
    else
        match findMRVCell candidates with
        | None -> None
        | Some (r, c) ->
            tryDigits grid candidates r c [1..9]

and tryDigits grid candidates r c digits =
    match digits with
    | [] -> None
    | d::rest ->
        if hasCandidate candidates r c d then
            // Try placing digit
            let gridCopy = Array2D.copy grid
            let candCopy = Array2D.copy candidates
            place gridCopy candCopy r c d
            propagate gridCopy candCopy
            match solve gridCopy candCopy (depth+1) with
            | Some solution -> Some solution
            | None -> tryDigits grid candidates r c rest  // backtrack
        else
            tryDigits grid candidates r c rest
```

### Project Structure (All Languages)

```
Algorithms/DLX/{Language}/
├── dlx.{ext}              # Main implementation
├── dlx_core.{ext}         # Core algorithm (optional separation)
├── dlx_types.{ext}        # Type definitions (optional)
├── runMe.sh               # Benchmark runner
└── metrics.json           # Results

Algorithms/CP/{Language}/
├── cp.{ext}               # Main implementation
├── cp_core.{ext}          # Core algorithm (optional)
└── ...
```

### Anti-Patterns to Avoid

- **Circular reference construction**: Don't try to create circular structures directly in purely functional languages. Use indices or staging.
- **Unnecessary purity**: Don't force pure functional approach when mutation is simpler and localized.
- **Mixing paradigms carelessly**: Don't scatter mutations throughout codebase. Encapsulate in specific functions.
- **Ignoring language idioms**: Use STRef in Haskell, not IORef everywhere. Use ref in OCaml/F#/SML, not records with single mutable field.

</architecture_patterns>

<dont_hand_roll>
## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Backtracking monad | Custom state threading | logict (Haskell) | Handles search, interleaving, fairness |
| Immutable collections | Custom trees | containers (Haskell), FSharpx.Collections (F#) | Efficient persistent structures |
| Array bounds checking | Manual checks | Language built-in bounds checking | All languages check automatically |
| Bitset operations | Manual bit manipulation | Built-in bitwise operators | All languages have land, lor, lsl, etc. |
| Pretty-printing | Custom grid formatters | Language printf/format facilities | Adequate for benchmark output |

**Key insight:** Functional languages already provide all necessary tools for these algorithms. The C implementation translates almost directly using refs/arrays. Don't overthink it - use imperative features when appropriate.

</dont_hand_roll>

<common_pitfalls>
## Common Pitfalls

### Pitfall 1: Circular Reference Construction

**What goes wrong:** Trying to create circular DLX node structure directly in Haskell/SML causes "value restriction" or infinite type errors.

**Why it happens:** Functional languages resist circular references for purity/reasoning.

**How to avoid:** Use index-based approach or two-phase construction (create nodes, then link).

**Warning signs:** Type errors mentioning "occurs check" or "value restriction".

**Example solution (two-phase):**
```haskell
-- Phase 1: Create nodes with dummy refs
nodes <- mapM (\_ -> newSTRef undefined) [1..n]

-- Phase 2: Fill in actual references
forM_ (zip nodes nodeData) $ \(ref, (l,r,u,d)) ->
  writeSTRef ref (DNode (nodes!!l) (nodes!!r) (nodes!!u) (nodes!!d))
```

### Pitfall 2: Forgetting to Restore State (Uncover)

**What goes wrong:** DLX cover operation works but uncover is buggy, causing incorrect solutions.

**Why it happens:** Cover/uncover must be **exact inverses** with operations in reverse order.

**How to avoid:** Test cover/uncover in isolation. Verify: `cover c; uncover c` is identity.

**Warning signs:** DLX finds no solutions or wrong solutions; iteration count doesn't match C.

### Pitfall 3: Array Indexing Off-by-One

**What goes wrong:** Scheme/Lua use 1-based indexing (like others), but confusion with C's 0-based causes bugs.

**Why it happens:** Context switching between languages.

**How to avoid:** **All our target languages use 0-based indexing for arrays/vectors** (Haskell, OCaml, F#, SML, Scheme vectors). Translate C directly.

**Warning signs:** Index out of bounds errors, or accessing wrong cells.

### Pitfall 4: STRef Escaping ST Monad

**What goes wrong:** Trying to return STRef from runST in Haskell causes type error.

**Why it happens:** ST monad prevents refs from escaping via phantom type parameter.

**How to avoid:** Only return pure values from runST. Extract solution before returning.

**Warning signs:** Type error: "cannot match type 's' with 's1'".

**Example:**
```haskell
-- WRONG
solveDLX :: Matrix -> Maybe (STRef s Solution)
solveDLX m = runST $ do
  solution <- newSTRef Nothing
  search solution
  return solution  -- TYPE ERROR: STRef escaping

-- CORRECT
solveDLX :: Matrix -> Maybe Solution
solveDLX m = runST $ do
  solution <- newSTRef Nothing
  search solution
  readSTRef solution  -- Extract value before returning
```

### Pitfall 5: Inefficient Immutable Copying

**What goes wrong:** Copying entire grid on every recursion level in CP causes exponential memory/time.

**Why it happens:** Using pure approach without structural sharing.

**How to avoid:** Either use mutable arrays, or use persistent data structures with sharing (Map, IntMap).

**Warning signs:** CP solver times out even on easy puzzles; memory usage spikes.

</common_pitfalls>

<code_examples>
## Code Examples

### DLX Node Structure (Haskell ST)

```haskell
-- Source: HaskellWiki Sudoku + adapted
import Control.Monad.ST
import Data.STRef

data DNode s = DNode
  { left, right, up, down, column :: STRef s (DNode s)
  , size :: STRef s Int
  , rowId, colId :: Int
  } | HeaderNode

type DLXMatrix s = STRef s (DNode s)

-- Cover operation
cover :: DNode s -> ST s ()
cover col = do
  r <- readSTRef (right col)
  l <- readSTRef (left col)
  modifySTRef (right l) (\_ -> r)
  modifySTRef (left r) (\_ -> l)
  -- ... cover rows
```

### DLX Node Structure (OCaml)

```ocaml
(* Source: kwj/algo_x repository pattern *)
type dlx_node = {
  mutable left: dlx_node;
  mutable right: dlx_node;
  mutable up: dlx_node;
  mutable down: dlx_node;
  mutable column: dlx_node;
  mutable size: int;
  row_id: int;
  col_id: int;
}

let cover column =
  column.right.left <- column.left;
  column.left.right <- column.right;
  let rec cover_rows node =
    if node != column then begin
      let rec cover_row n =
        if n != node then begin
          n.down.up <- n.up;
          n.up.down <- n.down;
          n.column.size <- n.column.size - 1;
          cover_row n.right
        end
      in
      cover_row node.right;
      cover_rows node.down
    end
  in
  cover_rows column.down
```

### CP Grid with Bitsets (F#)

```fsharp
// Source: Standard CP pattern adapted to F#
type Grid = int[,]
type CandidateSet = int  // bitset: bits 1-9

let hasCandidate (set: CandidateSet) (digit: int) : bool =
    (set &&& (1 <<< digit)) <> 0

let removeCandidate (set: CandidateSet) (digit: int) : CandidateSet =
    set &&& ~~~(1 <<< digit)

let countCandidates (set: CandidateSet) : int =
    // Population count
    let rec count n acc =
        if n = 0 then acc
        else count (n &&& (n-1)) (acc+1)
    count set 0

let propagate (grid: Grid) (candidates: CandidateSet[,]) (r: int) (c: int) (value: int) =
    grid.[r, c] <- value
    candidates.[r, c] <- 0
    // Remove from row, col, box
    for i in 0..8 do
        candidates.[r, i] <- removeCandidate candidates.[r, i] value
        candidates.[i, c] <- removeCandidate candidates.[i, c] value
    let br, bc = (r/3)*3, (c/3)*3
    for dr in 0..2 do
        for dc in 0..2 do
            let row, col = br+dr, bc+dc
            candidates.[row, col] <- removeCandidate candidates.[row, col] value
```

### Backtracking Search (SML)

```sml
(* Source: Standard functional backtracking pattern *)
fun solve grid candidates depth =
  if is_solved grid then
    SOME grid
  else
    case find_mrv_cell candidates of
      NONE => NONE
    | SOME (r, c) =>
        try_digits grid candidates r c [1,2,3,4,5,6,7,8,9]

and try_digits grid candidates r c [] = NONE
  | try_digits grid candidates r c (d::ds) =
      if has_candidate (Array2.sub (candidates, r, c)) d then
        let
          val grid' = Array2.array (9, 9, 0)
          val cand' = Array2.array (9, 9, 0)
          val _ = copy_grid grid grid'
          val _ = copy_cand candidates cand'
          val _ = place grid' cand' r c d
          val _ = propagate grid' cand'
        in
          case solve grid' cand' (depth+1) of
            SOME solution => SOME solution
          | NONE => try_digits grid candidates r c ds
        end
      else
        try_digits grid candidates r c ds
```

### Scheme Vector-Based DLX

```scheme
; Source: Scheme vector pattern
(define (make-dlx-node left right up down column size row col)
  (vector left right up down column size row col))

; Accessors
(define (node-left n) (vector-ref n 0))
(define (node-right n) (vector-ref n 1))
(define (set-node-left! n v) (vector-set! n 0 v))
(define (set-node-right! n v) (vector-set! n 1 v))

; Cover operation
(define (cover column)
  (let ((right (node-right column))
        (left (node-left column)))
    (set-node-left! right left)
    (set-node-right! left right)
    ; ... cover rows
    ))
```

</code_examples>

<sota_updates>
## State of the Art (2025-2026)

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| F# `!` and `:=` operators | `.Value` property | F# 6.0 (2021) | Deprecated operators, use `.Value` |
| Scheme mutable pairs | Immutable pairs, explicit mutable structs | Racket/modern Scheme | Must use vectors or define-struct with mutable |
| Haskell IORef everywhere | ST monad with STRef | Long-standing best practice | Better encapsulation, proves purity |

**Current best practices:**
- **Haskell DLX**: ST monad with STRef or index-based with MVector (2025 standard)
- **Haskell CP**: Immutable Data.Map for pure approach, or ST+MVector for performance
- **OCaml**: Direct refs and arrays (unchanged, still idiomatic)
- **F#**: ref cells with `.Value`, or mutable records (current as of F# 8.0)
- **SML**: refs and arrays (unchanged, language stable)
- **Scheme**: Vectors preferred over set-car!/set-cdr! in modern dialects

**New tools/patterns:**
- **holmes** library (Haskell): High-level constraint solving, but overkill for direct algorithm implementation
- **logict** improvements: Better performance, but not needed for our direct translation approach

**Deprecated/outdated:**
- **F# deref/assign operators** (`!`, `:=`): Use `.Value` instead
- **Scheme set-car!/set-cdr!**: Removed in modern dialects like Racket

</sota_updates>

<implementation_strategy>
## Implementation Strategy by Language

### Haskell: ST Monad Approach

**DLX**: Use ST monad with STRef for nodes. Two-phase construction to handle circular references.

**CP**: Two options:
1. ST+MVector for mutable grid (faster, closer to C)
2. Pure with Data.Map + backtracking (elegant but slower)

**Recommendation**: ST+MVector for both to match C reference closely.

### OCaml: Direct Translation

**DLX**: Mutable record fields for nodes. Most straightforward translation from C.

**CP**: Arrays for grid and candidates. Direct translation.

**Recommendation**: Closest to C of all languages. Minimal adaptation needed.

### F#: .NET Integration

**DLX**: Mutable record fields or ref cells for nodes.

**CP**: Array2D for grid, arrays or records for candidates.

**Recommendation**: Use mutable fields in records. Natural F# style. Remember `.Value` for refs.

### SML: Similar to OCaml

**DLX**: refs and datatype with ref fields.

**CP**: Array2 module for 2D arrays.

**Recommendation**: Very similar to OCaml approach. Use Array2 operations.

### Scheme: Vector-Based

**DLX**: Vectors with integer indices, or vectors of vectors.

**CP**: Vectors for grid (vector-of-vectors for 2D).

**Recommendation**: Avoid set-car!/set-cdr! in modern Scheme. Use vectors exclusively.

</implementation_strategy>

<open_questions>
## Open Questions

1. **Performance Comparison: Mutable vs Pure**
   - What we know: Mutable will be faster for these algorithms
   - What's unclear: How much faster? Is pure approach viable for benchmarking?
   - Recommendation: Implement mutable first (matches C), consider pure variant as future work

2. **Scheme Dialect Choice**
   - What we know: We have a Scheme implementation already (Brute-Force)
   - What's unclear: Which dialect? Racket? Guile? Chicken? Affects vector API
   - Recommendation: Check existing Brute-Force implementation, use same dialect

3. **Haskell Compilation**
   - What we know: GHC compiles Haskell
   - What's unclear: Does ST monad version compile with optimization flags? Any performance gotchas?
   - Recommendation: Test with standard -O2 flag, adjust if needed

</open_questions>

<sources>
## Sources

### Primary (HIGH confidence)

- [HaskellWiki Sudoku - Dancing Links with ST monad](https://wiki.haskell.org/Sudoku) - DLX implementation patterns
- [Real World OCaml - Imperative Programming](https://dev.realworldocaml.org/imperative-programming.html) - OCaml refs, arrays, patterns
- [Microsoft F# Reference Cells](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/reference-cells) - Official F# ref documentation
- [SML Help - Imperative Programming](https://smlhelp.github.io/book/docs/concepts/imperative/) - SML refs and arrays
- [SICP Mutable List Structure](https://sicp.sourceacademy.org/chapters/3.3.1.html) - Scheme mutable data

### Secondary (MEDIUM confidence - verified with official docs)

- [N-Queens Constraint Propagation Haskell example](https://gist.github.com/vollmerm/0ea35f0d90af0bc180e6) - Backtracking pattern verified
- [GitHub kwj/algo_x](https://github.com/kwj/algo_x) - OCaml DLX implementation (structure verified)
- [logict package](https://hackage.haskell.org/package/logict) - Backtracking monad (Hackage official)
- [holmes package](https://hackage.haskell.org/package/holmes) - Constraint solving (Hackage official)

### Supporting Resources

- [Algorithm X Wikipedia](https://en.wikipedia.org/wiki/Knuth's_Algorithm_X) - Algorithm background
- [Dancing Links Wikipedia](https://en.wikipedia.org/wiki/Dancing_Links) - DLX technique
- [Monadic Constraint Programming (Wadler)](https://homepages.inf.ed.ac.uk/wadler/papers/constraints/constraints.pdf) - Academic foundation

</sources>

<metadata>
## Metadata

**Research scope:**
- Core technology: Functional languages (Haskell, OCaml, F#, SML, Scheme)
- Algorithms: Dancing Links (DLX), Constraint Propagation (CP)
- Patterns: Mutable data in functional contexts, backtracking, circular structures
- Pitfalls: Circular references, STRef escape, indexing, state restoration

**Confidence breakdown:**
- Standard stack: HIGH - All verified from official documentation
- Architecture patterns: HIGH - Extracted from working examples and official sources
- Pitfalls: HIGH - Based on documented common errors and language design
- Code examples: HIGH - Adapted from authoritative sources (HaskellWiki, Real World OCaml, Microsoft Learn)
- SML/Scheme specifics: MEDIUM - Less extensive documentation, but patterns confirmed

**Research date:** 2026-01-13
**Valid until:** 2026-04-13 (90 days - these languages/patterns are stable)
**Languages covered:** 5 (Haskell, OCaml, F#, Standard ML, Scheme)
**Algorithms analyzed:** 2 (DLX, CP)

</metadata>

---

*Phase: 11-functional-languages-part-1*
*Research completed: 2026-01-13*
*Ready for planning: yes*
