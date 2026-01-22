// Io CP Sudoku Solver
// Constraint Propagation with MRV

// Globals
values := List clone
candidates := List clone
iterations := 0
peers := List clone

// Bitwise helpers
hasCandidate := method(mask, digit, (mask & (1 << digit)) != 0)
removeCandidate := method(mask, digit, mask & ((1 << digit) bitwiseComplement))
countCandidates := method(mask,
    c := 0
    for(i, 1, 9, if((mask & (1 << i)) != 0, c = c + 1))
    c
)
getFirstCandidate := method(mask,
    for(i, 1, 9, if((mask & (1 << i)) != 0, return i))
    0
)

// Initialize peers cache
initPeers := method(
    peers empty
    for(r, 0, 8,
        rPeers := List clone
        for(c, 0, 8,
            p := List clone
            // Row
            for(cc, 0, 8, if(cc != c, p append(list(r, cc))))
            // Col
            for(rr, 0, 8, if(rr != r, p append(list(rr, c))))
            // Box
            br := (r / 3) floor * 3
            bc := (c / 3) floor * 3
            for(rr, br, br+2,
                for(cc, bc, bc+2,
                    if(rr != r and cc != c, p append(list(rr, cc)))
                )
            )
            rPeers append(p)
        )
        peers append(rPeers)
    )
)

getPeers := method(r, c, peers at(r) at(c))

// Forward declaration
eliminate := nil

assign := method(r, c, val,
    iterations = iterations + 1
    values at(r) atPut(c, val)
    candidates at(r) atPut(c, 1 << val)
    
    p := getPeers(r, c)
    // We use a simple loop index to return false quickly
    p foreach(coord,
        if(eliminate(coord at(0), coord at(1), val) == false, return false)
    )
    true
)

eliminate = method(r, c, val,
    mask := candidates at(r) at(c)
    if(hasCandidate(mask, val) == false, return true)
    
    mask = removeCandidate(mask, val)
    candidates at(r) atPut(c, mask)
    
cnt := countCandidates(mask)
    if(cnt == 0, return false)
    
    if(cnt == 1 and values at(r) at(c) == 0,
        val2 := getFirstCandidate(mask)
        if(assign(r, c, val2) == false, return false)
    )
    
    true
)

// MRV Search
solve := method(
    // Find cell with min candidates
    minCnt := 10
    minR := -1
    minC := -1
    
    isComplete := true
    for(r, 0, 8,
        for(c, 0, 8,
            if(values at(r) at(c) == 0,
                isComplete = false
                cnt := countCandidates(candidates at(r) at(c))
                if(cnt < minCnt,
                    minCnt = cnt
                    minR = r
                    minC = c
                )
            )
        )
    )
    
    if(isComplete, return true)
    if(minR == -1, return false) // Should not happen if not complete
    
candMask := candidates at(minR) at(minC)
    
    // Save state
    savedValues := values clone map(clone)
    savedCandidates := candidates clone map(clone)
    
    for(val, 1, 9,
        if(hasCandidate(candMask, val),
            if(assign(minR, minC, val),
                if(solve, return true)
            )
            // Restore state
            values = savedValues clone map(clone)
            candidates = savedCandidates clone map(clone)
        )
    )
    
    false
)

printPuzzle := method(
    writeln("\nPuzzle:")
    values foreach(row,
        row foreach(v, write(v, " "))
        writeln("")
    )
)

runMatrix := method(filename,
    displayPath := filename
    if(filename beginsWithSeq("/app/Matrices/"),
        displayPath = "../" .. filename slice(14)
    )
    writeln(displayPath)
    
    f := File with(filename)
    if(f exists not, return)
    f openForReading
    lines := f readLines
    f close
    
    values empty
    candidates empty
    
    lines foreach(line,
        line = line strip
        if(line size > 0 and line at(0) != 35,
            parts := line split(" ") select(x, x size > 0) map(asNumber)
            if(parts size == 9,
                values append(parts)
                // Echo
                parts foreach(v, write(v, " "))
                writeln("")
            )
        )
    )
    
    // Init candidates
    for(r, 0, 8,
        rowCands := List clone
        for(c, 0, 8, rowCands append(1022)) // 1-9 set (bits 1..9) -> 1111111110 = 1022
        candidates append(rowCands)
    )
    
    initPeers
    
    printPuzzle
    iterations = 0
    
    // Initial propagation
    valid := true
    for(r, 0, 8,
        for(c, 0, 8,
            v := values at(r) at(c)
            if(v != 0,
                if(assign(r, c, v) == false, valid = false)
            )
        )
    )
    
    if(valid and solve,
        printPuzzle
        writeln("\nSolved in Iterations=", iterations)
    ,
        writeln("\nNo solution found")
    )
)

t_start := Date now
args := System args
args foreach(arg,
    if(arg endsWithSeq(".matrix"), runMatrix(arg))
)
t_end := Date now
writeln("Seconds to process ", (t_end - t_start) asNumber asString(0, 3))