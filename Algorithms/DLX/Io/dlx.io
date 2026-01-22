// Io DLX Sudoku Solver

Node := Object clone do(
    left := nil
    right := nil
    up := nil
    down := nil
    colHeader := nil
    rowIndex := 0
)

Column := Node clone do(
    size := 0
    id := 0
    
    init := method(
        self left = self
        self right = self
        self up = self
        self down = self
    )
)

// Global state
head := Column clone
head init
cols := List clone
solution := List clone
iterations := 0
grid := List clone

// Initialize columns
initColumns := method(
    cols empty
    head left = head
    head right = head
    
    for(i, 0, 323,
        c := Column clone
        c init
        c id = i
        
        // Link into header list
        c right = head
        c left = head left
        head left right = c
        head left = c
        
        cols append(c)
    )
)

// Add node to column
addNode := method(cIdx, rIdx,
    c := cols at(cIdx)
    n := Node clone
    n colHeader = c
    n rowIndex = rIdx
    
    // Link vertical
    n down = c
    n up = c up
    c up down = n
    c up = n
    
    c size = c size + 1
    n
)

// Link row nodes horizontally
linkRow := method(nodes,
    first := nodes at(0)
    for(i, 1, nodes size - 1,
        n := nodes at(i)
        n left = nodes at(i-1)
        n left right = n
    )
    last := nodes last
    last right = first
    first left = last
)

// Exact Cover helpers
cover := method(c,
    c right left = c left
    c left right = c right
    
    i := c down
    while(i != c,
        j := i right
        while(j != i,
            j down up = j up
            j up down = j down
            j colHeader size = j colHeader size - 1
            j = j right
        )
        i = i down
    )
)

uncover := method(c,
    i := c up
    while(i != c,
        j := i left
        while(j != i,
            j colHeader size = j colHeader size + 1
            j down up = j
            j up down = j
            j = j left
        )
        i = i up
    )
    c right left = c
    c left right = c
)

search := method(k, 
    if(head right == head, return true) // Solution found
    
    // Choose column with min size
    c := head right
    minSize := c size
    curr := c right
    while(curr != head,
        if(curr size < minSize,
            minSize = curr size
            c = curr
        )
        curr = curr right
    )
    
    cover(c)
    
    r := c down
    while(r != c,
        solution append(r)
        iterations = iterations + 1
        
        j := r right
        while(j != r,
            cover(j colHeader)
            j = j right
        )
        
        if(search(k + 1), return true)
        
        // Backtrack
        j = r left
        while(j != r,
            uncover(j colHeader)
            j = j left
        )
        
        solution removeLast
        r = r down
    )
    
    uncover(c)
    false
)

// Sudoku constraints
buildMatrix := method(inputGrid,
    initColumns
    
    for(r, 0, 8,
        for(c, 0, 8,
            val := inputGrid at(r) at(c)
            
            startV := 1
            endV := 9
            if(val != 0,
                startV = val
                endV = val
            )
            
            for(v, startV, endV,
                // Calculate row index (just for solution mapping)
                // r, c, v (1-based)
                // encoded: r*81 + c*9 + (v-1)
                rIdx := r*81 + c*9 + (v-1)
                
                // Calculate column constraints
                c1 := r*9 + c // Cell constraint (0-80)
                c2 := 81 + r*9 + (v-1) // Row constraint (81-161)
                c3 := 162 + c*9 + (v-1) // Col constraint (162-242)
                b := (r/3) floor * 3 + (c/3) floor
                c4 := 243 + b*9 + (v-1) // Box constraint (243-323)
                
                n1 := addNode(c1, rIdx)
                n2 := addNode(c2, rIdx)
                n3 := addNode(c3, rIdx)
                n4 := addNode(c4, rIdx)
                
                linkRow(list(n1, n2, n3, n4))
            )
        )
    )
)

printSolution := method(
    outGrid := List clone
    for(i, 0, 8, outGrid append(list(0,0,0,0,0,0,0,0,0)))
    
    solution foreach(node,
        idx := node rowIndex
        v := (idx % 9) + 1
        tmp := (idx / 9) floor
        c := tmp % 9
        r := (tmp / 9) floor
        outGrid at(r) atPut(c, v)
    )
    
    writeln("\nPuzzle:")
    outGrid foreach(row,
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
    
    grid empty
    lines foreach(line,
        line = line strip
        if(line size > 0 and line at(0) != 35,
            parts := line split(" ") select(x, x size > 0) map(asNumber)
            if(parts size == 9,
                grid append(parts)
                parts foreach(v, write(v, " "))
                writeln("")
            )
        )
    )
    
    buildMatrix(grid)
    
    solution empty
    iterations = 0
    if(search(0),
        printSolution
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
