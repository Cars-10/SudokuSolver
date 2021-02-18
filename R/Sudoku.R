#options(echo=TRUE) # if you want see commands in output file


readMatrixFile <- function(filename) {
    puzzle <<- as.matrix(read.table(filename, header = FALSE, nrows = 9,
                          comment.char = "#", colClasses = "numeric"))
    #print(puzzle)
}

printMatrix <- function() {
    writeLines("\nPuzzle:")
    for (j in 1:9) {
        line <- ""
        for (i in 1:9) {
            line <- paste(line, puzzle[j,i])
        }
        writeLines(line)
    }
}


isPossible <- function(y,x,val) {
    if (DEBUG>0) writeLines(paste("is possible ", y-1,"," ,x-1,",", val))
	# Find if a matching number (val) already exists
	# in the same row (y) or column (x) or within its rectangle
	for (i in 1:9) 	{puzzle[i,x] == val && return (0)}
	for (i in 1:9) 	{puzzle[y,i] == val && return (0)}

	# Search the Rectangle containing x & y
	# Find which 3x3 square we are in using the floor quotient
	x0 <- as.integer(floor((x-1)/3)) * 3
	y0 <- as.integer(floor((y-1)/3)) * 3

    for (i in 1:3) {
        for (j in 1:3) {
			if (DEBUG>2) writeLines(paste("Is Possible: y0+j= j=, x0+i= i= Puzzle[y0+j,x0+i]=, val=", y0+j-1, j-1, x0+i-1, i-1, puzzle[y0+j,x0+i], val))
			puzzle[y0+j,x0+i] == val && return (0)
        }
    }
	if(DEBUG>0) writeLines("YES possible")
	return (1)
}

solve <- function() {
    for (j in 1:9) {
        for (i in 1:9) {
            if (puzzle[j,i] == 0 ) {
                if (DEBUG>0) writeLines(paste("Solve: j=",j,"i=",i,":",puzzle[j,i]))
                for (val in 1:9) {
                    count <<- count + 1
                    if (DEBUG>0) writeLines(paste("Count= ",count))
                    if (isPossible(j,i,val)==1) {
                        puzzle[j,i] <<- val
                        if(solve() == 2) return (2) # Makes sure to do a quick exit when solution was found
                        puzzle[j,i] <<- 0
                    }
                }
                return (0)
            }
        }
    }
    printMatrix()
    writeLines(paste("\nSolved in Iterations=", count,"\n"));
    return (2)
}

options(digits.secs = 6) # This is set so that milliseconds are displayed
start.time <- Sys.time()

#assign("puzzle", "", envir = .GlobalEnv)
DEBUG <<- 0
args <- commandArgs(trailingOnly = TRUE)

for (arg in args) {
    if (endsWith(matrix(arg),".matrix")) {
        print(arg)
        readMatrixFile(arg)
        printMatrix()
        count <<- 0
        solve()
    }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
writeLines(paste("Seconds to process ",format(time.taken, digits=3, nsmall=3)))
