using Printf
using Dates

function printMatrix(puzzle)
    println("\nPuzzle:")
    for i in 1:9
        println(join(puzzle[i,:]," "))
    end
end

function readMatrixFile!(filename::AbstractString, puzzle)
    open(filename) do f
        j=1
        for (i, line) in enumerate(eachline(f))
            if !startswith(line,"#") && !isempty(strip(line))
                # print("Line $i: $line\n")
                puzzle[j,1],puzzle[j,2],puzzle[j,3],puzzle[j,4],puzzle[j,5],puzzle[j,6],
                puzzle[j,7],puzzle[j,8],puzzle[j,9]=map((x) -> parse(Int,x),split(line))
                j += 1
            end
        end
    end
end

function isPossible(y,x,val, puzzle)
    DEBUG > 0 && @printf("Is possible %i, %i, %i\n", y-1, x-1, val)
	# Find if a matching number (val) already exists
	# in the same row (y) or column (x) or within its rectangle
	for i in 1:9
		puzzle[i,x] == val && return false
    end
	for i in 1:9
		puzzle[y,i] == val && return false
    end

	# Search the Rectangle containing x & y
	# Find which 3x3 square we are in using the floor quotient
	x0 = Int(floor((x-1)/3)) * 3
	y0 = Int(floor((y-1)/3)) * 3

    for i in 1:3
        for j in 1:3
			DEBUG > 2 && @printf("Is Possible: y0+j=%i j=%i, x0+i=%i i=%i Puzzle[y0+j,x0+i]=%i, val=%i\n", y0+j-1, j-1, x0+i-1, i-1, puzzle[y0+j,x0+i], val)
			puzzle[y0+j,x0+i] == val && return false
		end
	end
	DEBUG > 0 && @printf("YES possible\n")
	return true
end

function solve!(puzzle)
    for j in 1:9
        for i in 1:9
            if puzzle[j,i] == 0
                DEBUG >0 && @printf("Solve: j=%i,i=%i: %i\n", j-1, i-1, puzzle[j,i])
                for val in 1:9
                    global count += 1
                    DEBUG >0 && @printf("Count= %i\n", count)
                    if isPossible(j,i,val, puzzle)
                        puzzle[j,i] = val
						solve!(puzzle) == 2 && return 2 # Makes sure to do a quick exit when solution was found
						puzzle[j,i] = 0
                    end
                end
                return 0
            end
        end
    end
    printMatrix(puzzle)
    @printf("\nSolved in Iterations=%i\n\n", count)
end

DEBUG = 0
count = 0

function main()
    # create 9x9 array
    puzzle = zeros(Int,9,9)
    for (arg) in ARGS
        println("$arg")
        if endswith(arg,".matrix")
            readMatrixFile!(arg, puzzle) 
            printMatrix(puzzle)
            global count = 0
			solve!(puzzle)
        end
    end
end

start = DateTime(now())
main()
duration = (DateTime(now())-start).value/1000
@printf("Seconds to process %.3f\n",duration)