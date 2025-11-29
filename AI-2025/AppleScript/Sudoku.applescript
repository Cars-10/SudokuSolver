use AppleScript version "2.4" -- Yosemite (10.10) or later
use scripting additions

property puzzle : {}
property count_iter : 0

on printPuzzle()
	log "Puzzle:"
	repeat with r from 1 to 9
		set lineStr to ""
		repeat with c from 1 to 9
			set val to item c of item r of puzzle
			set lineStr to lineStr & val & " "
		end repeat
		log lineStr
	end repeat
end printPuzzle

on readMatrixFile(filename)
	log filename
	set puzzle to {}
	repeat 9 times
		set end of puzzle to {0, 0, 0, 0, 0, 0, 0, 0, 0}
	end repeat
	
	try
		set fileContent to read filename
		set fileLines to paragraphs of fileContent
		set row to 1
		repeat with aLine in fileLines
			if length of aLine > 0 and character 1 of aLine is not "#" then
				set parts to words of aLine
				if (count of parts) = 9 then
					repeat with col from 1 to 9
						set item col of item row of puzzle to (item col of parts) as integer
					end repeat
					set row to row + 1
					if row > 9 then exit repeat
				end if
			end if
		end repeat
	on error errMsg
		log "Error reading file: " & errMsg
	end try
end readMatrixFile

on isPossible(r, c, val)
	repeat with i from 1 to 9
		if item c of item i of puzzle = val then return false
		if item i of item r of puzzle = val then return false
	end repeat
	
	set r0 to ((r - 1) div 3) * 3
	set c0 to ((c - 1) div 3) * 3
	
	repeat with i from 1 to 3
		repeat with j from 1 to 3
			if item (c0 + j) of item (r0 + i) of puzzle = val then return false
		end repeat
	end repeat
	return true
end isPossible

on solve()
	repeat with r from 1 to 9
		repeat with c from 1 to 9
			if item c of item r of puzzle = 0 then
				repeat with val from 1 to 9
					set count_iter to count_iter + 1
					if isPossible(r, c, val) then
						set item c of item r of puzzle to val
						if solve() then return true
						set item c of item r of puzzle to 0
					end if
				end repeat
				return false
			end if
		end repeat
	end repeat
	
	printPuzzle()
	log "Solved in Iterations=" & count_iter
	log ""
	return true
end solve

on run argv
	set startTime to current date
	
	repeat with filename in argv
		if filename ends with ".matrix" then
			readMatrixFile(filename)
			printPuzzle()
			set count_iter to 0
			set solved to solve()
		end if
	end repeat
	
	set endTime to current date
	set timeDiff to endTime - startTime
	log "Seconds to process " & timeDiff
end run
