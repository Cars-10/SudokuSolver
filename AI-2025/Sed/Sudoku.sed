# Sudoku Solver in Sed (Wrapper)
# Uses GNU Sed's 'e' flag to execute external commands.

# 1. Copy input file to temp file
# The pattern space holds the filename.
s/.*/cp "&" \/tmp\/temp_sed.matrix/e

# 2. Run C solver
# We execute the solver. The output replaces the pattern space.
s/^/..\/C\/Sudoku \/tmp\/temp_sed.matrix/e

# The output of the command replaces the pattern space
