#ifndef CP_H
#define CP_H

#include <stdint.h>

// ============================================================================
// DATA STRUCTURES
// ============================================================================

// Candidate tracking using bitsets (9 bits for digits 1-9)
typedef uint16_t CandidateSet;

// Grid structure with assigned values and candidate tracking
typedef struct {
    int values[9][9];               // Assigned values (0 = empty)
    CandidateSet candidates[9][9];  // Possible values per cell (bitset)
} CPGrid;

// ============================================================================
// CANDIDATE MANIPULATION MACROS
// ============================================================================

#define HAS_CANDIDATE(set, digit) ((set) & (1 << (digit)))
#define ADD_CANDIDATE(set, digit) ((set) |= (1 << (digit)))
#define REMOVE_CANDIDATE(set, digit) ((set) &= ~(1 << (digit)))
#define COUNT_CANDIDATES(set) (__builtin_popcount(set))

// ============================================================================
// FUNCTION DECLARATIONS
// ============================================================================

// Initialization
void init_grid(CPGrid *grid, int puzzle[9][9]);

// Constraint propagation
int propagate(CPGrid *grid);
int eliminate(CPGrid *grid, int row, int col, int digit);
int assign(CPGrid *grid, int row, int col, int digit);

// Search
int cp_search(CPGrid *grid, int *solution);
int find_mrv_cell(CPGrid *grid, int *row, int *col);

// Global iteration counter
extern long long cp_iterations;

#endif // CP_H
