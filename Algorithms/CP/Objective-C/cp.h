#ifndef CP_H
#define CP_H

#import <Foundation/Foundation.h>
#include <stdint.h>

// ============================================================================
// DATA STRUCTURES
// ============================================================================

// Candidate tracking using bitsets (9 bits for digits 1-9)
typedef uint16_t CandidateSet;

// ============================================================================
// CANDIDATE MANIPULATION MACROS
// ============================================================================

#define HAS_CANDIDATE(set, digit) ((set) & (1 << (digit)))
#define ADD_CANDIDATE(set, digit) ((set) |= (1 << (digit)))
#define REMOVE_CANDIDATE(set, digit) ((set) &= ~(1 << (digit)))
#define COUNT_CANDIDATES(set) (__builtin_popcount(set))

// ============================================================================
// CP GRID - Grid structure with assigned values and candidate tracking
// ============================================================================

@interface CPGrid : NSObject {
@public
    int values[9][9];               // Assigned values (0 = empty)
    CandidateSet candidates[9][9];  // Possible values per cell (bitset)
}

- (void)copyFrom:(CPGrid*)other;

@end

// ============================================================================
// FUNCTION DECLARATIONS (C-style for algorithm)
// ============================================================================

// Initialization
void initGrid(CPGrid *grid, int puzzle[9][9]);

// Constraint propagation
int propagate(CPGrid *grid);
int eliminate(CPGrid *grid, int row, int col, int digit);
int assign(CPGrid *grid, int row, int col, int digit);

// Search
int cpSearch(CPGrid *grid, int *solution);
int findMrvCell(CPGrid *grid, int *row, int *col);

// Helper functions
int countCandidates(CandidateSet cs);
int getFirstCandidate(CandidateSet cs);
void getPeers(int row, int col, int peers[20][2]);

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================

extern long long cpIterations;

#endif // CP_H
