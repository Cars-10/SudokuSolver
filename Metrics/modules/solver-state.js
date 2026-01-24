/**
 * Sudoku Solver State History Management
 *
 * Provides immutable state history with memory limits for step-by-step
 * animation playback. Uses circular buffer pattern to cap memory usage.
 *
 * Key Features:
 * - Immutable state storage via Object.freeze
 * - Configurable memory limit (default: 10,000 states)
 * - Forward/backward navigation with position pointer
 * - Circular buffer removes oldest states when limit exceeded
 * - Stats tracking for UI display
 *
 * Usage:
 *   const history = new SolverHistory(10000);
 *   solver.onStateChange = (state) => history.push(state);
 *   solver.solve();
 *
 *   // Navigate through history
 *   history.stepBack();
 *   history.stepForward();
 *   history.goToStart();
 *   history.goToEnd();
 */

export class SolverHistory {
    constructor(maxStates = 10000) {
        this.states = [];           // Array of frozen state objects
        this.position = -1;         // Current playback position (-1 = no states)
        this.maxStates = maxStates; // Memory limit
        this.totalPushed = 0;       // Track total states seen (even if discarded)
    }

    /**
     * Add new state to history (freezes for immutability)
     * Truncates any future states beyond current position
     * Enforces memory limit by removing oldest states
     *
     * @param {Object} state - State object with grid, row, col, value, iteration, depth, isBacktrack
     */
    push(state) {
        // When pushing, truncate any future states beyond current position
        // This handles the case where user steps back and then solver continues from there
        this.states.length = this.position + 1;

        // Create frozen copy for immutability
        // Deep freeze the grid (both outer array and inner row arrays)
        const frozenState = Object.freeze({
            grid: Object.freeze(state.grid.map(row => Object.freeze([...row]))),
            row: state.row,
            col: state.col,
            value: state.value,
            iteration: state.iteration,
            depth: state.depth,
            isBacktrack: state.isBacktrack,
            isSolved: state.isSolved || false
        });

        this.states.push(frozenState);
        this.position++;
        this.totalPushed++;

        // Enforce memory limit: remove oldest states if exceeded
        if (this.states.length > this.maxStates) {
            const excess = this.states.length - this.maxStates;
            this.states.splice(0, excess);
            this.position -= excess;

            // Safety: ensure position doesn't go negative
            if (this.position < 0) {
                this.position = 0;
            }
        }
    }

    /**
     * Check if backward navigation is possible
     */
    canStepBack() {
        return this.position > 0;
    }

    /**
     * Check if forward navigation is possible
     */
    canStepForward() {
        return this.position < this.states.length - 1;
    }

    /**
     * Step backward to previous state
     * @returns {Object|null} Previous state or null if at start
     */
    stepBack() {
        if (this.canStepBack()) {
            this.position--;
        }
        return this.getCurrentState();
    }

    /**
     * Step forward to next state
     * @returns {Object|null} Next state or null if at end
     */
    stepForward() {
        if (this.canStepForward()) {
            this.position++;
        }
        return this.getCurrentState();
    }

    /**
     * Get state at current position
     * @returns {Object|null} Current state or null if no states
     */
    getCurrentState() {
        if (this.position >= 0 && this.position < this.states.length) {
            return this.states[this.position];
        }
        return null;
    }

    /**
     * Jump to specific position in history
     * @param {number} index - Target position (0-based)
     * @returns {Object|null} State at index or null if invalid
     */
    goTo(index) {
        if (index >= 0 && index < this.states.length) {
            this.position = index;
        }
        return this.getCurrentState();
    }

    /**
     * Jump to first state in history
     * @returns {Object|null} First state or null if no states
     */
    goToStart() {
        if (this.states.length > 0) {
            this.position = 0;
        }
        return this.getCurrentState();
    }

    /**
     * Jump to last state in history
     * @returns {Object|null} Last state or null if no states
     */
    goToEnd() {
        if (this.states.length > 0) {
            this.position = this.states.length - 1;
        }
        return this.getCurrentState();
    }

    /**
     * Clear all history and reset to initial state
     */
    clear() {
        this.states = [];
        this.position = -1;
        this.totalPushed = 0;
    }

    /**
     * Get statistics for UI display
     * @returns {Object} Stats object with totalStates, currentPosition, totalPushed, memoryLimitReached
     */
    getStats() {
        return {
            totalStates: this.states.length,
            currentPosition: this.position,
            totalPushed: this.totalPushed,
            memoryLimitReached: this.totalPushed > this.maxStates,
            percentage: this.states.length > 0
                ? Math.round((this.position / (this.states.length - 1)) * 100)
                : 0
        };
    }

    /**
     * Get all states (for debugging or export)
     * Note: Returns reference to internal array - do not mutate!
     * @returns {Array} Array of frozen state objects
     */
    getAllStates() {
        return this.states;
    }

    /**
     * Get total number of states in history
     * @returns {number} Number of states
     */
    getLength() {
        return this.states.length;
    }
}

// ============================================================================
// Shared Instance Management
// ============================================================================

/**
 * Singleton instance for shared use across components
 * Use getSharedHistory() to access
 */
let sharedInstance = null;

/**
 * Get or create shared history instance
 * Useful for components that need to share the same history
 *
 * @param {number} maxStates - Maximum states to store (only used on first call)
 * @returns {SolverHistory} Shared instance
 */
export function getSharedHistory(maxStates = 10000) {
    if (!sharedInstance) {
        sharedInstance = new SolverHistory(maxStates);
    }
    return sharedInstance;
}

/**
 * Reset shared history instance
 * Call this when starting a new solve to clear previous history
 */
export function resetSharedHistory() {
    if (sharedInstance) {
        sharedInstance.clear();
    }
    sharedInstance = null;
}

// ============================================================================
// Testing Code (commented out - uncomment to run in browser console)
// ============================================================================

/*
// Test 1: Memory limit enforcement
console.log('=== Test 1: Memory Limit ===');
const history1 = new SolverHistory(100); // Small limit for testing
for (let i = 0; i < 200; i++) {
    history1.push({
        grid: [[i]],
        row: 0,
        col: 0,
        value: i % 9 + 1,
        iteration: i,
        depth: 0,
        isBacktrack: false
    });
}
console.assert(history1.states.length === 100, 'Should cap at 100 states');
console.assert(history1.totalPushed === 200, 'Should track total pushed');
console.assert(history1.getStats().memoryLimitReached === true, 'Should indicate memory limit reached');
console.log('✓ Memory limit test passed');

// Test 2: Navigation
console.log('\n=== Test 2: Navigation ===');
const history2 = new SolverHistory();
history2.push({ grid: [[1]], row: 0, col: 0, value: 1, iteration: 1, depth: 0, isBacktrack: false });
history2.push({ grid: [[2]], row: 0, col: 1, value: 2, iteration: 2, depth: 0, isBacktrack: false });
history2.push({ grid: [[3]], row: 0, col: 2, value: 3, iteration: 3, depth: 0, isBacktrack: false });

console.assert(history2.canStepBack() === true, 'Should be able to step back');
console.assert(history2.position === 2, 'Should be at position 2');

history2.stepBack();
console.assert(history2.getCurrentState().iteration === 2, 'Should be at iteration 2');
console.assert(history2.canStepForward() === true, 'Should be able to step forward');

history2.stepForward();
console.assert(history2.getCurrentState().iteration === 3, 'Should be at iteration 3');

history2.goToStart();
console.assert(history2.getCurrentState().iteration === 1, 'Should be at iteration 1');

history2.goToEnd();
console.assert(history2.getCurrentState().iteration === 3, 'Should be at iteration 3');
console.log('✓ Navigation test passed');

// Test 3: Immutability
console.log('\n=== Test 3: Immutability ===');
const history3 = new SolverHistory();
const originalGrid = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
history3.push({ grid: originalGrid, row: 0, col: 0, value: 1, iteration: 1, depth: 0, isBacktrack: false });

const state = history3.getCurrentState();
let immutable = false;
try {
    state.grid[0][0] = 999; // Should throw in strict mode or fail silently
    immutable = state.grid[0][0] !== 999; // If frozen, value won't change
} catch (e) {
    immutable = true; // Threw error because frozen
}
console.assert(immutable, 'Grid should be frozen/immutable');
console.log('✓ Immutability test passed');

// Test 4: Stats
console.log('\n=== Test 4: Stats ===');
const history4 = new SolverHistory(10);
for (let i = 0; i < 5; i++) {
    history4.push({ grid: [[i]], row: 0, col: 0, value: i, iteration: i, depth: 0, isBacktrack: false });
}
const stats = history4.getStats();
console.assert(stats.totalStates === 5, 'Should have 5 states');
console.assert(stats.currentPosition === 4, 'Should be at position 4');
console.assert(stats.totalPushed === 5, 'Should have pushed 5 states');
console.assert(stats.memoryLimitReached === false, 'Should not have reached limit');
console.assert(stats.percentage === 100, 'Should be at 100%');
console.log('✓ Stats test passed');

console.log('\n=== All Tests Passed ===');
*/
