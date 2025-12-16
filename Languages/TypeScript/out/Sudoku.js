"use strict";
var __values = (this && this.__values) || function(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
var e_1, _a;
Object.defineProperty(exports, "__esModule", { value: true });
// Had to use full path here in order for it to resovle and compile
var now = require("performance-now");
var start = now();
var count;
var DEBUG = 0; //0 off, 1 High Level, 3 Low Level
// JavaScript only has one dimensional arrays, so for 2D need to create an array of arrays
var puzzle = [];
for (var i = 0; i < 9; i++) {
    puzzle[i] = [];
}
function printPuzzle() {
    var line = "";
    console.log("\nPuzzle:");
    for (var j = 0; j < 9; j++) {
        for (var i = 0; i < 9; i++) {
            line += puzzle[j][i] + " ";
        }
        line += "\n";
    }
    console.log("%s", line);
}
function readMatrixFile(filename) {
    var line_count = 0;
    var data = fs_1.default.readFileSync(filename, 'utf8');
    if (DEBUG == 3)
        console.log('Read File Contents');
    var lines = data.split('\n');
    for (i = 0; i < lines.length; i++) {
        if (lines[i].trim().length === 0)
            continue;
        if (DEBUG == 3)
            console.log('lines substring %s', lines[i].substring(0, 1));
        if (lines[i].substring(0, 1).localeCompare('#') != 0) {
            var entries = lines[i].trim().split(/\s+/);
            if (DEBUG == 3)
                console.log('line %s', lines[i]);
            for (var j = 0; j < entries.length; j++) {
                if (DEBUG == 3)
                    console.log('Entries %d, %s', j, entries[j]);
                if (line_count < 9 && j < 9) {
                    puzzle[line_count][j] = parseInt(entries[j]);
                }
            }
            line_count++;
        }
    }
}
function isPossible(y, x, val) {
    if (DEBUG)
        console.log('Is possible %i, %i, %i count=%i\n', y, x, val, count); // Find if a matching number (val) already exists
    // in the same row (y) or column (x) or within its rectangle
    for (var i_1 = 0; i_1 < 9; i_1++)
        if (puzzle[i_1][x] == val)
            return 0;
    for (var i_2 = 0; i_2 < 9; i_2++)
        if (puzzle[y][i_2] == val)
            return 0;
    // Search the Rectangle containing x & y
    // Find which 3x3 square we are in using the floor quotient
    var x0 = Math.floor(x / 3) * 3;
    var y0 = Math.floor(y / 3) * 3;
    if (DEBUG == 3)
        console.log('Is possible x=%i x0=%i, y=%i y0=%i, val=%i', x, x0, y, y0, val);
    for (var i_3 = 0; i_3 < 3; i_3++) {
        for (var j = 0; j < 3; j++) {
            if (DEBUG == 3)
                console.log('y0+i=%i i=%i, x0+j=%i j=%i Puzzle[y0+i][x0+j]=%i, val=%i', y0 + i_3, i_3, x0 + j, j, puzzle[y0 + i_3][x0 + j], val);
            if (puzzle[y0 + i_3][x0 + j] == val)
                return 0;
        }
    }
    if (DEBUG)
        console.log('YES possible %i, %i, %i', y, x, val);
    return 1;
}
function solve() {
    for (var j = 0; j < 9; j++) {
        for (var i_4 = 0; i_4 < 9; i_4++) {
            if (DEBUG == 3)
                console.log('i=%i,j=%i:%i', i_4, j, puzzle[i_4][j]);
            if (puzzle[j][i_4] == 0) {
                for (var val = 1; val < 10; val++) {
                    count += 1;
                    if (isPossible(j, i_4, val) == 1) {
                        puzzle[j][i_4] = val;
                        if (DEBUG == 3)
                            console.log("Count = %d", count);
                        if (solve() == 2)
                            return 2; //Makes sure to do a quick exit when solution was found
                        puzzle[j][i_4] = 0;
                    }
                }
                return 0;
            }
        }
    }
    printPuzzle();
    console.log('\nSolved in Iterations=%i\n', count);
    return 2;
}
var ext = " ";
var path_1 = __importDefault(require("path"));
var process_1 = __importDefault(require("process"));
var fs_1 = __importDefault(require("fs"));
var iterator = process_1.default.argv.values();
try {
    for (var iterator_1 = __values(iterator), iterator_1_1 = iterator_1.next(); !iterator_1_1.done; iterator_1_1 = iterator_1.next()) {
        var arg = iterator_1_1.value;
        console.log(arg);
        if (DEBUG)
            console.log('Main: %s\n', arg);
        ext = path_1.default.extname(arg);
        if (ext.localeCompare(".matrix") == 0) {
            console.log('Matrix File: %s', arg);
            readMatrixFile(arg);
            printPuzzle();
            count = 0;
            solve();
        }
    }
}
catch (e_1_1) { e_1 = { error: e_1_1 }; }
finally {
    try {
        if (iterator_1_1 && !iterator_1_1.done && (_a = iterator_1.return)) _a.call(iterator_1);
    }
    finally { if (e_1) throw e_1.error; }
}
var time = (now() - start) / 1000.0;
console.log('Seconds to process %s\n', time.toFixed(3));
//# sourceMappingURL=Sudoku.js.map