// Had to use full path here in order for it to resovle and compile
import now from '/usr/local/lib/node_modules/npm/node_modules/performance-now';

var start = now();
var count;
var DEBUG = 0; //0 off, 1 High Level, 3 Low Level

// JavaScript only has one dimensional arrays, so for 2D need to create an array of arrays
var puzzle = [];
for (var i=0;i<9;i++) {
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
    const data = fs.readFileSync(filename, 'utf8') 
    if (DEBUG == 3) console.log('Read File Contents');
    var lines = data.split('\n');
    for (i = 0 ; i < lines.length; i++) {
        if (DEBUG == 3) console.log('lines substring %s',lines[i].substring(0,1));
        if (lines[i].substring(0,1).localeCompare('#') != 0) {
            var entries = lines[i].split(' ');
            if (DEBUG == 3) console.log('line %s',lines[i]);
            for (var j = 0 ; j < entries.length; j++) {
                if (DEBUG == 3) console.log('Entries %d, %s',j,entries[j]);
                puzzle[line_count][j] = parseInt(entries[j]);
            }
            line_count++;
        }
    }
}

function isPossible(y, x, val) {
    if (DEBUG) console.log('Is possible %i, %i, %i count=%i\n' ,y ,x ,val, count);  // Find if a matching number (val) already exists
    // in the same row (y) or column (x) or within its rectangle
    for (let i = 0; i <9; i++) if(puzzle[i][x] == val) return 0; 
    for (let i = 0; i <9; i++) if(puzzle[y][i] == val) return 0; 
    
    // Search the Rectangle containing x & y
    // Find which 3x3 square we are in using the floor quotient
    let x0= Math.floor(x/3)*3;
    let y0= Math.floor(y/3)*3;
    if (DEBUG == 3) console.log('Is possible x=%i x0=%i, y=%i y0=%i, val=%i' , x, x0, y, y0, val);

    for (let i = 0; i <3; i++)
    {
        for (let j = 0; j <3; j++)
        {
            if (DEBUG == 3) console.log('y0+i=%i i=%i, x0+j=%i j=%i Puzzle[y0+i][x0+j]=%i, val=%i', y0+i,i, x0+j,j, puzzle[y0+i][x0+j] , val);
            if(puzzle[y0+i][x0+j] == val ) return 0; 
        }
    }
    if (DEBUG) console.log('YES possible %i, %i, %i' ,y ,x ,val);
    return 1;
}

function solve() {
    for (let j = 0; j < 9; j++) {
        for (let i = 0; i < 9; i++) {
            if (DEBUG == 3) console.log('i=%i,j=%i:%i' ,i,j,puzzle[i][j]);
            if (puzzle[j][i] == 0) {
                for (let val = 1; val < 10; val++) {
                    count += 1;
                    if (isPossible(j,i,val) == 1)
                    {
                        puzzle[j][i] = val;
                        if (DEBUG == 3) console.log("Count = %d", count);
                        if(solve() == 2) return 2; //Makes sure to do a quick exit when solution was found
                        puzzle[j][i] = 0;
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
import path from 'path';
import process from 'process';
import fs from 'fs';




var iterator = process.argv.values();
for (let arg of iterator) {
    console.log(arg);
    if (DEBUG) console.log('Main: %s\n',arg)
    ext = path.extname(arg);   
    if (ext.localeCompare(".matrix")==0) {
        console.log('Matrix File: %s',arg) 
        readMatrixFile(arg);
        printPuzzle(); 
        count = 0;
        solve();
    }
}
var time = (now() - start)/1000.0 ;
console.log('Seconds to process %s\n', time.toFixed(3) ); 
