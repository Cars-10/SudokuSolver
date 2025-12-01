#import <Foundation/Foundation.h>

@interface Sudoku : NSObject {
    int board[9][9];
    long iterations;
}

- (void)readMatrix:(NSString *)filename;
- (void)printBoard;
- (BOOL)solve;
- (long)getIterations;

@end

@implementation Sudoku

- (instancetype)init {
    self = [super init];
    if (self) {
        iterations = 0;
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                board[i][j] = 0;
            }
        }
    }
    return self;
}

- (void)readMatrix:(NSString *)filename {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    
    if (error) {
        NSLog(@"Error reading file: %@", error);
        exit(1);
    }
    
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    int row = 0;
    
    for (NSString *line in lines) {
        NSString *trimmedLine = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
        if ([trimmedLine length] == 0) continue;
        if ([trimmedLine characterAtIndex:0] == '#') continue;
        
        if (row >= 9) break;
        
        int col = 0;
        for (int i = 0; i < [trimmedLine length]; i++) {
            unichar c = [trimmedLine characterAtIndex:i];
            if (c >= '0' && c <= '9') {
                if (col < 9) {
                    board[row][col] = c - '0';
                    col++;
                }
            } else if (c == '.') {
                 if (col < 9) {
                    board[row][col] = 0;
                    col++;
                }
            }
        }
        row++;
    }
}

- (void)printBoard {
    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            printf("%d ", board[i][j]);
        }
        printf("\n");
    }
}

- (BOOL)isValidRow:(int)row col:(int)col num:(int)num {
    // Row check
    for (int c = 0; c < 9; c++) {
        if (board[row][c] == num) return NO;
    }
    
    // Col check
    for (int r = 0; r < 9; r++) {
        if (board[r][col] == num) return NO;
    }
    
    // Box check
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;
    
    for (int r = 0; r < 3; r++) {
        for (int c = 0; c < 3; c++) {
            if (board[boxRow + r][boxCol + c] == num) return NO;
        }
    }
    
    return YES;
}

- (BOOL)findEmptyRow:(int *)row col:(int *)col {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (board[r][c] == 0) {
                *row = r;
                *col = c;
                return YES;
            }
        }
    }
    return NO;
}

- (BOOL)solve {
    int row, col;
    
    if (![self findEmptyRow:&row col:&col]) {
        return YES;
    }
    
    for (int num = 1; num <= 9; num++) {
        iterations++;
        if ([self isValidRow:row col:col num:num]) {
            board[row][col] = num;
            
            if ([self solve]) {
                return YES;
            }
            
            board[row][col] = 0;
        }
    }
    
    return NO;
}

- (long)getIterations {
    return iterations;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        if (argc < 2) {
            printf("Usage: %s <matrix_file>\n", argv[0]);
            return 1;
        }
        
        NSString *filename = [NSString stringWithUTF8String:argv[1]];
        Sudoku *sudoku = [[Sudoku alloc] init];
        
        [sudoku readMatrix:filename];
        
        printf("Puzzle:\n");
        [sudoku printBoard];
        
        if ([sudoku solve]) {
            printf("Puzzle:\n");
            [sudoku printBoard];
            printf("Solved in Iterations=%ld\n", [sudoku getIterations]);
        } else {
            printf("No solution found.\n");
        }
    }
    return 0;
}
