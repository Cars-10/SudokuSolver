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
        int i, j;
        iterations = 0;
        for (i = 0; i < 9; i++) {
            for (j = 0; j < 9; j++) {
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
        fprintf(stderr, "Error reading file: %s\n", [[error localizedDescription] UTF8String]);
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
        NSArray *parts = [trimmedLine componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
        for (NSString *part in parts) {
            if ([part length] == 0) continue;
            if (col < 9) {
                board[row][col] = [part intValue];
                printf("%d ", board[row][col]);
                col++;
            }
        }
        if (col > 0) {
            printf("\n");
            row++;
        }
    }
}

- (void)printBoard {
    int i, j;
    printf("\nPuzzle:\n");
    for (i = 0; i < 9; i++) {
        for (j = 0; j < 9; j++) {
            printf("%d ", board[i][j]);
        }
        printf("\n");
    }
}

- (BOOL)isValidRow:(int)row col:(int)col num:(int)num {
    int i, r, c;
    // Row check
    for (i = 0; i < 9; i++) {
        if (board[row][i] == num) return NO;
    }
    
    // Col check
    for (i = 0; i < 9; i++) {
        if (board[i][col] == num) return NO;
    }
    
    // Box check
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;
    
    for (r = 0; r < 3; r++) {
        for (c = 0; c < 3; c++) {
            if (board[boxRow + r][boxCol + c] == num) return NO;
        }
    }
    
    return YES;
}

- (BOOL)findEmptyRow:(int *)row col:(int *)col {
    int r, c;
    for (r = 0; r < 9; r++) {
        for (c = 0; c < 9; c++) {
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
    int row, col, num;
    
    if (![self findEmptyRow:&row col:&col]) {
        [self printBoard];
        return YES;
    }
    
    for (num = 1; num <= 9; num++) {
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
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    if (argc < 2) {
        printf("Usage: %s <matrix_file>\n", argv[0]);
        return 1;
    }
    
    NSString *filename = [NSString stringWithUTF8String:argv[1]];
    
    // Print filename like C reference
    printf("%s\n\n", [filename UTF8String]);
    
    Sudoku *sudoku = [[Sudoku alloc] init];
    [sudoku readMatrix:filename];
    
    [sudoku printBoard];
    
    if ([sudoku solve]) {
        printf("\nSolved in Iterations=%ld\n\n", [sudoku getIterations]);
    } else {
        printf("No solution found.\n");
    }
    [pool release];
    return 0;
}
