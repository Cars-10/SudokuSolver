#import <Foundation/Foundation.h>

int puzzle[9][9];
long long count = 0;

void printPuzzle() {
    printf("\nPuzzle:\n");
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            printf("%d ", puzzle[j][i]);
        }
        printf("\n");
    }
}

void readMatrixFile(NSString *filename) {
    printf("%s\n", [filename UTF8String]);
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        printf("Error reading file: %s\n", [[error localizedDescription] UTF8String]);
        return;
    }
    
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    int row = 0;
    for (NSString *line in lines) {
        NSString *trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        if ([trimmed length] == 0 || [trimmed hasPrefix:@"#"]) continue;
        
        NSArray *parts = [trimmed componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
        NSMutableArray *validParts = [NSMutableArray array];
        for (NSString *part in parts) {
            if ([part length] > 0) {
                [validParts addObject:part];
            }
        }
        
        if ([validParts count] == 9) {
            for (int col = 0; col < 9; col++) {
                puzzle[row][col] = [validParts[col] intValue];
            }
            row++;
            if (row == 9) break;
        }
    }
}

int isPossible(int y, int x, int val) {
    for (int i = 0; i < 9; i++) {
        if (puzzle[i][x] == val) return 0;
        if (puzzle[y][i] == val) return 0;
    }
    
    int x0 = (x / 3) * 3;
    int y0 = (y / 3) * 3;
    
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (puzzle[y0 + i][x0 + j] == val) return 0;
        }
    }
    return 1;
}

int solve() {
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[j][i] == 0) {
                for (int val = 1; val <= 9; val++) {
                    count++;
                    if (isPossible(j, i, val)) {
                        puzzle[j][i] = val;
                        if (solve() == 2) return 2;
                        puzzle[j][i] = 0;
                    }
                }
                return 0;
            }
        }
    }
    printPuzzle();
    printf("\nSolved in Iterations=%lld\n\n", count);
    return 2;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSDate *start = [NSDate date];
        
        for (int i = 1; i < argc; i++) {
            NSString *arg = [NSString stringWithUTF8String:argv[i]];
            if ([arg hasSuffix:@".matrix"]) {
                readMatrixFile(arg);
                printPuzzle();
                count = 0;
                solve();
            }
        }
        
        NSDate *end = [NSDate date];
        NSTimeInterval executionTime = [end timeIntervalSinceDate:start];
        printf("Seconds to process %.3f\n", executionTime);
    }
    return 0;
}
