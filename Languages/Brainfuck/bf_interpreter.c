#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAPE_SIZE 30000

void interpret(const char *code, FILE *input) {
    unsigned char tape[TAPE_SIZE] = {0};
    unsigned char *ptr = tape;
    const char *pc = code;
    
    while (*pc) {
        switch (*pc) {
            case '>': ++ptr; break;
            case '<': --ptr; break;
            case '+': ++*ptr; break;
            case '-': --*ptr; break;
            case '.': putchar(*ptr); fflush(stdout); break;
            case ',': {
                int c = fgetc(input);
                *ptr = (c == EOF) ? 0 : (unsigned char)c;
                break;
            }
            case '[':
                if (!*ptr) {
                    int loop = 1;
                    while (loop > 0) {
                        ++pc;
                        if (*pc == '[') ++loop;
                        else if (*pc == ']') --loop;
                    }
                }
                break;
            case ']':
                if (*ptr) {
                    int loop = 1;
                    while (loop > 0) {
                        --pc;
                        if (*pc == '[') --loop;
                        else if (*pc == ']') ++loop;
                    }
                }
                break;
        }
        ++pc;
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <bf_file> [input_file]\n", argv[0]);
        return 1;
    }
    
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("fopen source");
        return 1;
    }
    
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    char *code = malloc(size + 1);
    fread(code, 1, size, f);
    code[size] = 0;
    fclose(f);
    
    FILE *input = stdin;
    if (argc > 2) {
        input = fopen(argv[2], "rb");
        if (!input) {
            perror("fopen input");
            free(code);
            return 1;
        }
    }
    
    interpret(code, input);
    
    if (input != stdin) fclose(input);
    free(code);
    return 0;
}
