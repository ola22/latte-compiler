#include <stdlib.h>
#include <string.h>
#include <stdio.h>




extern void printInt(int n) {
    printf("%d\n", n);
}


extern void printString(const char* str) {
    printf("%s\n", str);
}


int readInt() {
    
    int x;
    scanf("%d\n", &x);

    return x;
}


char* readString() {
    char* s = (char*)malloc(1);
    size_t len;

    getline(&s, &len, stdin);

    len = strlen(s);
    s[len - 1] = '\0';

    return s;
}


void error() {
    printf("Runtime error");
    exit(1);
}


char* addStrings(char* a, char* b) {
    size_t len_a, len_b;
    len_a = strlen(a);
    len_b = strlen(b);

    char* s = malloc(len_a + len_b + 1);
    memcpy(s, a, len_a);
    memcpy(s + len_a, b, len_b);
    s[len_a + len_b] = '\0';

    return s;
}