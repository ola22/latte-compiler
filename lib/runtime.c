#include <stdlib.h>
#include <string.h>
#include <stdio.h>


// Funkcje biblioteczne napisane w języku C Funkcje 


void printInt_c(int64_t n) {
    printf("%ld\n", n);
}


void printString_c(const char* str) {
    printf("%s\n", str);
}


int64_t readInt_c() {
    
    int x;
    scanf("%d\n", &x);

    return (int64_t)x;
}


char* readString_c() {
    char* s = (char*)malloc(1);
    size_t len;

    getline(&s, &len, stdin);

    len = strlen(s);
    s[len - 1] = '\0';

    return s;
}


void error_c() {
    printf("Runtime error called by error function\n");
    exit(1);
}


char* strconcat_c(char* a, char* b) {
    size_t len_a, len_b;
    len_a = strlen(a);
    len_b = strlen(b);

    char* s = malloc(len_a + len_b + 1);
    memcpy(s, a, len_a);
    memcpy(s + len_a, b, len_b);
    s[len_a + len_b] = '\0';

    return s;
}


int64_t strcmp_c(char* a, char* b)
{
    return strcmp(a, b) == 0;
}


int64_t strcmpn_c(char* a, char* b)
{
    return strcmp(a, b) != 0;
}

