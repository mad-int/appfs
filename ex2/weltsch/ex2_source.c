#include <stdio.h>

// use decimal representation of ascii chars
// that need to be escaped in the string
// this makes it easier to substitute these assignments later
static const char NEW_LINE = 10;
static const char QUOTE = 34;

int main() {
    char *string = "#include <stdio.h>%c%c// use decimal representation of ascii chars%c// that need to be escaped in the string%c// this makes it easier to substitute these assignments later%cstatic const char NEW_LINE = 10;%cstatic const char QUOTE = 34;%c%cint main() {%c    char *string = %c%s%c;%c%c    printf(string, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, QUOTE, string, QUOTE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE);%c%c    return 0;%c}%c";

    printf(string, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, QUOTE, string, QUOTE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE, NEW_LINE);

    return 0;
}
