#include <stdlib.h>
#include <string.h>
#include <stdio.h>
int main() {
char newline = 10;
char comma = 44;
char quotation_mark = 34;
char semicolon = 59;
char r_curly_brace =125;
char *line[33] = {
"#include <stdlib.h>",
"#include <string.h>",
"#include <stdio.h>",
"int main() {",
"char newline = 10;",
"char comma = 44;",
"char quotation_mark = 34;",
"char semicolon = 59;",
"char r_curly_brace =125;",
"char *line[33] = {",
"for (int i = 0; i < 10; i++) {",
"printf(line[i]);",
"putchar(newline);",
"}",
"for (int i = 0; i < 32; i++) {",
"putchar(quotation_mark);",
"printf(line[i]);",
"putchar(quotation_mark);",
"putchar(comma);",
"putchar(newline);",
"}",
"putchar(quotation_mark);",
"printf(line[32]);",
"putchar(quotation_mark);",
"putchar(newline);",
"putchar(r_curly_brace);",
"putchar(semicolon);",
"putchar(newline);",
"for (int i = 10; i < 33; i++) {",
"printf(line[i]);",
"putchar(newline);",
"}",
"}"
};
for (int i = 0; i < 10; i++) {
printf(line[i]);
putchar(newline);
}
for (int i = 0; i < 32; i++) {
putchar(quotation_mark);
printf(line[i]);
putchar(quotation_mark);
putchar(comma);
putchar(newline);
}
putchar(quotation_mark);
printf(line[32]);
putchar(quotation_mark);
putchar(newline);
putchar(r_curly_brace);
putchar(semicolon);
putchar(newline);
for (int i = 10; i < 33; i++) {
printf(line[i]);
putchar(newline);
}
}
