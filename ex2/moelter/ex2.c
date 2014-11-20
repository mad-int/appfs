char *begin="char *begin=%c%s%c;%c#define emph '%c'%c#define newline '%cn'%c#define bslash '%c%c'%c#include <stdio.h>%cvoid main(){printf(begin,emph,begin,emph,newline,emph,newline,bslash,newline,bslash,bslash,newline,newline,newline);}%c";
#define emph '"'
#define newline '\n'
#define bslash '\\'
#include <stdio.h>
void main(){printf(begin,emph,begin,emph,newline,emph,newline,bslash,newline,bslash,bslash,newline,newline,newline);}