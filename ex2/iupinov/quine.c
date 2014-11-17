#include <stdio.h>
int main()
{
	char nl = 0x0a, dq = 0x22, tab = 0x09;
	char str[]="#include <stdio.h>%cint main()%c{%c%cchar nl = 0x0a, dq = 0x22, tab = 0x09;%c%cchar str[]=%c%s%c;%c%cprintf(str, nl, nl, nl, tab, nl, tab, dq, str, dq, nl, tab, nl, nl);%c}%c";
	printf(str, nl, nl, nl, tab, nl, tab, dq, str, dq, nl, tab, nl, nl);
}
