#include <stdlib.h> // strtoi
#include <stdio.h> // fprintf
#include <string.h>

#include "misc.h"

TYPE strtov(char* str, char** test)
{
#if defined INT
   return strtol(str, test, 10);
#else
   return strtod(str, test);
#endif /* #if defined INT */
}

int vtostr(char* str, TYPE value)
{
#if defined INT
   sprintf(str, "%i", value);
#else
   sprintf(str, "%f", value);
#endif /* #if defined INT */
   return strlen(str);
}