#include <stdlib.h> // strtoi
#include <stdio.h> // fprintf
#include <string.h>

#include "misc.h"

TYPE strtov(
   char* str,         /**< string to be converted */
   char** test        /**< test string for non-numerical tail of string */
)
{
#if defined INT
   return strtol(str, test, 10);
#else
   return strtod(str, test);
#endif /* #if defined INT */
}

int vtostr(
   char* str,         /**< string to write value */
   TYPE value         /**< value to be converted */
)
{
#if defined INT
   sprintf(str, "%i", value);
#else
   sprintf(str, "%f", value);
#endif /* #if defined INT */
   return strlen(str);
}
