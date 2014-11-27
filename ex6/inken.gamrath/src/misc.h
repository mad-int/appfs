#ifndef _MISC_H_
#define _MISC_H_

#include "def.h"

/* convert string to TYPE */
extern TYPE strtov(char* str, char** test);
/* print value to stdout */
extern int vtostr(char* str, TYPE value);

#endif /* _MISC_H_ */