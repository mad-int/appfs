#ifndef _TEMPLATE_H_
#define _TEMPLATE_H_


#if !defined(DOUBLE) && defined(INT)
#define TYPE int
#else /* if !defined DOUBLE && defined INT */
#define TYPE double

#ifndef DOUBLE
#define DOUBLE
#endif /* #ifndef DOUBLE */

#ifdef INT
#undef INT
#endif /* #ifdef INT */

#endif /* if !defined DOUBLE && defined INT */


#include "probdata.h"

#endif /* _TEMPLATE_H_ */