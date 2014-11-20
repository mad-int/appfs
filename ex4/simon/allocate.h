/**@file   allocate.h
 * @brief  Wrapper fuer malloc
 * @author Thorsten Koch
 * $Id: allocate.h,v 1.4 2004/05/13 12:12:42 bzfkocht Exp $
 */
#ifndef _ALLOCATE_H_
#define _ALLOCATE_H_

/*lint -sem(  allocate, 1n > 0 && 2n > 0, @p) */
extern void*  allocate(int elems, int size);
/*lint -sem(  deallocate, custodial(1), 1p) */
extern void   deallocate(void* p);

#endif /* _ALLOCATE_H_ */
