/**@file   allocate.c
 * @brief  Wrapper fuer malloc
 * @author Thorsten Koch
 * $Id: allocate.c,v 1.7 2004/06/17 08:20:44 bzfkocht Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "allocate.h"

#ifdef USE_MSHELL
#include "mshell.h"
#endif

/**@brief   Alloziere Speicher.
 * @ingroup Sonstiges
 *
 *  Kann der Speicher nicht zur Verf&uuml;gung gestellt werden, so
 *  wird das Programm mit einer Fehlermeldung abgebrochen.
 *  Es muss mindestens ein Byte Speicher angefordert werden.
 *  Der Speicher ist auf 0 initialisiert, bzw. FALSE wenn es sich um
 *  #Bool Elemente handelt.
 *
 *
 *  @param elems Anzahl Elemente
 *  @param size  Groesse eines Elements in Bytes
 *  @return Zeiger auf einen Speicherbereich von mindestens
 *          #elems * #size Groesse.
 */
void* allocate(int elems, int size)
{
    void* p;
    
    assert(elems > 0);
    assert(size  > 0);
    
    if (NULL == (p = calloc((size_t)elems, (size_t)size)))
    {
        perror("allocate");
        exit(EXIT_FAILURE);
    }
    assert(p != NULL);
    
    return p;
}

/**@brief   Speicher wieder freigeben.
 * @ingroup Sonstiges
 *
 *  Gibt den Speicher wieder frei. Darf nur mit Speicherbereichen
 *  verwendet werden, die mit #allocate alloziert wurden.
 *
 *  @param p Zeiger auf einen Speicherbereich der von #allocate
 *           zur&uuml;ckgeliefert wurde
 */
void deallocate(void* p)
{
    assert(p != NULL);
    
    free(p);
}

