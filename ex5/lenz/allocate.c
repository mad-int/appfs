#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "allocate.h"

#include "mshell.h"

void* allocate(int elems, int size)
  {
   void* p;

   assert(elems > 0);
   assert(size > 0);

   if (NULL == (p = calloc((size_t)elems, (size_t)size)))
   {
      perror("allocate");
      exit(EXIT_FAILURE);
   }
   assert(p != NULL);

   return p;
}

void deallocate(void* p)
{
   assert(p != NULL);

   free(p);
}