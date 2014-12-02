/**@file   splitline.c
 * @brief  Split line into fields  
 * @author Thorsten Koch
 * @date   20Nov2014
 */  
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "mshell.h"
#include "splitline.h"

struct line_fields
{
   int         size;
   int         used;
   char*       line;
   char**      field;
};

#define LFS_INITIAL_SIZE   100

static bool is_valid(const LFS* lfs)
{
   if (NULL == lfs || NULL == lfs->line || NULL == lfs->field)
      return false;
   if (lfs->size < LFS_INITIAL_SIZE || lfs->size < lfs->used || lfs->used < 0)
      return false;
   return true;   
}

void lfs_free(LFS* lfs)
{
   assert(is_valid(lfs));

   free(lfs->line);
   free(lfs->field);
   free(lfs);      
}

LFS* lfs_split_line(LFS* lfs, const char* line, const char* comment)
{
   assert(NULL != line);
   assert(NULL != comment);

   if (NULL == lfs)
   {
      lfs        = calloc(1, sizeof(*lfs));
      lfs->size  = LFS_INITIAL_SIZE;
      lfs->field = calloc((size_t)lfs->size, sizeof(*lfs->field));
   }
   else
   {
      assert(is_valid(lfs));
      
      lfs->used = 0;
      
      free(lfs->line);
   }
   lfs->line = strdup(line); // strdup = strcpy(malloc(strlen(s)), s);

   char* s = strpbrk(lfs->line, comment); 

   if (NULL != s) /* else line is not terminated or too long */
      *s = '\0';  /* clip comment */
       
   /* Check for garbage characters, this includes "\n", "\r"
    */
   for(s = lfs->line; *s != '\0'; s++)
      if (!isprint(*s))
         *s = ' ';

   s = lfs->line;
   
   while(isspace(*s))
      s++;

   while(*s != '\0')
   {
      assert(lfs->used <= lfs->size);
      
      if (lfs->used == lfs->size)
         lfs->field = realloc(lfs->field, sizeof(*lfs->field) * (lfs->size *= 2)); // good idea to write like this?

      assert(!isspace(*s));

      lfs->field[lfs->used] = s;
      lfs->used++;
      
      while(*s != '\0' && !isspace(*s))
         s++;

      if (*s == '\0')
         continue;

      *s++ = '\0';
      
      //printf("field[%d]=\"%s\"\n", lfs->used - 1, lfs->field[lfs->used - 1]);

      while(isspace(*s))
         s++;
   }
   return lfs;   
}

int lfs_used_fields(const LFS* lfs)
{
   assert(is_valid(lfs));

   return lfs->used;
}

const char* lfs_get_field(const LFS* lfs, int fno)
{
   assert(is_valid(lfs));

   /* Design decision:
    */
   assert(fno >= 0);
   assert(fno <  lfs->used);

   /* The above could be dependend on the input.
    * we have to make sure  it is not.
    * otherwise:
    * if (fno >= lfs->used) return NULL
    * problem: we always have to check.
    * Design by contract
    */
   return lfs->field[fno];
}

void lfs_print(const LFS* lfs, FILE* fp)
{
   assert(is_valid(lfs));
   assert(NULL != fp);

   for(int i = 0; i < lfs->used; i++)
      fprintf(fp, "Field %3d: \"%s\"\n", i, lfs->field[i]);
}
