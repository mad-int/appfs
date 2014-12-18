#ifndef _STRUCT_BP_H_
#define _STRUCT_BP_H_


struct BP
{
   int    size;
   int    redundant;
   int    m;
   int    n;
   TYPE* coefs;
   TYPE* rhs;
};

#endif /* _STRUCT_BP_H_ */
