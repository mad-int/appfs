#ifndef _ENUM_BASIC_H_
#define _ENUM_BASIC_H_

extern unsigned int enumerate_basic(
   const BIP*         bip,
   const int          cols,
   const int          rows,
   const int          equations,
   const Numb**       col, /* [cols] */
   const Numb*        rhs, /* [rows] */
   void  (*report_sol)(const BIP* bip, unsigned int x));

#endif // _ENUM_BASIC_H_

