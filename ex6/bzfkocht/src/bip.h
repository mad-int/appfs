/**@file   bip.h
 * @brief  BIP management header
 * @author Thorsten Koch
 * @date   22Nov2014
 */  
#ifndef _BIP_H_
#define _BIP_H_

#if defined(USE_INT)
typedef int Numb;
#elif defined(USE_LONGLONG) 
typedef long long Numb;
#elif defined(USE_FLOAT) 
typedef float Numb;
#elif defined(USE_DOUBLE)
typedef double Numb;
#elif defined(USE_LONGDOUBLE)
typedef long double Numb;
#else
#error "No number type defined"
#endif

typedef struct binary_program BIP;

typedef enum { LE = 0, GE = 1, EQ = 2 } Sense;

// Good Interface?
// extern void bip_exit(void)  ?

extern void bip_init(void);
extern void bip_free(BIP* bip);
extern BIP* bip_read(const char* filename);
extern void bip_print(const BIP* bip, FILE* fp);
extern void bip_scale(BIP* bip);
extern bool bip_is_feasible(const BIP* bip, const unsigned int x);
extern int bip_rows(const BIP* bip);
extern int bip_cols(const BIP* bip);
extern Numb bip_a(const BIP* bip, int r, int c);
extern Numb bip_rhs(const BIP* bip, int r);
extern Sense bip_sense(const BIP* bip, int r);

#endif // _BIP_ENUM_H_
