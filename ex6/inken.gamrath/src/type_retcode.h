#ifndef _TYPE_RETCODE_H_
#define _TYPE_RETCODE_H_

/** return codes for methods: non-positive return codes are errors */
typedef enum BP_Retcode
{
   BP_OKAY               =  +2,       /**< normal termination */
   BP_INFEASIBLE         =  +1,       /**< infeasibility was detected */
   BP_ERROR              =   0,       /**< unspecified error */
   BP_READERROR          =  -2,       /**< read error */
   BP_NOFILE             =  -3,       /**< file not found error */
   BP_INVALIDDATA        =  -4,       /**< error in input data */
} BP_RETCODE;           /**< return code for method */

#endif /* _TYPE_RETCODE_H_ */
