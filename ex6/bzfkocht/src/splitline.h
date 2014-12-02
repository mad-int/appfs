/**@file   splitline.h
 * @brief  Split line into fields Header
 * @author Thorsten Koch
 * @date   20Nov2014
 */  
#ifndef _SPLITLINE_H_
#define _SPLITLINE_H_

typedef struct line_fields LFS; ///< line filed structure

/*lint -sem(lfs_free, 1p == 1) */
extern void lfs_free(LFS* lfs);

/*lint -sem(lfs_split_line, nulterm(2) && nulterm(3), @p) */
extern LFS* lfs_split_line(LFS* lfs, const char* line, const char* comment);

/*lint -sem(lfs_used_fields, 1p == 1, @n >= 0) */
extern int  lfs_used_fields(const LFS* lfs);

/*lint -sem(lfs_get_field, 1p == 1 && 2n >= 0, nulterm(@p)) */
extern const char* lfs_get_field(const LFS* lfs, int fno);

/*lint -sem(lfs_print, 1p == 1 && 2p == 1) */
extern void lfs_print(const LFS* lfs, FILE* fp);

#endif /* _SPLITLINE_H_ */
