# 0 "csmith_r2.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "csmith_r2.c"
# 10 "csmith_r2.c"
# 1 "/usr/include/csmith/csmith.h" 1
# 40 "/usr/include/csmith/csmith.h"
# 1 "/usr/include/string.h" 1 3 4
# 26 "/usr/include/string.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 1 3 4
# 33 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 402 "/usr/include/features.h" 3 4
# 1 "/usr/include/features-time64.h" 1 3 4
# 20 "/usr/include/features-time64.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 21 "/usr/include/features-time64.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 1 3 4
# 19 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 2 3 4
# 22 "/usr/include/features-time64.h" 2 3 4
# 403 "/usr/include/features.h" 2 3 4
# 510 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 1 3 4
# 730 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 731 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/long-double.h" 1 3 4
# 732 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 2 3 4
# 511 "/usr/include/features.h" 2 3 4
# 534 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 1 3 4
# 10 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4
# 535 "/usr/include/features.h" 2 3 4
# 34 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 2 3 4
# 27 "/usr/include/string.h" 2 3 4






# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stddef.h" 1 3 4
# 214 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stddef.h" 3 4

# 214 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 34 "/usr/include/string.h" 2 3 4
# 43 "/usr/include/string.h" 3 4
extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void *memmove (void *__dest, const void *__src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));





extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
        int __c, size_t __n)
    __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__access__ (__write_only__, 1, 4)));




extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 80 "/usr/include/string.h" 3 4
extern int __memcmpeq (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 107 "/usr/include/string.h" 3 4
extern void *memchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 141 "/usr/include/string.h" 3 4
extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncat (char *__restrict __dest, const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcoll (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strxfrm (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
    __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2))) __attribute__ ((__access__ (__write_only__, 1, 3)));



# 1 "/usr/include/x86_64-linux-gnu/bits/types/locale_t.h" 1 3 4
# 22 "/usr/include/x86_64-linux-gnu/bits/types/locale_t.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/types/__locale_t.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/types/__locale_t.h" 3 4
struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;
# 23 "/usr/include/x86_64-linux-gnu/bits/types/locale_t.h" 2 3 4

typedef __locale_t locale_t;
# 173 "/usr/include/string.h" 2 3 4


extern int strcoll_l (const char *__s1, const char *__s2, locale_t __l)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));


extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
    locale_t __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)))
     __attribute__ ((__access__ (__write_only__, 1, 3)));





extern char *strdup (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));






extern char *strndup (const char *__string, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
# 246 "/usr/include/string.h" 3 4
extern char *strchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 273 "/usr/include/string.h" 3 4
extern char *strrchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 286 "/usr/include/string.h" 3 4
extern char *strchrnul (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern size_t strcspn (const char *__s, const char *__reject)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern size_t strspn (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 323 "/usr/include/string.h" 3 4
extern char *strpbrk (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 350 "/usr/include/string.h" 3 4
extern char *strstr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strtok (char *__restrict __s, const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



extern char *__strtok_r (char *__restrict __s,
    const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));

extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));
# 380 "/usr/include/string.h" 3 4
extern char *strcasestr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));







extern void *memmem (const void *__haystack, size_t __haystacklen,
       const void *__needle, size_t __needlelen)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 3)))
    __attribute__ ((__access__ (__read_only__, 1, 2)))
    __attribute__ ((__access__ (__read_only__, 3, 4)));



extern void *__mempcpy (void *__restrict __dest,
   const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *mempcpy (void *__restrict __dest,
        const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern size_t strlen (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern size_t strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern char *strerror (int __errnum) __attribute__ ((__nothrow__ , __leaf__));
# 432 "/usr/include/string.h" 3 4
extern int strerror_r (int __errnum, char *__buf, size_t __buflen) __asm__ ("" "__xpg_strerror_r") __attribute__ ((__nothrow__ , __leaf__))

                        __attribute__ ((__nonnull__ (2)))
    __attribute__ ((__access__ (__write_only__, 2, 3)));
# 458 "/usr/include/string.h" 3 4
extern char *strerror_l (int __errnum, locale_t __l) __attribute__ ((__nothrow__ , __leaf__));



# 1 "/usr/include/strings.h" 1 3 4
# 23 "/usr/include/strings.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stddef.h" 1 3 4
# 24 "/usr/include/strings.h" 2 3 4










extern int bcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bcopy (const void *__src, void *__dest, size_t __n)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 68 "/usr/include/strings.h" 3 4
extern char *index (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 96 "/usr/include/strings.h" 3 4
extern char *rindex (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));






extern int ffs (int __i) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));





extern int ffsl (long int __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern int strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));






extern int strcasecmp_l (const char *__s1, const char *__s2, locale_t __loc)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));



extern int strncasecmp_l (const char *__s1, const char *__s2,
     size_t __n, locale_t __loc)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 4)));



# 463 "/usr/include/string.h" 2 3 4



extern void explicit_bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)))
    __attribute__ ((__access__ (__write_only__, 1, 2)));



extern char *strsep (char **__restrict __stringp,
       const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strsignal (int __sig) __attribute__ ((__nothrow__ , __leaf__));
# 489 "/usr/include/string.h" 3 4
extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));



extern char *__stpncpy (char *__restrict __dest,
   const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern size_t strlcpy (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__access__ (__write_only__, 1, 3)));



extern size_t strlcat (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__access__ (__read_write__, 1, 3)));
# 552 "/usr/include/string.h" 3 4

# 41 "/usr/include/csmith/csmith.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/float.h" 1 3 4
# 42 "/usr/include/csmith/csmith.h" 2
# 1 "/usr/include/math.h" 1 3 4
# 27 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 1 3 4
# 28 "/usr/include/math.h" 2 3 4









# 1 "/usr/include/x86_64-linux-gnu/bits/types.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 1 3 4
# 19 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/timesize.h" 2 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;






typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;



typedef long int __quad_t;
typedef unsigned long int __u_quad_t;







typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
# 141 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/typesizes.h" 1 3 4
# 142 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/time64.h" 1 3 4
# 143 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef long int __suseconds64_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;




typedef int __sig_atomic_t;
# 38 "/usr/include/math.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/math-vector.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/math-vector.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/math-vector.h" 2 3 4
# 41 "/usr/include/math.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/floatn.h" 1 3 4
# 120 "/usr/include/x86_64-linux-gnu/bits/floatn.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/floatn-common.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/floatn-common.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/long-double.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/floatn-common.h" 2 3 4
# 121 "/usr/include/x86_64-linux-gnu/bits/floatn.h" 2 3 4
# 44 "/usr/include/math.h" 2 3 4
# 152 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/flt-eval-method.h" 1 3 4
# 153 "/usr/include/math.h" 2 3 4
# 163 "/usr/include/math.h" 3 4
typedef float float_t;
typedef double double_t;
# 204 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/fp-logb.h" 1 3 4
# 205 "/usr/include/math.h" 2 3 4
# 247 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/fp-fast.h" 1 3 4
# 248 "/usr/include/math.h" 2 3 4
# 312 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 3 4
extern int __fpclassify (double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));


extern int __signbit (double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));



extern int __isinf (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __finite (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __isnan (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __iseqsig (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));


extern int __issignaling (double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
# 313 "/usr/include/math.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern double acos (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __acos (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double asin (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __asin (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double atan (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __atan (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double atan2 (double __y, double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __atan2 (double __y, double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern double cos (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __cos (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double sin (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __sin (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double tan (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __tan (double __x) __attribute__ ((__nothrow__ , __leaf__));




 extern double cosh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __cosh (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double sinh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __sinh (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double tanh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __tanh (double __x) __attribute__ ((__nothrow__ , __leaf__));
# 85 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern double acosh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __acosh (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double asinh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __asinh (double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern double atanh (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __atanh (double __x) __attribute__ ((__nothrow__ , __leaf__));





 extern double exp (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __exp (double __x) __attribute__ ((__nothrow__ , __leaf__));


extern double frexp (double __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__)); extern double __frexp (double __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__));


extern double ldexp (double __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__)); extern double __ldexp (double __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__));


 extern double log (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __log (double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern double log10 (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __log10 (double __x) __attribute__ ((__nothrow__ , __leaf__));


extern double modf (double __x, double *__iptr) __attribute__ ((__nothrow__ , __leaf__)); extern double __modf (double __x, double *__iptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));
# 134 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern double expm1 (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __expm1 (double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern double log1p (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __log1p (double __x) __attribute__ ((__nothrow__ , __leaf__));


extern double logb (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __logb (double __x) __attribute__ ((__nothrow__ , __leaf__));




 extern double exp2 (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __exp2 (double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern double log2 (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __log2 (double __x) __attribute__ ((__nothrow__ , __leaf__));






 extern double pow (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __pow (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));


extern double sqrt (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __sqrt (double __x) __attribute__ ((__nothrow__ , __leaf__));



 extern double hypot (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __hypot (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));




 extern double cbrt (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __cbrt (double __x) __attribute__ ((__nothrow__ , __leaf__));






extern double ceil (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __ceil (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern double fabs (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __fabs (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern double floor (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __floor (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern double fmod (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __fmod (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));
# 192 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isinf (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));




extern int finite (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern double drem (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __drem (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));



extern double significand (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __significand (double __x) __attribute__ ((__nothrow__ , __leaf__));






extern double copysign (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __copysign (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern double nan (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__)); extern double __nan (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__));
# 228 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isnan (double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));





extern double j0 (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __j0 (double) __attribute__ ((__nothrow__ , __leaf__));
extern double j1 (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __j1 (double) __attribute__ ((__nothrow__ , __leaf__));
extern double jn (int, double) __attribute__ ((__nothrow__ , __leaf__)); extern double __jn (int, double) __attribute__ ((__nothrow__ , __leaf__));
extern double y0 (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __y0 (double) __attribute__ ((__nothrow__ , __leaf__));
extern double y1 (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __y1 (double) __attribute__ ((__nothrow__ , __leaf__));
extern double yn (int, double) __attribute__ ((__nothrow__ , __leaf__)); extern double __yn (int, double) __attribute__ ((__nothrow__ , __leaf__));





 extern double erf (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __erf (double) __attribute__ ((__nothrow__ , __leaf__));
 extern double erfc (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __erfc (double) __attribute__ ((__nothrow__ , __leaf__));
extern double lgamma (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __lgamma (double) __attribute__ ((__nothrow__ , __leaf__));




extern double tgamma (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __tgamma (double) __attribute__ ((__nothrow__ , __leaf__));





extern double gamma (double) __attribute__ ((__nothrow__ , __leaf__)); extern double __gamma (double) __attribute__ ((__nothrow__ , __leaf__));







extern double lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__)); extern double __lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__));






extern double rint (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __rint (double __x) __attribute__ ((__nothrow__ , __leaf__));


extern double nextafter (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __nextafter (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));

extern double nexttoward (double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __nexttoward (double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));
# 290 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern double remainder (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __remainder (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));



extern double scalbn (double __x, int __n) __attribute__ ((__nothrow__ , __leaf__)); extern double __scalbn (double __x, int __n) __attribute__ ((__nothrow__ , __leaf__));



extern int ilogb (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern int __ilogb (double __x) __attribute__ ((__nothrow__ , __leaf__));
# 308 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern double scalbln (double __x, long int __n) __attribute__ ((__nothrow__ , __leaf__)); extern double __scalbln (double __x, long int __n) __attribute__ ((__nothrow__ , __leaf__));



extern double nearbyint (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern double __nearbyint (double __x) __attribute__ ((__nothrow__ , __leaf__));



extern double round (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __round (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern double trunc (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __trunc (double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern double remquo (double __x, double __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__)); extern double __remquo (double __x, double __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__));






extern long int lrint (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lrint (double __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llrint (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llrint (double __x) __attribute__ ((__nothrow__ , __leaf__));



extern long int lround (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lround (double __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llround (double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llround (double __x) __attribute__ ((__nothrow__ , __leaf__));



extern double fdim (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)); extern double __fdim (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__));



extern double fmax (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __fmax (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern double fmin (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern double __fmin (double __x, double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern double fma (double __x, double __y, double __z) __attribute__ ((__nothrow__ , __leaf__)); extern double __fma (double __x, double __y, double __z) __attribute__ ((__nothrow__ , __leaf__));
# 450 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern double scalb (double __x, double __n) __attribute__ ((__nothrow__ , __leaf__)); extern double __scalb (double __x, double __n) __attribute__ ((__nothrow__ , __leaf__));
# 314 "/usr/include/math.h" 2 3 4
# 329 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 3 4
extern int __fpclassifyf (float __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));


extern int __signbitf (float __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));



extern int __isinff (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __finitef (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __isnanf (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __iseqsigf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));


extern int __issignalingf (float __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
# 330 "/usr/include/math.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern float acosf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __acosf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float asinf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __asinf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float atanf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __atanf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float atan2f (float __y, float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __atan2f (float __y, float __x) __attribute__ ((__nothrow__ , __leaf__));


 extern float cosf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __cosf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float sinf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __sinf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float tanf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __tanf (float __x) __attribute__ ((__nothrow__ , __leaf__));




 extern float coshf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __coshf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float sinhf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __sinhf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float tanhf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __tanhf (float __x) __attribute__ ((__nothrow__ , __leaf__));
# 85 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern float acoshf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __acoshf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float asinhf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __asinhf (float __x) __attribute__ ((__nothrow__ , __leaf__));

 extern float atanhf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __atanhf (float __x) __attribute__ ((__nothrow__ , __leaf__));





 extern float expf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __expf (float __x) __attribute__ ((__nothrow__ , __leaf__));


extern float frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__)); extern float __frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__));


extern float ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__)); extern float __ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__));


 extern float logf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __logf (float __x) __attribute__ ((__nothrow__ , __leaf__));


 extern float log10f (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __log10f (float __x) __attribute__ ((__nothrow__ , __leaf__));


extern float modff (float __x, float *__iptr) __attribute__ ((__nothrow__ , __leaf__)); extern float __modff (float __x, float *__iptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));
# 134 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern float expm1f (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __expm1f (float __x) __attribute__ ((__nothrow__ , __leaf__));


 extern float log1pf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __log1pf (float __x) __attribute__ ((__nothrow__ , __leaf__));


extern float logbf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __logbf (float __x) __attribute__ ((__nothrow__ , __leaf__));




 extern float exp2f (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __exp2f (float __x) __attribute__ ((__nothrow__ , __leaf__));


 extern float log2f (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __log2f (float __x) __attribute__ ((__nothrow__ , __leaf__));






 extern float powf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __powf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));


extern float sqrtf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __sqrtf (float __x) __attribute__ ((__nothrow__ , __leaf__));



 extern float hypotf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __hypotf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));




 extern float cbrtf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __cbrtf (float __x) __attribute__ ((__nothrow__ , __leaf__));






extern float ceilf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __ceilf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern float fabsf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __fabsf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern float floorf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __floorf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern float fmodf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __fmodf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));
# 192 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isinff (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));




extern int finitef (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern float dremf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __dremf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));



extern float significandf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __significandf (float __x) __attribute__ ((__nothrow__ , __leaf__));






extern float copysignf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __copysignf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern float nanf (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__)); extern float __nanf (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__));
# 228 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isnanf (float __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));





extern float j0f (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __j0f (float) __attribute__ ((__nothrow__ , __leaf__));
extern float j1f (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __j1f (float) __attribute__ ((__nothrow__ , __leaf__));
extern float jnf (int, float) __attribute__ ((__nothrow__ , __leaf__)); extern float __jnf (int, float) __attribute__ ((__nothrow__ , __leaf__));
extern float y0f (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __y0f (float) __attribute__ ((__nothrow__ , __leaf__));
extern float y1f (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __y1f (float) __attribute__ ((__nothrow__ , __leaf__));
extern float ynf (int, float) __attribute__ ((__nothrow__ , __leaf__)); extern float __ynf (int, float) __attribute__ ((__nothrow__ , __leaf__));





 extern float erff (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __erff (float) __attribute__ ((__nothrow__ , __leaf__));
 extern float erfcf (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __erfcf (float) __attribute__ ((__nothrow__ , __leaf__));
extern float lgammaf (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __lgammaf (float) __attribute__ ((__nothrow__ , __leaf__));




extern float tgammaf (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __tgammaf (float) __attribute__ ((__nothrow__ , __leaf__));





extern float gammaf (float) __attribute__ ((__nothrow__ , __leaf__)); extern float __gammaf (float) __attribute__ ((__nothrow__ , __leaf__));







extern float lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__)); extern float __lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__));






extern float rintf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __rintf (float __x) __attribute__ ((__nothrow__ , __leaf__));


extern float nextafterf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __nextafterf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));

extern float nexttowardf (float __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __nexttowardf (float __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));
# 290 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern float remainderf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __remainderf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));



extern float scalbnf (float __x, int __n) __attribute__ ((__nothrow__ , __leaf__)); extern float __scalbnf (float __x, int __n) __attribute__ ((__nothrow__ , __leaf__));



extern int ilogbf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern int __ilogbf (float __x) __attribute__ ((__nothrow__ , __leaf__));
# 308 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern float scalblnf (float __x, long int __n) __attribute__ ((__nothrow__ , __leaf__)); extern float __scalblnf (float __x, long int __n) __attribute__ ((__nothrow__ , __leaf__));



extern float nearbyintf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern float __nearbyintf (float __x) __attribute__ ((__nothrow__ , __leaf__));



extern float roundf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __roundf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern float truncf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __truncf (float __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern float remquof (float __x, float __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__)); extern float __remquof (float __x, float __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__));






extern long int lrintf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lrintf (float __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llrintf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llrintf (float __x) __attribute__ ((__nothrow__ , __leaf__));



extern long int lroundf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lroundf (float __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llroundf (float __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llroundf (float __x) __attribute__ ((__nothrow__ , __leaf__));



extern float fdimf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)); extern float __fdimf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__));



extern float fmaxf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __fmaxf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern float fminf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern float __fminf (float __x, float __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern float fmaf (float __x, float __y, float __z) __attribute__ ((__nothrow__ , __leaf__)); extern float __fmaf (float __x, float __y, float __z) __attribute__ ((__nothrow__ , __leaf__));
# 450 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern float scalbf (float __x, float __n) __attribute__ ((__nothrow__ , __leaf__)); extern float __scalbf (float __x, float __n) __attribute__ ((__nothrow__ , __leaf__));
# 331 "/usr/include/math.h" 2 3 4
# 398 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 3 4
extern int __fpclassifyl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));


extern int __signbitl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));



extern int __isinfl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __finitel (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __isnanl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __iseqsigl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));


extern int __issignalingl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
# 399 "/usr/include/math.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern long double acosl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __acosl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double asinl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __asinl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double atanl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __atanl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double atan2l (long double __y, long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __atan2l (long double __y, long double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern long double cosl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __cosl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double sinl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __sinl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double tanl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __tanl (long double __x) __attribute__ ((__nothrow__ , __leaf__));




 extern long double coshl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __coshl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double sinhl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __sinhl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double tanhl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __tanhl (long double __x) __attribute__ ((__nothrow__ , __leaf__));
# 85 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern long double acoshl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __acoshl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double asinhl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __asinhl (long double __x) __attribute__ ((__nothrow__ , __leaf__));

 extern long double atanhl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __atanhl (long double __x) __attribute__ ((__nothrow__ , __leaf__));





 extern long double expl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __expl (long double __x) __attribute__ ((__nothrow__ , __leaf__));


extern long double frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__)); extern long double __frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__ , __leaf__));


extern long double ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__)); extern long double __ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__ , __leaf__));


 extern long double logl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __logl (long double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern long double log10l (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __log10l (long double __x) __attribute__ ((__nothrow__ , __leaf__));


extern long double modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__ , __leaf__)); extern long double __modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));
# 134 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
 extern long double expm1l (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __expm1l (long double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern long double log1pl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __log1pl (long double __x) __attribute__ ((__nothrow__ , __leaf__));


extern long double logbl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __logbl (long double __x) __attribute__ ((__nothrow__ , __leaf__));




 extern long double exp2l (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __exp2l (long double __x) __attribute__ ((__nothrow__ , __leaf__));


 extern long double log2l (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __log2l (long double __x) __attribute__ ((__nothrow__ , __leaf__));






 extern long double powl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __powl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));


extern long double sqrtl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __sqrtl (long double __x) __attribute__ ((__nothrow__ , __leaf__));



 extern long double hypotl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __hypotl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));




 extern long double cbrtl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __cbrtl (long double __x) __attribute__ ((__nothrow__ , __leaf__));






extern long double ceill (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __ceill (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern long double fabsl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __fabsl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern long double floorl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __floorl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern long double fmodl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __fmodl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));
# 192 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isinfl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));




extern int finitel (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern long double dreml (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __dreml (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));



extern long double significandl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __significandl (long double __x) __attribute__ ((__nothrow__ , __leaf__));






extern long double copysignl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __copysignl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern long double nanl (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__)); extern long double __nanl (const char *__tagb) __attribute__ ((__nothrow__ , __leaf__));
# 228 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern int isnanl (long double __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));





extern long double j0l (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __j0l (long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double j1l (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __j1l (long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double jnl (int, long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __jnl (int, long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double y0l (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __y0l (long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double y1l (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __y1l (long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double ynl (int, long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __ynl (int, long double) __attribute__ ((__nothrow__ , __leaf__));





 extern long double erfl (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __erfl (long double) __attribute__ ((__nothrow__ , __leaf__));
 extern long double erfcl (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __erfcl (long double) __attribute__ ((__nothrow__ , __leaf__));
extern long double lgammal (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __lgammal (long double) __attribute__ ((__nothrow__ , __leaf__));




extern long double tgammal (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __tgammal (long double) __attribute__ ((__nothrow__ , __leaf__));





extern long double gammal (long double) __attribute__ ((__nothrow__ , __leaf__)); extern long double __gammal (long double) __attribute__ ((__nothrow__ , __leaf__));







extern long double lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__)); extern long double __lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__ , __leaf__));






extern long double rintl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __rintl (long double __x) __attribute__ ((__nothrow__ , __leaf__));


extern long double nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));

extern long double nexttowardl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __nexttowardl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));
# 290 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern long double remainderl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __remainderl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));



extern long double scalbnl (long double __x, int __n) __attribute__ ((__nothrow__ , __leaf__)); extern long double __scalbnl (long double __x, int __n) __attribute__ ((__nothrow__ , __leaf__));



extern int ilogbl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern int __ilogbl (long double __x) __attribute__ ((__nothrow__ , __leaf__));
# 308 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern long double scalblnl (long double __x, long int __n) __attribute__ ((__nothrow__ , __leaf__)); extern long double __scalblnl (long double __x, long int __n) __attribute__ ((__nothrow__ , __leaf__));



extern long double nearbyintl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long double __nearbyintl (long double __x) __attribute__ ((__nothrow__ , __leaf__));



extern long double roundl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __roundl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern long double truncl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __truncl (long double __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));




extern long double remquol (long double __x, long double __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__)); extern long double __remquol (long double __x, long double __y, int *__quo) __attribute__ ((__nothrow__ , __leaf__));






extern long int lrintl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lrintl (long double __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llrintl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llrintl (long double __x) __attribute__ ((__nothrow__ , __leaf__));



extern long int lroundl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long int __lroundl (long double __x) __attribute__ ((__nothrow__ , __leaf__));
__extension__
extern long long int llroundl (long double __x) __attribute__ ((__nothrow__ , __leaf__)); extern long long int __llroundl (long double __x) __attribute__ ((__nothrow__ , __leaf__));



extern long double fdiml (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)); extern long double __fdiml (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__));



extern long double fmaxl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __fmaxl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));


extern long double fminl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)); extern long double __fminl (long double __x, long double __y) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));



extern long double fmal (long double __x, long double __y, long double __z) __attribute__ ((__nothrow__ , __leaf__)); extern long double __fmal (long double __x, long double __y, long double __z) __attribute__ ((__nothrow__ , __leaf__));
# 450 "/usr/include/x86_64-linux-gnu/bits/mathcalls.h" 3 4
extern long double scalbl (long double __x, long double __n) __attribute__ ((__nothrow__ , __leaf__)); extern long double __scalbl (long double __x, long double __n) __attribute__ ((__nothrow__ , __leaf__));
# 400 "/usr/include/math.h" 2 3 4
# 481 "/usr/include/math.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 1 3 4
# 20 "/usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h" 3 4
extern int __fpclassifyf128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));


extern int __signbitf128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));



extern int __isinff128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __finitef128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __isnanf128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));


extern int __iseqsigf128 (_Float128 __x, _Float128 __y) __attribute__ ((__nothrow__ , __leaf__));


extern int __issignalingf128 (_Float128 __value) __attribute__ ((__nothrow__ , __leaf__))
     __attribute__ ((__const__));
# 482 "/usr/include/math.h" 2 3 4
# 854 "/usr/include/math.h" 3 4
extern int signgam;
# 934 "/usr/include/math.h" 3 4
enum
  {
    FP_NAN =

      0,
    FP_INFINITE =

      1,
    FP_ZERO =

      2,
    FP_SUBNORMAL =

      3,
    FP_NORMAL =

      4
  };
# 1472 "/usr/include/math.h" 3 4

# 43 "/usr/include/csmith/csmith.h" 2


# 1 "/usr/include/csmith/random_inc.h" 1
# 51 "/usr/include/csmith/random_inc.h"
# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 1 3 4
# 34 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/syslimits.h" 1 3 4






# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 1 3 4
# 210 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 3 4
# 1 "/usr/include/limits.h" 1 3 4
# 26 "/usr/include/limits.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/limits.h" 2 3 4
# 195 "/usr/include/limits.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 2 3 4
# 161 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 1 3 4
# 38 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 3 4
# 1 "/usr/include/linux/limits.h" 1 3 4
# 39 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 2 3 4
# 81 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/pthread_stack_min-dynamic.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/pthread_stack_min-dynamic.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/pthread_stack_min.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/bits/pthread_stack_min-dynamic.h" 2 3 4
# 82 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 2 3 4
# 162 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 2 3 4
# 196 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/posix2_lim.h" 1 3 4
# 200 "/usr/include/limits.h" 2 3 4
# 211 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 2 3 4
# 8 "/usr/lib/gcc/x86_64-linux-gnu/14/include/syslimits.h" 2 3 4
# 35 "/usr/lib/gcc/x86_64-linux-gnu/14/include/limits.h" 2 3 4
# 52 "/usr/include/csmith/random_inc.h" 2



# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stdint.h" 1 3 4
# 9 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stdint.h" 3 4
# 1 "/usr/include/stdint.h" 1 3 4
# 26 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 1 3 4
# 27 "/usr/include/stdint.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/wchar.h" 1 3 4
# 29 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 30 "/usr/include/stdint.h" 2 3 4




# 1 "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h" 3 4
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
# 35 "/usr/include/stdint.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h" 3 4
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
# 38 "/usr/include/stdint.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/stdint-least.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/stdint-least.h" 3 4
typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;


typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;
# 42 "/usr/include/stdint.h" 2 3 4





typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 60 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 76 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 90 "/usr/include/stdint.h" 3 4
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
# 10 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stdint.h" 2 3 4
# 56 "/usr/include/csmith/random_inc.h" 2



# 1 "/usr/include/assert.h" 1 3 4
# 66 "/usr/include/assert.h" 3 4



extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



# 60 "/usr/include/csmith/random_inc.h" 2
# 89 "/usr/include/csmith/random_inc.h"
# 1 "/usr/include/csmith/platform_generic.h" 1
# 39 "/usr/include/csmith/platform_generic.h"
# 1 "/usr/include/stdio.h" 1 3 4
# 28 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h" 1 3 4
# 29 "/usr/include/stdio.h" 2 3 4





# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stddef.h" 1 3 4
# 35 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-linux-gnu/14/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 38 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h" 1 3 4




# 1 "/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h" 1 3 4
# 13 "/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h" 3 4
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 6 "/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h" 2 3 4




typedef struct _G_fpos_t
{
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
# 41 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h" 1 3 4
# 10 "/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h" 3 4
typedef struct _G_fpos64_t
{
  __off64_t __pos;
  __mbstate_t __state;
} __fpos64_t;
# 42 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/types/__FILE.h" 1 3 4



struct _IO_FILE;
typedef struct _IO_FILE __FILE;
# 43 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/types/FILE.h" 1 3 4



struct _IO_FILE;


typedef struct _IO_FILE FILE;
# 44 "/usr/include/stdio.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h" 1 3 4
# 35 "/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h" 3 4
struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;




typedef void _IO_lock_t;





struct _IO_FILE
{
  int _flags;


  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;


  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _flags2;
  __off_t _old_offset;


  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];

  _IO_lock_t *_lock;







  __off64_t _offset;

  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  struct _IO_FILE **_prevchain;
  int _mode;

  char _unused2[15 * sizeof (int) - 5 * sizeof (void *)];
};
# 45 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h" 3 4
typedef __ssize_t cookie_read_function_t (void *__cookie, char *__buf,
                                          size_t __nbytes);







typedef __ssize_t cookie_write_function_t (void *__cookie, const char *__buf,
                                           size_t __nbytes);







typedef int cookie_seek_function_t (void *__cookie, __off64_t *__pos, int __w);


typedef int cookie_close_function_t (void *__cookie);






typedef struct _IO_cookie_io_functions_t
{
  cookie_read_function_t *read;
  cookie_write_function_t *write;
  cookie_seek_function_t *seek;
  cookie_close_function_t *close;
} cookie_io_functions_t;
# 48 "/usr/include/stdio.h" 2 3 4





typedef __gnuc_va_list va_list;
# 64 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;
# 78 "/usr/include/stdio.h" 3 4
typedef __ssize_t ssize_t;






typedef __fpos_t fpos_t;
# 129 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio_lim.h" 1 3 4
# 130 "/usr/include/stdio.h" 2 3 4
# 149 "/usr/include/stdio.h" 3 4
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;






extern int remove (const char *__filename) __attribute__ ((__nothrow__ , __leaf__));

extern int rename (const char *__old, const char *__new) __attribute__ ((__nothrow__ , __leaf__));



extern int renameat (int __oldfd, const char *__old, int __newfd,
       const char *__new) __attribute__ ((__nothrow__ , __leaf__));
# 184 "/usr/include/stdio.h" 3 4
extern int fclose (FILE *__stream) __attribute__ ((__nonnull__ (1)));
# 194 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile (void)
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;
# 211 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char[20]) __attribute__ ((__nothrow__ , __leaf__)) ;




extern char *tmpnam_r (char __s[20]) __attribute__ ((__nothrow__ , __leaf__)) ;
# 228 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
   __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (__builtin_free, 1)));






extern int fflush (FILE *__stream);
# 245 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 264 "/usr/include/stdio.h" 3 4
extern FILE *fopen (const char *__restrict __filename,
      const char *__restrict __modes)
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;




extern FILE *freopen (const char *__restrict __filename,
        const char *__restrict __modes,
        FILE *__restrict __stream) __attribute__ ((__nonnull__ (3)));
# 299 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, const char *__modes) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;





extern FILE *fopencookie (void *__restrict __magic_cookie,
     const char *__restrict __modes,
     cookie_io_functions_t __io_funcs) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;




extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (fclose, 1))) ;
# 334 "/usr/include/stdio.h" 3 4
extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__nonnull__ (1)));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







extern int fprintf (FILE *__restrict __stream,
      const char *__restrict __format, ...) __attribute__ ((__nonnull__ (1)));




extern int printf (const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nonnull__ (1)));




extern int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));



extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));





extern int vasprintf (char **__restrict __ptr, const char *__restrict __f,
        __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0))) ;
extern int __asprintf (char **__restrict __ptr,
         const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) ;
extern int asprintf (char **__restrict __ptr,
       const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) ;




extern int vdprintf (int __fd, const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));







extern int fscanf (FILE *__restrict __stream,
     const char *__restrict __format, ...) __attribute__ ((__nonnull__ (1)));




extern int scanf (const char *__restrict __format, ...) ;

extern int sscanf (const char *__restrict __s,
     const char *__restrict __format, ...) __attribute__ ((__nothrow__ , __leaf__));
# 463 "/usr/include/stdio.h" 3 4
extern int fscanf (FILE *__restrict __stream, const char *__restrict __format, ...) __asm__ ("" "__isoc99_fscanf")

                                __attribute__ ((__nonnull__ (1)));
extern int scanf (const char *__restrict __format, ...) __asm__ ("" "__isoc99_scanf")
                              ;
extern int sscanf (const char *__restrict __s, const char *__restrict __format, ...) __asm__ ("" "__isoc99_sscanf") __attribute__ ((__nothrow__ , __leaf__))

                      ;
# 490 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) __attribute__ ((__nonnull__ (1)));





extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (const char *__restrict __s,
      const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
# 540 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vfscanf")



     __attribute__ ((__format__ (__scanf__, 2, 0))) __attribute__ ((__nonnull__ (1)));
extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vscanf")

     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (const char *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vsscanf") __attribute__ ((__nothrow__ , __leaf__))



     __attribute__ ((__format__ (__scanf__, 2, 0)));
# 575 "/usr/include/stdio.h" 3 4
extern int fgetc (FILE *__stream) __attribute__ ((__nonnull__ (1)));
extern int getc (FILE *__stream) __attribute__ ((__nonnull__ (1)));





extern int getchar (void);






extern int getc_unlocked (FILE *__stream) __attribute__ ((__nonnull__ (1)));
extern int getchar_unlocked (void);
# 600 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream) __attribute__ ((__nonnull__ (1)));
# 611 "/usr/include/stdio.h" 3 4
extern int fputc (int __c, FILE *__stream) __attribute__ ((__nonnull__ (2)));
extern int putc (int __c, FILE *__stream) __attribute__ ((__nonnull__ (2)));





extern int putchar (int __c);
# 627 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream) __attribute__ ((__nonnull__ (2)));







extern int putc_unlocked (int __c, FILE *__stream) __attribute__ ((__nonnull__ (2)));
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream) __attribute__ ((__nonnull__ (1)));


extern int putw (int __w, FILE *__stream) __attribute__ ((__nonnull__ (2)));







extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     __attribute__ ((__access__ (__write_only__, 1, 2))) __attribute__ ((__nonnull__ (3)));
# 689 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
                             size_t *__restrict __n, int __delimiter,
                             FILE *__restrict __stream) __attribute__ ((__nonnull__ (4)));
extern __ssize_t getdelim (char **__restrict __lineptr,
                           size_t *__restrict __n, int __delimiter,
                           FILE *__restrict __stream) __attribute__ ((__nonnull__ (4)));


extern __ssize_t getline (char **__restrict __lineptr,
                          size_t *__restrict __n,
                          FILE *__restrict __stream) __attribute__ ((__nonnull__ (3)));







extern int fputs (const char *__restrict __s, FILE *__restrict __stream)
  __attribute__ ((__nonnull__ (2)));





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream) __attribute__ ((__nonnull__ (2)));






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream)
  __attribute__ ((__nonnull__ (4)));




extern size_t fwrite (const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s) __attribute__ ((__nonnull__ (4)));
# 756 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream)
  __attribute__ ((__nonnull__ (4)));
extern size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream)
  __attribute__ ((__nonnull__ (4)));







extern int fseek (FILE *__stream, long int __off, int __whence)
  __attribute__ ((__nonnull__ (1)));




extern long int ftell (FILE *__stream) __attribute__ ((__nonnull__ (1)));




extern void rewind (FILE *__stream) __attribute__ ((__nonnull__ (1)));
# 793 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence)
  __attribute__ ((__nonnull__ (1)));




extern __off_t ftello (FILE *__stream) __attribute__ ((__nonnull__ (1)));
# 819 "/usr/include/stdio.h" 3 4
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos)
  __attribute__ ((__nonnull__ (1)));




extern int fsetpos (FILE *__stream, const fpos_t *__pos) __attribute__ ((__nonnull__ (1)));
# 850 "/usr/include/stdio.h" 3 4
extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







extern void perror (const char *__s) __attribute__ ((__cold__));




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 887 "/usr/include/stdio.h" 3 4
extern int pclose (FILE *__stream) __attribute__ ((__nonnull__ (1)));





extern FILE *popen (const char *__command, const char *__modes)
  __attribute__ ((__malloc__)) __attribute__ ((__malloc__ (pclose, 1))) ;






extern char *ctermid (char *__s) __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__access__ (__write_only__, 1)));
# 931 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 949 "/usr/include/stdio.h" 3 4
extern int __uflow (FILE *);
extern int __overflow (FILE *, int);
# 973 "/usr/include/stdio.h" 3 4

# 40 "/usr/include/csmith/platform_generic.h" 2



# 42 "/usr/include/csmith/platform_generic.h"
static void
platform_main_begin(void)
{

}

static void
__attribute__ ((noinline))
platform_main_end(uint32_t crc, int flag)
{





 printf ("checksum = %X\n", crc);
# 120 "/usr/include/csmith/platform_generic.h"
}
# 90 "/usr/include/csmith/random_inc.h" 2
# 100 "/usr/include/csmith/random_inc.h"
# 1 "/usr/include/csmith/safe_math.h" 1
# 13 "/usr/include/csmith/safe_math.h"
static int8_t
(safe_unary_minus_func_int8_t_s)(int8_t si )
{
 
  return






    -si;
}

static int8_t
(safe_add_func_int8_t_s_s)(int8_t si1, int8_t si2 )
{
 
  return






    (si1 + si2);
}

static int8_t
(safe_sub_func_int8_t_s_s)(int8_t si1, int8_t si2 )
{
 
  return






    (si1 - si2);
}

static int8_t
(safe_mul_func_int8_t_s_s)(int8_t si1, int8_t si2 )
{
 
  return






    si1 * si2;
}

static int8_t
(safe_mod_func_int8_t_s_s)(int8_t si1, int8_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 75 "/usr/include/csmith/safe_math.h" 3 4
                           (-128)
# 75 "/usr/include/csmith/safe_math.h"
                                   ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 % si2);
}

static int8_t
(safe_div_func_int8_t_s_s)(int8_t si1, int8_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 87 "/usr/include/csmith/safe_math.h" 3 4
                           (-128)
# 87 "/usr/include/csmith/safe_math.h"
                                   ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 / si2);
}

static int8_t
(safe_lshift_func_int8_t_s_s)(int8_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32) || (left > (
# 99 "/usr/include/csmith/safe_math.h" 3 4
                                                                         (127) 
# 99 "/usr/include/csmith/safe_math.h"
                                                                                  >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static int8_t
(safe_lshift_func_int8_t_s_u)(int8_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32) || (left > (
# 111 "/usr/include/csmith/safe_math.h" 3 4
                                                            (127) 
# 111 "/usr/include/csmith/safe_math.h"
                                                                     >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static int8_t
(safe_rshift_func_int8_t_s_s)(int8_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32))?
    ((left)) :

    (left >> ((int)right));
}

static int8_t
(safe_rshift_func_int8_t_s_u)(int8_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32)) ?
    ((left)) :

    (left >> ((unsigned int)right));
}



static int16_t
(safe_unary_minus_func_int16_t_s)(int16_t si )
{
 
  return






    -si;
}

static int16_t
(safe_add_func_int16_t_s_s)(int16_t si1, int16_t si2 )
{
 
  return






    (si1 + si2);
}

static int16_t
(safe_sub_func_int16_t_s_s)(int16_t si1, int16_t si2 )
{
 
  return






    (si1 - si2);
}

static int16_t
(safe_mul_func_int16_t_s_s)(int16_t si1, int16_t si2 )
{
 
  return






    si1 * si2;
}

static int16_t
(safe_mod_func_int16_t_s_s)(int16_t si1, int16_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 205 "/usr/include/csmith/safe_math.h" 3 4
                           (-32767-1)
# 205 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 % si2);
}

static int16_t
(safe_div_func_int16_t_s_s)(int16_t si1, int16_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 217 "/usr/include/csmith/safe_math.h" 3 4
                           (-32767-1)
# 217 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 / si2);
}

static int16_t
(safe_lshift_func_int16_t_s_s)(int16_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32) || (left > (
# 229 "/usr/include/csmith/safe_math.h" 3 4
                                                                         (32767) 
# 229 "/usr/include/csmith/safe_math.h"
                                                                                   >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static int16_t
(safe_lshift_func_int16_t_s_u)(int16_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32) || (left > (
# 241 "/usr/include/csmith/safe_math.h" 3 4
                                                            (32767) 
# 241 "/usr/include/csmith/safe_math.h"
                                                                      >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static int16_t
(safe_rshift_func_int16_t_s_s)(int16_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32))?
    ((left)) :

    (left >> ((int)right));
}

static int16_t
(safe_rshift_func_int16_t_s_u)(int16_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32)) ?
    ((left)) :

    (left >> ((unsigned int)right));
}



static int32_t
(safe_unary_minus_func_int32_t_s)(int32_t si )
{
 
  return


    (si==
# 280 "/usr/include/csmith/safe_math.h" 3 4
        (-2147483647-1)
# 280 "/usr/include/csmith/safe_math.h"
                 ) ?
    ((si)) :


    -si;
}

static int32_t
(safe_add_func_int32_t_s_s)(int32_t si1, int32_t si2 )
{
 
  return


    (((si1>0) && (si2>0) && (si1 > (
# 294 "/usr/include/csmith/safe_math.h" 3 4
                                   (2147483647)
# 294 "/usr/include/csmith/safe_math.h"
                                            -si2))) || ((si1<0) && (si2<0) && (si1 < (
# 294 "/usr/include/csmith/safe_math.h" 3 4
                                                                                      (-2147483647-1)
# 294 "/usr/include/csmith/safe_math.h"
                                                                                               -si2)))) ?
    ((si1)) :


    (si1 + si2);
}

static int32_t
(safe_sub_func_int32_t_s_s)(int32_t si1, int32_t si2 )
{
 
  return


    (((si1^si2) & (((si1 ^ ((si1^si2) & (~
# 308 "/usr/include/csmith/safe_math.h" 3 4
                                         (2147483647)
# 308 "/usr/include/csmith/safe_math.h"
                                                  )))-si2)^si2)) < 0) ?
    ((si1)) :


    (si1 - si2);
}

static int32_t
(safe_mul_func_int32_t_s_s)(int32_t si1, int32_t si2 )
{
 
  return


    (((si1 > 0) && (si2 > 0) && (si1 > (
# 322 "/usr/include/csmith/safe_math.h" 3 4
                                       (2147483647) 
# 322 "/usr/include/csmith/safe_math.h"
                                                 / si2))) || ((si1 > 0) && (si2 <= 0) && (si2 < (
# 322 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                 (-2147483647-1) 
# 322 "/usr/include/csmith/safe_math.h"
                                                                                                           / si1))) || ((si1 <= 0) && (si2 > 0) && (si1 < (
# 322 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                                                                           (-2147483647-1) 
# 322 "/usr/include/csmith/safe_math.h"
                                                                                                                                                                     / si2))) || ((si1 <= 0) && (si2 <= 0) && (si1 != 0) && (si2 < (
# 322 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                                                                                                                                                    (2147483647) 
# 322 "/usr/include/csmith/safe_math.h"
                                                                                                                                                                                                                                              / si1)))) ?
    ((si1)) :


    si1 * si2;
}

static int32_t
(safe_mod_func_int32_t_s_s)(int32_t si1, int32_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 335 "/usr/include/csmith/safe_math.h" 3 4
                           (-2147483647-1)
# 335 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 % si2);
}

static int32_t
(safe_div_func_int32_t_s_s)(int32_t si1, int32_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 347 "/usr/include/csmith/safe_math.h" 3 4
                           (-2147483647-1)
# 347 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 / si2);
}

static int32_t
(safe_lshift_func_int32_t_s_s)(int32_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32) || (left > (
# 359 "/usr/include/csmith/safe_math.h" 3 4
                                                                         (2147483647) 
# 359 "/usr/include/csmith/safe_math.h"
                                                                                   >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static int32_t
(safe_lshift_func_int32_t_s_u)(int32_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32) || (left > (
# 371 "/usr/include/csmith/safe_math.h" 3 4
                                                            (2147483647) 
# 371 "/usr/include/csmith/safe_math.h"
                                                                      >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static int32_t
(safe_rshift_func_int32_t_s_s)(int32_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32))?
    ((left)) :

    (left >> ((int)right));
}

static int32_t
(safe_rshift_func_int32_t_s_u)(int32_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32)) ?
    ((left)) :

    (left >> ((unsigned int)right));
}




static int64_t
(safe_unary_minus_func_int64_t_s)(int64_t si )
{
 
  return


    (si==
# 411 "/usr/include/csmith/safe_math.h" 3 4
        (-9223372036854775807L -1)
# 411 "/usr/include/csmith/safe_math.h"
                 ) ?
    ((si)) :


    -si;
}

static int64_t
(safe_add_func_int64_t_s_s)(int64_t si1, int64_t si2 )
{
 
  return


    (((si1>0) && (si2>0) && (si1 > (
# 425 "/usr/include/csmith/safe_math.h" 3 4
                                   (9223372036854775807L)
# 425 "/usr/include/csmith/safe_math.h"
                                            -si2))) || ((si1<0) && (si2<0) && (si1 < (
# 425 "/usr/include/csmith/safe_math.h" 3 4
                                                                                      (-9223372036854775807L -1)
# 425 "/usr/include/csmith/safe_math.h"
                                                                                               -si2)))) ?
    ((si1)) :


    (si1 + si2);
}

static int64_t
(safe_sub_func_int64_t_s_s)(int64_t si1, int64_t si2 )
{
 
  return


    (((si1^si2) & (((si1 ^ ((si1^si2) & (~
# 439 "/usr/include/csmith/safe_math.h" 3 4
                                         (9223372036854775807L)
# 439 "/usr/include/csmith/safe_math.h"
                                                  )))-si2)^si2)) < 0) ?
    ((si1)) :


    (si1 - si2);
}

static int64_t
(safe_mul_func_int64_t_s_s)(int64_t si1, int64_t si2 )
{
 
  return


    (((si1 > 0) && (si2 > 0) && (si1 > (
# 453 "/usr/include/csmith/safe_math.h" 3 4
                                       (9223372036854775807L) 
# 453 "/usr/include/csmith/safe_math.h"
                                                 / si2))) || ((si1 > 0) && (si2 <= 0) && (si2 < (
# 453 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                 (-9223372036854775807L -1) 
# 453 "/usr/include/csmith/safe_math.h"
                                                                                                           / si1))) || ((si1 <= 0) && (si2 > 0) && (si1 < (
# 453 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                                                                           (-9223372036854775807L -1) 
# 453 "/usr/include/csmith/safe_math.h"
                                                                                                                                                                     / si2))) || ((si1 <= 0) && (si2 <= 0) && (si1 != 0) && (si2 < (
# 453 "/usr/include/csmith/safe_math.h" 3 4
                                                                                                                                                                                                                                    (9223372036854775807L) 
# 453 "/usr/include/csmith/safe_math.h"
                                                                                                                                                                                                                                              / si1)))) ?
    ((si1)) :


    si1 * si2;
}

static int64_t
(safe_mod_func_int64_t_s_s)(int64_t si1, int64_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 466 "/usr/include/csmith/safe_math.h" 3 4
                           (-9223372036854775807L -1)
# 466 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 % si2);
}

static int64_t
(safe_div_func_int64_t_s_s)(int64_t si1, int64_t si2 )
{
 
  return

    ((si2 == 0) || ((si1 == 
# 478 "/usr/include/csmith/safe_math.h" 3 4
                           (-9223372036854775807L -1)
# 478 "/usr/include/csmith/safe_math.h"
                                    ) && (si2 == (-1)))) ?
    ((si1)) :

    (si1 / si2);
}

static int64_t
(safe_lshift_func_int64_t_s_s)(int64_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32) || (left > (
# 490 "/usr/include/csmith/safe_math.h" 3 4
                                                                         (9223372036854775807L) 
# 490 "/usr/include/csmith/safe_math.h"
                                                                                   >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static int64_t
(safe_lshift_func_int64_t_s_u)(int64_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32) || (left > (
# 502 "/usr/include/csmith/safe_math.h" 3 4
                                                            (9223372036854775807L) 
# 502 "/usr/include/csmith/safe_math.h"
                                                                      >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static int64_t
(safe_rshift_func_int64_t_s_s)(int64_t left, int right )
{
 
  return

    ((left < 0) || (((int)right) < 0) || (((int)right) >= 32))?
    ((left)) :

    (left >> ((int)right));
}

static int64_t
(safe_rshift_func_int64_t_s_u)(int64_t left, unsigned int right )
{
 
  return

    ((left < 0) || (((unsigned int)right) >= 32)) ?
    ((left)) :

    (left >> ((unsigned int)right));
}







static uint8_t
(safe_unary_minus_func_uint8_t_u)(uint8_t ui )
{
 
  return -ui;
}

static uint8_t
(safe_add_func_uint8_t_u_u)(uint8_t ui1, uint8_t ui2 )
{
 
  return ui1 + ui2;
}

static uint8_t
(safe_sub_func_uint8_t_u_u)(uint8_t ui1, uint8_t ui2 )
{
 
  return ui1 - ui2;
}

static uint8_t
(safe_mul_func_uint8_t_u_u)(uint8_t ui1, uint8_t ui2 )
{
 
  return ((unsigned int)ui1) * ((unsigned int)ui2);
}

static uint8_t
(safe_mod_func_uint8_t_u_u)(uint8_t ui1, uint8_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 % ui2);
}

static uint8_t
(safe_div_func_uint8_t_u_u)(uint8_t ui1, uint8_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 / ui2);
}

static uint8_t
(safe_lshift_func_uint8_t_u_s)(uint8_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32) || (left > (
# 596 "/usr/include/csmith/safe_math.h" 3 4
                                                           (255) 
# 596 "/usr/include/csmith/safe_math.h"
                                                                     >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static uint8_t
(safe_lshift_func_uint8_t_u_u)(uint8_t left, unsigned int right )
{
 
  return

    ((((unsigned int)right) >= 32) || (left > (
# 608 "/usr/include/csmith/safe_math.h" 3 4
                                              (255) 
# 608 "/usr/include/csmith/safe_math.h"
                                                        >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static uint8_t
(safe_rshift_func_uint8_t_u_s)(uint8_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32)) ?
    ((left)) :

    (left >> ((int)right));
}

static uint8_t
(safe_rshift_func_uint8_t_u_u)(uint8_t left, unsigned int right )
{
 
  return

    (((unsigned int)right) >= 32) ?
    ((left)) :

    (left >> ((unsigned int)right));
}



static uint16_t
(safe_unary_minus_func_uint16_t_u)(uint16_t ui )
{
 
  return -ui;
}

static uint16_t
(safe_add_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 )
{
 
  return ui1 + ui2;
}

static uint16_t
(safe_sub_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 )
{
 
  return ui1 - ui2;
}

static uint16_t
(safe_mul_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 )
{
 
  return ((unsigned int)ui1) * ((unsigned int)ui2);
}

static uint16_t
(safe_mod_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 % ui2);
}

static uint16_t
(safe_div_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 / ui2);
}

static uint16_t
(safe_lshift_func_uint16_t_u_s)(uint16_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32) || (left > (
# 698 "/usr/include/csmith/safe_math.h" 3 4
                                                           (65535) 
# 698 "/usr/include/csmith/safe_math.h"
                                                                      >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static uint16_t
(safe_lshift_func_uint16_t_u_u)(uint16_t left, unsigned int right )
{
 
  return

    ((((unsigned int)right) >= 32) || (left > (
# 710 "/usr/include/csmith/safe_math.h" 3 4
                                              (65535) 
# 710 "/usr/include/csmith/safe_math.h"
                                                         >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static uint16_t
(safe_rshift_func_uint16_t_u_s)(uint16_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32)) ?
    ((left)) :

    (left >> ((int)right));
}

static uint16_t
(safe_rshift_func_uint16_t_u_u)(uint16_t left, unsigned int right )
{
 
  return

    (((unsigned int)right) >= 32) ?
    ((left)) :

    (left >> ((unsigned int)right));
}



static uint32_t
(safe_unary_minus_func_uint32_t_u)(uint32_t ui )
{
 
  return -ui;
}

static uint32_t
(safe_add_func_uint32_t_u_u)(uint32_t ui1, uint32_t ui2 )
{
 
  return ui1 + ui2;
}

static uint32_t
(safe_sub_func_uint32_t_u_u)(uint32_t ui1, uint32_t ui2 )
{
 
  return ui1 - ui2;
}

static uint32_t
(safe_mul_func_uint32_t_u_u)(uint32_t ui1, uint32_t ui2 )
{
 
  return ((unsigned int)ui1) * ((unsigned int)ui2);
}

static uint32_t
(safe_mod_func_uint32_t_u_u)(uint32_t ui1, uint32_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 % ui2);
}

static uint32_t
(safe_div_func_uint32_t_u_u)(uint32_t ui1, uint32_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 / ui2);
}

static uint32_t
(safe_lshift_func_uint32_t_u_s)(uint32_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32) || (left > (
# 800 "/usr/include/csmith/safe_math.h" 3 4
                                                           (4294967295U) 
# 800 "/usr/include/csmith/safe_math.h"
                                                                      >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static uint32_t
(safe_lshift_func_uint32_t_u_u)(uint32_t left, unsigned int right )
{
 
  return

    ((((unsigned int)right) >= 32) || (left > (
# 812 "/usr/include/csmith/safe_math.h" 3 4
                                              (4294967295U) 
# 812 "/usr/include/csmith/safe_math.h"
                                                         >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static uint32_t
(safe_rshift_func_uint32_t_u_s)(uint32_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32)) ?
    ((left)) :

    (left >> ((int)right));
}

static uint32_t
(safe_rshift_func_uint32_t_u_u)(uint32_t left, unsigned int right )
{
 
  return

    (((unsigned int)right) >= 32) ?
    ((left)) :

    (left >> ((unsigned int)right));
}




static uint64_t
(safe_unary_minus_func_uint64_t_u)(uint64_t ui )
{
 
  return -ui;
}

static uint64_t
(safe_add_func_uint64_t_u_u)(uint64_t ui1, uint64_t ui2 )
{
 
  return ui1 + ui2;
}

static uint64_t
(safe_sub_func_uint64_t_u_u)(uint64_t ui1, uint64_t ui2 )
{
 
  return ui1 - ui2;
}

static uint64_t
(safe_mul_func_uint64_t_u_u)(uint64_t ui1, uint64_t ui2 )
{
 
  return ((unsigned long long)ui1) * ((unsigned long long)ui2);
}

static uint64_t
(safe_mod_func_uint64_t_u_u)(uint64_t ui1, uint64_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 % ui2);
}

static uint64_t
(safe_div_func_uint64_t_u_u)(uint64_t ui1, uint64_t ui2 )
{
 
  return

    (ui2 == 0) ?
    ((ui1)) :

    (ui1 / ui2);
}

static uint64_t
(safe_lshift_func_uint64_t_u_s)(uint64_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32) || (left > (
# 903 "/usr/include/csmith/safe_math.h" 3 4
                                                           (18446744073709551615UL) 
# 903 "/usr/include/csmith/safe_math.h"
                                                                      >> ((int)right)))) ?
    ((left)) :

    (left << ((int)right));
}

static uint64_t
(safe_lshift_func_uint64_t_u_u)(uint64_t left, unsigned int right )
{
 
  return

    ((((unsigned int)right) >= 32) || (left > (
# 915 "/usr/include/csmith/safe_math.h" 3 4
                                              (18446744073709551615UL) 
# 915 "/usr/include/csmith/safe_math.h"
                                                         >> ((unsigned int)right)))) ?
    ((left)) :

    (left << ((unsigned int)right));
}

static uint64_t
(safe_rshift_func_uint64_t_u_s)(uint64_t left, int right )
{
 
  return

    ((((int)right) < 0) || (((int)right) >= 32)) ?
    ((left)) :

    (left >> ((int)right));
}

static uint64_t
(safe_rshift_func_uint64_t_u_u)(uint64_t left, unsigned int right )
{
 
  return

    (((unsigned int)right) >= 32) ?
    ((left)) :

    (left >> ((unsigned int)right));
}
# 953 "/usr/include/csmith/safe_math.h"
static float
(safe_add_func_float_f_f)(float sf1, float sf2 )
{
 
  return

    (fabsf((0.5f * sf1) + (0.5f * sf2)) > (0.5f * 3.40282346638528859811704183484516925e+38F
# 959 "/usr/include/csmith/safe_math.h"
                                                        )) ?
    (sf1) :

    (sf1 + sf2);
}

static float
(safe_sub_func_float_f_f)(float sf1, float sf2 )
{
 
  return

    (fabsf((0.5f * sf1) - (0.5f * sf2)) > (0.5f * 3.40282346638528859811704183484516925e+38F
# 971 "/usr/include/csmith/safe_math.h"
                                                        )) ?
    (sf1) :

    (sf1 - sf2);
}

static float
(safe_mul_func_float_f_f)(float sf1, float sf2 )
{
 
  return


    (fabsf((0x1.0p-100f * sf1) * (0x1.0p-28f * sf2)) > (0x1.0p-100f * (0x1.0p-28f * 3.40282346638528859811704183484516925e+38F
# 984 "/usr/include/csmith/safe_math.h"
                                                                                          ))) ?



    (sf1) :

    (sf1 * sf2);
}

static float
(safe_div_func_float_f_f)(float sf1, float sf2 )
{
 
  return


    ((fabsf(sf2) < 1.0f) && (((sf2 == 0.0f) || (fabsf((0x1.0p-49f * sf1) / (0x1.0p100f * sf2))) > (0x1.0p-100f * (0x1.0p-49f * 3.40282346638528859811704183484516925e+38F
# 1000 "/usr/include/csmith/safe_math.h"
                                                                                                                                     ))))) ?



    (sf1) :

    (sf1 / sf2);
}




static double
(safe_add_func_double_f_f)(double sf1, double sf2 )
{
 
  return

    (fabs((0.5 * sf1) + (0.5 * sf2)) > (0.5 * ((double)1.79769313486231570814527423731704357e+308L)
# 1018 "/usr/include/csmith/safe_math.h"
                                                    )) ?
    (sf1) :

    (sf1 + sf2);
}

static double
(safe_sub_func_double_f_f)(double sf1, double sf2 )
{
 
  return

    (fabs((0.5 * sf1) - (0.5 * sf2)) > (0.5 * ((double)1.79769313486231570814527423731704357e+308L)
# 1030 "/usr/include/csmith/safe_math.h"
                                                    )) ?
    (sf1) :

    (sf1 - sf2);
}

static double
(safe_mul_func_double_f_f)(double sf1, double sf2 )
{
 
  return


    (fabs((0x1.0p-100 * sf1) * (0x1.0p-924 * sf2)) > (0x1.0p-100 * (0x1.0p-924 * ((double)1.79769313486231570814527423731704357e+308L)
# 1043 "/usr/include/csmith/safe_math.h"
                                                                                       ))) ?



    (sf1) :

    (sf1 * sf2);
}

static double
(safe_div_func_double_f_f)(double sf1, double sf2 )
{
 
  return


    ((fabs(sf2) < 1.0) && (((sf2 == 0.0) || (fabs((0x1.0p-974 * sf1) / (0x1.0p100 * sf2))) > (0x1.0p-100 * (0x1.0p-974 * ((double)1.79769313486231570814527423731704357e+308L)
# 1059 "/usr/include/csmith/safe_math.h"
                                                                                                                               ))))) ?



    (sf1) :

    (sf1 / sf2);
}
# 1193 "/usr/include/csmith/safe_math.h"
static int32_t
(safe_convert_func_float_to_int32_t)(float sf1 )
{
 
  return

    ((sf1 <= 
# 1199 "/usr/include/csmith/safe_math.h" 3 4
            (-2147483647-1)
# 1199 "/usr/include/csmith/safe_math.h"
                     ) || (sf1 >= 
# 1199 "/usr/include/csmith/safe_math.h" 3 4
                                  (2147483647)
# 1199 "/usr/include/csmith/safe_math.h"
                                           )) ?
    (
# 1200 "/usr/include/csmith/safe_math.h" 3 4
   (2147483647)
# 1200 "/usr/include/csmith/safe_math.h"
   ) :

    ((int32_t)(sf1));
}
# 101 "/usr/include/csmith/random_inc.h" 2
# 46 "/usr/include/csmith/csmith.h" 2

static uint32_t crc32_tab[256];
static uint32_t crc32_context = 0xFFFFFFFFUL;

static void
crc32_gentab (void)
{
 uint32_t crc;
 const uint32_t poly = 0xEDB88320UL;
 int i, j;

 for (i = 0; i < 256; i++) {
  crc = i;
  for (j = 8; j > 0; j--) {
   if (crc & 1) {
    crc = (crc >> 1) ^ poly;
   } else {
    crc >>= 1;
   }
  }
  crc32_tab[i] = crc;
 }
}

static void
crc32_byte (uint8_t b) {
 crc32_context =
  ((crc32_context >> 8) & 0x00FFFFFF) ^
  crc32_tab[(crc32_context ^ b) & 0xFF];
}
# 96 "/usr/include/csmith/csmith.h"
static void
crc32_8bytes (uint64_t val)
{
 crc32_byte ((val>>0) & 0xff);
 crc32_byte ((val>>8) & 0xff);
 crc32_byte ((val>>16) & 0xff);
 crc32_byte ((val>>24) & 0xff);
 crc32_byte ((val>>32) & 0xff);
 crc32_byte ((val>>40) & 0xff);
 crc32_byte ((val>>48) & 0xff);
 crc32_byte ((val>>56) & 0xff);
}

static void
transparent_crc (uint64_t val, char* vname, int flag)
{
 crc32_8bytes(val);
 if (flag) {
    printf("...checksum after hashing %s : %lX\n", vname, crc32_context ^ 0xFFFFFFFFUL);
 }
}



static void
transparent_crc_bytes (char *ptr, int nbytes, char* vname, int flag)
{
    int i;
    for (i=0; i<nbytes; i++) {
        crc32_byte(ptr[i]);
    }
 if (flag) {
    printf("...checksum after hashing %s : %lX\n", vname, crc32_context ^ 0xFFFFFFFFUL);
 }
}
# 11 "csmith_r2.c" 2


static long __undefined;


#pragma pack(push)
#pragma pack(1)
struct S0 {
   int64_t f0;
   int8_t f1;
   const signed f2 : 18;
   const volatile uint32_t f3;
};
#pragma pack(pop)

union U1 {
   int8_t * const f0;
   uint32_t f1;
   uint16_t f2;
   uint32_t f3;
};


static int8_t g_2 = 0x28L;
static int32_t g_4 = 0xE5664DC9L;
static int16_t g_17[4] = {1L,1L,1L,1L};
static const union U1 g_41 = {0};
static int8_t *g_45 = (void*)0;
static int8_t **g_44 = &g_45;
static int8_t *** volatile g_43[6][6] = {{(void*)0,(void*)0,(void*)0,&g_44,(void*)0,(void*)0},{(void*)0,&g_44,&g_44,&g_44,&g_44,(void*)0},{(void*)0,(void*)0,&g_44,(void*)0,(void*)0,(void*)0},{&g_44,(void*)0,(void*)0,&g_44,&g_44,&g_44},{&g_44,&g_44,&g_44,(void*)0,(void*)0,&g_44},{(void*)0,(void*)0,(void*)0,&g_44,(void*)0,(void*)0}};
static int8_t *** volatile g_46 = &g_44;
static int32_t g_60 = 0xCC3231A2L;
static int8_t * const *g_73 = &g_45;
static int8_t * const **g_72[8] = {&g_73,&g_73,&g_73,&g_73,&g_73,&g_73,&g_73,&g_73};
static int8_t g_86 = 0L;
static int16_t g_89 = 0x3A40L;
static int32_t *g_104 = &g_60;
static int32_t ** volatile g_103[5][6] = {{&g_104,&g_104,&g_104,&g_104,&g_104,&g_104},{&g_104,&g_104,&g_104,&g_104,&g_104,&g_104},{&g_104,&g_104,&g_104,&g_104,&g_104,&g_104},{&g_104,&g_104,&g_104,&g_104,&g_104,&g_104},{&g_104,&g_104,&g_104,&g_104,&g_104,&g_104}};
static int32_t ** volatile g_105 = &g_104;
static const int32_t *g_112 = &g_60;
static const int32_t **g_111 = &g_112;
static volatile int32_t g_119 = (-2L);
static int32_t g_120 = 0x22B5D766L;
static volatile int64_t g_140 = 4L;
static volatile int64_t *g_139 = &g_140;
static uint8_t g_143 = 1UL;
static int32_t g_159[3] = {(-1L),(-1L),(-1L)};
static uint16_t g_161 = 65535UL;
static uint32_t g_165 = 0xF864726AL;
static const int8_t * const g_168 = &g_2;
static const int8_t * const *g_167 = &g_168;
static const int8_t * const **g_166 = &g_167;
static int16_t g_181 = 0xF6C4L;
static uint64_t g_191 = 1UL;
static uint64_t g_193 = 0xB8CC65BC01747DCELL;
static int32_t g_222 = (-1L);
static volatile int64_t g_226 = 1L;
static int32_t * volatile g_265 = &g_159[1];
static uint16_t g_277 = 0x4464L;
static struct S0 g_279 = {0x681C3699C864A9CFLL,0x38L,-17,18446744073709551611UL};
static int8_t **g_290 = &g_45;
static uint32_t g_296 = 4294967295UL;
static int16_t g_324 = (-5L);
static int16_t g_349 = 0L;
static uint64_t **g_363 = (void*)0;
static uint64_t *** volatile g_364[3][9][9] = {{{(void*)0,&g_363,&g_363,(void*)0,&g_363,&g_363,(void*)0,(void*)0,(void*)0},{&g_363,(void*)0,(void*)0,&g_363,&g_363,&g_363,(void*)0,(void*)0,&g_363},{&g_363,&g_363,&g_363,&g_363,&g_363,(void*)0,&g_363,(void*)0,&g_363},{(void*)0,(void*)0,&g_363,&g_363,&g_363,(void*)0,(void*)0,&g_363,&g_363},{&g_363,(void*)0,&g_363,&g_363,(void*)0,(void*)0,(void*)0,&g_363,&g_363},{&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363,(void*)0},{&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363},{&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363},{(void*)0,&g_363,&g_363,&g_363,&g_363,&g_363,(void*)0,&g_363,(void*)0}},{{&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363},{&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363,&g_363,&g_363,&g_363},{&g_363,(void*)0,(void*)0,&g_363,&g_363,(void*)0,&g_363,&g_363,(void*)0},{&g_363,&g_363,&g_363,&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363},{(void*)0,(void*)0,(void*)0,(void*)0,&g_363,&g_363,&g_363,&g_363,&g_363},{&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363},{&g_363,&g_363,(void*)0,&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363},{(void*)0,&g_363,&g_363,(void*)0,&g_363,&g_363,(void*)0,(void*)0,(void*)0},{&g_363,(void*)0,(void*)0,&g_363,&g_363,&g_363,(void*)0,(void*)0,&g_363}},{{&g_363,&g_363,&g_363,&g_363,&g_363,(void*)0,&g_363,(void*)0,&g_363},{(void*)0,(void*)0,&g_363,&g_363,&g_363,(void*)0,(void*)0,&g_363,&g_363},{&g_363,(void*)0,&g_363,&g_363,(void*)0,(void*)0,(void*)0,&g_363,&g_363},{&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363,(void*)0},{&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363},{&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363,&g_363},{(void*)0,&g_363,&g_363,&g_363,&g_363,&g_363,(void*)0,&g_363,(void*)0},{&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363,&g_363,(void*)0,&g_363},{&g_363,(void*)0,&g_363,(void*)0,&g_363,&g_363,&g_363,&g_363,&g_363}}};
static volatile uint32_t * const g_388 = (void*)0;
static volatile uint32_t * const * volatile g_387 = &g_388;
static volatile uint32_t g_391[6][2] = {{0xB5BE49F8L,0xB5BE49F8L},{0xB5BE49F8L,1UL},{0UL,0xD7D2F8A9L},{1UL,0xD7D2F8A9L},{0UL,1UL},{0xB5BE49F8L,0xB5BE49F8L}};
static uint32_t *g_394 = &g_296;
static uint32_t * volatile * volatile g_393 = &g_394;
static uint32_t * volatile * volatile * volatile g_392 = &g_393;
static int64_t g_439 = 0x8D1FB8C1EB45D0DBLL;
static uint32_t * volatile g_473 = &g_165;
static uint32_t * volatile * volatile g_472 = &g_473;
static const uint32_t g_478[10] = {0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL,0xDA3CF38AL};
static const uint64_t g_502 = 18446744073709551606UL;
static const uint64_t *g_501 = &g_502;
static uint32_t g_515 = 0x1F9C4100L;
static struct S0 g_590 = {0L,0L,347,0UL};
static volatile uint32_t g_669[8][6] = {{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL},{6UL,6UL,18446744073709551615UL,6UL,6UL,18446744073709551615UL}};
static union U1 g_674[1] = {{0}};
static union U1 *g_673 = &g_674[0];
static int32_t ** volatile *g_696 = &g_105;
static int32_t ** volatile ** volatile g_695 = &g_696;
static uint32_t **g_703 = &g_394;
static uint32_t ***g_702 = &g_703;
static uint32_t ****g_701[10] = {&g_702,&g_702,&g_702,&g_702,&g_702,&g_702,&g_702,&g_702,&g_702,&g_702};
static uint32_t ***** volatile g_700 = &g_701[5];
static struct S0 *g_724[9] = {&g_590,&g_279,&g_590,&g_279,&g_590,&g_279,&g_590,&g_279,&g_590};
static struct S0 ** volatile g_723 = &g_724[8];
static uint32_t g_762 = 3UL;
static int64_t g_769 = 0L;
static int8_t g_800[8] = {0xC9L,1L,0xC9L,0xC9L,1L,0xC9L,0xC9L,1L};
static const volatile uint16_t *g_805 = (void*)0;
static const volatile uint16_t ** volatile g_804 = &g_805;
static volatile uint32_t g_834 = 0xA05D6E0AL;
static struct S0 g_865 = {0x7D64BE20F6F8F4E8LL,0x0CL,262,0x4B892F1AL};
static uint32_t g_886[3] = {0UL,0UL,0UL};
static const union U1 *g_932 = &g_674[0];
static const union U1 ** volatile g_931[6][4][6] = {{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}},{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}},{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}},{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}},{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}},{{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932},{&g_932,&g_932,&g_932,&g_932,&g_932,&g_932}}};
static const union U1 ** volatile g_933 = &g_932;
static int32_t **g_983 = &g_104;
static int32_t ***g_982 = &g_983;
static int32_t ****g_981[7][9] = {{&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,(void*)0},{&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982},{&g_982,&g_982,&g_982,(void*)0,(void*)0,(void*)0,&g_982,&g_982,&g_982},{&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982},{&g_982,(void*)0,(void*)0,&g_982,(void*)0,&g_982,(void*)0,&g_982,(void*)0},{&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982,&g_982},{(void*)0,&g_982,(void*)0,(void*)0,&g_982,&g_982,&g_982,&g_982,&g_982}};
static volatile uint64_t g_991 = 0x6DDFCF6D3D5DD967LL;
static uint32_t * const *g_1041 = &g_394;
static uint32_t * const **g_1040 = &g_1041;
static uint32_t * const ***g_1039 = &g_1040;
static const union U1 **g_1046 = &g_932;
static const union U1 *** volatile g_1045 = &g_1046;
static struct S0 g_1054 = {-2L,-4L,-146,18446744073709551608UL};
static volatile int16_t *g_1089 = (void*)0;
static volatile int16_t * volatile *g_1088[3][1] = {{&g_1089},{&g_1089},{&g_1089}};
static uint16_t *g_1133[8] = {&g_277,&g_277,&g_277,&g_277,&g_277,&g_277,&g_277,&g_277};
static uint16_t **g_1132 = &g_1133[6];
static volatile struct S0 g_1160 = {0L,0xA2L,225,18446744073709551615UL};
static int8_t g_1264 = (-5L);
static union U1 * volatile *g_1290[4][5][7] = {{{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,(void*)0},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,(void*)0},{(void*)0,&g_673,&g_673,(void*)0,&g_673,&g_673,(void*)0},{&g_673,(void*)0,&g_673,(void*)0,(void*)0,&g_673,&g_673},{&g_673,&g_673,(void*)0,&g_673,(void*)0,&g_673,&g_673}},{{&g_673,&g_673,&g_673,(void*)0,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,(void*)0,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673}},{{(void*)0,&g_673,&g_673,&g_673,(void*)0,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,(void*)0,&g_673},{(void*)0,&g_673,&g_673,&g_673,&g_673,(void*)0,(void*)0},{&g_673,&g_673,&g_673,(void*)0,&g_673,&g_673,(void*)0},{&g_673,&g_673,&g_673,&g_673,(void*)0,(void*)0,(void*)0}},{{&g_673,&g_673,&g_673,(void*)0,&g_673,&g_673,&g_673},{&g_673,&g_673,(void*)0,(void*)0,&g_673,(void*)0,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,(void*)0},{&g_673,&g_673,(void*)0,&g_673,&g_673,(void*)0,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,(void*)0,(void*)0}}};
static union U1 * volatile * volatile *g_1289[9][7][4] = {{{(void*)0,&g_1290[2][2][2],&g_1290[3][0][2],&g_1290[2][4][1]},{&g_1290[1][0][3],&g_1290[2][2][2],&g_1290[3][0][1],&g_1290[2][2][0]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[1][3][3],&g_1290[2][2][2]},{&g_1290[2][2][0],&g_1290[2][2][0],(void*)0,&g_1290[2][2][0]},{&g_1290[1][2][1],&g_1290[2][2][0],&g_1290[3][0][1],&g_1290[2][2][0]},{&g_1290[2][2][0],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,&g_1290[0][2][2],&g_1290[3][2][5],(void*)0}},{{(void*)0,&g_1290[3][4][5],&g_1290[2][2][0],(void*)0},{(void*)0,(void*)0,&g_1290[3][2][5],(void*)0},{&g_1290[2][2][0],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[3][4][5]},{&g_1290[1][3][3],(void*)0,&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],&g_1290[2][0][3],&g_1290[2][2][5],(void*)0},{(void*)0,&g_1290[2][4][1],&g_1290[2][2][0],&g_1290[2][2][0]}},{{&g_1290[0][4][3],&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],&g_1290[2][4][1],&g_1290[0][1][5],(void*)0},{&g_1290[2][2][0],&g_1290[2][0][3],&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[3][0][2],(void*)0,&g_1290[1][3][2],&g_1290[3][4][5]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[0][0][6],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,(void*)0,&g_1290[3][0][1],(void*)0}},{{&g_1290[2][2][0],&g_1290[3][4][5],&g_1290[3][0][1],(void*)0},{(void*)0,&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][4][1]},{&g_1290[0][0][6],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[2][2][0],(void*)0,&g_1290[1][3][2],(void*)0},{&g_1290[3][0][2],(void*)0,&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],(void*)0,&g_1290[0][1][5],&g_1290[2][0][3]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0}},{{&g_1290[0][4][3],&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][0][3]},{(void*)0,(void*)0,&g_1290[2][2][5],&g_1290[2][2][0]},{&g_1290[2][2][0],(void*)0,&g_1290[2][2][0],(void*)0},{&g_1290[1][3][3],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][4][1]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[3][2][5],(void*)0},{(void*)0,&g_1290[3][4][5],&g_1290[2][2][0],(void*)0}},{{(void*)0,(void*)0,&g_1290[3][2][5],(void*)0},{&g_1290[2][2][0],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[3][4][5]},{&g_1290[1][3][3],(void*)0,&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],&g_1290[2][0][3],&g_1290[2][2][5],(void*)0},{(void*)0,&g_1290[2][4][1],&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[0][4][3],&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0]}},{{&g_1290[2][2][0],&g_1290[2][4][1],&g_1290[0][1][5],(void*)0},{&g_1290[2][2][0],&g_1290[2][0][3],&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[3][0][2],(void*)0,&g_1290[1][3][2],&g_1290[3][4][5]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[0][0][6],(void*)0,&g_1290[2][2][0],(void*)0},{(void*)0,(void*)0,&g_1290[3][0][1],(void*)0},{&g_1290[2][2][0],&g_1290[3][4][5],&g_1290[3][0][1],(void*)0}},{{(void*)0,&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][4][1]},{&g_1290[0][0][6],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[2][2][0],(void*)0,&g_1290[1][3][2],(void*)0},{&g_1290[3][0][2],(void*)0,&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],(void*)0,&g_1290[0][1][5],&g_1290[2][0][3]},{&g_1290[2][2][0],(void*)0,&g_1290[1][0][3],&g_1290[2][2][2]},{&g_1290[3][2][5],(void*)0,(void*)0,&g_1290[2][2][0]}},{{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[2][2][0],&g_1290[1][3][1],(void*)0,&g_1290[2][2][0]},{&g_1290[2][2][5],&g_1290[2][2][0],(void*)0,&g_1290[2][2][0]},{&g_1290[1][3][2],&g_1290[2][2][0],&g_1290[2][2][0],(void*)0},{&g_1290[2][2][0],(void*)0,&g_1290[3][1][4],&g_1290[2][2][0]},{&g_1290[2][2][0],(void*)0,&g_1290[2][2][0],&g_1290[2][2][0]},{&g_1290[2][2][0],&g_1290[2][2][0],&g_1290[3][1][4],&g_1290[1][3][1]}}};
static union U1 * volatile * volatile **g_1288 = &g_1289[3][0][3];
static int64_t g_1420[4] = {0x64C405734159A0DCLL,0x64C405734159A0DCLL,0x64C405734159A0DCLL,0x64C405734159A0DCLL};
static const uint32_t g_1451[3] = {4294967295UL,4294967295UL,4294967295UL};
static const uint32_t *g_1450 = &g_1451[1];
static const uint32_t **g_1449 = &g_1450;
static struct S0 g_1460 = {4L,0x42L,335,0UL};
static int32_t ** const volatile g_1489 = &g_104;
static struct S0 g_1514 = {0x45210E7F69E185E8LL,0xF2L,450,0x95A09C24L};
static struct S0 *** volatile g_1520 = (void*)0;
static uint16_t *** volatile g_1572 = &g_1132;
static volatile int16_t g_1583 = (-3L);
static int32_t *g_1593 = &g_222;
static int32_t * const volatile *g_1592 = &g_1593;
static int32_t * const volatile **g_1591 = &g_1592;
static volatile struct S0 g_1638 = {0xACF68B5BD6CE4CD5LL,9L,84,0UL};
static uint32_t ** const *g_1675 = &g_703;
static uint32_t ** const **g_1674 = &g_1675;
static uint32_t ** const *** volatile g_1673 = &g_1674;
static uint32_t *****g_1676 = &g_701[6];
static uint32_t *g_1694[4][5][1] = {{{&g_165},{&g_165},{(void*)0},{(void*)0},{(void*)0}},{{&g_165},{&g_165},{(void*)0},{(void*)0},{(void*)0}},{{&g_165},{&g_165},{(void*)0},{(void*)0},{(void*)0}},{{&g_165},{&g_165},{(void*)0},{(void*)0},{(void*)0}}};
static uint32_t ** const volatile g_1693 = &g_1694[1][2][0];
static uint32_t ** const volatile *g_1692 = &g_1693;
static uint32_t ** const volatile ** volatile g_1691 = &g_1692;
static uint8_t g_1736 = 0x61L;
static union U1 ** const volatile g_1741[4] = {&g_673,&g_673,&g_673,&g_673};
static union U1 ** const volatile g_1742 = &g_673;
static int64_t *g_1800[1] = {&g_1420[0]};
static struct S0 g_1804 = {0x43C933EB9CB494DCLL,0x92L,-385,0xB4AE10A4L};
static volatile uint64_t g_1854 = 9UL;
static volatile struct S0 g_1874 = {7L,-1L,-335,1UL};
static volatile uint32_t g_1930 = 0x24956218L;
static struct S0 * volatile ** const g_2025 = (void*)0;
static struct S0 g_2035[1] = {{-2L,8L,-464,18446744073709551615UL}};
static uint64_t ***g_2052 = &g_363;
static uint64_t ****g_2051 = &g_2052;
static volatile struct S0 g_2180 = {8L,0xEEL,264,7UL};
static struct S0 g_2225[5][1][10] = {{{{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{-7L,0x96L,-282,0x989A8E6CL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{5L,0x73L,-91,18446744073709551606UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{5L,0x73L,-91,18446744073709551606UL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{0xE1289C799DE59521LL,0x39L,-105,0UL}}},{{{5L,0x73L,-91,18446744073709551606UL},{5L,0x73L,-91,18446744073709551606UL},{0x25AF851A6A6CDA6DLL,0x08L,-400,0xF412F7C2L},{-7L,0x96L,-282,0x989A8E6CL},{5L,0x73L,-91,18446744073709551606UL},{0L,0x16L,477,0x856E9A28L},{-7L,0x96L,-282,0x989A8E6CL},{-7L,0x96L,-282,0x989A8E6CL},{0L,0x16L,477,0x856E9A28L},{5L,0x73L,-91,18446744073709551606UL}}},{{{5L,0x73L,-91,18446744073709551606UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{5L,0x73L,-91,18446744073709551606UL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{0xE1289C799DE59521LL,0x39L,-105,0UL},{-7L,0x96L,-282,0x989A8E6CL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{-7L,0x96L,-282,0x989A8E6CL}}},{{{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{5L,0x73L,-91,18446744073709551606UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{0xE1289C799DE59521LL,0x39L,-105,0UL},{5L,0x73L,-91,18446744073709551606UL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{0xE1289C799DE59521LL,0x39L,-105,0UL},{-7L,0x96L,-282,0x989A8E6CL},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L},{0x534D2820C1F3F4E8LL,6L,-54,0x36A48ED7L}}},{{{5L,0x73L,-91,18446744073709551606UL},{-7L,0x96L,-282,0x989A8E6CL},{0x25AF851A6A6CDA6DLL,0x08L,-400,0xF412F7C2L},{5L,0x73L,-91,18446744073709551606UL},{5L,0x73L,-91,18446744073709551606UL},{0x25AF851A6A6CDA6DLL,0x08L,-400,0xF412F7C2L},{-7L,0x96L,-282,0x989A8E6CL},{5L,0x73L,-91,18446744073709551606UL},{0L,0x16L,477,0x856E9A28L},{-7L,0x96L,-282,0x989A8E6CL}}}};
static struct S0 g_2273 = {-2L,0x06L,-9,0x11628A28L};
static const struct S0 g_2352 = {4L,0xAAL,-418,0x6E823E85L};
static struct S0 g_2377 = {9L,-5L,119,0UL};
static union U1 g_2379[1][6][4] = {{{{0},{0},{0},{0}},{{0},{0},{0},{0}},{{0},{0},{0},{0}},{{0},{0},{0},{0}},{{0},{0},{0},{0}},{{0},{0},{0},{0}}}};
static uint64_t *****g_2407 = &g_2051;
static uint32_t g_2416[8] = {1UL,1UL,1UL,1UL,1UL,1UL,1UL,1UL};
static int16_t g_2490 = 0L;
static uint32_t ***g_2555 = (void*)0;
static uint32_t ****g_2554 = &g_2555;
static int32_t g_2593 = 0xCF950DA9L;
static int8_t g_2597 = 0xE2L;
static volatile int32_t g_2609 = 0x9267ABBBL;
static int8_t ***g_2618 = (void*)0;
static int8_t ****g_2617 = &g_2618;
static int8_t ****g_2620 = (void*)0;
static int8_t g_2623[9] = {0x95L,0x95L,0x95L,0x95L,0x95L,0x95L,0x95L,0x95L,0x95L};
static volatile uint8_t g_2659 = 0x2EL;
static int32_t g_2708 = 0x998F1688L;
static uint16_t g_2723 = 1UL;
static struct S0 g_2773 = {-3L,0x0EL,-443,18446744073709551615UL};
static uint32_t g_2808 = 18446744073709551615UL;
static uint32_t g_2843 = 0xE4D2FDE8L;
static uint8_t g_2844[6][8] = {{0x6FL,253UL,255UL,0UL,255UL,253UL,0x6FL,1UL},{5UL,1UL,255UL,0x1DL,1UL,0x1DL,255UL,1UL},{255UL,246UL,0x48L,0UL,1UL,1UL,1UL,0UL},{5UL,246UL,5UL,1UL,255UL,0x1DL,1UL,0UL},{255UL,0x1DL,1UL,0x1DL,255UL,1UL,5UL,246UL},{255UL,1UL,5UL,246UL,5UL,1UL,255UL,0x1DL}};
static uint64_t g_2918[7] = {9UL,9UL,9UL,9UL,9UL,9UL,9UL};
static int16_t g_2941 = 0x2490L;
static uint16_t g_2969 = 0xFC78L;
static const int32_t *** const volatile g_3010 = &g_111;
static volatile struct S0 g_3049 = {0x566F400436DE5345LL,0x7FL,453,4UL};
static int32_t g_3104 = 0x12281F33L;
static int32_t g_3112 = 4L;
static int32_t g_3138 = 0x563F9F56L;
static int64_t **g_3174 = &g_1800[0];
static uint16_t g_3182 = 65531UL;
static int32_t g_3213 = (-1L);



static uint8_t func_1(void);
static uint16_t func_5(int32_t * p_6, int32_t * p_7);
static int32_t * func_8(int64_t p_9);
static int64_t func_27(uint32_t p_28, int8_t * p_29, int8_t * p_30, uint32_t p_31, int16_t * p_32);
static int8_t * func_33(const int8_t p_34, uint32_t p_35, uint64_t p_36, const union U1 p_37, int16_t * p_38);
static int32_t * func_49(int32_t * p_50, int8_t p_51, int32_t * p_52, uint8_t p_53, int8_t * const ** p_54);
static int32_t * func_55(int8_t * p_56, union U1 p_57);
static union U1 func_68(int8_t p_69);
static uint8_t func_90(const int32_t ** const p_91, int64_t p_92);
static const int32_t ** func_93(int32_t p_94, int32_t p_95, int8_t *** p_96, union U1 p_97, int8_t * p_98);
# 223 "csmith_r2.c"
static uint8_t func_1(void)
{
    int32_t *l_3[6] = {&g_4,&g_4,&g_4,&g_4,&g_4,&g_4};
    int8_t *l_2455 = &g_1264;
    union U1 l_2456 = {0};
    const uint16_t l_2509 = 0x5AA3L;
    uint64_t *l_2518 = &g_191;
    uint64_t **l_2517 = &l_2518;
    uint16_t l_2528[8] = {65531UL,65531UL,65531UL,65531UL,65531UL,65531UL,65531UL,65531UL};
    const int16_t l_2529 = (-9L);
    struct S0 **l_2624 = &g_724[0];
    int8_t l_2744 = 0x2EL;
    const uint16_t *l_2781[9][6] = {{&l_2528[7],&l_2528[7],&l_2528[7],&g_277,(void*)0,&g_161},{&l_2509,&l_2528[7],&g_2723,&g_2723,&g_2723,&l_2528[7]},{&l_2528[7],&g_161,&g_161,&l_2528[2],&l_2528[4],&l_2528[7]},{&l_2528[1],&g_277,&l_2528[1],(void*)0,&l_2528[7],&l_2509},{&g_277,&g_277,&l_2528[7],&l_2528[7],&l_2528[4],(void*)0},{&g_161,&g_161,&l_2528[7],&g_2723,&g_2723,&l_2528[7]},{&l_2528[7],&l_2528[7],&l_2528[7],&g_161,(void*)0,(void*)0},{&l_2528[7],&l_2528[7],(void*)0,&l_2528[1],(void*)0,&l_2528[7]},{&l_2509,&l_2528[7],(void*)0,&l_2528[7],&l_2528[7],(void*)0}};
    const uint16_t **l_2780[7] = {&l_2781[1][1],&l_2781[1][1],&l_2781[1][4],&l_2781[1][1],&l_2781[1][1],&l_2781[1][4],&l_2781[1][1]};
    uint32_t *****l_2785 = &g_701[5];
    int32_t l_2795 = (-1L);
    int32_t l_2867 = 0xCBEAE1DAL;
    uint32_t ****l_2915 = (void*)0;
    int64_t l_3058 = 1L;
    union U1 **l_3066 = &g_673;
    union U1 ***l_3065 = &l_3066;
    uint32_t l_3067[5][2][2] = {{{18446744073709551607UL,18446744073709551607UL},{18446744073709551607UL,18446744073709551607UL}},{{18446744073709551607UL,18446744073709551607UL},{18446744073709551607UL,18446744073709551607UL}},{{18446744073709551607UL,18446744073709551607UL},{18446744073709551607UL,18446744073709551607UL}},{{18446744073709551607UL,18446744073709551607UL},{18446744073709551607UL,18446744073709551607UL}},{{18446744073709551607UL,18446744073709551607UL},{18446744073709551607UL,18446744073709551607UL}}};
    int8_t ***l_3108 = &g_290;
    uint32_t l_3180[9] = {4294967295UL,4294967295UL,4294967295UL,4294967295UL,4294967295UL,4294967295UL,4294967295UL,4294967295UL,4294967295UL};
    int64_t **l_3181 = (void*)0;
    uint32_t l_3210 = 18446744073709551613UL;
    int i, j, k;
    if ((g_4 = g_2))
    {
        uint64_t l_2424 = 0UL;
        uint32_t l_2440 = 0x772CBAF9L;
        int32_t l_2442 = 0x32A8583CL;
        uint16_t l_2457 = 0x746CL;
        uint32_t l_2466 = 4294967293UL;
        int16_t l_2495 = (-1L);
        int32_t l_2538 = 0xB6EED34CL;
        uint32_t ****l_2557 = &g_2555;
        int16_t l_2564 = (-1L);
        uint16_t ***l_2565 = &g_1132;
        int32_t l_2591 = 0xF5C5127CL;
        int32_t l_2603 = (-1L);
        int32_t l_2604 = 0x422CC811L;
        int32_t l_2605[10][1] = {{5L},{1L},{5L},{1L},{5L},{1L},{5L},{1L},{5L},{1L}};
        int32_t l_2606 = 1L;
        uint8_t l_2610 = 0x53L;
        uint32_t l_2630[2];
        union U1 l_2639 = {0};
        uint64_t *l_2644[3][8][7] = {{{&g_193,&g_193,&l_2424,&g_193,&g_193,(void*)0,(void*)0},{(void*)0,(void*)0,&l_2424,&g_193,&g_193,&g_193,&l_2424},{&l_2424,&l_2424,(void*)0,&g_191,&g_193,&g_193,&g_193},{(void*)0,&l_2424,(void*)0,&g_193,&g_191,&g_193,&g_193},{&g_191,&g_191,&g_193,&g_191,&g_193,&l_2424,&g_193},{&l_2424,&g_191,&g_191,(void*)0,&g_193,&g_191,&g_193},{&l_2424,&g_193,&g_193,&l_2424,&g_193,&g_191,&g_193},{&g_193,&g_191,&g_191,&g_193,&g_191,&g_193,&g_193}},{{&g_191,&l_2424,(void*)0,&g_193,&g_193,(void*)0,&g_191},{&g_191,&l_2424,&g_191,&l_2424,&g_191,&g_193,&g_193},{&l_2424,&g_193,&l_2424,&g_193,(void*)0,(void*)0,&g_193},{&g_193,(void*)0,&l_2424,(void*)0,&g_193,&g_191,&g_193},{&g_193,&l_2424,&g_193,&g_193,&l_2424,&g_193,(void*)0},{&l_2424,&l_2424,(void*)0,&g_193,&g_191,(void*)0,&g_193},{&g_191,(void*)0,&g_191,&g_191,&g_193,&g_193,&l_2424},{&g_191,&l_2424,&g_193,&g_193,&l_2424,&g_191,&g_193}},{{(void*)0,&l_2424,&g_191,&g_191,(void*)0,&g_193,&g_191},{&g_193,(void*)0,&g_191,(void*)0,(void*)0,&g_191,(void*)0},{&g_193,&g_193,&g_193,&g_191,(void*)0,&g_193,&g_191},{&l_2424,&l_2424,&g_191,&l_2424,(void*)0,(void*)0,&l_2424},{&g_191,&l_2424,(void*)0,&g_191,&g_193,(void*)0,&g_193},{&g_191,&l_2424,&g_193,(void*)0,&l_2424,&l_2424,&g_191},{&g_193,&g_193,&l_2424,&g_191,&l_2424,&l_2424,&g_191},{&l_2424,&g_193,&l_2424,&g_193,&g_193,&g_193,&g_191}}};
        int32_t l_2665 = (-8L);
        const int32_t l_2690 = 0x6AD5D72BL;
        uint64_t l_2691 = 0xC13ACCC940D12F2ELL;
        int32_t l_2807 = 9L;
        uint32_t ****l_2868 = &g_702;
        uint64_t l_2940 = 3UL;
        int16_t l_2948[7] = {(-2L),0x12E3L,0x12E3L,(-2L),0x12E3L,0x12E3L,(-2L)};
        int16_t l_2958 = 0x47F2L;
        int32_t l_2959 = 0x3F690ABCL;
        uint8_t l_2965 = 0xD2L;
        uint32_t l_2980 = 7UL;
        int32_t **l_3019 = (void*)0;
        uint8_t l_3040 = 0x2BL;
        struct S0 *l_3073 = &g_1460;
        const uint16_t l_3089 = 0x5802L;
        const uint64_t **l_3109 = &g_501;
        int i, j, k;
        for (i = 0; i < 2; i++)
            l_2630[i] = 1UL;
    }
    else
    {
        int8_t l_3153 = 9L;
        uint8_t l_3211 = 0x3EL;
        int32_t l_3236 = 0x4BA7D4E5L;
        uint16_t *l_3248[1][8][6] = {{{&g_2723,&g_277,&g_3182,(void*)0,&l_2528[2],&g_2969},{&g_2969,&g_2969,&g_2723,&g_2969,&g_2969,&g_277},{&g_2969,&g_2723,&g_2969,(void*)0,(void*)0,(void*)0},{&g_2723,&l_2528[2],&l_2528[2],&g_2723,&g_2723,(void*)0},{&g_3182,(void*)0,&g_2969,&g_277,(void*)0,&g_277},{&g_2723,&g_161,&g_2723,&g_2969,(void*)0,&g_2969},{&g_2969,(void*)0,&g_3182,&g_2723,&g_2723,&g_3182},{&l_2528[2],&l_2528[2],&g_2723,&g_2723,(void*)0,&g_2969}}};
        uint16_t **l_3249 = &l_3248[0][7][5];
        int8_t ***l_3258 = &g_44;
        int8_t ***l_3259 = &g_290;
        uint8_t l_3266 = 8UL;
        int i, j, k;
        for (g_3104 = 0; (g_3104 != 0); g_3104 += 8)
        {
            uint16_t l_3154 = 65535UL;
            uint8_t l_3179 = 5UL;
            int8_t l_3184 = (-1L);
            int32_t l_3187 = (-4L);
            for (l_2744 = 0; (l_2744 <= 5); l_2744 += 1)
            {
                int32_t ** const ***l_3152 = (void*)0;
                uint32_t l_3217 = 0xBFF543DBL;
                uint32_t **l_3224 = &g_394;
                int32_t l_3237 = (-1L);
                int i;
                (**g_696) = l_3[l_2744];
                if ((((void*)0 == l_3152) <= (((l_3153 < (((0x00L > (((void*)0 == (*g_982)) != l_3154)) >= ((*g_1691) == (*g_2554))) & (*****g_1676))) || (**g_1132)) && g_2273.f3)))
                {
                    int64_t l_3169 = 1L;
                    int32_t l_3209 = 1L;
                    for (l_2795 = 1; (l_2795 >= 0); l_2795 -= 1)
                    {
                        int64_t **l_3173 = &g_1800[0];
                        int32_t l_3183 = 1L;
                        int16_t l_3185 = (-1L);
                        uint32_t *l_3186 = &g_515;
                        int64_t *l_3212 = &g_1514.f0;
                        int i, j;
                        l_3187 |= ((((((((uint32_t)(((((((int32_t)((uint16_t)(((((uint8_t)(((uint8_t)((-1L) != g_391[l_2744][l_2795]) % (uint8_t)(((++(***g_702)) >= g_159[l_2795]) ^ (((*l_3186) = (((int8_t)((*g_2407) != (*g_2407)) - (int8_t)(l_3169 < ((((**g_1132) = (l_3183 |= ((~((((int8_t)((((g_3174 = l_3173) != ((((int32_t)((int64_t)((l_3179 != (**g_1132)) ^ g_159[l_2795]) % (int64_t)1L) - (int32_t)0UL) || l_3180[3]) , l_3181)) , l_3153) && (**g_3174)) - (int8_t)g_3182) , (**g_1132)) ^ g_1460.f1)) != (-10L)))) && l_3184) == l_3185))) || 0x8C27L)) || (-1L)))) ^ (*g_104)) + (uint8_t)l_3185) && (*g_139)) | (**g_1449)) | g_159[l_2795]) - (uint16_t)l_3169) - (int32_t)(**g_1449)) | 0x332F221014819540LL) != l_3153) & 8UL) ^ (**g_167)) , l_3169) + (uint32_t)0xCD5EF6CFL) != 0xE8DE3CD7L) , (-1L)) > g_2225[4][0][6].f1) >= 0x199F462BL) < l_3169) , (***g_982));
                        (**g_983) |= ((int16_t)((((*l_3212) ^= (((**g_1132)--) || (((int32_t)((int64_t)0x195A2CEBF596DD43LL + (int64_t)(~l_3169)) - (int32_t)((uint32_t)((****g_1039)--) - (uint32_t)(((int8_t)(-4L) * (int8_t)(((uint8_t)(l_3169 ^ ((l_3185 || ((int16_t)g_159[l_2795] + (int16_t)(l_3209 == (l_3210 == l_3179)))) == l_3211)) * (uint8_t)g_1054.f3) || l_3187)) < 0x9BB6D998L))) || (**g_3174)))) != g_3213) >= (-5L)) * (int16_t)l_3154);
                        if (g_391[l_2744][l_2795])
                            continue;
                    }
                }
                else
                {
                    int32_t *l_3214[1];
                    uint8_t *l_3225[9][7][4] = {{{&g_143,&l_3211,&l_3211,&g_2844[3][3]},{&l_3179,&g_2844[2][1],&g_1736,&g_1736},{(void*)0,&g_1736,&g_1736,&g_1736},{(void*)0,&g_2844[2][1],&l_3179,&g_2844[3][3]},{&g_1736,&l_3211,(void*)0,&l_3179},{(void*)0,&g_143,&l_3211,(void*)0},{&g_2844[5][2],&g_1736,(void*)0,&l_3211}},{{&g_2844[3][3],&g_1736,&g_1736,&l_3211},{&l_3211,&g_1736,&g_1736,&g_2844[5][7]},{&g_143,(void*)0,&g_143,&g_1736},{&g_1736,&l_3179,&l_3179,&l_3179},{&l_3179,&g_1736,(void*)0,&g_1736},{&g_143,&g_2844[3][3],&g_143,&l_3211},{(void*)0,&g_143,&g_143,&g_2844[5][7]}},{{(void*)0,&g_143,&g_143,(void*)0},{&g_143,&g_2844[5][7],(void*)0,&g_143},{&l_3179,&g_1736,&l_3179,&l_3211},{&g_1736,(void*)0,&g_143,&g_1736},{&g_143,&g_143,&g_1736,&g_143},{&l_3211,&l_3179,&g_1736,&g_143},{&g_2844[3][3],&g_143,(void*)0,&l_3179}},{{&g_2844[5][2],&g_1736,&l_3211,&l_3211},{&g_1736,&g_1736,&g_1736,&g_1736},{(void*)0,&g_1736,&g_1736,(void*)0},{&g_143,&g_2844[5][7],&g_2844[5][2],&g_1736},{&g_2844[3][3],&g_2844[5][7],&l_3179,(void*)0},{&g_2844[5][7],&g_1736,(void*)0,&g_1736},{&g_143,&g_1736,&g_143,&l_3211}},{{&l_3211,&g_1736,&g_143,&l_3179},{&g_143,&g_143,&g_2844[5][2],&g_143},{&g_143,&l_3179,&l_3211,&g_143},{(void*)0,&g_143,&l_3179,&g_1736},{&g_1736,(void*)0,&g_143,&l_3211},{&g_143,&g_1736,&g_143,&g_143},{&g_143,&g_2844[5][7],&g_1736,(void*)0}},{{&g_1736,&g_143,(void*)0,&g_2844[5][7]},{&g_2844[5][2],&g_143,(void*)0,&l_3211},{&g_1736,&g_2844[3][3],&g_1736,&g_1736},{&g_143,&g_1736,&g_143,&l_3179},{&g_143,&l_3179,&g_143,&g_1736},{&g_1736,(void*)0,&l_3179,&g_2844[5][7]},{(void*)0,&g_1736,&l_3211,&l_3211}},{{&g_143,&g_1736,&g_2844[5][2],&l_3211},{&g_143,&g_1736,&g_143,(void*)0},{&l_3211,&g_143,&g_143,&l_3211},{&g_143,(void*)0,(void*)0,&g_143},{&g_2844[5][7],&g_1736,&l_3179,&g_1736},{&g_2844[3][3],(void*)0,&g_2844[5][2],&g_1736},{&g_143,&g_1736,&g_1736,&g_143}},{{(void*)0,(void*)0,&g_1736,&l_3211},{&g_1736,&g_143,&l_3211,(void*)0},{&g_2844[5][2],&g_1736,(void*)0,&l_3211},{&g_2844[3][3],&g_1736,&g_1736,&l_3211},{(void*)0,&l_3211,&l_3179,&g_1736},{&g_2844[2][1],&g_143,&g_143,&g_1736},{&g_143,&g_1736,&g_143,&g_1736}},{{&g_1736,&l_3211,(void*)0,&l_3179},{&l_3179,&g_143,&g_2844[0][6],&g_143},{(void*)0,&g_143,&l_3211,&g_1736},{(void*)0,&g_143,&g_2844[0][6],(void*)0},{&l_3179,&g_1736,(void*)0,&g_2844[0][6]},{&g_1736,(void*)0,&g_143,&l_3211},{&g_143,&g_1736,&g_143,&g_1736}}};
                    int i, j, k;
                    for (i = 0; i < 1; i++)
                        l_3214[i] = &g_159[1];
                    (**g_696) = l_3214[0];
                    (*g_111) = func_8(((((int8_t)((l_3217 , ((**g_703)++)) <= ((*g_501) == (**g_3174))) >> (int8_t)6) || ((**l_2517) = ((uint16_t)(((l_3187 = (((*g_702) = l_3224) != (void*)0)) > (((((***g_1572) >= ((int16_t)((l_3153 == (((*g_1450) && ((((uint8_t)((uint16_t)((uint16_t)(((((uint8_t)(g_143 = l_3236) >> (uint8_t)g_277) , (**g_1449)) || 4294967292UL) ^ 0x9598L) * (uint16_t)1L) >> (uint16_t)g_515) + (uint8_t)g_1460.f2) < l_3211) , l_3154)) , g_2708)) < 0x136BE30A6A2492F4LL) / (int16_t)g_886[0])) >= l_3184) && (***g_1572)) <= l_3184)) , l_3184) >> (uint16_t)g_2416[2]))) == l_3236));
                    (*g_983) = &l_2795;
                    if (l_3237)
                        continue;
                }
            }
            for (g_296 = 0; (g_296 != 8); g_296++)
            {
                uint64_t l_3240 = 1UL;
                l_3240 &= (*g_265);
            }
        }
        l_3266 &= ((**g_983) = ((((*g_394) , (((uint32_t)(((int16_t)(!(((void*)0 == &g_724[2]) != (((*g_1132) = (*g_1132)) != ((*l_3249) = l_3248[0][2][4])))) << (int16_t)(((uint8_t)((int8_t)((int64_t)(*g_139) % (int64_t)((uint8_t)(((*g_2617) = (l_3258 = l_3108)) == l_3259) << (uint8_t)((int8_t)((int8_t)((((int8_t)((void*)0 == (**l_2785)) - (int8_t)g_2597) <= 0UL) < l_3211) >> (int8_t)1) >> (int8_t)4))) % (int8_t)(-1L)) >> (uint8_t)1) ^ g_2969)) != l_3153) / (uint32_t)6UL) & l_3153)) && 0x27L) | (**g_3174)));
    }
    return g_193;
}







static uint16_t func_5(int32_t * p_6, int32_t * p_7)
{
    uint32_t *l_1175 = &g_165;
    uint32_t **l_1177 = (void*)0;
    uint32_t ***l_1176 = &l_1177;
    int32_t ****l_1178 = &g_982;
    uint8_t l_1207[9];
    uint32_t l_1209 = 18446744073709551615UL;
    const int16_t l_1210[10] = {1L,0x06D6L,1L,0x06D6L,1L,0x06D6L,1L,0x06D6L,1L,0x06D6L};
    int32_t l_1243 = 0x2DA00917L;
    int32_t l_1245 = 6L;
    int32_t l_1248 = 1L;
    int32_t l_1249 = 0L;
    int32_t l_1250 = 1L;
    int32_t l_1251 = 0x5E4CCA56L;
    int32_t l_1253 = 0x9D1323E9L;
    int32_t l_1256 = 0xF615AB18L;
    int32_t l_1258 = 0L;
    int32_t l_1260 = 0x5C9760FDL;
    int32_t l_1261 = 0x7A03FA37L;
    int32_t l_1262 = 0xD73C7FA9L;
    int32_t l_1266 = 1L;
    int32_t l_1267 = 0xACC41E36L;
    int32_t l_1268 = 5L;
    int32_t l_1270[9] = {4L,4L,4L,4L,4L,4L,4L,4L,4L};
    union U1 ** const l_1293[9][7] = {{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673},{&g_673,&g_673,&g_673,&g_673,&g_673,&g_673,&g_673}};
    union U1 ** const *l_1292 = &l_1293[5][4];
    union U1 ** const **l_1291[9][1] = {{&l_1292},{&l_1292},{&l_1292},{&l_1292},{&l_1292},{&l_1292},{&l_1292},{&l_1292},{&l_1292}};
    int32_t l_1299[5];
    int8_t l_1352 = 0xE9L;
    int8_t l_1378 = 0x27L;
    uint64_t *l_1390 = &g_193;
    uint64_t **l_1389 = &l_1390;
    uint32_t *l_1524 = &g_886[0];
    int8_t l_1562 = 0xB4L;
    uint16_t ** const l_1571 = &g_1133[6];
    int32_t l_1582[2][5][6] = {{{0xC13F5D6AL,0xC13F5D6AL,8L,7L,8L,0xC13F5D6AL},{8L,(-1L),7L,7L,(-1L),8L},{0xC13F5D6AL,8L,7L,8L,0xC13F5D6AL,0xC13F5D6AL},{(-1L),8L,8L,(-1L),(-1L),(-1L)},{(-1L),(-1L),(-1L),8L,8L,(-1L)}},{{0xC13F5D6AL,0xC13F5D6AL,8L,7L,8L,0xC13F5D6AL},{8L,(-1L),7L,7L,(-1L),8L},{0xC13F5D6AL,8L,7L,8L,0xC13F5D6AL,0xC13F5D6AL},{(-1L),8L,8L,(-1L),(-1L),(-1L)},{(-1L),(-1L),(-1L),8L,8L,7L}}};
    int8_t l_1584 = 0xABL;
    int16_t l_1585 = 0xDFCDL;
    int16_t l_1586[4];
    int16_t l_1587 = (-6L);
    int8_t l_1597[1][8] = {{0xFAL,0xFAL,0xE7L,0xFAL,0xFAL,0xE7L,0xFAL,0xFAL}};
    int32_t l_1666 = (-10L);
    int16_t l_1842 = 0x9E46L;
    uint32_t l_1857 = 1UL;
    uint32_t **l_1866 = &l_1524;
    int8_t l_1939[10][4][6] = {{{1L,0x99L,0xF4L,0L,0xF7L,(-6L)},{0x53L,1L,(-6L),0x29L,(-5L),(-1L)},{0x4DL,0L,0x9AL,(-4L),0x1FL,(-1L)},{(-5L),0x58L,(-1L),0xD2L,(-2L),0x63L}},{{(-1L),0xC4L,(-6L),(-1L),(-6L),(-4L)},{1L,0x57L,0x99L,0x1DL,0x51L,0x4DL},{(-1L),0L,0xFFL,(-1L),0x79L,0x05L},{(-1L),(-1L),0xA8L,0L,(-1L),0x4AL}},{{(-1L),(-2L),0L,(-1L),(-10L),5L},{0x01L,0x25L,0x01L,0x57L,(-5L),(-6L)},{(-1L),0L,(-1L),0L,0x40L,0x30L},{0L,0L,2L,(-3L),(-5L),(-5L)}},{{0x17L,0x4DL,(-5L),(-7L),0x43L,0xD2L},{0x51L,0L,0xCBL,(-5L),(-1L),0xFFL},{(-5L),0x98L,0x79L,(-1L),2L,(-2L)},{0L,0xA4L,0L,0xFFL,(-1L),(-1L)}},{{0x01L,0x31L,0x31L,0x01L,0x1FL,(-5L)},{(-2L),(-8L),0xF4L,(-1L),0xDCL,(-1L)},{0xC9L,(-1L),0L,0x29L,0xDCL,(-5L)},{0x9AL,(-8L),(-1L),0x1DL,0x1FL,0xA8L}},{{0x98L,0x31L,0xE0L,0xB6L,(-1L),0x63L},{(-2L),0xA4L,5L,(-6L),2L,0x1DL},{1L,0x98L,0L,(-6L),(-1L),0x36L},{0x4DL,0L,1L,(-5L),0x43L,0x4CL}},{{(-7L),0x4DL,0xA8L,(-1L),(-5L),2L},{0x29L,0L,0x05L,(-1L),0x40L,(-4L)},{0x0BL,0L,0L,(-5L),(-5L),3L},{1L,0x25L,0xE0L,(-1L),(-10L),0x30L}},{{(-5L),(-2L),0x36L,0xDCL,(-1L),0xC4L},{0x17L,(-1L),(-6L),0xCBL,0x79L,0xB6L},{0L,0L,0L,1L,0L,0xCBL},{0x64L,0x53L,(-5L),0x9AL,0xE0L,5L}},{{0L,0x30L,(-1L),0xF4L,(-6L),0x01L},{(-2L),(-6L),(-5L),4L,0L,0x53L},{(-7L),0xC4L,(-1L),(-7L),0x98L,(-1L)},{0xA7L,0xF7L,0xF5L,0x30L,0x57L,0x30L}},{{(-6L),0L,(-6L),(-10L),0x99L,0xDFL},{0x53L,0xAEL,0x4AL,0x17L,(-7L),0x05L},{(-1L),0L,(-5L),0x17L,0xE0L,(-10L)},{0x53L,0xCBL,(-1L),(-10L),2L,0xD1L}}};
    uint32_t l_1971 = 0x011814A6L;
    uint16_t l_1972[3];
    int8_t *** const l_1989 = &g_290;
    uint32_t l_2124 = 0x6697B6F9L;
    int32_t l_2151 = 0x35DEB648L;
    int32_t ***l_2226 = &g_983;
    int32_t **l_2233 = (void*)0;
    int32_t ** const *l_2232 = &l_2233;
    uint32_t *****l_2252 = &g_701[8];
    const uint32_t ** const *l_2255 = &g_1449;
    const uint32_t ** const **l_2254 = &l_2255;
    const uint32_t ** const ***l_2253 = &l_2254;
    int32_t l_2261 = (-9L);
    uint32_t l_2265 = 5UL;
    int32_t *l_2268 = &g_159[1];
    uint64_t l_2278 = 0xD079DABEB17A40FELL;
    int64_t l_2296 = 0x2E78D347270AFDD5LL;
    uint32_t l_2345 = 0xAF1BE36FL;
    const uint64_t **l_2364 = &g_501;
    uint64_t *****l_2406 = (void*)0;
    int i, j, k;
    for (i = 0; i < 9; i++)
        l_1207[i] = 253UL;
    for (i = 0; i < 5; i++)
        l_1299[i] = (-1L);
    for (i = 0; i < 4; i++)
        l_1586[i] = 0x39C1L;
    for (i = 0; i < 3; i++)
        l_1972[i] = 0xE554L;
    return (***l_2226);
}







static int32_t * func_8(int64_t p_9)
{
    int64_t l_10 = 0xFE662DB8E9646B8DLL;
    const int16_t *l_40[5][8] = {{&g_17[0],&g_17[2],&g_17[3],&g_17[2],&g_17[2],&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,&g_17[1],(void*)0,&g_17[2],&g_17[2],(void*)0,&g_17[1]},{&g_17[0],&g_17[0],&g_17[3],&g_17[2],&g_17[2],&g_17[2],&g_17[1],&g_17[2]},{&g_17[2],&g_17[0],&g_17[0],&g_17[2],(void*)0,&g_17[2],&g_17[2],&g_17[2]},{&g_17[0],&g_17[2],&g_17[2],&g_17[2],&g_17[0],(void*)0,&g_17[3],&g_17[1]}};
    int8_t *l_1138 = &g_800[0];
    int8_t *l_1139 = (void*)0;
    int8_t *l_1149 = &g_590.f1;
    int32_t *l_1172 = &g_159[1];
    int i, j;
    if (l_10)
    {
        int16_t *l_15 = (void*)0;
        int16_t *l_16[4] = {&g_17[2],&g_17[2],&g_17[2],&g_17[2]};
        int32_t l_18[3][2][6] = {{{(-6L),0x1AA12AFDL,0x9F4F9A02L,0x1AA12AFDL,(-6L),(-6L)},{(-1L),0x1AA12AFDL,0x1AA12AFDL,(-1L),(-1L),(-1L)}},{{(-1L),(-1L),(-1L),0x1AA12AFDL,0x1AA12AFDL,(-1L)},{(-6L),(-6L),0x1AA12AFDL,0x9F4F9A02L,0x1AA12AFDL,(-6L)}},{{0x1AA12AFDL,(-1L),0x9F4F9A02L,0x9F4F9A02L,(-1L),0x1AA12AFDL},{(-6L),0x1AA12AFDL,0x9F4F9A02L,0x1AA12AFDL,(-6L),(-6L)}}};
        uint32_t *l_1148 = &g_515;
        union U1 l_1150[1] = {{0}};
        int i, j, k;
        (*g_104) &= (((*l_1148) &= ((***g_702) &= ((g_4 == ((uint16_t)(((uint8_t)(((((l_18[0][1][4] = 0L) , p_9) , ((uint64_t)(((uint64_t)((uint16_t)l_18[1][0][0] >> (uint16_t)((uint64_t)0x0F3BD791F9E6A012LL - (uint64_t)func_27(g_17[2], (l_1138 = func_33(g_4, (!((void*)0 == l_40[4][1])), g_4, g_41, &g_17[3])), l_1139, p_9, &g_17[3]))) % (uint64_t)p_9) <= 0x5997L) + (uint64_t)l_10)) , 0x4B91L) || l_10) % (uint8_t)255UL) >= (**g_1132)) << (uint16_t)13)) , p_9))) , 0xA8591CEAL);
        (*g_111) = func_55(l_1149, l_1150[0]);
    }
    else
    {
        const uint16_t l_1157[5][1][10] = {{{0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL}},{{0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL}},{{0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL}},{{0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL}},{{0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL,0x5EDEL}}};
        int32_t l_1171[3][6] = {{0x89EAFDD2L,0x06981327L,0x89EAFDD2L,0x06981327L,0x89EAFDD2L,0x06981327L},{1L,0x06981327L,1L,0x06981327L,1L,0x06981327L},{0x89EAFDD2L,0x06981327L,0x89EAFDD2L,0x06981327L,0x89EAFDD2L,0x06981327L}};
        int i, j, k;
        l_1171[1][1] = ((uint64_t)(func_68((((uint16_t)((uint32_t)(((0x4BF1L != l_1157[3][0][5]) < l_1157[3][0][5]) > ((uint8_t)(g_1160 , ((int32_t)l_1157[2][0][1] / (int32_t)(l_1157[1][0][4] , ((int16_t)(func_68((p_9 >= ((uint8_t)((uint8_t)((int8_t)0L >> (int8_t)p_9) + (uint8_t)p_9) << (uint8_t)4))) , p_9) - (int16_t)p_9)))) % (uint8_t)p_9)) / (uint32_t)(***g_1040)) << (uint16_t)g_161) < p_9)) , p_9) / (uint64_t)p_9);
    }
    return l_1172;
}







static int64_t func_27(uint32_t p_28, int8_t * p_29, int8_t * p_30, uint32_t p_31, int16_t * p_32)
{
    int32_t *l_1140 = &g_159[2];
    int32_t *l_1141 = &g_120;
    int32_t *l_1142 = &g_120;
    int32_t *l_1143 = &g_159[1];
    int32_t *l_1144[2];
    uint64_t l_1145 = 18446744073709551609UL;
    int i;
    for (i = 0; i < 2; i++)
        l_1144[i] = &g_60;
    --l_1145;
    return p_28;
}







static int8_t * func_33(const int8_t p_34, uint32_t p_35, uint64_t p_36, const union U1 p_37, int16_t * p_38)
{
    int8_t **l_42 = (void*)0;
    int8_t *l_58 = &g_2;
    uint32_t **l_699 = &g_394;
    uint32_t ***l_698 = &l_699;
    uint32_t **** const l_697[6][4] = {{&l_698,&l_698,&l_698,&l_698},{&l_698,&l_698,&l_698,&l_698},{&l_698,&l_698,&l_698,&l_698},{&l_698,&l_698,&l_698,&l_698},{&l_698,&l_698,&l_698,&l_698},{&l_698,&l_698,&l_698,&l_698}};
    const int32_t l_709 = 0x7A591B96L;
    int32_t l_744 = 0x3763FD0FL;
    int32_t l_746 = 0xADF698EDL;
    int32_t l_749 = 0L;
    int32_t l_752 = 0x83331BF2L;
    int32_t l_753 = 0x8AFC3400L;
    int32_t l_754 = 0x537FB405L;
    int64_t l_756 = 0xFCAF8554B0520DF7LL;
    int32_t l_760 = (-8L);
    int32_t l_761[3][10] = {{0xE342038DL,0xE342038DL,0L,6L,0x5E380C9AL,6L,0L,0xE342038DL,0xE342038DL,0L},{1L,6L,0L,0L,6L,1L,0L,1L,6L,0L},{(-6L),0xE342038DL,(-6L),0L,0L,0L,0L,(-6L),0xE342038DL,(-6L)}};
    int32_t l_869 = 0x4201D0C2L;
    int8_t l_881 = (-5L);
    uint8_t l_882 = 2UL;
    int32_t *l_930 = (void*)0;
    struct S0 **l_965 = &g_724[8];
    int32_t ****l_985 = &g_982;
    int8_t ***l_1007 = &g_290;
    int8_t ****l_1006 = &l_1007;
    int16_t *l_1087 = &g_17[2];
    int16_t **l_1086 = &l_1087;
    uint32_t l_1112 = 0x7CEAD347L;
    int i, j;
    (*g_46) = l_42;
    for (g_2 = 0; (g_2 < 8); g_2 += 9)
    {
        int8_t *l_64[4];
        int8_t **l_65 = (void*)0;
        int8_t **l_66 = (void*)0;
        int8_t **l_67 = &l_64[2];
        int32_t l_70[5][8][3] = {{{0xA39D8E7DL,0xD3B28C33L,9L},{(-1L),0xBEAAF72AL,0xBEAAF72AL},{1L,0x4D5916FFL,0x37E3B525L},{1L,0L,7L},{1L,9L,0xF4439D3FL},{(-1L),0xFDCBC339L,(-3L)},{0xA39D8E7DL,9L,0x4D5916FFL},{0x4995FB43L,0L,(-2L)}},{{(-9L),0x4D5916FFL,0x4D5916FFL},{0xD6A391DEL,0xBEAAF72AL,(-3L)},{0xAB93B572L,0xD3B28C33L,0xF4439D3FL},{0xD6A391DEL,(-2L),7L},{(-9L),(-3L),0x37E3B525L},{0x4995FB43L,(-2L),0xBEAAF72AL},{0xA39D8E7DL,0xD3B28C33L,9L},{(-1L),0xBEAAF72AL,0xBEAAF72AL}},{{1L,0x4D5916FFL,0x37E3B525L},{1L,0L,7L},{1L,9L,0xF4439D3FL},{(-1L),0xFDCBC339L,(-3L)},{0xA39D8E7DL,9L,0x4D5916FFL},{0x4995FB43L,0L,(-2L)},{(-9L),0x4D5916FFL,0x4D5916FFL},{0xD6A391DEL,0xBEAAF72AL,(-3L)}},{{0xAB93B572L,0xD3B28C33L,0xF4439D3FL},{0xD6A391DEL,(-2L),7L},{(-9L),(-3L),0x37E3B525L},{0x4995FB43L,(-2L),0xBEAAF72AL},{0xA39D8E7DL,0xD3B28C33L,9L},{(-1L),0xBEAAF72AL,0xBEAAF72AL},{1L,0x4D5916FFL,0x37E3B525L},{1L,0L,7L}},{{1L,9L,0xF4439D3FL},{(-1L),0xFDCBC339L,(-3L)},{0xA39D8E7DL,9L,0x4D5916FFL},{0x4995FB43L,0L,(-2L)},{(-9L),0x4D5916FFL,0x4D5916FFL},{0xD6A391DEL,0xBEAAF72AL,(-3L)},{0xAB93B572L,0xD3B28C33L,0xF4439D3FL},{0xD6A391DEL,(-2L),7L}}};
        int i, j, k;
        for (i = 0; i < 4; i++)
            l_64[i] = &g_2;
    }
    for (g_165 = 1; (g_165 <= 5); g_165 += 1)
    {
        const uint16_t l_710 = 0xCE97L;
        int16_t *l_713 = &g_181;
        int16_t *l_714 = (void*)0;
        int16_t *l_715 = &g_89;
        int32_t l_739 = 0x1E345F12L;
        int32_t l_742 = (-6L);
        int32_t l_743 = 0x24017A80L;
        int32_t l_745[4];
        uint32_t **l_824 = (void*)0;
        uint64_t ***l_839 = &g_363;
        uint64_t ****l_838 = &l_839;
        uint32_t l_851[1];
        struct S0 *l_888 = (void*)0;
        int8_t l_891 = 0xCAL;
        int32_t l_925 = (-1L);
        int32_t l_952 = (-3L);
        const int8_t l_953 = 3L;
        int8_t **l_968 = &g_45;
        uint32_t l_1062 = 0x7C70E36AL;
        union U1 **l_1101 = &g_673;
        uint16_t **l_1135 = (void*)0;
        uint16_t **l_1136 = &g_1133[6];
        int i;
        for (i = 0; i < 4; i++)
            l_745[i] = 0xAB902159L;
        for (i = 0; i < 1; i++)
            l_851[i] = 0x8C026557L;
    }
    return (*g_290);
}







static int32_t * func_49(int32_t * p_50, int8_t p_51, int32_t * p_52, uint8_t p_53, int8_t * const ** p_54)
{
    const int32_t *l_74 = &g_4;
    const int32_t **l_75 = &l_74;
    int32_t l_84 = 1L;
    uint8_t l_87 = 0UL;
    int8_t *l_100 = (void*)0;
    int32_t l_431 = 1L;
    int32_t l_432[3][5] = {{1L,1L,1L,1L,1L},{0x099F71D8L,0x099F71D8L,0x099F71D8L,0x099F71D8L,0x099F71D8L},{1L,1L,1L,1L,1L}};
    uint64_t l_456 = 0x909DC5050F4D023DLL;
    int8_t *l_459 = &g_279.f1;
    int32_t l_566 = 0xF3055E7BL;
    uint16_t l_627 = 4UL;
    union U1 l_628 = {0};
    int16_t l_649 = (-1L);
    int8_t *l_670 = &g_590.f1;
    int i, j;
    (*l_75) = l_74;
    for (p_53 = 0; (p_53 <= 5); p_53 += 1)
    {
        int8_t *l_85 = &g_86;
        int64_t *l_88 = (void*)0;
        const int16_t *l_99 = &g_17[2];
        int32_t l_395[6][7];
        int32_t *l_396 = &g_120;
        uint32_t ** const l_457 = &g_394;
        int64_t l_463[1][1][6] = {{{0x506C693F10E83FCALL,0x506C693F10E83FCALL,0x506C693F10E83FCALL,0x506C693F10E83FCALL,0x506C693F10E83FCALL,0x506C693F10E83FCALL}}};
        uint32_t *l_514 = &g_515;
        int64_t l_643 = 0xE119C30B112E5F26LL;
        int16_t l_651 = 0x22C3L;
        const union U1 *l_685[2];
        const union U1 **l_684[5];
        int i, j, k;
        for (i = 0; i < 6; i++)
        {
            for (j = 0; j < 7; j++)
                l_395[i][j] = 0x2A269663L;
        }
        for (i = 0; i < 2; i++)
            l_685[i] = &g_674[0];
        for (i = 0; i < 5; i++)
            l_684[i] = &l_685[0];
        (*l_396) = ((((int16_t)((uint16_t)((int64_t)(g_89 |= ((((int16_t)g_17[2] - (int16_t)(18446744073709551607UL != (((*l_85) &= l_84) != l_87))) , 3UL) >= g_4)) - (int64_t)((((func_90(func_93((18446744073709551613UL || ((l_99 != l_99) <= (**l_75))), (*l_74), &g_44, g_41, l_100), p_53) || (**l_75)) ^ (-1L)) >= l_395[1][2]) , (**l_75))) % (uint16_t)7L) * (int16_t)0xABC9L) != l_395[2][6]) & 18446744073709551614UL);
        (*l_396) = (g_296 , (-10L));
    }
    (*g_695) = &g_103[2][1];
    return (***g_695);
}







static int32_t * func_55(int8_t * p_56, union U1 p_57)
{
    int32_t *l_59[4][4] = {{&g_4,&g_60,&g_4,&g_4},{&g_60,&g_60,&g_60,&g_60},{&g_60,&g_4,&g_60,&g_4},{&g_60,&g_4,&g_4,&g_60}};
    uint32_t l_61 = 0x102089CEL;
    int i, j;
    --l_61;
    return &g_4;
}







static union U1 func_68(int8_t p_69)
{
    union U1 l_71 = {0};
    return l_71;
}







static uint8_t func_90(const int32_t ** const p_91, int64_t p_92)
{
    const uint32_t * const l_258 = &g_165;
    uint64_t *l_260 = &g_191;
    int32_t l_267 = 1L;
    int8_t **l_293 = &g_45;
    uint16_t **l_303 = (void*)0;
    int32_t l_313 = 0x4B26B4A2L;
    int32_t l_316 = (-6L);
    int32_t l_318 = 0L;
    int32_t l_319[4];
    int64_t l_386 = 0xC24C430DF94836F7LL;
    volatile uint32_t *l_390[8] = {&g_391[0][1],&g_391[0][1],&g_391[0][1],&g_391[0][1],&g_391[0][1],&g_391[0][1],&g_391[0][1],&g_391[0][1]};
    volatile uint32_t * const * volatile l_389[5] = {&l_390[5],&l_390[5],&l_390[5],&l_390[5],&l_390[5]};
    int i;
    for (i = 0; i < 4; i++)
        l_319[i] = 0x652B751EL;
    for (g_161 = 0; (g_161 <= 7); g_161 += 1)
    {
        uint32_t l_243[7][1][4] = {{{0x2754F78DL,4UL,0xAA003E9EL,4UL}},{{0x9D498514L,0x82585733L,18446744073709551613UL,0xAA003E9EL}},{{4UL,0x82585733L,0x82585733L,4UL}},{{0x82585733L,4UL,0x9D498514L,0x2754F78DL}},{{0x82585733L,0x9D498514L,0x82585733L,18446744073709551613UL}},{{4UL,0x2754F78DL,18446744073709551613UL,18446744073709551613UL}},{{0x9D498514L,0x9D498514L,0xAA003E9EL,0x2754F78DL}}};
        int8_t *l_254 = &g_2;
        uint64_t *l_261[5][10] = {{(void*)0,&g_191,(void*)0,&g_191,&g_191,(void*)0,&g_191,(void*)0,&g_191,&g_191},{(void*)0,&g_191,(void*)0,&g_191,&g_191,(void*)0,&g_191,(void*)0,&g_191,&g_191},{(void*)0,&g_191,(void*)0,&g_191,&g_191,(void*)0,&g_191,(void*)0,&g_191,&g_191},{(void*)0,&g_191,(void*)0,&g_191,&g_191,(void*)0,&g_191,(void*)0,&g_191,&g_191},{(void*)0,&g_191,(void*)0,&g_191,&g_191,(void*)0,&g_191,(void*)0,&g_191,&g_191}};
        int32_t *l_266 = &g_159[0];
        int8_t **l_291 = &g_45;
        int8_t **l_292 = &g_45;
        union U1 l_299[5] = {{0},{0},{0},{0},{0}};
        int32_t l_305 = 0L;
        int32_t l_317 = (-1L);
        int32_t l_322 = 0x2CF00A70L;
        int32_t l_325 = 0xB8D93F8CL;
        int i, j, k;
        for (g_143 = 0; (g_143 <= 5); g_143 += 1)
        {
            int32_t *l_240 = &g_120;
            int32_t *l_241 = &g_159[1];
            int32_t *l_242 = (void*)0;
            l_243[1][0][2]--;
            return p_92;
        }
        for (g_143 = 0; (g_143 <= 5); g_143 += 1)
        {
            union U1 l_255[8] = {{0},{0},{0},{0},{0},{0},{0},{0}};
            uint32_t *l_257 = &g_165;
            uint32_t **l_256 = &l_257;
            uint64_t * const l_259 = &g_191;
            uint64_t **l_262 = &l_261[3][5];
            uint16_t *l_263 = &l_255[4].f2;
            int32_t l_264 = 1L;
            int i, j;
        }
        (*l_266) = l_267;
        if ((**p_91))
            continue;
        for (g_222 = 1; (g_222 <= 7); g_222 += 1)
        {
            uint16_t *l_276 = &g_277;
            int8_t **l_288[5] = {(void*)0,(void*)0,(void*)0,(void*)0,(void*)0};
            int8_t ***l_289[5];
            uint32_t *l_294 = (void*)0;
            uint32_t *l_295 = &g_296;
            int32_t l_297 = (-9L);
            int32_t *l_298 = &g_120;
            int32_t l_312 = 4L;
            int32_t l_314 = 2L;
            int32_t l_315 = 0x1A3F0E9EL;
            int32_t l_320 = 0xF026A4BCL;
            int32_t l_321 = 0xEF8D0AE2L;
            int32_t l_323 = 0x47D163DEL;
            int i;
            for (i = 0; i < 5; i++)
                l_289[i] = (void*)0;
            (*l_298) |= (((int32_t)(((uint16_t)g_143 >> (uint16_t)((((int32_t)(((uint16_t)(p_92 == ((p_92 || ((*l_276) = g_222)) <= ((*l_266) = (+p_92)))) / (uint16_t)0x45ABL) && ((g_279 , (((*l_295) = ((uint8_t)((uint16_t)(--(*l_276)) % (uint16_t)(((uint8_t)((l_292 = (l_291 = (g_290 = l_288[2]))) != l_293) >> (uint8_t)g_159[0]) , 65535UL)) << (uint8_t)l_267)) && l_297)) , p_92)) % (int32_t)p_92) ^ p_92) >= 0xF2L)) <= 0L) + (int32_t)5L) , (**g_105));
            (*l_298) = 1L;
            (*g_111) = (g_279 , func_55((*l_293), l_299[3]));
            for (g_60 = 4; (g_60 >= 1); g_60 -= 1)
            {
                int32_t *l_300[2][4][4] = {{{&g_4,&g_4,&l_297,&g_4},{&g_4,(void*)0,(void*)0,&g_4},{(void*)0,&g_4,(void*)0,(void*)0},{&g_4,&g_4,&l_297,&g_4}},{{&g_4,(void*)0,(void*)0,&g_4},{(void*)0,&g_4,(void*)0,(void*)0},{&g_4,&g_4,&l_297,&g_4},{&g_4,(void*)0,(void*)0,&g_4}}};
                int16_t l_347 = 1L;
                const uint64_t *l_374 = &g_193;
                const uint64_t **l_373 = &l_374;
                const uint64_t ***l_372 = &l_373;
                int i, j, k;
                for (g_181 = 7; (g_181 >= 0); g_181 -= 1)
                {
                    for (g_165 = 0; (g_165 <= 4); g_165 += 1)
                    {
                        int i, j;
                        l_267 = l_267;
                    }
                }
                if ((l_305 = ((*l_266) = (((l_298 = l_300[1][0][1]) == &l_297) | ((int16_t)(g_296 != ((void*)0 != l_303)) >> (int16_t)(~p_92))))))
                {
                    int16_t *l_306 = (void*)0;
                    int32_t l_310[2];
                    int i;
                    for (i = 0; i < 2; i++)
                        l_310[i] = (-1L);
                    for (g_277 = 1; (g_277 <= 7); g_277 += 1)
                    {
                        int8_t *l_307 = (void*)0;
                        uint32_t **l_308 = (void*)0;
                        uint32_t **l_309 = &l_294;
                        int32_t l_311[3];
                        uint32_t l_326 = 0xA360B774L;
                        int i, j;
                        for (i = 0; i < 3; i++)
                            l_311[i] = (-1L);
                        (*l_266) = ((l_306 == &g_17[2]) > ((((*l_309) = func_55(l_307, g_41)) != (void*)0) < (g_4 == l_310[1])));
                        ++l_326;
                    }
                    (*g_111) = func_55((*l_293), g_41);
                }
                else
                {
                    int8_t **l_344 = &g_45;
                    const int32_t l_348 = 0xFBD77C71L;
                    int8_t l_362[2];
                    int i;
                    for (i = 0; i < 2; i++)
                        l_362[i] = 0x1CL;
                    (*l_266) = ((((int32_t)((uint16_t)(p_92 < ((*l_266) < (((uint64_t)((int16_t)((uint8_t)(((void*)0 != p_91) | g_324) * (uint8_t)(((int16_t)(((int8_t)((((+(l_344 != (*g_46))) ^ (((uint16_t)65529UL >> (uint16_t)10) < 18446744073709551607UL)) != p_92) > l_347) >> (int8_t)(*l_266)) == p_92) << (int16_t)12) , 0xC4L)) % (int16_t)g_279.f0) - (uint64_t)l_348) <= (-3L)))) - (uint16_t)g_277) + (int32_t)p_92) ^ p_92) > g_349);
                    for (l_267 = 5; (l_267 >= 0); l_267 -= 1)
                    {
                        const int64_t l_352[8][10][3] = {{{0xDE6F619C15D4301BLL,0xD7FADE429BEF84BFLL,1L},{0L,1L,0L},{0xD7FADE429BEF84BFLL,0xD7FADE429BEF84BFLL,0L},{7L,(-1L),0xEF1349D586583F67LL},{0x76508F860F2E0DB1LL,0L,(-7L)},{1L,0xDB582213720E50DBLL,0xE735E70CFACC17FFLL},{(-7L),0xDE6F619C15D4301BLL,(-7L)},{0x4F117ED82E38CEF1LL,(-4L),0L},{(-2L),0xA810D62903FEF63FLL,0x76508F860F2E0DB1LL},{0xDB582213720E50DBLL,0x54195F14F3697CACLL,1L}},{{0xD7FADE429BEF84BFLL,0x6831ED015881643CLL,0x6831ED015881643CLL},{0xDB582213720E50DBLL,0xEF1349D586583F67LL,1L},{(-2L),1L,0L},{0x4F117ED82E38CEF1LL,1L,0xDB582213720E50DBLL},{(-7L),0x76508F860F2E0DB1LL,1L},{1L,1L,7L},{0xDE6F619C15D4301BLL,1L,0x648DD55F3FAD2A69LL},{0x54195F14F3697CACLL,0xEF1349D586583F67LL,(-1L)},{0L,0x6831ED015881643CLL,1L},{(-1L),0x54195F14F3697CACLL,(-1L)}},{{0xEBB463BC3F93BFE9LL,0xA810D62903FEF63FLL,0x648DD55F3FAD2A69LL},{0L,(-4L),7L},{0x6831ED015881643CLL,0xDE6F619C15D4301BLL,1L},{(-8L),0xDB582213720E50DBLL,0xDB582213720E50DBLL},{0x6831ED015881643CLL,0L,0L},{0L,0xE735E70CFACC17FFLL,1L},{0xEBB463BC3F93BFE9LL,0L,0x6831ED015881643CLL},{(-1L),7L,1L},{0L,0L,0x76508F860F2E0DB1LL},{0x54195F14F3697CACLL,0xE735E70CFACC17FFLL,0L}},{{0xDE6F619C15D4301BLL,0L,(-7L)},{1L,0xDB582213720E50DBLL,0xE735E70CFACC17FFLL},{(-7L),0xDE6F619C15D4301BLL,(-7L)},{0x4F117ED82E38CEF1LL,(-4L),0L},{(-2L),0xA810D62903FEF63FLL,0x76508F860F2E0DB1LL},{0xDB582213720E50DBLL,0x54195F14F3697CACLL,1L},{0xD7FADE429BEF84BFLL,0x6831ED015881643CLL,0x6831ED015881643CLL},{0xDB582213720E50DBLL,0xEF1349D586583F67LL,1L},{(-2L),1L,0L},{0x4F117ED82E38CEF1LL,1L,0xDB582213720E50DBLL}},{{(-7L),0x76508F860F2E0DB1LL,1L},{1L,1L,7L},{0xDE6F619C15D4301BLL,1L,0x648DD55F3FAD2A69LL},{0x54195F14F3697CACLL,0xEF1349D586583F67LL,(-1L)},{0L,0x6831ED015881643CLL,1L},{(-1L),0x54195F14F3697CACLL,(-1L)},{0xEBB463BC3F93BFE9LL,0xA810D62903FEF63FLL,0x648DD55F3FAD2A69LL},{0L,(-4L),7L},{0x6831ED015881643CLL,0xDE6F619C15D4301BLL,1L},{(-8L),0xDB582213720E50DBLL,0xDB582213720E50DBLL}},{{0x6831ED015881643CLL,0L,0L},{0L,0xE735E70CFACC17FFLL,1L},{0xEBB463BC3F93BFE9LL,0L,0x6831ED015881643CLL},{(-1L),7L,1L},{0L,0L,0x76508F860F2E0DB1LL},{0x54195F14F3697CACLL,0xE735E70CFACC17FFLL,0L},{0xDE6F619C15D4301BLL,0L,(-7L)},{1L,0xDB582213720E50DBLL,0xE735E70CFACC17FFLL},{(-7L),0xDE6F619C15D4301BLL,(-7L)},{0x4F117ED82E38CEF1LL,(-4L),0L}},{{(-2L),0xA810D62903FEF63FLL,0x76508F860F2E0DB1LL},{0xDB582213720E50DBLL,0x54195F14F3697CACLL,1L},{0xD7FADE429BEF84BFLL,0x6831ED015881643CLL,0x6831ED015881643CLL},{0xDB582213720E50DBLL,0xEF1349D586583F67LL,1L},{(-2L),1L,0L},{0x4F117ED82E38CEF1LL,1L,0xDB582213720E50DBLL},{(-7L),0x76508F860F2E0DB1LL,1L},{1L,1L,7L},{0xEBB463BC3F93BFE9LL,0x6831ED015881643CLL,1L},{0x4F117ED82E38CEF1LL,0L,0xE735E70CFACC17FFLL}},{{0x76508F860F2E0DB1LL,0xD7FADE429BEF84BFLL,0x6831ED015881643CLL},{0xE735E70CFACC17FFLL,0x4F117ED82E38CEF1LL,0xE735E70CFACC17FFLL},{0xA810D62903FEF63FLL,0L,1L},{(-1L),0xEF1349D586583F67LL,0x54195F14F3697CACLL},{0xD7FADE429BEF84BFLL,0xEBB463BC3F93BFE9LL,(-2L)},{1L,(-8L),(-8L)},{0xD7FADE429BEF84BFLL,0x648DD55F3FAD2A69LL,0x76508F860F2E0DB1LL},{(-1L),0xDB582213720E50DBLL,7L},{0xA810D62903FEF63FLL,0x76508F860F2E0DB1LL,0xD7FADE429BEF84BFLL},{0xE735E70CFACC17FFLL,0x54195F14F3697CACLL,0L}}};
                        uint16_t *l_361 = &l_299[3].f2;
                        int i, j, k;
                        l_362[1] ^= ((((*l_361) = ((((**p_91) & ((uint16_t)((p_92 != l_352[1][8][1]) ^ p_92) * (uint16_t)(g_349 = ((246UL != ((uint16_t)((*l_276)++) >> (uint16_t)p_92)) >= ((((uint16_t)(2UL == ((((uint16_t)(*l_266) - (uint16_t)p_92) >= g_159[2]) , 0x2596L)) % (uint16_t)g_181) || p_92) || 0x5254C33BL))))) & p_92) , (*l_266))) < 0x9B9AL) ^ 0xB0A3849F3B21594CLL);
                        if ((**p_91))
                            continue;
                    }
                    for (l_267 = 5; (l_267 >= 0); l_267 -= 1)
                    {
                        uint64_t ***l_365 = &g_363;
                        int32_t l_366 = (-1L);
                        int8_t ****l_385 = &l_289[g_60];
                        (*l_365) = g_363;
                        if (l_366)
                            break;
                        l_319[2] = ((uint32_t)(0x33B18722L >= (((uint16_t)(0xCDB404E5L & ((+(*l_266)) < (p_92 && ((((((l_365 != l_372) < (((int8_t)((-9L) > ((((uint64_t)((int16_t)((int64_t)((int8_t)(((*l_385) = (void*)0) != &g_44) % (int8_t)(-1L)) + (int64_t)l_362[0]) << (int16_t)p_92) + (uint64_t)p_92) && 1L) || (-10L))) % (int8_t)l_386) | p_92)) , 0x10L) ^ p_92) || p_92) != (-9L))))) / (uint16_t)g_349) & l_318)) + (uint32_t)(*g_104));
                        if ((*g_265))
                            break;
                    }
                }
                l_389[3] = g_387;
            }
        }
    }
    g_392 = (void*)0;
    return p_92;
}







static const int32_t ** func_93(int32_t p_94, int32_t p_95, int8_t *** p_96, union U1 p_97, int8_t * p_98)
{
    int32_t *l_115 = (void*)0;
    int8_t l_157 = 2L;
    int32_t l_202 = (-5L);
    int32_t l_203 = 0x693BE22EL;
    int32_t l_207 = 3L;
    int32_t l_208 = 9L;
    int32_t l_210[5] = {(-5L),(-5L),(-5L),(-5L),(-5L)};
    const int32_t **l_225 = &g_112;
    int32_t *l_227 = &g_159[1];
    int32_t *l_228 = &l_207;
    int32_t *l_229 = &g_60;
    int32_t *l_230 = &g_120;
    int32_t *l_231 = &l_202;
    int32_t *l_232 = &l_202;
    int32_t *l_233 = &l_208;
    int32_t *l_234[3];
    int32_t l_235 = (-6L);
    int32_t l_236[6] = {(-2L),0x51D6868DL,0x51D6868DL,(-2L),0x51D6868DL,0x51D6868DL};
    uint32_t l_237 = 0x163A2F53L;
    int i;
    for (i = 0; i < 3; i++)
        l_234[i] = &l_207;
    for (g_60 = 25; (g_60 >= (-18)); --g_60)
    {
        int64_t l_141 = 0L;
        uint32_t l_154 = 4294967295UL;
        const int8_t * const **l_169[3];
        int32_t l_194 = 0xE29D7D8EL;
        int32_t l_196 = (-7L);
        int32_t l_201 = 1L;
        uint64_t *l_218[2];
        uint16_t **l_219 = (void*)0;
        int32_t *l_220 = (void*)0;
        int32_t *l_221 = &g_222;
        int i;
        for (i = 0; i < 3; i++)
            l_169[i] = &g_167;
        for (i = 0; i < 2; i++)
            l_218[i] = &g_193;
        for (p_95 = 7; (p_95 >= 1); p_95 -= 1)
        {
            const int32_t *l_110 = &g_60;
            const int32_t **l_109 = &l_110;
            int8_t *l_113 = &g_2;
            int32_t l_156 = (-1L);
            int32_t l_205 = 0xC63F9E7CL;
            int32_t l_211 = 0x3211C853L;
            int32_t l_212 = (-7L);
            for (p_94 = 1; (p_94 <= 7); p_94 += 1)
            {
                (*g_105) = &g_4;
            }
            if ((+((void*)0 == &g_104)))
            {
                const int32_t *l_108 = &g_60;
                const int32_t **l_107[5][7][7] = {{{&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,(void*)0,(void*)0,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108,(void*)0},{(void*)0,&l_108,&l_108,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108},{&l_108,&l_108,(void*)0,(void*)0,&l_108,&l_108,&l_108},{(void*)0,&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108}},{{&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108,&l_108},{&l_108,(void*)0,&l_108,&l_108,(void*)0,&l_108,&l_108},{&l_108,(void*)0,&l_108,&l_108,(void*)0,(void*)0,&l_108},{&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108},{&l_108,(void*)0,&l_108,&l_108,(void*)0,&l_108,&l_108},{&l_108,(void*)0,&l_108,(void*)0,&l_108,&l_108,(void*)0},{&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108,&l_108}},{{(void*)0,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108},{(void*)0,&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108},{&l_108,&l_108,(void*)0,&l_108,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,&l_108,&l_108,&l_108},{(void*)0,&l_108,&l_108,(void*)0,&l_108,(void*)0,&l_108},{(void*)0,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108}},{{&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108},{&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,(void*)0,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108}},{{(void*)0,(void*)0,&l_108,(void*)0,&l_108,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108},{&l_108,&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108},{(void*)0,&l_108,&l_108,&l_108,(void*)0,(void*)0,&l_108},{&l_108,&l_108,(void*)0,&l_108,&l_108,&l_108,&l_108},{&l_108,&l_108,(void*)0,(void*)0,(void*)0,&l_108,&l_108},{&l_108,&l_108,&l_108,&l_108,(void*)0,&l_108,(void*)0}}};
                int i, j, k;
                return g_111;
            }
            else
            {
                union U1 l_114 = {0};
                int32_t **l_116 = &g_104;
                int32_t *l_118 = &g_60;
                int32_t **l_117 = &l_118;
                (*g_111) = ((*l_117) = ((*l_116) = (l_115 = func_55(l_113, l_114))));
            }
            for (p_94 = 7; (p_94 >= 0); p_94 -= 1)
            {
                int32_t l_182 = 0x7F110A1BL;
                uint16_t *l_183 = &g_161;
                int32_t l_185[1][3];
                int64_t *l_189 = &l_141;
                const int32_t **l_217 = &g_112;
                int i, j;
                for (i = 0; i < 1; i++)
                {
                    for (j = 0; j < 3; j++)
                        l_185[i][j] = 1L;
                }
                for (g_120 = 0; (g_120 <= 7); g_120 += 1)
                {
                    uint8_t *l_142 = &g_143;
                    int32_t *l_155 = (void*)0;
                    int32_t *l_158 = &g_159[1];
                    uint16_t *l_160[4][4] = {{&g_161,&g_161,&g_161,&g_161},{&g_161,&g_161,&g_161,&g_161},{&g_161,&g_161,&g_161,&g_161},{&g_161,&g_161,&g_161,&g_161}};
                    int8_t l_162 = 0x6DL;
                    uint32_t *l_163 = (void*)0;
                    uint32_t *l_164 = &g_165;
                    int16_t l_186 = 0xF434L;
                    int32_t l_204 = 0xACA09A69L;
                    int32_t l_209[3];
                    int i, j;
                    for (i = 0; i < 3; i++)
                        l_209[i] = 2L;
                    if (((*l_110) , ((uint16_t)((int16_t)(((g_41 , g_72[p_94]) != (l_169[1] = (((*l_164) = ((int16_t)((uint16_t)(((**l_109) && ((uint64_t)(((int16_t)((uint16_t)(p_97.f2 = (((uint64_t)((uint8_t)(((void*)0 != g_139) >= ((*l_158) = (l_141 != ((l_141 >= ((((((++(*l_142)) , ((uint64_t)(((int32_t)(l_156 = ((int32_t)((uint16_t)(((*l_109) == (*l_109)) | (*l_115)) * (uint16_t)l_154) % (int32_t)p_94)) - (int32_t)p_95) < 0x6FF6L) - (uint64_t)l_141)) <= (*l_110)) , 0x05L) && l_157) != 0x6517A65E2B664D79LL)) , 0x186A86B27BD0F622LL)))) / (uint8_t)l_154) - (uint64_t)p_94) == p_95)) << (uint16_t)8) / (int16_t)0xDBA4L) , l_162) + (uint64_t)0UL)) && p_95) - (uint16_t)9L) * (int16_t)0xA52EL)) , g_166))) , g_143) - (int16_t)g_120) >> (uint16_t)(*l_115))))
                    {
                        int16_t *l_180[8][10][3] = {{{(void*)0,&g_17[2],&g_17[2]},{&g_17[2],&g_17[3],&g_181},{&g_181,&g_17[2],(void*)0},{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],&g_17[3],&g_17[2]},{(void*)0,&g_181,(void*)0},{&g_17[3],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,(void*)0},{&g_17[3],&g_17[0],&g_17[2]},{(void*)0,&g_17[0],&g_17[0]}},{{&g_17[2],&g_17[2],(void*)0},{&g_17[2],&g_17[2],&g_181},{&g_181,&g_17[2],&g_17[2]},{&g_17[2],&g_17[2],(void*)0},{(void*)0,&g_17[2],&g_17[2]},{&g_17[3],&g_17[2],(void*)0},{&g_17[2],&g_17[2],&g_17[2]},{(void*)0,&g_17[0],&g_181},{&g_17[2],&g_17[0],&g_181},{&g_181,(void*)0,&g_17[3]}},{{&g_181,&g_17[2],&g_181},{&g_17[2],&g_181,&g_181},{&g_17[2],&g_17[3],&g_17[2]},{&g_17[2],&g_17[2],(void*)0},{&g_17[2],(void*)0,&g_181},{(void*)0,&g_17[2],&g_17[2]},{(void*)0,&g_181,(void*)0},{(void*)0,&g_181,&g_17[3]},{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],&g_17[2],(void*)0}},{{&g_17[2],(void*)0,&g_17[2]},{(void*)0,&g_17[2],&g_181},{&g_17[2],&g_181,&g_181},{(void*)0,&g_17[2],&g_17[2]},{(void*)0,(void*)0,&g_17[2]},{&g_17[2],&g_17[2],&g_17[2]},{&g_181,&g_17[2],&g_17[2]},{&g_17[2],&g_181,&g_17[2]},{&g_17[2],&g_181,(void*)0},{&g_17[1],&g_17[2],&g_17[2]}},{{&g_17[2],(void*)0,&g_17[2]},{&g_17[2],(void*)0,&g_17[2]},{&g_17[0],&g_17[2],&g_17[2]},{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,&g_181},{&g_17[0],&g_17[2],&g_181},{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,(void*)0},{&g_17[0],&g_181,&g_17[2]},{&g_17[2],&g_17[0],&g_17[3]}},{{&g_17[2],&g_17[2],(void*)0},{&g_17[1],&g_181,&g_17[2]},{&g_17[2],&g_17[2],&g_181},{&g_17[2],&g_17[0],&g_17[2]},{&g_181,&g_181,&g_17[2]},{&g_17[2],(void*)0,(void*)0},{(void*)0,&g_17[2],&g_17[3]},{(void*)0,&g_17[2],&g_17[2]},{&g_17[2],(void*)0,&g_17[3]},{(void*)0,&g_17[2],(void*)0}},{{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,&g_17[2]},{&g_17[2],(void*)0,&g_181},{(void*)0,&g_17[2],&g_17[2]},{(void*)0,&g_181,(void*)0},{(void*)0,&g_181,&g_17[3]},{&g_17[2],&g_17[2],&g_17[2]},{&g_17[2],&g_17[2],(void*)0},{&g_17[2],(void*)0,&g_17[2]},{(void*)0,&g_17[2],&g_181}},{{&g_17[2],&g_181,&g_181},{(void*)0,&g_17[2],&g_17[2]},{(void*)0,(void*)0,&g_17[2]},{&g_17[2],&g_17[2],&g_17[2]},{&g_181,&g_17[2],&g_17[2]},{&g_17[2],&g_181,&g_17[2]},{&g_17[2],&g_181,(void*)0},{&g_17[1],&g_17[2],&g_17[2]},{&g_17[2],(void*)0,&g_17[2]},{&g_17[2],(void*)0,&g_17[2]}}};
                        uint16_t **l_184 = &l_160[0][1];
                        uint64_t *l_190 = &g_191;
                        uint64_t *l_192 = &g_193;
                        int32_t *l_195 = &g_159[1];
                        int32_t *l_197 = &l_196;
                        int32_t *l_198 = &l_196;
                        int32_t *l_199 = &l_185[0][0];
                        int32_t *l_200[2][7][10] = {{{&g_60,&l_194,&l_196,&l_194,&g_4,&l_182,&g_60,&g_159[1],&l_182,&g_60},{&l_182,&g_60,&g_159[1],&l_182,&g_60,(void*)0,&l_196,&l_182,&g_120,&g_159[1]},{&g_120,&l_185[0][0],&l_156,(void*)0,&g_4,&l_185[0][2],&l_185[0][0],&g_4,&g_159[0],&g_60},{&l_185[0][0],&l_182,&l_196,&g_120,&l_182,&l_185[0][2],&g_4,(void*)0,&l_182,&g_4},{(void*)0,(void*)0,(void*)0,&l_182,&l_182,&l_182,(void*)0,(void*)0,(void*)0,&l_196},{&g_120,(void*)0,&l_185[0][2],&g_60,&l_185[0][0],&l_185[0][0],&l_196,&g_120,&l_185[0][0],&g_4},{(void*)0,&g_60,&l_185[0][0],&g_60,&g_60,(void*)0,&g_60,&g_60,(void*)0,(void*)0}},{{&g_60,&g_120,(void*)0,&l_182,&g_4,&l_156,(void*)0,&l_185[0][0],&l_182,(void*)0},{&l_185[0][0],&l_185[0][0],&g_159[0],&g_120,&g_4,&l_185[0][0],(void*)0,&g_159[1],&g_159[0],(void*)0},{&g_4,(void*)0,&g_159[1],&l_196,&g_60,&l_194,(void*)0,(void*)0,&l_185[0][0],&g_159[1]},{(void*)0,&l_185[0][0],&l_196,&l_156,(void*)0,&g_159[1],&g_159[1],(void*)0,&l_156,&l_196},{(void*)0,(void*)0,&g_120,(void*)0,&l_182,&l_185[0][0],&g_4,&g_4,&g_4,&g_4},{&g_120,&l_156,&l_194,&g_4,&l_182,&l_156,&g_4,&l_156,&l_156,&g_4},{&g_60,(void*)0,(void*)0,&l_185[0][2],&g_4,&g_60,&g_159[1],(void*)0,(void*)0,&g_60}}};
                        int16_t l_206 = 1L;
                        uint64_t l_213 = 18446744073709551615UL;
                        int i, j, k;
                        (*l_158) = ((g_4 >= (-1L)) ^ ((int8_t)((((p_95 , (l_154 , (((*l_183) = (((int16_t)(((((!((!(*l_115)) >= ((uint32_t)((int16_t)(l_182 = (p_98 != &g_143)) / (int16_t)(func_68((((((-1L) == (((*l_184) = l_183) == &g_161)) && l_185[0][0]) ^ 0UL) ^ (*g_139))) , p_94)) % (uint32_t)(*g_112)))) == 1UL) >= 0x2EL) > g_17[0]) <= 0xC988BF1FL) >> (int16_t)4) <= g_120)) < (*l_110)))) & p_95) || 1L) , (*l_115)) << (int8_t)p_94));
                        (*l_158) ^= (l_186 ^ ((0x686DD1E6761AD414LL >= l_154) < ((*l_192) = ((int8_t)(l_156 = (*l_115)) << (int8_t)((l_189 != &g_140) , (((*l_190) = (p_98 != p_98)) ^ 0x4AAA90A817FA558ALL))))));
                        --l_213;
                    }
                    else
                    {
                        int8_t l_216[10][8][2];
                        int i, j, k;
                        for (i = 0; i < 10; i++)
                        {
                            for (j = 0; j < 8; j++)
                            {
                                for (k = 0; k < 2; k++)
                                    l_216[i][j][k] = 4L;
                            }
                        }
                        if (l_216[9][6][1])
                            break;
                        return l_217;
                    }
                }
                for (l_196 = 0; l_196 < 8; l_196 += 1)
                {
                    g_72[l_196] = &g_73;
                }
            }
        }
        p_95 = (((((void*)0 == &l_203) , p_95) , (((*l_221) = (((l_154 , (((g_120 & (((*g_168) >= (((l_196 = 0xB3468687B3F213AFLL) || ((4294967294UL | l_196) , l_154)) , g_143)) || 0x9C7174ADL)) , l_219) != l_219)) || p_94) == 0xA0L)) , 0xD8L)) ^ l_154);
    }
    for (l_208 = 13; (l_208 != (-3)); l_208 -= 8)
    {
        return l_225;
    }
    ++l_237;
    return l_225;
}





int main (int argc, char* argv[])
{
    int i, j, k;
    int print_hash_value = 0;
    if (argc == 2 && strcmp(argv[1], "1") == 0) print_hash_value = 1;
    platform_main_begin();
    crc32_gentab();
    func_1();
    transparent_crc(g_2, "g_2", print_hash_value);
    transparent_crc(g_4, "g_4", print_hash_value);
    for (i = 0; i < 4; i++)
    {
        transparent_crc(g_17[i], "g_17[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_60, "g_60", print_hash_value);
    transparent_crc(g_86, "g_86", print_hash_value);
    transparent_crc(g_89, "g_89", print_hash_value);
    transparent_crc(g_119, "g_119", print_hash_value);
    transparent_crc(g_120, "g_120", print_hash_value);
    transparent_crc(g_140, "g_140", print_hash_value);
    transparent_crc(g_143, "g_143", print_hash_value);
    for (i = 0; i < 3; i++)
    {
        transparent_crc(g_159[i], "g_159[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_161, "g_161", print_hash_value);
    transparent_crc(g_165, "g_165", print_hash_value);
    transparent_crc(g_181, "g_181", print_hash_value);
    transparent_crc(g_191, "g_191", print_hash_value);
    transparent_crc(g_193, "g_193", print_hash_value);
    transparent_crc(g_222, "g_222", print_hash_value);
    transparent_crc(g_226, "g_226", print_hash_value);
    transparent_crc(g_277, "g_277", print_hash_value);
    transparent_crc(g_279.f0, "g_279.f0", print_hash_value);
    transparent_crc(g_279.f1, "g_279.f1", print_hash_value);
    transparent_crc(g_279.f2, "g_279.f2", print_hash_value);
    transparent_crc(g_279.f3, "g_279.f3", print_hash_value);
    transparent_crc(g_296, "g_296", print_hash_value);
    transparent_crc(g_324, "g_324", print_hash_value);
    transparent_crc(g_349, "g_349", print_hash_value);
    for (i = 0; i < 6; i++)
    {
        for (j = 0; j < 2; j++)
        {
            transparent_crc(g_391[i][j], "g_391[i][j]", print_hash_value);
            if (print_hash_value) printf("index = [%d][%d]\n", i, j);

        }
    }
    transparent_crc(g_439, "g_439", print_hash_value);
    for (i = 0; i < 10; i++)
    {
        transparent_crc(g_478[i], "g_478[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_502, "g_502", print_hash_value);
    transparent_crc(g_515, "g_515", print_hash_value);
    transparent_crc(g_590.f0, "g_590.f0", print_hash_value);
    transparent_crc(g_590.f1, "g_590.f1", print_hash_value);
    transparent_crc(g_590.f2, "g_590.f2", print_hash_value);
    transparent_crc(g_590.f3, "g_590.f3", print_hash_value);
    for (i = 0; i < 8; i++)
    {
        for (j = 0; j < 6; j++)
        {
            transparent_crc(g_669[i][j], "g_669[i][j]", print_hash_value);
            if (print_hash_value) printf("index = [%d][%d]\n", i, j);

        }
    }
    transparent_crc(g_762, "g_762", print_hash_value);
    transparent_crc(g_769, "g_769", print_hash_value);
    for (i = 0; i < 8; i++)
    {
        transparent_crc(g_800[i], "g_800[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_834, "g_834", print_hash_value);
    transparent_crc(g_865.f0, "g_865.f0", print_hash_value);
    transparent_crc(g_865.f1, "g_865.f1", print_hash_value);
    transparent_crc(g_865.f2, "g_865.f2", print_hash_value);
    transparent_crc(g_865.f3, "g_865.f3", print_hash_value);
    for (i = 0; i < 3; i++)
    {
        transparent_crc(g_886[i], "g_886[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_991, "g_991", print_hash_value);
    transparent_crc(g_1054.f0, "g_1054.f0", print_hash_value);
    transparent_crc(g_1054.f1, "g_1054.f1", print_hash_value);
    transparent_crc(g_1054.f2, "g_1054.f2", print_hash_value);
    transparent_crc(g_1054.f3, "g_1054.f3", print_hash_value);
    transparent_crc(g_1160.f0, "g_1160.f0", print_hash_value);
    transparent_crc(g_1160.f1, "g_1160.f1", print_hash_value);
    transparent_crc(g_1160.f2, "g_1160.f2", print_hash_value);
    transparent_crc(g_1160.f3, "g_1160.f3", print_hash_value);
    transparent_crc(g_1264, "g_1264", print_hash_value);
    for (i = 0; i < 4; i++)
    {
        transparent_crc(g_1420[i], "g_1420[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    for (i = 0; i < 3; i++)
    {
        transparent_crc(g_1451[i], "g_1451[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_1460.f0, "g_1460.f0", print_hash_value);
    transparent_crc(g_1460.f1, "g_1460.f1", print_hash_value);
    transparent_crc(g_1460.f2, "g_1460.f2", print_hash_value);
    transparent_crc(g_1460.f3, "g_1460.f3", print_hash_value);
    transparent_crc(g_1514.f0, "g_1514.f0", print_hash_value);
    transparent_crc(g_1514.f1, "g_1514.f1", print_hash_value);
    transparent_crc(g_1514.f2, "g_1514.f2", print_hash_value);
    transparent_crc(g_1514.f3, "g_1514.f3", print_hash_value);
    transparent_crc(g_1583, "g_1583", print_hash_value);
    transparent_crc(g_1638.f0, "g_1638.f0", print_hash_value);
    transparent_crc(g_1638.f1, "g_1638.f1", print_hash_value);
    transparent_crc(g_1638.f2, "g_1638.f2", print_hash_value);
    transparent_crc(g_1638.f3, "g_1638.f3", print_hash_value);
    transparent_crc(g_1736, "g_1736", print_hash_value);
    transparent_crc(g_1804.f0, "g_1804.f0", print_hash_value);
    transparent_crc(g_1804.f1, "g_1804.f1", print_hash_value);
    transparent_crc(g_1804.f2, "g_1804.f2", print_hash_value);
    transparent_crc(g_1804.f3, "g_1804.f3", print_hash_value);
    transparent_crc(g_1854, "g_1854", print_hash_value);
    transparent_crc(g_1874.f0, "g_1874.f0", print_hash_value);
    transparent_crc(g_1874.f1, "g_1874.f1", print_hash_value);
    transparent_crc(g_1874.f2, "g_1874.f2", print_hash_value);
    transparent_crc(g_1874.f3, "g_1874.f3", print_hash_value);
    transparent_crc(g_1930, "g_1930", print_hash_value);
    for (i = 0; i < 1; i++)
    {
        transparent_crc(g_2035[i].f0, "g_2035[i].f0", print_hash_value);
        transparent_crc(g_2035[i].f1, "g_2035[i].f1", print_hash_value);
        transparent_crc(g_2035[i].f2, "g_2035[i].f2", print_hash_value);
        transparent_crc(g_2035[i].f3, "g_2035[i].f3", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_2180.f0, "g_2180.f0", print_hash_value);
    transparent_crc(g_2180.f1, "g_2180.f1", print_hash_value);
    transparent_crc(g_2180.f2, "g_2180.f2", print_hash_value);
    transparent_crc(g_2180.f3, "g_2180.f3", print_hash_value);
    for (i = 0; i < 5; i++)
    {
        for (j = 0; j < 1; j++)
        {
            for (k = 0; k < 10; k++)
            {
                transparent_crc(g_2225[i][j][k].f0, "g_2225[i][j][k].f0", print_hash_value);
                transparent_crc(g_2225[i][j][k].f1, "g_2225[i][j][k].f1", print_hash_value);
                transparent_crc(g_2225[i][j][k].f2, "g_2225[i][j][k].f2", print_hash_value);
                transparent_crc(g_2225[i][j][k].f3, "g_2225[i][j][k].f3", print_hash_value);
                if (print_hash_value) printf("index = [%d][%d][%d]\n", i, j, k);

            }
        }
    }
    transparent_crc(g_2273.f0, "g_2273.f0", print_hash_value);
    transparent_crc(g_2273.f1, "g_2273.f1", print_hash_value);
    transparent_crc(g_2273.f2, "g_2273.f2", print_hash_value);
    transparent_crc(g_2273.f3, "g_2273.f3", print_hash_value);
    transparent_crc(g_2352.f0, "g_2352.f0", print_hash_value);
    transparent_crc(g_2352.f1, "g_2352.f1", print_hash_value);
    transparent_crc(g_2352.f2, "g_2352.f2", print_hash_value);
    transparent_crc(g_2352.f3, "g_2352.f3", print_hash_value);
    transparent_crc(g_2377.f0, "g_2377.f0", print_hash_value);
    transparent_crc(g_2377.f1, "g_2377.f1", print_hash_value);
    transparent_crc(g_2377.f2, "g_2377.f2", print_hash_value);
    transparent_crc(g_2377.f3, "g_2377.f3", print_hash_value);
    for (i = 0; i < 8; i++)
    {
        transparent_crc(g_2416[i], "g_2416[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_2490, "g_2490", print_hash_value);
    transparent_crc(g_2593, "g_2593", print_hash_value);
    transparent_crc(g_2597, "g_2597", print_hash_value);
    transparent_crc(g_2609, "g_2609", print_hash_value);
    for (i = 0; i < 9; i++)
    {
        transparent_crc(g_2623[i], "g_2623[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_2659, "g_2659", print_hash_value);
    transparent_crc(g_2708, "g_2708", print_hash_value);
    transparent_crc(g_2723, "g_2723", print_hash_value);
    transparent_crc(g_2773.f0, "g_2773.f0", print_hash_value);
    transparent_crc(g_2773.f1, "g_2773.f1", print_hash_value);
    transparent_crc(g_2773.f2, "g_2773.f2", print_hash_value);
    transparent_crc(g_2773.f3, "g_2773.f3", print_hash_value);
    transparent_crc(g_2808, "g_2808", print_hash_value);
    transparent_crc(g_2843, "g_2843", print_hash_value);
    for (i = 0; i < 6; i++)
    {
        for (j = 0; j < 8; j++)
        {
            transparent_crc(g_2844[i][j], "g_2844[i][j]", print_hash_value);
            if (print_hash_value) printf("index = [%d][%d]\n", i, j);

        }
    }
    for (i = 0; i < 7; i++)
    {
        transparent_crc(g_2918[i], "g_2918[i]", print_hash_value);
        if (print_hash_value) printf("index = [%d]\n", i);

    }
    transparent_crc(g_2941, "g_2941", print_hash_value);
    transparent_crc(g_2969, "g_2969", print_hash_value);
    transparent_crc(g_3049.f0, "g_3049.f0", print_hash_value);
    transparent_crc(g_3049.f1, "g_3049.f1", print_hash_value);
    transparent_crc(g_3049.f2, "g_3049.f2", print_hash_value);
    transparent_crc(g_3049.f3, "g_3049.f3", print_hash_value);
    transparent_crc(g_3104, "g_3104", print_hash_value);
    transparent_crc(g_3112, "g_3112", print_hash_value);
    transparent_crc(g_3138, "g_3138", print_hash_value);
    transparent_crc(g_3182, "g_3182", print_hash_value);
    transparent_crc(g_3213, "g_3213", print_hash_value);
    platform_main_end(crc32_context ^ 0xFFFFFFFFUL, print_hash_value);
    return 0;
}
