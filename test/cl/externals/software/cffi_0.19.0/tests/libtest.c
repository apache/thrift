/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * libtest.c --- auxiliary C lib for testing purposes
 *
 * Copyright (C) 2005-2007, Luis Oliveira  <loliveira(@)common-lisp.net>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <stdbool.h>

/* MSVC doesn't have stdint.h and uses a different syntax for stdcall */
#ifndef _MSC_VER
#include <stdint.h>
#endif

#ifdef WIN32
#ifdef _MSC_VER
#define STDCALL __stdcall
#else
#define STDCALL __attribute__((stdcall))
#endif
#else
#define STDCALL
#endif

/*
 * Some functions that aren't available on WIN32
 */

DLLEXPORT
float my_sqrtf(float n)
{
    return (float) sqrt((double) n);
}

DLLEXPORT
char *my_strdup(const char *str)
{
    char *p = malloc(strlen(str) + 1);
    strcpy(p, str);
    return p;
}

DLLEXPORT
void my_strfree(char *str)
{
    free(str);
}

DLLEXPORT
long long my_llabs(long long n)
{
    return n < 0 ? -n : n;
}


DLLEXPORT
unsigned long long ullong(unsigned long long n)
{
    return n == ULLONG_MAX ? n : 42;
}

/*
 * Foreign Globals
 *
 * (var_int is used in MISC-TYPES.EXPAND.3 as well)
 */

DLLEXPORT char *         dll_version        = "20120107";

/* TODO: look into signed char vs. unsigned char issue */
DLLEXPORT char           var_char           = -127;
DLLEXPORT unsigned char  var_unsigned_char  = 255;
DLLEXPORT short          var_short          = -32767;
DLLEXPORT unsigned short var_unsigned_short = 65535;
DLLEXPORT int            var_int            = -32767;
DLLEXPORT unsigned int   var_unsigned_int   = 65535;
DLLEXPORT long           var_long           = -2147483647L;
DLLEXPORT unsigned long  var_unsigned_long  = 4294967295UL;
DLLEXPORT float          var_float          = 42.0f;
DLLEXPORT double         var_double         = 42.0;
DLLEXPORT void *         var_pointer        = NULL;
DLLEXPORT char *         var_string         = "Hello, foreign world!";

DLLEXPORT long long          var_long_long          = -9223372036854775807LL;
DLLEXPORT unsigned long long var_unsigned_long_long = 18446744073709551615ULL;

DLLEXPORT float float_max = FLT_MAX;
DLLEXPORT float float_min = FLT_MIN;
DLLEXPORT double double_max = DBL_MAX;
DLLEXPORT double double_min = DBL_MIN;

/*
 * Callbacks
 */

DLLEXPORT
int expect_char_sum(char (*f)(char, char))
{
    return f('a', 3) == 'd';
}

DLLEXPORT
int expect_unsigned_char_sum(unsigned char (*f)(unsigned char, unsigned char))
{
    return f(UCHAR_MAX-1, 1) == UCHAR_MAX;
}

DLLEXPORT
int expect_short_sum(short (*f)(short a, short b))
{
    return f(SHRT_MIN+1, -1) == SHRT_MIN;
}

DLLEXPORT
int expect_unsigned_short_sum(unsigned short (*f)(unsigned short,
                                                  unsigned short))
{
    return f(USHRT_MAX-1, 1) == USHRT_MAX;
}

/* used in MISC-TYPES.EXPAND.4 as well */
DLLEXPORT
int expect_int_sum(int (*f)(int, int))
{
    return f(INT_MIN+1, -1) == INT_MIN;
}

DLLEXPORT
int expect_unsigned_int_sum(unsigned int (*f)(unsigned int, unsigned int))
{
    return f(UINT_MAX-1, 1) == UINT_MAX;
}

DLLEXPORT
int expect_long_sum(long (*f)(long, long))
{
    return f(LONG_MIN+1, -1) == LONG_MIN;
}

DLLEXPORT
int expect_unsigned_long_sum(unsigned long (*f)(unsigned long, unsigned long))
{
    return f(ULONG_MAX-1, 1) == ULONG_MAX;
}

DLLEXPORT
int expect_long_long_sum(long long (*f)(long long, long long))
{
    return f(LLONG_MIN+1, -1) == LLONG_MIN;
}

DLLEXPORT
int expect_unsigned_long_long_sum (unsigned long long
                                   (*f)(unsigned long long, unsigned long long))
{
    return f(ULLONG_MAX-1, 1) == ULLONG_MAX;
}

DLLEXPORT
int expect_float_sum(float (*f)(float, float))
{
    /*printf("\n>>> FLOAT: %f <<<\n", f(20.0f, 22.0f));*/
    return f(20.0f, 22.0f) == 42.0f;
}

DLLEXPORT
int expect_double_sum(double (*f)(double, double))
{
    /*printf("\n>>> DOUBLE: %f<<<\n", f(-20.0, -22.0));*/
    return f(-20.0, -22.0) == -42.0;
}

DLLEXPORT
int expect_long_double_sum(long double (*f)(long double, long double))
{
    /*printf("\n>>> DOUBLE: %f<<<\n", f(-20.0, -22.0));*/
    return f(-20.0, -22.0) == -42.0;
}

DLLEXPORT
int expect_pointer_sum(void* (*f)(void*, int))
{
    return f(NULL, 0xDEAD) == (void *) 0xDEAD;
}

DLLEXPORT
int expect_strcat(char* (*f)(char*, char*))
{
    char *ret = f("Hello, ", "C world!");
    int res = strcmp(ret, "Hello, C world!") == 0;
    /* commented out as a quick fix on platforms that don't
       foreign allocate in C malloc space. */
    /*free(ret);*/ /* is this allowed? */
    return res;
}

DLLEXPORT
void pass_int_ref(void (*f)(int*))
{
    int x = 1984;
    f(&x);
}

/*
 * Enums
 */

typedef enum {
    ONE = 1,
    TWO,
    THREE,
    FOUR,
    FORTY_ONE = 41,
    FORTY_TWO
} numeros;

DLLEXPORT
int check_enums(numeros one, numeros two, numeros three, numeros four,
                numeros forty_one, numeros forty_two)
{
    if (one == ONE && two == TWO && three == THREE && four == FOUR &&
        forty_one == FORTY_ONE && forty_two == FORTY_TWO)
        return 1;

    return 0;
}

typedef enum { FALSE, TRUE } another_boolean;

DLLEXPORT
another_boolean return_enum(int x)
{
    if (x == 0)
        return FALSE;
    else
        return TRUE;
}

/*
 * Booleans
 */

DLLEXPORT
int equalequal(int a, unsigned int b)
{
    return ((unsigned int) a) == b;
}

DLLEXPORT
char bool_and(unsigned char a, char b)
{
    return a && b;
}

DLLEXPORT
unsigned long bool_xor(long a, unsigned long b)
{
    return (a && !b) || (!a && b);
}

DLLEXPORT
unsigned sizeof_bool(void)
{
    return (unsigned) sizeof(_Bool);
}

DLLEXPORT
unsigned bool_to_unsigned(_Bool b)
{
    return (unsigned) b;
}

DLLEXPORT
_Bool unsigned_to_bool(unsigned u)
{
    return (_Bool) u;
}

/*
 * Test struct alignment issues. These comments assume the x86 gABI.
 * Hopefully these tests will spot alignment issues in others archs
 * too.
 */

/*
 * STRUCT.ALIGNMENT.1
 */

struct s_ch {
    char a_char;
};

/* This struct's size should be 2 bytes */
struct s_s_ch {
    char another_char;
    struct s_ch a_s_ch;
};

DLLEXPORT
struct s_s_ch the_s_s_ch = { 2, { 1 } };

/*
 * STRUCT.ALIGNMENT.2
 */

/* This one should be alignment should be the same as short's alignment. */
struct s_short {
    char a_char;
    char another_char;
    short a_short;
};

struct s_s_short {
    char yet_another_char;
    struct s_short a_s_short; /* so this should be 2-byte aligned */
};  /* size: 6 bytes */

DLLEXPORT
struct s_s_short the_s_s_short = { 4, { 1, 2, 3 } };

/*
 * STRUCT.ALIGNMENT.3
 */

/* This test will, among other things, check for the existence tail padding. */

struct s_double {
    char a_char;       /* 1 byte */
                       /* padding: 3 bytes */
    double a_double;   /* 8 bytes */
    char another_char; /* 1 byte */
                       /* padding: 3 bytes */
};                     /* total size: 16 bytes */

struct s_s_double {
    char yet_another_char;      /* 1 byte */
                                /* 3 bytes padding */
    struct s_double a_s_double; /* 16 bytes */
    short a_short;              /* 2 byte */
                                /* 2 bytes padding */
};                              /* total size: 24 bytes */

DLLEXPORT
struct s_s_double the_s_s_double = { 4, { 1, 2.0, 3 }, 5 };

/*
 * STRUCT.ALIGNMENT.4
 */
struct s_s_s_double {
    short another_short;            /* 2 bytes */
                                    /* 2 bytes padding */
    struct s_s_double a_s_s_double; /* 24 bytes */
    char last_char;                 /* 1 byte */
                                    /* 3 bytes padding */
};                                  /* total size: 32 */

DLLEXPORT
struct s_s_s_double the_s_s_s_double = { 6, { 4, { 1, 2.0, 3 }, 5 }, 7 };

/*
 * STRUCT.ALIGNMENT.5
 */

/* MacOSX ABI says: "The embedding alignment of the first element in a data
   structure is equal to the element's natural alignment." and "For subsequent
   elements that have a natural alignment greater than 4 bytes, the embedding
   alignment is 4, unless the element is a vector." */

/* note: these rules will apply to the structure itself. So, unless it is
   the first element of another structure, its alignment will be 4. */

/* the following offsets and sizes are specific to darwin/ppc32 */

struct s_double2 {
    double a_double;            /* 8 bytes (alignment 8) */
    short a_short;              /* 2 bytes */
                                /* 6 bytes padding */
};                              /* total size: 16 */

struct s_s_double2 {
    char a_char;                  /* 1 byte */
                                  /* 3 bytes padding */
    struct s_double2 a_s_double2; /* 16 bytes, alignment 4 */
    short another_short;          /* 2 bytes */
                                  /* 2 bytes padding */
};                                /* total size: 24 bytes */
                                  /* alignment: 4 */

DLLEXPORT
struct s_s_double2 the_s_s_double2 = { 3, { 1.0, 2 }, 4 };

/*
 * STRUCT.ALIGNMENT.6
 */

/* Same as STRUCT.ALIGNMENT.5 but with long long. */

struct s_long_long {
    long long a_long_long;      /* 8 bytes (alignment 8) */
    short a_short;              /* 2 bytes */
                                /* 6 bytes padding */
};                              /* total size: 16 */

struct s_s_long_long {
    char a_char;                      /* 1 byte */
                                      /* 3 bytes padding */
    struct s_long_long a_s_long_long; /* 16 bytes, alignment 4 */
    short a_short;                    /* 2 bytes */
                                      /* 2 bytes padding */
};                                    /* total size: 24 bytes */
                                      /* alignment: 4 */

DLLEXPORT
struct s_s_long_long the_s_s_long_long = { 3, { 1, 2 }, 4 };

/*
 * STRUCT.ALIGNMENT.7
 */

/* Another test for Darwin's PPC32 ABI. */

struct s_s_double3 {
    struct s_double2 a_s_double2; /* 16 bytes, alignment 8*/
    short another_short;          /* 2 bytes */
                                  /* 6 bytes padding */
};                                /* total size: 24 */

struct s_s_s_double3 {
    struct s_s_double3 a_s_s_double3; /* 24 bytes */
    char a_char;                      /* 1 byte */
                                      /* 7 bytes padding */
};                                    /* total size: 32 */

DLLEXPORT
struct s_s_s_double3 the_s_s_s_double3 = { { { 1.0, 2 }, 3 }, 4 };

/*
 * STRUCT.ALIGNMENT.8
 */

/* Same as STRUCT.ALIGNMENT.[56] but with unsigned long long. */

struct s_unsigned_long_long {
    unsigned long long an_unsigned_long_long; /* 8 bytes (alignment 8) */
    short a_short;                            /* 2 bytes */
                                              /* 6 bytes padding */
};                                            /* total size: 16 */

struct s_s_unsigned_long_long {
    char a_char;                                         /* 1 byte */
                                                         /* 3 bytes padding */
    struct s_unsigned_long_long a_s_unsigned_long_long;  /* 16 bytes, align 4 */
    short a_short;                                       /* 2 bytes */
                                                         /* 2 bytes padding */
};                                           /* total size: 24 bytes */
                                             /*  alignment: 4 */

DLLEXPORT
struct s_s_unsigned_long_long the_s_s_unsigned_long_long = { 3, { 1, 2 }, 4 };

/* STRUCT.ALIGNMENT.x */

/* commented this test out because this is not standard C
   and MSVC++ (or some versions of it at least) won't compile it. */

/*
struct empty_struct {};

struct with_empty_struct {
    struct empty_struct foo;
    int an_int;
};

DLLEXPORT
struct with_empty_struct the_with_empty_struct = { {}, 42 };
*/

/*
 * STRUCT-VALUES.*
 */

struct pair { int a, b; };

DLLEXPORT
int pair_sum(struct pair p)
{
    return p.a + p.b;
}

DLLEXPORT
int pair_pointer_sum(struct pair *p)
{
    return p->a + p->b;
}

DLLEXPORT
struct pair make_pair(int a, int b)
{
    return (struct pair) { a, b };
}

DLLEXPORT
struct pair *alloc_pair(int a, int b)
{
    struct pair *p = malloc(sizeof(struct pair));
    p->a = a;
    p->b = b;
    return p;
}

struct pair_plus_one {
    struct pair p;
    int c;
};

DLLEXPORT
int pair_plus_one_sum(struct pair_plus_one p)
{
    return p.p.a + p.p.b + p.c;
}

DLLEXPORT
int pair_plus_one_pointer_sum(struct pair_plus_one *p)
{
    return p->p.a + p->p.b + p->c;
}

DLLEXPORT
struct pair_plus_one make_pair_plus_one(int a, int b, int c)
{
    return (struct pair_plus_one) { { a, b }, c };
}

DLLEXPORT
struct pair_plus_one *alloc_pair_plus_one(int a, int b, int c)
{
    struct pair_plus_one *p = malloc(sizeof(struct pair_plus_one));
    p->p.a = a;
    p->p.b = b;
    p->c = c;
    return p;
}

/*
 * DEFCFUN.NOARGS and DEFCFUN.NOOP
 */

DLLEXPORT
int noargs()
{
    return 42;
}

DLLEXPORT
void noop()
{
    return;
}

/*
 * DEFCFUN.BFF.1
 *
 * (let ((rettype (find-type :long))
 *       (arg-types (n-random-types-no-ll 127)))
 *   (c-function rettype arg-types)
 *   (gen-function-test rettype arg-types))
 */

DLLEXPORT long sum_127_no_ll
  (long a1, unsigned long a2, short a3, unsigned short a4, float a5,
   double a6, unsigned long a7, float a8, unsigned char a9, unsigned
   short a10, short a11, unsigned long a12, double a13, long a14,
   unsigned int a15, void* a16, unsigned int a17, unsigned short a18,
   long a19, float a20, void* a21, float a22, int a23, int a24, unsigned
   short a25, long a26, long a27, double a28, unsigned char a29, unsigned
   int a30, unsigned int a31, int a32, unsigned short a33, unsigned int
   a34, void* a35, double a36, double a37, long a38, short a39, unsigned
   short a40, long a41, char a42, long a43, unsigned short a44, void*
   a45, int a46, unsigned int a47, double a48, unsigned char a49,
   unsigned char a50, float a51, int a52, unsigned short a53, double a54,
   short a55, unsigned char a56, unsigned long a57, float a58, float a59,
   float a60, void* a61, void* a62, unsigned int a63, unsigned long a64,
   char a65, short a66, unsigned short a67, unsigned long a68, void* a69,
   float a70, double a71, long a72, unsigned long a73, short a74,
   unsigned int a75, unsigned short a76, int a77, unsigned short a78,
   char a79, double a80, short a81, unsigned char a82, float a83, char
   a84, int a85, double a86, unsigned char a87, int a88, unsigned long
   a89, double a90, short a91, short a92, unsigned int a93, unsigned char
   a94, float a95, long a96, float a97, long a98, long a99, int a100, int
   a101, unsigned int a102, char a103, char a104, unsigned short a105,
   unsigned int a106, unsigned short a107, unsigned short a108, int a109,
   long a110, char a111, double a112, unsigned int a113, char a114, short
   a115, unsigned long a116, unsigned int a117, short a118, unsigned char
   a119, float a120, void* a121, double a122, int a123, long a124, char
   a125, unsigned short a126, float a127)
{
    return (long) a1 + a2 + a3 + a4 + ((long) a5) + ((long) a6) + a7 +
        ((long) a8) + a9 + a10 + a11 + a12 + ((long) a13) + a14 + a15 +
        ((intptr_t) a16) + a17 + a18 + a19 + ((long) a20) +
        ((intptr_t) a21) + ((long) a22) + a23 + a24 + a25 + a26 + a27 +
        ((long) a28) + a29 + a30 + a31 + a32 + a33 + a34 + ((intptr_t) a35) +
        ((long) a36) + ((long) a37) + a38 + a39 + a40 + a41 + a42 + a43 + a44 +
        ((intptr_t) a45) + a46 + a47 + ((long) a48) + a49 + a50 +
        ((long) a51) + a52 + a53 + ((long) a54) + a55 + a56 + a57 + ((long) a58) +
        ((long) a59) + ((long) a60) + ((intptr_t) a61) +
        ((intptr_t) a62) + a63 + a64 + a65 + a66 + a67 + a68 +
        ((intptr_t) a69) + ((long) a70) + ((long) a71) + a72 + a73 + a74 +
        a75 + a76 + a77 + a78 + a79 + ((long) a80) + a81 + a82 + ((long) a83) +
        a84 + a85 + ((long) a86) + a87 + a88 + a89 + ((long) a90) + a91 + a92 +
        a93 + a94 + ((long) a95) + a96 + ((long) a97) + a98 + a99 + a100 + a101 +
        a102 + a103 + a104 + a105 + a106 + a107 + a108 + a109 + a110 + a111 +
        ((long) a112) + a113 + a114 + a115 + a116 + a117 + a118 + a119 +
        ((long) a120) + ((intptr_t) a121) + ((long) a122) + a123 + a124 +
        a125 + a126 + ((long) a127);
}

/*
 * DEFCFUN.BFF.2
 *
 * (let ((rettype (find-type :long-long))
 *       (arg-types (n-random-types 127)))
 *   (c-function rettype arg-types)
 *   (gen-function-test rettype arg-types))
 */

DLLEXPORT long long sum_127
  (void* a1, void* a2, float a3, unsigned long a4, void* a5, long long
  a6, double a7, double a8, unsigned short a9, int a10, long long a11,
  long a12, short a13, unsigned int a14, long a15, unsigned char a16,
  int a17, double a18, short a19, short a20, long long a21, unsigned
  int a22, unsigned short a23, short a24, void* a25, short a26,
  unsigned short a27, unsigned short a28, int a29, long long a30,
  void* a31, int a32, unsigned long a33, unsigned long a34, void* a35,
  unsigned long long a36, float a37, int a38, short a39, void* a40,
  unsigned long long a41, long long a42, unsigned long a43, unsigned
  long a44, unsigned long long a45, unsigned long a46, char a47,
  double a48, long a49, unsigned int a50, int a51, short a52, void*
  a53, long a54, unsigned long long a55, int a56, unsigned short a57,
  unsigned long long a58, float a59, void* a60, float a61, unsigned
  short a62, unsigned long a63, float a64, unsigned int a65, unsigned
  long long a66, void* a67, double a68, unsigned long long a69, double
  a70, double a71, long long a72, void* a73, unsigned short a74, long
  a75, void* a76, short a77, double a78, long a79, unsigned char a80,
  void* a81, unsigned char a82, long a83, double a84, void* a85, int
  a86, double a87, unsigned char a88, double a89, short a90, long a91,
  int a92, long a93, double a94, unsigned short a95, unsigned int a96,
  int a97, char a98, long long a99, double a100, float a101, unsigned
  long a102, short a103, void* a104, float a105, long long a106, int
  a107, long long a108, long long a109, double a110, unsigned long
  long a111, double a112, unsigned long a113, char a114, char a115,
  unsigned long a116, short a117, unsigned char a118, unsigned char
  a119, int a120, int a121, float a122, unsigned char a123, unsigned
  char a124, double a125, unsigned long long a126, char a127)
{
    return (long long) ((intptr_t) a1) + ((intptr_t) a2) + ((long) a3) +
        a4 + ((intptr_t) a5) + a6 + ((long) a7) + ((long) a8) + a9 + a10 +
        a11 + a12 + a13 + a14 + a15 + a16 + a17 + ((long) a18) + a19 + a20 +
        a21 + a22 + a23 + a24 + ((intptr_t) a25) + a26 + a27 + a28 + a29 +
        a30 + ((intptr_t) a31) + a32 + a33 + a34 + ((intptr_t) a35) +
        a36 + ((long) a37) + a38 + a39 + ((intptr_t) a40) + a41 + a42 + a43 +
        a44 + a45 + a46 + a47 + ((long) a48) + a49 + a50 + a51 + a52 +
        ((intptr_t) a53) + a54 + a55 + a56 + a57 + a58 + ((long) a59) +
        ((intptr_t) a60) + ((long) a61) + a62 + a63 + ((long) a64) + a65 + a66
        + ((intptr_t) a67) + ((long) a68) + a69 + ((long) a70) + ((long) a71) +
        a72 + ((intptr_t) a73) + a74 + a75 + ((intptr_t) a76) + a77 +
        ((long) a78) + a79 + a80 + ((intptr_t) a81) + a82 + a83 + ((long) a84)
        + ((intptr_t) a85) + a86 + ((long) a87) + a88 + ((long) a89) + a90 +
        a91 + a92 + a93 + ((long) a94) + a95 + a96 + a97 + a98 + a99 +
        ((long) a100) + ((long) a101) + a102 + a103 + ((intptr_t) a104) +
        ((long) a105) + a106 + a107 + a108 + a109 + ((long) a110) + a111 +
        ((long) a112) + a113 + a114 + a115 + a116 + a117 + a118 + a119 + a120 +
        a121 + ((long) a122) + a123 + a124 + ((long) a125) + a126 + a127;
}

/*
 * CALLBACKS.BFF.1  (cb-test :no-long-long t)
 */

DLLEXPORT long call_sum_127_no_ll
  (long (*func)
   (unsigned long, void*, long, double, unsigned long, float, float,
    int, unsigned int, double, double, double, void*, unsigned short,
    unsigned short, void*, long, long, int, short, unsigned short,
    unsigned short, char, long, void*, void*, char, unsigned char,
    unsigned long, short, int, int, unsigned char, short, long, long,
    void*, unsigned short, char, double, unsigned short, void*, short,
    unsigned long, unsigned short, float, unsigned char, short, float,
    short, char, unsigned long, unsigned long, char, float, long, void*,
    short, float, unsigned int, float, unsigned int, double, unsigned int,
    unsigned char, int, long, char, short, double, int, void*, char,
    unsigned short, void*, unsigned short, void*, unsigned long, double,
    void*, long, float, unsigned short, unsigned short, void*, float, int,
    unsigned int, double, float, long, void*, unsigned short, float,
    unsigned char, unsigned char, float, unsigned int, float, unsigned
    short, double, unsigned short, unsigned long, unsigned int, unsigned
    long, void*, unsigned char, char, char, unsigned short, unsigned long,
    float, short, void*, long, unsigned short, short, double, short, int,
    char, unsigned long, long, int, void*, double, unsigned char))
{
    return
        func(948223085, (void *) 803308438, -465723152, 20385,
             219679466, -10035, 13915, -1193455756, 1265303699, 27935, -18478,
             -10508, (void *) 215389089, 55561, 55472, (void *) 146070433,
             -1040819989, -17851453, -1622662247, -19473, 20837, 30216, 79,
             986800400, (void *) 390281604, (void *) 1178532858, 19, 117,
             78337699, -5718, -991300738, 872160910, 184, 926, -1487245383,
             1633973783, (void *) 33738609, 53985, -116, 31645, 27196, (void *)
             145569903, -6960, 17252220, 47404, -10491, 88, -30438, -21212,
             -1982, -16, 1175270, 7949380, -121, 8559, -432968526, (void *)
             293455312, 11894, -8394, 142421516, -25758, 3422998, 4004,
             15758212, 198, -1071899743, -1284904617, -11, -17219, -30039,
             311589092, (void *) 541468577, 123, 63517, (void *) 1252504506,
             39368, (void *) 10057868, 134781408, -7143, (void *) 72825877,
             -1190798667, -30862, 63757, 14965, (void *) 802391252, 22008,
             -517289619, 806091099, 1125, 451, -498145176, (void *) 55960931,
             15379, 4629, 184, 254, 22532, 465856451, -1669, 49416, -16546,
             2983, 4337541, 65292495, 39253529, (void *) 669025, 211, 85, -19,
             24298, 65358, 16776, -29957, (void *) 124311, -163231228, 2610,
             -7806, 26434, -21913, -753615541, 120, 358697932, -1198889034,
             -2131350926, (void *) 3749492036, -13413, 17);
}

/*
 * CALLBACKS.BFF.2  (cb-test)
 */

DLLEXPORT long long call_sum_127
  (long long (*func)
   (short, char, void*, float, long, double, unsigned long long,
    unsigned short, unsigned char, char, char, unsigned short, unsigned
    long long, unsigned short, long long, unsigned short, unsigned long
    long, unsigned char, unsigned char, unsigned long long, long long,
    char, float, unsigned int, float, float, unsigned int, float, char,
    unsigned char, long, long long, unsigned char, double, long,
    double, unsigned int, unsigned short, long long, unsigned int, int,
    unsigned long long, long, short, unsigned int, unsigned int,
    unsigned long long, unsigned int, long, void*, unsigned char, char,
    long long, unsigned short, unsigned int, float, unsigned char,
    unsigned long, long long, float, long, float, int, float, unsigned
    short, unsigned long long, short, unsigned long, long, char,
    unsigned short, long long, short, double, void*, unsigned int,
    char, unsigned int, void*, void*, unsigned char, void*, unsigned
    short, unsigned char, long, void*, char, long, unsigned short,
    unsigned char, double, unsigned long long, unsigned short, unsigned
    short, unsigned int, long, char, long, char, short, unsigned short,
    unsigned long, unsigned long, short, long long, long long, long
    long, double, unsigned short, unsigned char, short, unsigned char,
    long, long long, unsigned long long, unsigned int, unsigned long,
    unsigned char, long long, unsigned char, unsigned long long,
    double, unsigned char, long long, unsigned char, char, long long))
{
    return
        func(-8573, 14, (void *) 832601021, -32334, -1532040888,
             -18478, 2793023182591311826, 2740, 230, 103, 97, 13121,
             5112369026351511084, 7763, -8134147951003417418, 34348,
             5776613699556468853, 19, 122, 1431603726926527625,
             439503521880490337, -112, -21557, 1578969190, -22008, -4953,
             2127745975, -7262, -6, 180, 226352974, -3928775366167459219, 134,
             -17730, -1175042526, 23868, 3494181009, 57364,
             3134876875147518682, 104531655, -1286882727, 803577887579693487,
             1349268803, 24912, 3313099419, 3907347884, 1738833249233805034,
             2794230885, 1008818752, (void *) 1820044575, 189, 61,
             -931654560961745071, 57531, 3096859985, 10405, 220, 3631311224,
             -8531370353478907668, 31258, 678896693, -32150, -1869057813,
             -19877, 62841, 4161660185772906873, -23869, 4016251006, 610353435,
             105, 47315, -1051054492535331660, 6846, -15163, (void *)
             736672359, 2123928476, -122, 3859258652, (void *) 3923394833,
             (void *) 1265031970, 161, (void *) 1993867800, 55056, 122,
             1562112760, (void *) 866615125, -79, -1261399547, 31737, 254,
             -31279, 5462649659172897980, 5202, 7644, 174224940, -337854382,
             -45, -583502442, -37, -13266, 24520, 2198606699, 2890453969,
             -8282, -2295716637858246075, -1905178488651598878,
             -6384652209316714643, 14841, 35443, 132, 15524, 187, 2138878229,
             -5153032566879951000, 9056545530140684207, 4124632010, 276167701,
             56, -2307310370663738730, 66, 9113015627153789746, -9618, 167,
             755753399701306200, 119, -28, -990561962725435433);
}

/*
 * CALLBACKS.DOUBLE26
 */

DLLEXPORT double call_double26
  (double (*f)(double, double, double, double, double, double, double, double,
               double, double, double, double, double, double, double, double,
               double, double, double, double, double, double, double, double,
               double, double))
{
    return f(3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14,
             3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14, 3.14,
             3.14, 3.14, 3.14, 3.14);
}

/*
 * DEFCFUN.DOUBLE26 and FUNCALL.DOUBLE26
 */

DLLEXPORT
double sum_double26(double a1, double a2, double a3, double a4, double a5,
                    double a6, double a7, double a8, double a9, double a10,
                    double a11, double a12, double a13, double a14, double a15,
                    double a16, double a17, double a18, double a19, double a20,
                    double a21, double a22, double a23, double a24, double a25,
                    double a26)
{
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 +
        a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 +
        a26;
}

/*
 * CALLBACKS.FLOAT26
 */

DLLEXPORT float call_float26
  (float (*f)(float, float, float, float, float, float, float, float,
              float, float, float, float, float, float, float, float,
              float, float, float, float, float, float, float, float,
              float, float))
{
    return f(5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
             5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
             5.0, 5.0, 5.0, 5.0);
}

/*
 * DEFCFUN.FLOAT26 and FUNCALL.FLOAT26
 */

DLLEXPORT
float sum_float26(float a1, float a2, float a3, float a4, float a5,
                  float a6, float a7, float a8, float a9, float a10,
                  float a11, float a12, float a13, float a14, float a15,
                  float a16, float a17, float a18, float a19, float a20,
                  float a21, float a22, float a23, float a24, float a25,
                  float a26)
{
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 +
        a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 +
        a26;
}

/*
 * Symbol case.
 */

DLLEXPORT int UPPERCASEINT1    = 12345;
DLLEXPORT int UPPER_CASE_INT1  = 23456;
DLLEXPORT int MiXeDCaSeInT1    = 34567;
DLLEXPORT int MiXeD_CaSe_InT1  = 45678;

DLLEXPORT int UPPERCASEINT2    = 12345;
DLLEXPORT int UPPER_CASE_INT2  = 23456;
DLLEXPORT int MiXeDCaSeInT2    = 34567;
DLLEXPORT int MiXeD_CaSe_InT2  = 45678;

DLLEXPORT int UPPERCASEINT3    = 12345;
DLLEXPORT int UPPER_CASE_INT3  = 23456;
DLLEXPORT int MiXeDCaSeInT3    = 34567;
DLLEXPORT int MiXeD_CaSe_InT3  = 45678;

/*
 * FOREIGN-SYMBOL-POINTER.1
 */

DLLEXPORT int compare_against_abs(intptr_t p)
{
    return p == (intptr_t) abs;
}

/*
 * FOREIGN-SYMBOL-POINTER.2
 */

DLLEXPORT void xpto_fun() {}

DLLEXPORT
int compare_against_xpto_fun(intptr_t p)
{
    return p == (intptr_t) xpto_fun;
}

/*
 * [DEFCFUN|FUNCALL].NAMESPACE.1
 */

DLLEXPORT
int ns_function()
{
    return 1;
}

/*
 * FOREIGN-GLOBALS.NAMESPACE.*
 */

DLLEXPORT int ns_var = 1;

/*
 * DEFCFUN.STDCALL.1
 */

DLLEXPORT
int STDCALL stdcall_fun(int a, int b, int c)
{
    return a + b + c;
}

/*
 * CALLBACKS.STDCALL.1
 */

DLLEXPORT
int call_stdcall_fun(int (STDCALL *f)(int, int, int))
{
    int a = 42;
    f(1, 2, 3);
    return a;
}

/* Unlike the one above, this commented test below actually
 * works. But, alas, it doesn't compile with -std=c99. */

/*
DLLEXPORT
int call_stdcall_fun(int __attribute__((stdcall)) (*f)(int, int, int))
{
    asm("pushl $42");
    register int ebx asm("%ebx");
    f(1, 2, 3);
    asm("popl %ebx");
    return ebx;
}
*/

/* vim: ts=4 et
*/
