#ifndef __RUBY_PTR_H_
#define __RUBY_PTR_H_

 #if SIZEOF_VOIDP <= SIZEOF_LONG
  #define NUM2PTR(x) (void*)NUM2ULONG(x)
  #define PTR2NUM(x) ULONG2NUM((unsigned long)x)
#elif SIZEOF_VOIDP <= SIZEOF_LONG_LONG
  #define NUM2PTR(x) (void*)NUM2ULL(x)
  #define PTR2NUM(x) ULL2NUM((unsigned long long)x)
#else
 #error "Pointer size too large, could not determine a good way to convert a C pointer to a Ruby object. Please update ruby_ptr.h"
#endif

#endif