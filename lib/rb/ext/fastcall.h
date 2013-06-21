#ifndef __FASTCALL_H_
#define __FASTCALL_H_


typedef VALUE (*rfunc)(VALUE object, ...);

struct _fastcall;
typedef struct _fastcall fastcall;
struct _fastcall
{
  ID ruby_method;
  int ruby_argc;

  rfunc cfunc;
};

#define fastcall_init_c(fc, rf)  (fc).cfunc = rf
#define fastcall_init_ruby(fc, mid, argc) ((fc).cfunc = NULL , (fc).ruby_method = mid , (fc).ruby_argc = argc)
#define fastcall_call(fc, obj, ...) (((fc).cfunc == NULL) ? rb_funcall(obj, (fc).ruby_method, (fc).ruby_argc, __VA_ARGS__) : (fc).cfunc(obj, __VA_ARGS__))
//#define fastcall_call(fc, obj, ...) (((fc).cfunc == NULL) ? printf("RUBY CALL %s:%d\n", __FILE__, __LINE__), rb_funcall(obj, (fc).ruby_method, (fc).ruby_argc, __VA_ARGS__) : (fc).cfunc(obj, __VA_ARGS__))


#endif // __FASTCALL_H_
