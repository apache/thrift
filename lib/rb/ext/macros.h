
#define GET_TRANSPORT(obj) rb_ivar_get(obj, transport_ivar_id)
#define GET_STRICT_READ(obj) rb_ivar_get(obj, strict_read_ivar_id)
#define GET_STRICT_WRITE(obj) rb_ivar_get(obj, strict_write_ivar_id)
#define WRITE(obj, data, length) rb_funcall(obj, write_method_id, 1, rb_str_new(data, length))
#define CHECK_NIL(obj) if (NIL_P(obj)) { rb_raise(rb_eStandardError, "nil argument not allowed!");}
#define READ(obj, length) rb_funcall(GET_TRANSPORT(obj), read_method_id, 1, INT2FIX(length)) 