require 'mkmf'

$CFLAGS = "-g -O2 -Wall -Werror"

have_func("strlcpy", "string.h")

create_makefile 'thrift_native'
