rem
rem script for compiling the test lib with the free MSVC++ toolkit.
rem

cl /LD /DWIN32=1 /Tc libtest.c
del libtest.obj libtest.exp

cl /LD /DWIN32=1 /Tc libtest2.c
del libtest2.obj libtest2.exp
