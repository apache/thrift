ACLOCAL_AMFLAGS = -I ./aclocal

SUBDIRS = compiler/cpp lib if test
## Don't run make dist from a subversion working copy
## because it will pull in your .svn directories.
EXTRA_DIST = bootstrap.sh cleanup.sh doc test tutorial \
             thrift.el thrift.vim thrift.bnf CONTRIBUTORS LICENSE
