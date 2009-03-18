SUBDIRS = test

Makefile-perl.mk : Makefile.PL
	$(PERL) Makefile.PL MAKEFILE=Makefile-perl.mk INSTALLDIRS=$(INSTALLDIRS)

all-local: Makefile-perl.mk
	$(MAKE) -f Makefile-perl.mk
	find blib -name 'Makefile*' -exec rm -f {} \;

check-local:
	$(PERL) -Iblib/lib -I@abs_srcdir@ -I@builddir@/test/gen-perl \
		@abs_srcdir@/test.pl @abs_srcdir@/test/*.t

install-exec-local: Makefile-perl.mk
	$(MAKE) -f Makefile-perl.mk install DESTDIR=$(DESTDIR)/

clean-local:
	if test -f Makefile-perl.mk ; then \
		$(MAKE) -f Makefile-perl.mk clean ; \
	fi
	rm -f Makefile-perl.mk.old

EXTRA_DIST = \
	LICENSE \
	Makefile.PL \
	test.pl \
	lib/Thrift.pm \
	lib/Thrift.pm \
	lib/Thrift/BinaryProtocol.pm \
	lib/Thrift/BufferedTransport.pm \
	lib/Thrift/FramedTransport.pm \
	lib/Thrift/HttpClient.pm \
	lib/Thrift/MemoryBuffer.pm \
	lib/Thrift/Protocol.pm \
	lib/Thrift/Socket.pm \
	lib/Thrift/Transport.pm
