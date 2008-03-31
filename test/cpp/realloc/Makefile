# This probably should not go into "make check", because it is an experiment,
# not a test.  Specifically, it is meant to determine how likely realloc is
# to avoid a copy.  This is poorly documented.

run: realloc_test
	for it in 1 4 64 ; do \
		for nb in 1 8 64 512 ; do \
			for mins in 64 512 ; do \
				for maxs in 2048 262144 ; do \
					for db in 8 64 ; do \
						./realloc_test $$nb $$mins $$maxs $$db $$it \
					; done \
				; done \
			; done \
		; done \
	; done \
	> raw_stats

CFLAGS = -Wall -g -std=c99
LDLIBS = -ldl
realloc_test: realloc_test.c
