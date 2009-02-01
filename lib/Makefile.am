SUBDIRS = \
  cpp

if WITH_MONO
SUBDIRS += csharp
endif

if WITH_JAVA
SUBDIRS += java
endif

if WITH_PYTHON
SUBDIRS += py
endif

if WITH_ERLANG
SUBDIRS += erl
endif

if WITH_RUBY
SUBDIRS += rb
endif

if WITH_PERL
SUBDIRS += perl
endif

# All of the libs that don't use Automake need to go in here
# so they will end up in our release tarballs.
EXTRA_DIST = \
  cocoa \
  hs    \
  ocaml \
  php   \
  erl   \
  st
