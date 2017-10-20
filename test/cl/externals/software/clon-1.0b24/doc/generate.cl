;;; generate.cl --- Clon reference manual generation script

;; Copyright (C) 2010-2012, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(require "asdf")

(defconstant +introduction+
  "@macro clon
@t{Clon}
@end macro

@macro cmdline
command-line
@end macro

@macro CmdLine
Command-Line
@end macro

@macro cl
Common-Lisp
@end macro

@macro tty
@t{tty}
@end macro

@macro etc
@i{etc.}
@end macro

@ifinfo
@macro pxenduserref{node}
@pxref{\\node\\, , , clon-enduser, The Clon End-User Manual}
@end macro
@end ifinfo
@ifnotinfo
@macro pxenduserref{node}
@pxref{\\node\\, , , enduser, The Clon End-User Manual}
@end macro
@end ifnotinfo

@ifinfo
@macro pxuserref{node}
@pxref{\\node\\, , , clon-user, The Clon User Manual}
@end macro
@end ifinfo
@ifnotinfo
@macro pxuserref{node}
@pxref{\\node\\, , , user, The Clon User Manual}
@end macro
@end ifnotinfo

@clon{} is a library for managing @cmdline{} options in standalone @cl{}
applications. It provides a unified option syntax with both short and
long names, automatic completion of partial names and automatic
retrieval/conversion of option arguments from the @cmdline{}, associated
environment variables, fallback or default values. @clon{} comes with a
set of extensible option types (switches, paths, strings @etc{}).
@clon{} also provides automatic generation and formatting of help
strings, with support for highlighting on @tty{}'s through ISO/IEC 6429
SGR. This formatting is customizable through @emph{themes}.

Depending on the target audience, @clon{} stands for either ``The
@CmdLine{} Options Nuker'' or ``The @cl{} Options Nuker''. @clon{} also
has a recursive acronym: ``@clon{} Likes Options Nuking'', and a reverse
one: ``Never Omit to Link with @clon{}''. Other possible expansions of
the acronym are still being investigated.

This is the @clon{} reference manual, and as such, it is not meant to be
read. It may help you find sleep in case of insomnia though. @clon{}
comes with two human-readable manuals:
@itemize @bullet
@item
the ``end-user manual'' (@pxenduserref{Top}) is for the @clon{}
@emph{end-user}, that is, the user of an application powered by @clon{}.
It describes how to use the @cmdline{} of clonified@footnote{An
application using @clon{} for its @cmdline{} option management is said to
be @emph{clonified}. It is also possible to say @emph{clonfiscated}.
However, we advise against using @emph{clonistified}. The term
@emph{clonificated} is also considered bad style, and the use of
@emph{clonificationated} is strictly prohibited.} applications and how to
customize @clon{}'s output. Everybody should read this manual first.
@item
the ``user manual'' (@pxenduserref{Top}) is for the @clon{} @emph{user},
that is, the developer of a @cl{} application who wants to use @clon{} for
@cmdline{} option management. It describes how to clonify your application
and extend the library with your own option types.
@end itemize"
  "The reference manual's introductory text.")

(asdf:load-system :net.didierverna.declt)

(if (and (second sb-ext:*posix-argv*)
	 (string= (second sb-ext:*posix-argv*) "--web"))
    (net.didierverna.declt:declt :net.didierverna.clon
			     :library-name "Clon"
			     :texi-file "webreference.texi"
			     ;; but we don't care
			     :info-file "clon-webreference"
			     :introduction +introduction+
			     :license :bsd
			     :copyright-date "2010-2012, 2015")
  (net.didierverna.declt:declt :net.didierverna.clon
			   :library-name "Clon"
			   :texi-file "reference.texi"
			   :info-file "clon-reference"
			   :introduction +introduction+
			   :license :bsd
			   :copyright-date "2010-2012, 2015"
			   :hyperlinks t))

(uiop:quit)


;;; generate.cl ends here
