;;; generate.cl --- Declt reference manual generation script

;; Copyright (C) 2010-2013, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

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



;;; Code:

(require "asdf")

(defconstant +introduction+
  "@macro focus
@t{FoCus}
@end macro

@macro Fmt
@code{Format}
@end macro

@macro fmt
@code{format}
@end macro

@macro cl
Common-Lisp
@end macro

@macro etc
@i{etc.}
@end macro

@Fmt{} is a powerful utility in the @cl{} standard. Format strings are
written in what can be considered as a printing DSL (Domain Specific
Language). However powerful that language is though, it suffers from two
important limitations.

@enumerate
@item
It is not modifiable: only a predefined set of standard directive
characters can be used and it is not possible to alter their semantics.
@item
It is hardly extensible. The only embryonic extension mechanism
available, the @code{~/} directive, is extremely cumbersome to use. The
called function must either reside in the @code{cl-user} package, or you
must always use its fully qualified name in the format string, even if
the corresponding code lies in the appropriate package. For instance,
consider that there is a function called @code{my-format-function} in
the package named @code{:my.long.package.name}. Every time you want to
use this function, you need to write something like
@verbatim
(format t \"~/my.long.package.name:my-format-function/\" ...)
@end verbatim
which essentially makes the @code{/} directive unusable.
@end enumerate

@focus{} is a library designed to circumvent those limitations. It
allows you to customize the @fmt{} DSL by adding new directive
characters or modifying the standard ones. The semantics of these
directive characters is specified in a so-called @dfn{format table}, a
concept very close to that of readtables. @focus{} ultimately translates
into regular @fmt{} calls.

This is the @focus{} reference manual, and as such, it is not meant to be
read. It may help you find sleep in case of insomnia though.
@ifinfo
See @xref{Top, , The FoCus User Manual, focus-user},
@end ifinfo
@ifhtml
See @xref{Top, , The FoCus User Manual, user},
@end ifhtml
@ifnotinfo
@ifnothtml
@xref{Top, , , user, The FoCus User Manual},
@end ifnothtml
@end ifnotinfo
for a more human-readable guide to using @focus{}."
  "The reference manual's introductory text.")

(asdf:load-system :net.didierverna.declt)
(net.didierverna.declt:nickname-package)

;; ASDF doesn't understand my version numnbering scheme. That will change
;; soon, but in the meantime, I have to provide my version number explicitly
;; here (and so I need to load the system in order to get the VERSION
;; function).
(asdf:load-system :net.didierverna.focus)

(if (and (second sb-ext:*posix-argv*)
	 (string= (second sb-ext:*posix-argv*) "--web"))
    (declt:declt :net.didierverna.focus
		 :library "FoCus"
		 :texi-file "webreference.texi"
		 :info-file "focus-webreference" ; but we don't care
		 :introduction +introduction+
		 :version (net.didierverna.focus:version :long)
		 :license :bsd
		 :copyright "2015")
    (declt:declt :net.didierverna.focus
		 :library "FoCus"
		 :texi-file "reference.texi"
		 :info-file "focus-reference"
		 :introduction +introduction+
		 :version (net.didierverna.focus:version :long)
		 :license :bsd
		 :copyright "2015"
		 :hyperlinks t))

(uiop:quit)

;;; generate.cl ends here
