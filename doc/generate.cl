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

@macro cl
Common-Lisp
@end macro

@macro etc
@i{etc.}
@end macro

@focus{}

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
		 :library-name "FoCus"
		 :texi-file "webreference.texi"
		 :info-file "focus-webreference" ; but we don't care
		 :introduction +introduction+
		 :version (net.didierverna.focus:version :long)
		 :license :bsd
		 :copyright-date "2015")
    (declt:declt :net.didierverna.focus
		 :library-name "FoCus"
		 :texi-file "reference.texi"
		 :info-file "focus-reference"
		 :introduction +introduction+
		 :version (net.didierverna.focus:version :long)
		 :license :bsd
		 :copyright-date "2015"
		 :hyperlinks t))

(uiop:quit)

;;; generate.cl ends here
