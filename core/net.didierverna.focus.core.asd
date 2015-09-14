;;; net.didierverna.focus.core.asd --- ASDF system definition, core library

;; Copyright (C) 2015 Didier Verna

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

(asdf:load-system :net.didierverna.focus.setup)

(asdf:defsystem :net.didierverna.focus.core
  :long-name "Format Customizations, core library"
  :description "Customizable FORMAT strings and directives"
  :long-description "\
Focus's core functionality. For a more complete description of Focus, see the
net.didierverna.focus system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#focus"
  :source-control "https://github.com/didierverna/focus"
  :license "BSD"
  :version #.(net.didierverna.focus.setup:version :short)
  :depends-on (:net.didierverna.focus.setup)
  :serial t
  :components ((:file "meta")
	       (:module "src"
		:serial t
		:components ((:file "util")
			     (:file "directive")
			     (:file "table")
			     (:file "string")
			     (:file "wrapper")))))

;;; net.didierverna.focus.core.asd ends here
