;;; net.didierverna.focus.demos.quotation.asd --- ASDF system definition

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:



;;; Code:

(asdf:defsystem :net.didierverna.focus.demos.quotation
  :depends-on (:net.didierverna.focus.flv)
  :serial t
  :components ((:file "package")
	       (:file "table")
	       (:file "quotation")))

;;; net.didierverna.focus.demos.quotation.asd ends here
