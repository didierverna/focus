;;; package.lisp --- Package definition

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:



;;; Code:

(defpackage :net.didierverna.focus.demos.quotation
  (:use :cl :net.didierverna.focus)
  (:shadowing-import-from :net.didierverna.focus :format :formatter)
  (:export :quotation))

;;; package.lisp ends here
