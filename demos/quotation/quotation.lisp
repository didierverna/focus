;;; quotation.lisp --- Quotation function definition

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:



;;; Code:

(in-package      :net.didierverna.focus.demos.quotation)
(in-format-table :net.didierverna.focus.demos.quotation)

(defun quotation (who quotation)
  (format t "As ~A would say: ~`.~%" who quotation))

;;; quotation.lisp ends here
