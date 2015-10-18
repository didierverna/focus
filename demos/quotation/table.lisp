;;; table.lisp --- Format table definition

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.focus.demos.quotation)

(defun quotation-formatter (stream argument colonp atsignp &rest arguments)
  (declare (ignore colonp atsignp arguments))
  (write-char #\` stream)
  (write-string argument stream)
  (write-char #\' stream))

(register-format-table (make-format-table)
		       :net.didierverna.focus.demos.quotation)
(set-format-directive #\` :function 'quotation-formatter
			  :table :net.didierverna.focus.demos.quotation)

;;; table.lisp ends here
