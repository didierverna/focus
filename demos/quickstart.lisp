;;; quickstart.lisp --- Basic usage demonstration program

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of FoCus.

;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.


;;; Commentary:



;;; Code:

(in-package :cl-user)

(require "asdf")
(asdf:load-system :net.didierverna.focus)
(net.didierverna.focus:nickname-package)


(defpackage :quotation
  (:use :cl)
  (:export :quotation))

(in-package :quotation)

(defun quotation-formatter (stream argument colonp atsignp &rest arguments)
  (declare (ignore colonp atsignp arguments))
  (write-char #\` stream)
  (write-string argument stream)
  (write-char #\' stream))

(let ((table (focus:make-format-table)))
  (focus:with-format-table table
    (focus:set-format-directive #\` :function 'quotation-formatter))

  (defun quotation (who quotation)
    (focus:with-format-table table
      (focus:format t "As ~A would say: ~`.~%" who quotation))))


(in-package :cl-user)

(quotation:quotation "Bugs Bunny" "Errr, what's up Doc?")

;;; quickstart.lisp ends here
