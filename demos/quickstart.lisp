;;; quickstart.lisp --- Basic usage demonstration program

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
    (focus:set-format-directive #\` :function-name 'quotation-formatter))

  (defun quotation (who quotation)
    (focus:with-format-table table
      (focus:format t "As ~A would say: ~`.~%" who quotation))))


(in-package :cl-user)

(quotation:quotation "Bugs Bunny" "Errr, what's up Doc?")

;;; quickstart.lisp ends here
