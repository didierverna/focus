;;; wrapper.lisp --- Wrappers around the standard API

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

(in-package :net.didierverna.focus)
(in-readtable :net.didierverna.focus)


(defun format (destination control-string &rest args)
  "Wrapper around the standard FORMAT function.
When CONTROL-STRING is a string, it is interpreted according to the current
format table."
  (when (stringp control-string)
    (setq control-string (string-translation control-string)))
  (apply #'cl:format destination control-string args))

(defmacro formatter (control-string)
  "Wrapper around the standard FORMATTER macro.
CONTROL-STRING is interpreted according to the current format table."
  `(formatter (string-translation ,control-string)))

;;; wrapper.lisp ends here
