;;; flv.lisp --- File-local variables support

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :net.didierverna.focus)
(in-readtable :net.didierverna.focus)

(net.didierverna.asdf-flv:make-variable-file-local '*format-table*)
(net.didierverna.asdf-flv:make-variable-file-local '*compile*)

(defmacro in-format-table (table-or-name)
  "Set the current format table to TABLE-OR-NAME in the current file.
This macro behaves like IN-PACKAGE or IN-READTABLE and also sets *COMPILE* to
t, so as to enable compile-time behavior in FoCus."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *format-table* (find-table ,table-or-name)
	   *compile*      t)))

(export 'in-format-table)

;;; flv.lisp ends here
