;;; string.lisp --- Format strings management

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


;; ==========================================================================
;; String Translation
;; ==========================================================================

(define-condition format-string-error (focus-error)
  ((format-string :documentation "The format string related to the error."
		  :initarg :format-string
		  :reader format-string)
   (offset :documentation "The offset in the string where the error occurs."
	   :initform nil
	   :initarg :offset
	   :reader offset))
  (:documentation "A format string error."))

(define-condition missing-directive (format-string-error)
  ()
  (:report (lambda (error stream)
	     (format stream
		 "Missing directive character:~%  => ~S~%  => ~V@T^"
	       (format-string error)
	       (1+ (offset error)))))
  (:documentation "A missing directive error."))

(define-condition spurious-parameter (format-string-error)
  ()
  (:report (lambda (error stream)
	     (format stream "~
Spurious parameter found after ':' or '@' modifier:~%  => ~S~%  => ~V@T^"
	       (format-string error)
	       (1+ (offset error)))))
  (:documentation "A spurious parameter error."))

(define-condition spurious-modifier (format-string-error)
  ((modifier :documentation "The modifier character."
	     :initarg :modifier
	     :reader modifier))
  (:report (lambda (error stream)
	     (format stream "Spurious '~A' modifier:~%  => ~S~%  => ~V@T^"
	       (modifier error)
	       (format-string error)
	       (1+ (offset error)))))
  (:documentation "A spurious modifier error."))

(define-condition missing-delimiter (format-string-error)
  ((delimiter :documentation "The delimiter character."
	      :initarg :delimiter
	      :reader delimiter))
  (:report (lambda (error stream)
	     (format stream
		 "Missing matching '~A' delimiter:~%  => ~S~%  => ~V@T^"
	       (delimiter error)
	       (format-string error)
	       (1+ (offset error)))))
  (:documentation "A missing delimiter error."))


(defun directive-index (string start)
  "Return the next directive index in STRING from START, or nil."
  (position #\~ string :start start))

(defun directive-body-index (string start)
  "Return the next directive body index in STRING from START.
START is the position of the tilde character, so this function essentially
parses the directive arguments in order to skip them."
  (loop :with end := (length string)
	:with index := (1+ start)
	:and colonp := nil
	:and atsignp := nil
	:for char := (if (< index end)
			 (schar string index)
		       (error 'missing-directive
			      :format-string string
			      :offset start))
	:when (and (or colonp atsignp)
		   (or (char<= #\0 char #\9)
		       (member char '(#\- #\+ #\v #\V #\# #\' #\,))))
	  :do (error 'spurious-parameter
		     :format-string string
		     :offset index)
	:do (flet ((skip-comma (&optional (advance t))
		     (when advance (incf index))
		     (when (and (< index end)
				(char= (schar string index) #\,))
		       (incf index))))
	      (cond ((or (char<= #\0 char #\9)
			 (char= char #\+)
			 (char= char #\-))
		     (multiple-value-bind (value new-index)
			 (parse-integer string :start index :junk-allowed t)
		       (declare (ignore value))
		       (setf index new-index))
		     (skip-comma nil))
		    ((member char '(#\v #\V #\#))
		     (skip-comma))
		    ((char= char #\')
		     (incf index)
		     (when (= index end)
		       (error 'missing-directive
			      :format-string string
			      :offset start))
		     (skip-comma))
		    ((char= char #\,)
		     (incf index))
		    ((char= char #\:)
		     (if colonp
			 (error 'spurious-modifier
				:format-string string
				:offset index
				:modifier #\:)
		       (setf colonp t))
		     (incf index))
		    ((char= char #\@)
		     (if atsignp
			 (error 'spurious-modifier
				:format-string string
				:offset index
				:modifier #\@)
		       (setf atsignp t))
		     (incf index))
		    (t
		     (return index))))))

(defgeneric directive-body-translation (string start directive )
  (:documentation
   "Translate the next directive body in STRING into a standard one.
- START is the position of the directive's body (past the ~ character and the
  potential arguments).
- The translation is done according to DIRECTIVE.

Return two values:
- the translated directive's body as a string,
- the index of STRING's remainder.")
  (:method (string start (directive standard-directive))
    "Method for one-character and grouping standard directives."
    (declare (ignore string))
    (values (make-string 1 :initial-element (directive-character directive))
	    (1+ start)))
  (:method (string start (directive standard-delimiting-directive))
    "Method for the / standard directive."
    (let ((character (schar string start)))
      (let ((closing-character (position character string :start (1+ start))))
	(if closing-character
	    (values (format nil "~C~A~C"
		      (directive-character directive)
		      (subseq string (1+ start) closing-character)
		      (directive-character directive))
		    (1+ closing-character))
	  (error 'missing-delimiter
		 :format-string string
		 :offset start
		 :delimiter character)))))
  (:method (string start (directive function-directive))
    "Method for function directives."
    (let ((function (directive-function directive)))
      (values (format nil "/~A:~A/"
		(package-name (symbol-package function))
		(symbol-name function))
	      (1+ start)))))

(defun directive-translation
    (string start table
     &aux (index (directive-body-index string start))
	  (directive (gethash (schar string index) (table-mappings table))))
"Translate the next directive in STRING into a standard one.
- START is the position of the ~ character.
- The translation is done according to format TABLE.

Return two values:
- the translated directive as a string,
- the index of STRING's remainder.

Note that the directive arguments are copied as-is. Only the directive's body
actually involves a translation."
  (unless directive
    (error 'unknown-directive :directive (schar string index) :table table))
  (multiple-value-bind (translation remainder)
      (directive-body-translation string index directive)
    (values (concatenate 'string (subseq string start index) translation)
	    remainder)))

(defun string-translation (string &optional (table *format-table*))
  "Return the translation of format STRING into a standard one.
The translation is done according to format TABLE (the current format table by
default)."
  (apply #'concatenate
    'string
    (loop :with position := 0
	  :and end := (length string)
	  :and directive := nil
	  :while (< position end)
	  :for next := (or (directive-index string position) end)
	  :if (> next position)
	    :collect (subseq string position next)
	    :and :do (setq position next)
	  :else
	    :do (multiple-value-setq (directive position)
		  (directive-translation string position table))
	    :and :collect directive)))

;;; string.lisp ends here
