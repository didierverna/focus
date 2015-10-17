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


(define-condition format-string-error (focus-error)
  ((string :documentation "The format string."
	   :initarg :string
	   ;; The lack of polymorphism on standard functions sucks. I want
	   ;; to use just STRING here.
	   :reader format-string)
   (position :documentation "The position at which the error occurs."
	     :initform nil
	     :initarg :position
	     ;; Same here. I'd have preferred to use POSITION...
	     :reader string-position))
  (:documentation "A format string error."))



;; ==========================================================================
;; Directive Parsing
;; ==========================================================================

(define-condition missing-string-directive (format-string-error)
  ()
  (:report (lambda (error stream)
	     (cl:format stream
		 "Missing directive character:~%  => ~S~%  => ~V@T^"
	       (format-string error)
	       (1+ (string-position error)))))
  (:documentation "A missing directive error."))

(define-condition spurious-parameter (format-string-error)
  ()
  (:report (lambda (error stream)
	     (cl:format stream "~
Spurious parameter found after ':' or '@' modifier:~%  => ~S~%  => ~V@T^"
	       (format-string error)
	       (1+ (string-position error)))))
  (:documentation "A spurious parameter error."))

(define-condition spurious-modifier (format-string-error)
  ((modifier :documentation "The modifier character."
	     :initarg :modifier
	     :reader modifier))
  (:report (lambda (error stream)
	     (cl:format stream "Spurious '~A' modifier:~%  => ~S~%  => ~V@T^"
	       (modifier error)
	       (format-string error)
	       (1+ (string-position error)))))
  (:documentation "A spurious modifier error."))


(defun directive-body-position (string start)
  "Return a STRING's directive body position from START.
START is the position of the tilde character, so this function essentially
parses the directive arguments in order to skip them."
  (loop :with end := (length string)
	:with index := (1+ start)
	:and colonp := nil
	:and atsignp := nil
	:for char := (if (< index end)
			 (schar string index)
		       (error 'missing-string-directive
			      :string string
			      :position start))
	:when (and (or colonp atsignp)
		   (or (char<= #\0 char #\9)
		       (member char '(#\- #\+ #\v #\V #\# #\' #\,))))
	  :do (error 'spurious-parameter
		     :string string
		     :position index)
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
		       (error 'missing-string-directive
			      :string string
			      :position start))
		     (skip-comma))
		    ((char= char #\,)
		     (incf index))
		    ((char= char #\:)
		     (if colonp
			 (error 'spurious-modifier
				:string string
				:position index
				:modifier #\:)
		       (setf colonp t))
		     (incf index))
		    ((char= char #\@)
		     (if atsignp
			 (error 'spurious-modifier
				:string string
				:position index
				:modifier #\@)
		       (setf atsignp t))
		     (incf index))
		    (t
		     (return index))))))



;; ==========================================================================
;; Directive Translation
;; ==========================================================================

(define-condition missing-delimiter (format-string-error)
  ((delimiter :documentation "The delimiter character."
	      :initarg :delimiter
	      :reader delimiter))
  (:report (lambda (error stream)
	     (cl:format stream
		 "Missing matching '~A' delimiter:~%  => ~S~%  => ~V@T^"
	       (delimiter error)
	       (format-string error)
	       (1+ (string-position error)))))
  (:documentation "A missing delimiter error."))

(defgeneric standard-directive-body (string position directive)
  (:documentation
   "Translate a STRING directive's body into a standard one.
- POSITION is the position of the directive's body (i.e., past the ~ character
  and the potential arguments).
- The translation is done according to DIRECTIVE.

Return two values:
- the translated directive's body as a string,
- the STRING's remainder index.")
  (:method (string position (directive standard-directive))
    "Method for one-character and grouping standard directives."
    (declare (ignore string))
    (values (make-string 1 :initial-element (directive-character directive))
	    (1+ position)))
  (:method (string position (directive standard-delimiting-directive))
    "Method for the / standard directive."
    (let ((character (schar string position)))
      (let ((closing-character (position character string
				  :start (1+ position))))
	(if closing-character
	    (values (cl:format nil "~C~A~C"
		      (directive-character directive)
		      (subseq string (1+ position) closing-character)
		      (directive-character directive))
		    (1+ closing-character))
	  (error 'missing-delimiter
		 :string string
		 :position position
		 :delimiter character)))))
  (:method (string position (directive function-directive))
    "Method for function directives."
    (let ((function-name (directive-function-name directive)))
      (values (cl:format nil "/~A::~A/"
		(package-name (symbol-package function-name))
		(symbol-name function-name))
	      (1+ position)))))


(define-condition missing-table-directive (format-string-error)
  ((table :documentation "The format table."
	  :initarg :table
	  :reader table))
  (:report (lambda (error stream)
	     (cl:format stream
	       "No such directive in table ~A.~%  => ~S~%  => ~V@T^"
	       (table error)
	       (format-string error)
	       (1+ (string-position error)))))
  (:documentation "A missing table directive error."))

(defun standard-directive
    (string position table
     &aux (body-position (directive-body-position string position))
	  (directive (gethash (schar string body-position)
			      (table-mappings table))))
"Translate a STRING directive into a standard one.
- POSITION is the position of the ~ character.
- The translation is done according to format TABLE.

Return two values:
- the translated directive as a string,
- the STRING's remainder index.

Note that the directive arguments are copied as-is. Only the directive's body
actually involves a translation."
  (unless directive
    (error 'missing-table-directive
	   :string string
	   :position position
	   :table table))
  (multiple-value-bind (translation remainder)
      (standard-directive-body string body-position directive)
    (values (concatenate 'string (subseq string position body-position)
			 translation)
	    remainder)))



;; ==========================================================================
;; String Translation
;; ==========================================================================

(defun next-directive-position (string start)
  "Return the next directive position in STRING from START, or nil."
  (position #\~ string :start start))

;; #### NOTE: the TABLE optional argument is currently unused.
(defun standard-format-string
    (format-control &optional (table *format-table*))
  "Return the translation of FORMAT-CONTROL into a standard one.
When FORMAT-CONTROL is a string, the translation is done according to format
TABLE (the current table by default). Otherwise, FORMAT-CONTROL is returned
as-is."
  (if (stringp format-control)
      (apply #'concatenate
	     'string
	     (loop :with position := 0
		   :and end := (length format-control)
		   :and directive := nil
		   :while (< position end)
		   :for next := (or (next-directive-position format-control
							     position)
				    end)
		   :if (> next position)
		     :collect (subseq format-control position next)
		     :and :do (setq position next)
		   :else
		     :do (multiple-value-setq (directive position)
			   (standard-directive format-control position table))
		     :and :collect directive))
      format-control))

;;; string.lisp ends here
