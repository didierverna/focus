;;; table.lisp --- Format tables management

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
;; Current Table Management
;; ==========================================================================

(defvar *format-table* nil
  "The current format table.")

#|
(defmacro in-format-table (table-or-name)
  "Set the current format table to TABLE-OR-NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *format-table* (find-table ,table-or-name))))
|#

(defmacro with-format-table (table-or-name &body body)
  "Execute BODY with the current format table bound to TABLE-OR-NAME."
  `(let ((*format-table* (find-table ,table-or-name)))
     ,@body))



;; ==========================================================================
;; Table Data Structure
;; ==========================================================================

(defstruct (format-table
	    (:constructor %make-format-table)
	    (:conc-name table-))
  "Structure for format tables.
This structure holds the MAPPINGS from characters to directives."
  (mappings (make-hash-table) :type hash-table))

(define-condition format-table-error (focus-error)
  ((table :documentation "The format table."
	  :initarg :table
	  :reader table))
  (:documentation "A format table error."))

(defmethod print-object ((table format-table) stream
			 &aux (name (or (table-name table) ;; forward decl.
					"(UNREGISTERED)")))
  "Add format TABLE's name, if registered, to its printed representation."
  (print-unreadable-object (table stream :type t :identity t)
    (princ name stream)))

(defun make-format-table (&optional (initially :standard)
			  &aux (table (%make-format-table))
			       (mappings (table-mappings table)))
  "Create and return a new format table.
The table may be INITIALLY :standard (the default), :standard-upcase,
:standard-downcase or :blank."
  (ecase initially
    (:standard
     (dolist (directive +standard-directives+)
       (let ((char (directive-character directive)))
	 (setf (gethash char mappings) directive)
	 (when (both-case-p char)
	   (setf (gethash (char-downcase char) mappings) directive)))))
    (:standard-upcase
     (dolist (directive +standard-directives+)
       (setf (gethash (directive-character directive) mappings) directive)))
    (:standard-downcase
     (dolist (directive +standard-directives+)
       (let ((char (directive-character directive)))
	 (when (both-case-p char)
	   (setq char (char-downcase char)))
	 (setf (gethash char mappings) directive))))
    (:blank))
  table)



;; ==========================================================================
;; Table Lookup and (un)Registration
;; ==========================================================================

(define-condition table-lookup-error (focus-error)
  ((name :documentation "The looked up name."
	 :initarg :name
	 :reader name))
  (:documentation "A format table lookup error."))

(define-condition missing-table (table-lookup-error)
  ()
  (:report (lambda (error stream)
	     (cl:format stream "\
~A: no format table registered under that name." (name error))))
  (:documentation "A missing format table error."))

(define-condition table-collision (table-lookup-error)
  ()
  (:report (lambda (error stream)
	     (cl:format stream "~A: table name already in use."
			(name error))))
  (:documentation "A table collision error."))


(let ((format-tables (make-hash-table)))
  (defun lookup-table
      (name &optional (errorp t) &aux (table (gethash name format-tables)))
    "Look for a format table registered under NAME.
ERRORP (the default) means to throw a TABLE-NOT-REGISTERED error if no such
table is found. Otherwise, just return nil."
    (or table (when errorp (error 'missing-table :name name))))

  (defun table-name (table)
    "Find TABLE's name if registered, return nil otherwise."
    (loop :for key :being :the :hash-keys :in format-tables
	    :using (:hash-value value)
	  :when (eq table value)
	    :do (return key)))

  (defun register-format-table (table name &optional force)
    "Register TABLE under NAME (a symbol) and return it.
FORCE means overwrite an already existing registration under that
name. Otherwise (the default), throw a TABLE-COLLISION error."
    (when (and (lookup-table name nil) (not force))
      (error 'table-collision :name name))
    (setf (gethash name format-tables) table))

  (defun unregister-format-table (name)
    "Unregister NAMEd table."
    (remhash name format-tables)))


(defun find-table (table-or-name)
  "Return its table argument directly, or look it up by name."
  (if (format-table-p table-or-name)
      table-or-name
      (lookup-table table-or-name)))



;; ==========================================================================
;; Table Contents Management
;; ==========================================================================

;; #### NOTE: this is pedantic since we only have one sub-condition...
(define-condition table-directive-error (format-table-error)
  ((character :documentation "The directive character."
	      :initarg :character
	      ;; The lack of polymorphism on standard functions sucks. I want
	      ;; to use just CHARACTER here.
	      :reader table-character))
  (:documentation "A table directive error."))

(define-condition table-directive-collision (table-directive-error)
  ()
  (:report (lambda (error stream)
	     (cl:format stream "~~~A: directive already exists in table ~A."
	       (table-character error)
	       (table error))))
  (:documentation "A table directive collision error."))

(defpackage :net.didierverna.focus.user-functions
  (:documentation "The FoCus user functions package.
This package is used to store anonymous user functions (as provided to
SET-FORMAT-DIRECTIVE) under gentemp'ed names."))

(defun set-format-directive
    (char &key standard ((:function function-or-name))
	       (both-case t) force ((:table table-or-name) *format-table*)
	  &aux (table (find-table table-or-name)))
  "Set a ~CHAR directive in TABLE.
- TABLE (the current format table by default) may be a table or a table name.
- When BOTH-CASE (the default), operate on both case versions of CHAR.
- Attempting to override an existing directive throws a
  TABLE-DIRECTIVE-COLLISION error, unless FORCE is non-nil.

The operation to perform is as follows:
- If FUNCTION is provided (either a function or a function name), associate
  CHAR with it.
- If STANDARD is provided, associate CHAR with the standard directive denoted
  by STANDARD character (case does not matter).
- Otherwise, remove the ~CHAR directive from TABLE."
  (let ((mappings (table-mappings table))
	(other-char (when both-case (other-case char))))
    (cond ((or function-or-name standard)
	   (when (and (gethash char mappings) (not force))
	     (error 'table-directive-collision
	       :table table :character char))
	   (when (and other-char (gethash other-char mappings) (not force))
	     (error 'table-directive-collision
	       :table table :character other-char))
	   ;; #### NOTE: to be pedantic, we could enforce mutual exclusion of
	   ;; :STANDARD and :FUNCTION. Instead, we simply give precedence to
	   ;; the latter.
	   (let ((directive
		   (cond ((functionp function-or-name)
			  (let ((name
				  (gentemp "USER-FUNCTION"
					   (find-package :net.didierverna.focus.user-functions))))
			    (setf (symbol-function name) function-or-name)
			    (make-function-directive :function-name name)))
			 ((and function-or-name (symbolp function-or-name))
			  (make-function-directive
			   :function-name function-or-name))
			 (t
			  (find-standard-directive standard)))))
	     (setf (gethash char mappings) directive)
	     (when other-char
	       (setf (gethash other-char mappings) directive))))
	  (t
	   ;; #### NOTE: to be pedantic, we could check that there's actually
	   ;; a directive mapped to that name, before removal. This would add
	   ;; a second sub-condition to TABLE-DIRECTIVE-ERROR.
	   (let ((directive (gethash char mappings)))
	     (when (and (function-directive-p directive)
			(eq (symbol-package
			     (directive-function-name directive))
			    (find-package
			     :net.didierverna.focus.user-functions)))
	       (fmakunbound (directive-function-name directive))
	       (unintern (directive-function-name directive)
			 (symbol-package
			  (directive-function-name directive)))))
	   (remhash char mappings)
	   (when other-char
	     (remhash other-char mappings))))))

;;; table.lisp ends here
