;;; table.lisp --- FORMAT tables management

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
;; Table Data Structure
;; ==========================================================================

(defstruct (format-table
	    (:constructor %make-format-table)
	    (:conc-name table-))
  "Structure for FORMAT tables.
This structure holds its name and mappings from characters to directives."
  (name nil)
  (mappings (make-hash-table) :type hash-table))

(defun table-id (table)
  "Return TABLE's name or an identification string if anonymous."
  (or (table-name table)
      (with-output-to-string (str)
	(print-unreadable-object (table str :type t :identity t)
	  (princ "(ANONYMOUS)" str)))))



;; ==========================================================================
;; Table Lookup and (un)Registration
;; ==========================================================================

(defvar *format-tables* (make-hash-table)
  "The collection of all named FORMAT tables.")


(define-condition format-table-lookup-error (focus-error)
  ((name :documentation "The table name."
	 :initarg :name
	 :reader name))
  (:documentation "A format table lookup error."))

(define-condition existing-table (format-table-lookup-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Table ~S already exists." (name error))))
  (:documentation "An existing table error."))

(define-condition missing-table (format-table-lookup-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Table ~S not found." (name error))))
  (:documentation "A missing table error."))


(defun find-format-table (table &optional (errorp t))
  "Find a registered TABLE.
If TABLE already is a FORMAT table, just return it.
Otherwise, TABLE should be a symbol naming a registered table.

If a table is not found and ERRORP is non-nil (the default), signal an error.
Otherwise, just return nil."
  (if (format-table-p table)
      table
    (let ((the-table (gethash table *format-tables*)))
      (or the-table
	  (and errorp (error 'missing-table :name table))))))


(define-condition format-table-registration (focus-error)
  ((table :documentation "The table."
	  :initarg :table
	  :reader table))
  (:report (lambda (error stream)
	     (format stream
		 "Table ~A cannot be (un)registered because it is anonymous."
	       (table-id (table error)))))
  (:documentation "A table (un)registration error."))


(defun do-register-format-table (table)
  "Register named TABLE."
  (setf (gethash (table-name table) *format-tables*) table))

(defun register-format-table (table &aux (name (table-name table)))
  "Register TABLE.
TABLE must have a name."
  (unless name
    (error 'format-table-registration :table table))
  (do-register-format-table table))

(defun do-unregister-format-table (name)
  "Unregister NAMEd table."
  (remhash name *format-tables*))

(defun unregister-format-table (table &aux (name (if (format-table-p table)
						     (table-name table)
						   table)))
  "Unregister TABLE.
TABLE may be either a named format table, or a table name."
  (unless name
    (error 'format-table-registration :table table))
  (do-unregister-format-table name))



;; ==========================================================================
;; Table Creation
;; ==========================================================================

(defun make-format-table (&key name (initially :standard) (make-current t))
  "Create and return a new FORMAT table.
- NAME must be a symbol. If non-nil, the table is automatically registered.
  Otherwise, the table is considered to be anonymous and you must retain a
  reference to it yourself.
- The table may be INITIALLY :standard (default) or :blank.
- If MAKE-CURRENT, make the new table current."
  (when (and name (find-format-table name nil))
    (error 'existing-table :name name))
  (let* ((table (%make-format-table :name name))
	 (mappings (table-mappings table)))
    (when (eq initially :standard)
      (dolist (directive +standard-directives+)
	(let ((char (directive-character directive)))
	  (setf (gethash char mappings) directive)
	  (when (both-case-p char)
	    (setf (gethash (char-downcase char) mappings) directive)))))
    (when name
      (do-register-format-table table))
    (when make-current
      ;; forward-use of *FORMAT-TABLE*
      (setq *format-table* table))
    table))



;; ==========================================================================
;; Current Table Management
;; ==========================================================================

(defvar *format-table* (make-format-table :name 'default
					  ;; no need to set it twice ;-)
					  :make-current nil)
  "The current FORMAT table.
This variable behaves as *READTABLE* and *PACKAGE* with respect to LOAD and
COMPILE-FILE.")

(defmacro in-format-table (name)
  "Set the current FORMAT table to NAMEd table."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *format-table* (find-format-table ',name))))

(defmacro in-format-table* (table)
  "Set the current format table to TABLE."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *format-table* ,table)))

(defmacro with-format-table (name &body body)
  "Execute BODY with current format table bound to NAMEd table."
  `(let ((*format-table* (find-format-table ,name)))
     ,@body))

(defmacro with-format-table* (table &body body)
  "Execute BODY with current format table bound to TABLE."
  `(let ((*format-table* ,table))
     ,@body))



;; ==========================================================================
;; Table Contents Management
;; ==========================================================================

(define-condition format-table-error (focus-error)
  ((table :documentation "The format table related to the error."
	  :initarg :table
	  :reader table))
  (:documentation "An error related to a FORMAT table."))

(define-condition format-table-directive-error (format-table-error)
  ((character :documentation "The directive character."
	      :initarg :directive
	      :reader directive))
  (:documentation "A FORMAT table directive error."))

(define-condition existing-directive (format-table-directive-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Directive ~~~A already exists in table ~A."
	       (directive error)
	       (table-id (table error)))))
  (:documentation "An existing directive error."))

(define-condition unknown-directive (format-table-directive-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Directive ~~~A is unknown in table ~A."
	       (directive error)
	       (table-id (table error)))))
  (:documentation "An unknown directive error."))


(defun set-format-directive
    (char &key standard function (both-case t) force (table *format-table*)
	  &aux (table (find-format-table table)))
  "Set a CHAR directive in TABLE.
- TABLE (the current format table by default) may be a table or a table name.
- When BOTH-CASE (the default), operate on both case versions of CHAR.
- Attempting to override an existing directive signals an error, unless FORCE
  is non-nil.

The operation to perform is as follows:
- If FUNCTION is provided, associate CHAR with FUNCTION name.
- If STANDARD is provided, associate CHAR with the standard directive denoted
  by STANDARD character. Case does not matter.
- Otherwise, remove CHAR directive."
  (let ((mappings (table-mappings table))
	(other-char (when both-case (other-case char))))
    (cond ((or function standard)
	   (when (and (gethash char mappings) (not force))
	     (error 'existing-directive :directive char :table table))
	   (when (and other-char (gethash other-char mappings) (not force))
	     (error 'existing-directive :directive other-char :table table))
	   (let ((directive (if function
				(make-function-directive :function function)
			      (find-standard-directive standard))))
	     (setf (gethash char mappings) directive)
	     (when other-char
	       (setf (gethash other-char mappings) directive))))
	  (t
	   (remhash char mappings)
	   (when other-char
	     (remhash other-char mappings))))))

;;; table.lisp ends here
