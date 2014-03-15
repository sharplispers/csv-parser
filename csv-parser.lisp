;;                      -*- mode: lisp -*-
;;
;; CSV parsing/writing utilities, a la Microsoft Excel.
;;
;; Author: Alain Picard <apicard@optushome.com.au>
;;         (also alain.picard@memetrics.com)
;;
;; License:
;; This code is placed under the Lesser GNU Public License (LGPL)
;; (see http://www.fsf.org/licenses/lgpl.html) as
;; clarified for Lisp by Franz when they released AllegroServe (see
;; http://allegroserve.sourceforge.net/license-allogroserve.txt)
;;
;; What this clarification basically means is that compiling this
;; file and loading it into your lisp image, either at time of
;; delivery or runtime, does not make your program a derivative of
;; this one.  YOU ARE EXPRESSLY PERMITTED TO LOAD THIS FILE INTO
;; YOUR LISP IMAGE, AT ANY TIME, FOR ANY AND ALL (INCLUDING COMMERCIAL)
;; PURPOSES.  In particular, clause 5 of the LGPL is NOT invoked
;; by you embedding or loading this code, interpeted or compiled,
;; into your application.
;;
;; Of course, if you make modifications to this file, then the terms
;; of the LGPL hold, and you must redistribute the sources of this
;; file and your modifications with your application.
;;
;; Lastly, if you DO make useful changes to this code, I would
;; appreciate receiving the changes (though this NOT a requirement
;; of the license of this code.)
;;
;; This software is "as is", and has no warranty of any kind.  The
;; author assumes no responsibility for the consequences of any use
;; of this software.
;;
;;;;


(in-package :common-lisp-user)

(defpackage :csv-parser
  (:use :common-lisp)
  (:export  #:*field-separator*
	    #:*quote-character*
	    #:read-csv-line
	    #:do-csv-file
	    #:map-csv-file
	    #:write-csv-line))

(in-package :csv-parser)

(defparameter *field-separator* #\,
  "The character used to indicate the end of a field
   in a CSV file.")

(defparameter *quote-character* #\"
  "The character used to protect embedded field separators
   (usually commas) and whitespace within a field.

   To import the *quote-character* itself, you must have
   it printed twice in the input stream.")

(defvar *state* nil
  "Holds a function which knows how to handle chars based
   on what we've seen so far.")

(defvar *spaces-gobbled* nil
  "Keeps track of how many blanks have been skipped.  When emitting
   a field, we can trim extra right spaces, if appropriate, using this.")

(defvar *current-field* nil
  "Holds the field we are currently working on.")

(defvar *fields* nil
  "Holds the fields we have collected/parsed so far.")

(defvar *num-fields*
  "Holds the number of fields we have collected/parsed so far.")


;; Public
(defun read-csv-line (stream)
  "Read one line form a stream containing CSV data.
   Returns two values; a list of strings parsed, and
   the number of parsed values."
  (let ((*state* #'skip-white-space)
	(*spaces-gobbled* 0)
	(*fields* ())
	(*num-fields*  0)
	(*current-field* (make-empty-field)))
    (catch 'end-of-line
      (loop
       (funcall *state* (read-char stream nil :eof))))
    (values (nreverse *fields*)
	    *num-fields*)))

;; Public
(defun map-csv-file (file fn &key limit (skip-lines 0) (external-format :default)
                     ((:field-separator *field-separator*) *field-separator*)
                     ((:quote-character *quote-character*) *quote-character*))
  "Call FN (up to LIMIT times, if specified) with
   a list containing the fields parsed from the CSV
   file FILE.

   SKIP-LINES, if provided, is the number of lines to skip
   before starting to call FN.

   *FIELD-SEPARATOR* and *QUOTE-CHARACTER* can be bound to
   modify what separates fields and delimits fields."
  (with-open-file (stream file :direction :input :external-format external-format)
    (loop repeat skip-lines
          do (read-csv-line stream))
    (if limit
        (loop as line = (read-csv-line stream)
              while line
              repeat limit
              do (funcall fn line))
        (loop as line = (read-csv-line stream)
              while line
              do (funcall fn line)))))


;; Public
(defmacro do-csv-file (((fields num-fields) file &key limit (skip-lines 0)
                        (external-format :default)
                        ((:field-separator *field-separator*) *field-separator*)
                        ((:quote-character *quote-character*) *quote-character*))
                       &body body)
  "Repeatedly call BODY on CSV file FILE, binding
   FIELDS and NUM-FIELDS to a list containing the parsed fields,
   and the number of fields.
   Code runs inside a block with tagname NIL, so you
   may call (RETURN).

   *FIELD-SEPARATOR* and *QUOTE-CHARACTER* can be bound to
   modify what separates fields and delimits fields."
  (let ((stream (gensym "STREAM"))
	(count  (gensym "COUNT"))
	(glimit (gensym "LIMIT")))
    `(with-open-file (,stream ,file :direction :input :external-format ,external-format)
      (loop repeat ,skip-lines
            do     (read-csv-line ,stream))
      (loop for ,count upfrom 0
            with ,glimit = ,limit
            do
            (multiple-value-bind (,fields ,num-fields) (read-csv-line ,stream)
	      (when (or (null ,fields)
			(and ,glimit
			     (>= ,count ,glimit)))
		(return))
	      ,@body)))))


;;;; Utilities

(defun change-state (state)
  (setf *state*
	(ecase state
	  (:skip         #'skip-white-space)
	  (:first-quote  #'got-first-quote)
	  (:second-quote #'got-second-quote)
	  (:regular      #'regular-field))))

(declaim (inline add-char))
(defun add-char (char)
  (declare  (type character char))
  (vector-push-extend char *current-field*))

(defun make-empty-field ()
  (make-array 0
	      :fill-pointer 0
	      :adjustable   t
	      :element-type 'character))

(defun remove-last-n-chars (n)
  (setf (fill-pointer *current-field*)
	(- (length *current-field*)
	   n)))

(declaim (inline quote-char-p end-of-line-char-p end-of-field-char-p
		 white-space-char-p))
(defun quote-char-p (char)
  (char= char *quote-character*))

(defun end-of-line-char-p (char)
  (char= char #\Newline))

(defun end-of-field-char-p (char)
  (char= char *field-separator*))

(defun white-space-char-p (char)
  (or
   (char= char #\Space)
   (char= char #\Tab)
   (char= char #\Newline)
   (char= char #\Return)))	      ; For DOS style line termination


;;  States:
;;    * skip-white-space (initial state).
;;    * regular-field    handle things like ` foo bar '
;;    * got-first-quote  handle things like ` " foo X'
;;    * got-second-quote handle things like ` " foo  "X '
;;

(defun skip-white-space (char)
  (cond
    ((or (eq char :eof)
	 (end-of-line-char-p char))
     (when *fields*
       ;; If no fields are present, this was a completeley
       ;; blank line.  Otherwise, collect the last null field.
       (emit-field))
     (throw 'end-of-line nil))

    ((end-of-field-char-p char)
     ;; Careful to check for end-of-field _before_ whitespace,
     ;; as maybe TAB is the end-of-field marker.
     (emit-field))

    ((white-space-char-p char)
     ; skip
     nil)

    ((quote-char-p char)
     (change-state :first-quote))

    (t
     (change-state :regular)
     (add-char char))))

(defun regular-field (char)
  (cond
    ((or (eq char :eof)
	 (end-of-line-char-p char))
     (emit-field)
     (throw 'end-of-line nil))

    ((end-of-field-char-p char)
     (emit-field))

    ((white-space-char-p char)
     (add-char char)
     (incf *spaces-gobbled*))

    ((quote-char-p char)
     (error "Got a quote after regular characters; ~
             incorrectly formatted CSV file."))


    (t
     (setf *spaces-gobbled* 0)
     (add-char char))))

(defun got-first-quote (char)
  (cond
    ((eq char :eof)
     (error "Ran out of characters before finishing quoted field."))

    ((quote-char-p char)
     (change-state :second-quote))

    (t ; collect anything else
     (add-char char))))

(defun got-second-quote (char)
  (cond
    ((or (eq char :eof)
	 (end-of-line-char-p char))
     (emit-field)
     (throw 'end-of-line nil))

    ((quote-char-p char)
     ;; This is the weird embedded "" scenario
     (add-char char)
     (change-state :first-quote))

    ((end-of-field-char-p char)
     (emit-field))

    ((white-space-char-p char)
     (incf *spaces-gobbled*))

    (t
     (error "Got unexpected non-blank char after end of a quoted field"))))

(defun emit-field ()
  (cond
    ((eq *state* #'skip-white-space)
     (push nil *fields*))

    ((eq *state* #'got-second-quote)
     (push *current-field* *fields*))

    ((eq *state* #'regular-field)
     (remove-last-n-chars *spaces-gobbled*)
     (push *current-field* *fields*))

    (t
     (assert nil nil "Bug!")))

  (incf *num-fields*)
  (setf *spaces-gobbled* 0
	*state*          #'skip-white-space
	*current-field*  (make-empty-field)))



;;;; test harness
#+(or)
(trace skip-white-space got-first-quote got-second-quote regular-field
       emit-field add-char change-state remove-last-n-chars)

#+(or)(csv-tests)

;;  Call this; if nothing asserts, you win.
;;  (csv-tests)

(defun csv-tests ()
  (csv-test-blank)
  (csv-test-blanks)
  (csv-test-empty)
  (csv-test-empty-1)
  (csv-test-simple)
  (csv-test-quoted)
  (csv-test-space-handling)
  (csv-test-other-delimiters)
  (csv-test-embedded-lines)
  (csv-test-embedded-commas)
  (csv-test-round-trips))

(defun csv-test-blank ()
  (with-input-from-string (s "")
    (assert (eq nil (read-csv-line s)))
    (assert (eq nil (read-csv-line s)))))

(defun csv-test-blanks ()
  (with-input-from-string (s "  ")
    (assert (eq nil (read-csv-line s)))
    (assert (eq nil (read-csv-line s)))))

(defun csv-test-empty ()
  (with-input-from-string (s " ,,   , ")
    (assert (equal (list nil nil nil nil) (read-csv-line s)))
    (assert (eq nil (read-csv-line s)))))

;; Test due to Mahdu -- thanks!
(defun csv-test-empty-1 ()
 (let ((*field-separator* #\tab))
   (with-input-from-string
       (s (format nil " ~A~:*~A   ~:*~A " *field-separator*))
     (assert (equal (list nil nil nil nil) (read-csv-line s)))
     (assert (eq nil (read-csv-line s))))))

(defun csv-test-simple ()
  (with-input-from-string (s "foo,bar,baz")
    (assert (equal (list "foo" "bar" "baz")
		   (read-csv-line s)))))

(defun csv-test-quoted ()
  (with-input-from-string (s "\"foo\",\"bar\",\"baz\"")
    (assert (equal (list "foo" "bar" "baz")
		   (read-csv-line s)))))

(defun csv-test-space-handling ()
  ;; leading/trailing blanks (but not intra word blanks)
  ;; are stripped unless the whole thing is quoted
  (with-input-from-string (s "\"  foo  bar  \",  foo bar  ")
    (assert (equal (list "  foo  bar  " "foo bar")
		   (read-csv-line s)))))

(defun csv-test-other-delimiters ()
  (let ((*field-separator* #\|)
	(*quote-character* #\'))
    (with-input-from-string (s "'foo'|'bar'|'baz'")
      (assert (equal (list "foo" "bar" "baz")
		     (read-csv-line s))))))

(defun csv-test-double-quotes ()
  (with-input-from-string (s "\"foo \"\" bar\",2,3,\"\", \" \"")
    (assert (equal (list "foo \" bar" "2" "3" "" " ")
		   (read-csv-line s)))))

(defun csv-test-round-trip1 (row)
  (assert
   (equal row
          (with-input-from-string
              (in (with-output-to-string (out)
                    (write-csv-line out row)))
            (read-csv-line in)))))

(defun csv-test-round-trips ()
  (csv-test-round-trip1 (list "a" "b" "c"))
  (csv-test-round-trip1 (list "a" (string #\newline) "c"))
  (csv-test-round-trip1 (list "a" (string #\return) "c"))
  (csv-test-round-trip1 (list "a" (string #\tab) "c"))
  (csv-test-round-trip1 (list "a" "b" (string #\newline)))
  (csv-test-round-trip1 (list "a" nil "c"))
  (csv-test-round-trip1 (list "a" "" "c"))
  (csv-test-round-trip1 (list "a" "b" nil))
  (csv-test-round-trip1 (list "a" "b" "")))

(defun csv-test-embedded-lines ()
  (with-input-from-string (s "\"foo
bar\",\" 2 \",3")
    (assert (equal (list "foo
bar" " 2 " "3")
		   (read-csv-line s)))))

(defun csv-test-embedded-commas ()
  (with-input-from-string (s "\"foo , bar\",2,3")
    (assert (equal (list "foo , bar" "2" "3")
		   (read-csv-line s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Writing utilities

(defun write-csv-line (stream fields)
  "Write one CSV line to STREAM, containing fields.
   FIELDS is any (1d) sequence containing strings, symbols or numbers.

   Try to write it as esthetically pleasing as possible,
   i.e. don't output the *quote-character* unless necessary
   to protect the integrity of the data."

  (etypecase fields
    (cons   (write-csv-line-on-cons stream fields))
    (array  (write-csv-line-on-array stream fields))))

(defun write-csv-line-on-cons (stream fields)
  (loop for rest on fields
	while rest
	do
	(write-csv-field stream (first rest))
	(when (cdr rest)
	  (write-char *field-separator* stream)))
  (terpri stream))

(defun write-csv-line-on-array (stream fields)
  (loop for field across fields
	repeat (1- (length fields))
	do
	(write-csv-field stream field)
	(write-char *field-separator* stream)

	finally
	(write-csv-field stream (aref fields (1- (length fields))))
	(terpri stream)))

(defun write-csv-field (stream field)
  (etypecase field
    (null t)
    (number (princ field stream))
    (string (write-csv-string-safely stream field))
    (symbol (write-csv-string-safely stream (symbol-name field)))))

(defun special-char-p (char)
  (or (char= char *field-separator*)
      (char= char *quote-character*)
      (white-space-char-p char)))

(defun write-csv-string-safely (stream string)
  (if (or (find-if #'special-char-p string)
          (string= string ""))
      (write-protected-copy stream string)
      (princ string stream)))

(defun write-protected-copy (stream field)
  (write-char *quote-character* stream)
  (loop for c across field
	do
	(write-char c stream)
	(when (char= c *quote-character*)
	  ;; Double it
	  (write-char c stream)))
  (write-char *quote-character* stream))


;;; END OF FILE
