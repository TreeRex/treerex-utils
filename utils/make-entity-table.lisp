;;;; make-entity-table.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; Generate lookup table from W3C entity definitions file
;;;; Author: temerson (Tom Emerson)
;;;; Date: 2013-02-06

(in-package #:treerex-utils)

;;; The W3C Recommendation "XML Entity Definitions for Characters"
;;; <http://www.w3.org/TR/xml-entity-names/> defines the sets of character
;;; names and their mappings to Unicode.
;;;
;;; The combined entity file can be downloaded from the W3C:
;;; 
;;; curl -O http://www.w3.org/2003/entities/2007/w3centities-f.ent
;;; 
;;; The W3C table has some invalid values, so we special case them with
;;; +PATCHED_ENTITIES+

(defconstant +PATCHED-ENTITIES+ '(("amp" . "&") ("lt" . "<") ("nvlt" . "<⃒")))

(defun get-exception-mapping (s)
  (cdr (assoc s +PATCHED-ENTITIES+ :test #'string-equal)))

(defun expand-numeric-entities (s bmp-only)
  (when s
    (let (result)
      (cl-ppcre:do-register-groups ((#'(lambda (x) (parse-integer x :radix 16)) usv))
          ("(?i)&#x([0-9a-f]+);" s)
        (push (code-char usv) result))
      (when (or (not bmp-only) (every #'(lambda (v) (char< v #\U+10000)) result))
        (funcall #'concatenate 'string (nreverse result))))))

(defun read-entities (entity-file bmp-only &optional (stream *standard-output*))
  (let ((entity-regex
         (cl-ppcre:create-scanner "^<!ENTITY +([a-z]+) +\"([^\"]+)\"" :case-insensitive-mode t)))
    (with-open-file (in entity-file :direction :input)
      (loop for line = (read-line in nil)
           while line do
           (multiple-value-bind (match registers)
               (cl-ppcre:scan-to-strings entity-regex line)
             (declare (ignore match))
             (let ((entities (and registers
                                  (or (get-exception-mapping (aref registers 0))
                                      (expand-numeric-entities (aref registers 1) bmp-only)))))
               (when entities
                 (format stream "         (\"~A\" . ~S)~%" (aref registers 0) entities))))))))


(defun make-entity-table (entity-file output-file &key (bmp-only t))
  (with-open-file (out output-file :direction :output :if-exists :supersede
                       :external-format '(:character-encoding :utf-8))
    (format out ";;;; ~A  -*- mode: lisp; coding: utf-8 -*-~%" output-file)
    (format out ";;;;~%")
    (format out ";;;; Machine generated from ~A on ~A~%" entity-file (date-time-stamp))
    (terpri out)
    (format out "(in-package #:treerex-utils)~%")
    (terpri out)
    (format out "(defvar *ENTITY-NAMES* (make-hash-table :test 'equal))~%")
    (terpri out)
    (format out "(let ((entities~%       '(~%")
    (read-entities entity-file bmp-only out)
    (format out "         )))~%")
    (format out "  (dolist (cons entities)~%")
    (format out "    (setf (gethash (car cons) *ENTITY-NAMES*) (cdr cons))))~%")
    (terpri out)
    (format out ";;;; ~A ends here.~%" output-file)))
