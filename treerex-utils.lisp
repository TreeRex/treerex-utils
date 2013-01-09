;;;; -*- mode:Lisp folded-file:t -*-

(in-package #:treerex-utils)

;;{{{ File Utilities

(defmacro with-delimited-file ((var separator filename) &body body)
  (let ((in (gensym))
        (line (gensym)))
    `(with-open-file (,in ,filename :direction :input
                          :external-format
                          #+(or sbcl lispworks) :utf-8
                          #+ccl '(:character-encoding :utf-8))
       (loop for ,line = (read-line ,in nil)
            while ,line
          do (let ((,var (split ,separator ,line)))
               ,@body)))))

;;}}}

;;{{{ Text Utilities

(defun ideographicp (c)
  "Returns t if C is an ideographic character"
  (let ((cc (char-code c)))
    (or (<= #x4e00 cc #x9fff)
        (<= #x3400 cc #x4dbf)
        (<= #xf900 cc #xfaff))))

(defun hangulp (c)
  "Returns t if C is hangul"
  (<= #xac00 (char-code c) #xd7af))

;;}}}

