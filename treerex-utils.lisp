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

(declaim (inline trim-whitespace last-char first-char))

(defun trim-whitespace (s)
  "Removes leading and trailing whitespace from the string S,
or nil if S is nil."
  (unless (null s)
    (string-trim '(#\Space #\Tab) s)))

(defun last-char (s)
  "Returns the last character in the string S."
  (char s (1- (length s))))

(defun first-char (s)
  "Returns the first character in the string S."
  (char s 0))

;;{{{ ^-- CJK predicates

(declaim (inline ideographicp hangulp kanap katakana-only-p
                 hiragana-only-p kana-only-p))

(defun ideographicp (c)
  "Returns t if C is an ideographic character"
  (let ((cc (char-code c)))
    (or (<= #x4e00 cc #x9fff)
        (<= #x3400 cc #x4dbf)
        (<= #xf900 cc #xfaff))))

(defun hangulp (c)
  "Returns t if C is hangul"
  (<= #xac00 (char-code c) #xd7af))

(defun kanap (c)
  "Return t if C is hiragana or katakana."
  (let ((cc (char-code c)))
    (or (<= #x3040 cc #x309f)
        (<= #x30a0 cc #x30ff))))

(defun katakana-only-p (s)
  "Returns t if every character in S is katakana."
  (every #'(lambda (c) (<= #x30a0 (char-code c) #x30ff)) s))

(defun hiragana-only-p (s)
  "Returns t if every character in S is hiragana."
  (every #'(lambda (c) (<= #x3040 (char-code c) #x309f)) s))

(defun kana-only-p (s)
  "Returns T if every character in S is kana."
  (every #'(lambda (c) (kanap c)) s))

;;}}}
;;}}}

