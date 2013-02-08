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

;; this does not handle surrogate pairs or characters outside the BMP
(defun get-utf-8-bytes-for-string (s)
  "Return a list of the bytes in the UTF-8 encoding for S."
  (loop for c across s
       with result = '()
       do (let ((usv (char-code c)))
            (cond ((< usv #x80)
                   (push usv result))
                  ((< usv #x800)
                   (push (logior #b11000000 (ldb (byte 5 6) usv)) result)
                   (push (logior #b10000000 (ldb (byte 6 0) usv)) result))
                  ((< usv #x10000)
                   (push (logior #b11100000 (ldb (byte 4 12) usv)) result)
                   (push (logior #b10000000 (ldb (byte 6 6) usv)) result)
                   (push (logior #b10000000 (ldb (byte 6 0) usv)) result))
                  (t (error "Characters outside the BMP are not supported"))))
       finally (return (nreverse result))))

#+CCL
(defun decode-escaped-utf-8-string (s)
  "Given a string S containing ASCII characters and escaped 8-bit characters in the form
\[A-F0-9]{2} this function converts it into UTF-8."
  (let ((array (make-array 128 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))
    (cl-ppcre:do-matches-as-strings (m "(\\\\[A-F0-9]{2})|([^\\\\]{1})" s)
      (cond ((char= (char m 0) #\\)
             (vector-push-extend (parse-integer m :start 1 :radix 16) array))
            (t (vector-push-extend (char-code (char m 0)) array))))
    (ccl::decode-string-from-octets array :external-format :UTF-8)))

;;{{{ ^-- CJK predicates

(declaim (inline ideographicp hangulp kanap katakana-only-p
                 hiragana-only-p kana-only-p))

(defun char--code (c)
  (etypecase c
    (character (char-code c))
    (string    (char-code (char c 0)))
    (integer   c)))

(defun ideographicp (c)
  "Returns t if C is an ideographic character"
  (let ((cc (char--code c)))
    (or (<= #x4e00 cc #x9fff)
        (<= #x3400 cc #x4dbf)
        (<= #xf900 cc #xfaff))))

(defun hangulp (c)
  "Returns t if C is hangul"
  (<= #xac00 (char--code c) #xd7af))

(defun hiragana-p (c)
  "Returns t if C is hiragana."
  (<= #x3040 (char--code c) #x309f))


(defun kanap (c)
  "Return t if C is hiragana or katakana."
  (let ((cc (char--code c)))
    (or (<= #x3040 cc #x309f)
        (<= #x30a0 cc #x30ff))))

(defun katakana-only-p (s)
  "Returns t if every character in S is katakana."
  (every #'(lambda (c) (<= #x30a0 (char--code c) #x30ff)) s))

(defun hiragana-only-p (s)
  "Returns t if every character in S is hiragana."
  (every #'(lambda (c) (<= #x3040 (char--code c) #x309f)) s))

(defun kana-only-p (s)
  "Returns T if every character in S is kana."
  (every #'(lambda (c) (kanap c)) s))

;;}}}

;;{{{ ^-- XML/HTML Related

(define-condition unknown-entity-error (error)
  ((entity :initarg :entity :reader entity)))

(defun expand-numeric-entity (entity)
  (let ((hexp (char-equal #\x (char entity 1))))
    (string (code-char (parse-integer entity
                                      :start (if hexp 2 1)
                                      :radix (if hexp 16 10))))))

(defun entity-expander (match entity)
  (declare (ignore match))
  (cond ((char= #\# (char entity 0))
         (expand-numeric-entity entity))
        (t (let ((expansion (gethash entity *ENTITY-NAMES*)))
             (if (not expansion)
                 (error 'unknown-entity-error :entity entity)
                 expansion)))))

(defun expand-character-entities (s)
  "Expands the character entities in S. The regular expression used to match
entities is quite forgiving, insofar as the trailing ';' is optional and the
number of numerals/letters allowed in a numeric entity is unbounded. This
reflects the real-world usage where the data coming in may be corrupted or
otherwise bogus. It is left to the expander to deal with the actual values."
  (cl-ppcre:regex-replace-all "&((?i:[a-z]+)|(?i:#x?[0-9a-fA-F]+));?" s
                              #'entity-expander :simple-calls t))

;;}}}

;;}}}

;;{{{ Date/Time Utilities

(defun date-time-stamp ()
  "Returns a string with the current date and time."
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p day-of-week second))
    (format nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d GMT~@d" year month date hour minute (- tz))))

;;}}}
