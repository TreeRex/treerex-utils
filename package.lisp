;;;; package.lisp

(defpackage #:treerex-utils
  (:documentation "The treerex-utils package.")
  (:use #:cl #:cl-ppcre #:alexandria)
  (:nicknames :tru)
  (:export
   ;; File utilities
   #:sample-lines-from-file
   #:with-fields-in-file
   ;; Text Utilities
   #:trim-whitespace
   #:last-char
   #:first-char
   #+CCL #:get-utf-8-bytes-for-string
   #:decode-escaped-utf-8-string
   ;; CJK Text utilities
   #:ideographicp
   #:hangulp
   #:kanap
   #:katakana-only-p
   #:hiragana-only-p
   #:kana-only-p
   ;; XML/HTML utilities
   #:expand-character-entities
   ;; time utilities
   #:date-time-stamp
   ;; conditions
   #:unknown-entity-error               ;condition
   #:unknown-entity-string              ;accessor
   #:new-expansion                      ;restart
))

