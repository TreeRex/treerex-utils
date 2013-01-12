;;;; package.lisp

(defpackage #:treerex-utils
  (:documentation "The treerex-utils package.")
  (:use #:cl)
  (:shadowing-import-from #:cl-ppcre
                          #:split)
  (:export
   ;; File utilities
   #:with-delimited-file
   ;; Text Utilities
   #:trim-whitespace
   #:last-char
   #:first-char
   ;; CJK Text Utilities
   #:ideographicp
   #:hangulp
   #:kanap
   #:katakana-only-p
   #:hiragana-only-p
   #:kana-only-p
))

