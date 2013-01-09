;;;; package.lisp

(defpackage #:treerex-utils
  (:documentation "The treerex-utils package.")
  (:use #:cl)
  (:shadowing-import-from #:cl-ppcre
                          #:split)
  (:export #:with-delimited-file
           #:ideographicp
           #:hangulp))

