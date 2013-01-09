;;;; treerex-utils.asd

(asdf:defsystem #:treerex-utils
  :serial t
  :description "Miscellaneous utilities I find myself using over and over..."
  :author "Tom Emerson <tremerson@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "treerex-utils")))

