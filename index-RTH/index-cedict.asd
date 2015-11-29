;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-
;;;

(asdf:defsystem :index-cedict
  :description ""
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               ;;(:file "miscutils")
               (:file "index-cedict")
               )
  :depends-on (:split-sequence :babel :cl-ppcre))
