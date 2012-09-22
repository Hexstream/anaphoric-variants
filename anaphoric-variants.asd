(asdf:defsystem #:anaphoric-variants

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Gives access to anaphoric variants of operators through one macro: ANAPHORIC. The user explicitly provides a variable name, preserving sanity, in contrast to the traditional use of an evil implicit variable (\"IT\"). Some operators can bind additional handy variables when explicitly requested."

  :depends-on (#:map-bind)

  :version "1.0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "definitions")
	       (:file "main")))
