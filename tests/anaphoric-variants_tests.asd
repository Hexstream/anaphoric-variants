(asdf:defsystem #:anaphoric-variants_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "anaphoric-variants unit tests."

  :depends-on ("anaphoric-variants"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:anaphoric-variants_tests)))
