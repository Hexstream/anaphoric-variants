(cl:defpackage #:anaphoric-variants_tests
  (:use #:cl #:parachute)
  (:import-from #:anaphoric-variants #:anaphoric))

(cl:in-package #:anaphoric-variants_tests)

(define-test "featured examples"
  (is equal '(c d)
      (anaphoric tail
        (when (member 'b '(a b c d))
          (cdr tail))))
  (is equal '(0 list a b c)
      (anaphoric (key :type type :index index)
        (etypecase '(a b c)
          (list (list* index type key))
          (t (list index "Not a list." key)))))
  ;;; TODO: Test other anaphoric variants, and custom anaphoric variant definition.
  )
