(in-package #:anaphoric-variants)

(defmacro anaphoric (options &body form &environment env)
  (check-type form (cons t null))
  (anaphoric-variants:expand options (first form) env))
