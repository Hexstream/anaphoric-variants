(cl:defpackage #:anaphoric-variants
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:anaphoric ; import this single symbol for normal usage.

           #:info
           #:standard-info
           #:name
           #:form-lambda-list
           #:options-lambda-list
           #:expander
           #:atom-options-transformer
           #:locate
           #:expand
           #:ensure
           #:define))
