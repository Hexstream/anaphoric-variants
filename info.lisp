(in-package #:anaphoric-variants)

(defvar *infos* (make-hash-table :test 'eq))

(defgeneric anaphoric-variants:name (object))
(defgeneric anaphoric-variants:form-lambda-list (object))
(defgeneric anaphoric-variants:options-lambda-list (object))
(defgeneric anaphoric-variants:expander (object))

(defclass anaphoric-variants:info () ())

(defclass anaphoric-variants:standard-info (info)
  ((%name :initarg :name
          :reader anaphoric-variants:name
          :type symbol)
   (%options-lambda-list :initarg :options-lambda-list
                         :reader anaphoric-variants:options-lambda-list
                         :type list)
   (%form-lambda-list :initarg :form-lambda-list
                      :reader anaphoric-variants:form-lambda-list
                      :type list)
   (%expander :initarg :expander
              :reader anaphoric-variants:expander
              :type (or function symbol))
   (%atom-options-transformer :initarg :atom-options-transformer
                              :reader anaphoric-variants:atom-options-transformer
                              :type (or function symbol)
                              :initform #'list)))

(defun anaphoric-variants:locate (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *infos*)
      (and errorp
           (error "No anaphora kind with name ~S." name))))

(defun (setf %locate) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type name symbol)
  (check-type new info)
  (setf (gethash name *infos*) new))

(defun anaphoric-variants:expand (options form &optional env)
  (check-type form cons)
  (let* ((operator (first form))
         (info (anaphoric-variants:locate operator))
         (options
          (if (listp options)
              options
              (let ((transformed
                     (funcall (anaphoric-variants:atom-options-transformer info)
                              options)))
                (if (listp transformed)
                    transformed
                    (error "atom-options-transformer for ~S ~
                            returned ~S, which is not a list."
                           operator transformed))))))
    (funcall (anaphoric-variants:expander info) options form env)))

;; Not robust in the face of misplaced &environment.
(defun %extract-&environment (macro-lambda-list)
  '(values env-var ordinary-lambda-list)
  (let ((tail (member '&environment macro-lambda-list)))
    (cond (tail
           (when (member '&environment tail)
             (error "More than one ~S parameter in ~S."
                    '&environment macro-lambda-list))
           (values (second tail)
                   (append (ldiff macro-lambda-list tail)
                           (cddr tail))))
          (t (values nil macro-lambda-list)))))

(defun %check-expected-operator (actual expected)
  (unless (eq actual expected)
    (error "Wrong operator ~S, expected ~S." actual expected)))

(defun %make-expander (name options-lambda-list form-lambda-list body)
  (let ((options-var (gensym (string '#:options)))
        (form-var (gensym (string '#:form)))
        (operator-var (gensym (string '#:operator))))
    (multiple-value-bind (options-env-var
                          options-lambda-list
                          form-env-var
                          form-lambda-list)
        (multiple-value-call #'values
          (%extract-&environment options-lambda-list)
          (%extract-&environment form-lambda-list))
      (let* ((env-var (gensym (string '#:env)))
             (options-env-template
              (if options-env-var
                  (lambda (fill-in)
                    (list `(let ((,options-env-var ,env-var))
                             ,@fill-in)))
                  #'identity))
             (form-env-template
              (if form-env-var
                  (lambda (fill-in)
                    (list `(let ((,form-env-var ,env-var))
                             ,@fill-in)))
                  #'identity)))
        `(lambda (,options-var ,form-var ,env-var)
           ,@(unless (or options-env-var form-env-var)
                     (list `(declare (ignore ,env-var))))
           (let ((,operator-var (first ,form-var)))
             (%check-expected-operator ,operator-var ',name)
             (destructuring-bind ,options-lambda-list ,options-var
               ,@(funcall
                  options-env-template
                  `((destructuring-bind ,form-lambda-list (rest ,form-var)
                      ,@(funcall form-env-template body)))))))))))

(defun %remove-keys (keys plist)
  (let ((keys (if (listp keys) keys (list keys)))
        (processp nil))
    (map-bind (mapcan) ((key plist) (value (cdr plist)))
      (when (setf processp (not processp))
        (unless (member key keys)
          (list key value))))))

(defun anaphoric-variants:ensure
    (name form-lambda-list options-lambda-list expander
     &rest keys &key (class 'anaphoric-variants:standard-info) &allow-other-keys)
  (setf (%locate name)
        (apply #'make-instance class
               :name name
               :options-lambda-list options-lambda-list
               :form-lambda-list form-lambda-list
               :expander expander
               (%remove-keys :class keys))))

(defmacro anaphoric-variants:define
    (name options-lambda-list form-lambda-list &body body)
  `(anaphoric-variants:ensure
    ',name
    ',options-lambda-list
    ',form-lambda-list
    ,(%make-expander name options-lambda-list form-lambda-list body)))
