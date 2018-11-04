(in-package #:anaphoric-variants)

(defclass definitions-system (defsys:standard-system)
  ())

(defvar *definitions* (make-instance 'definitions-system))

(setf (defsys:locate (defsys:root-system) 'anaphoric)
      *definitions*)

(defgeneric form-lambda-list (object))
(defgeneric options-lambda-list (object))
(defgeneric expander (object))

(defclass definition () ())

(defclass standard-definition (definition defsys:name-mixin)
  ((%options-lambda-list :initarg :options-lambda-list
                         :reader options-lambda-list
                         :type list)
   (%form-lambda-list :initarg :form-lambda-list
                      :reader form-lambda-list
                      :type list)
   (%expander :initarg :expander
              :reader expander
              :type (or function symbol))
   (%atom-options-transformer :initarg :atom-options-transformer
                              :reader atom-options-transformer
                              :type (or function symbol)
                              :initform #'list)))

(defun %expand (options form &optional env)
  (check-type form cons)
  (let* ((operator (first form))
         (definition (defsys:locate *definitions* operator))
         (options
          (if (listp options)
              options
              (let ((transformed
                     (funcall (atom-options-transformer definition)
                              options)))
                (if (listp transformed)
                    transformed
                    (error "atom-options-transformer for ~S ~
                            returned ~S, which is not a list."
                           operator transformed))))))
    (funcall (expander definition) options form env)))

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

(defun %ensure (name form-lambda-list options-lambda-list expander)
  (setf (defsys:locate *definitions* name)
        (make-instance 'standard-definition
                       :name name
                       :options-lambda-list options-lambda-list
                       :form-lambda-list form-lambda-list
                       :expander expander)))

(defmethod defsys:expand-definition ((system definitions-system) name environment args &key)
  (destructuring-bind (options-lambda-list form-lambda-list &body body) args
    `(%ensure ',name
              ',options-lambda-list
              ',form-lambda-list
              ,(%make-expander name options-lambda-list form-lambda-list body))))

(defmacro anaphoric-variants:anaphoric (options &body form &environment env)
  (check-type form (cons t null))
  (%expand options (first form) env))
