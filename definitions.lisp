(in-package #:anaphoric-variants)

(defun %maybe-binding (maybe-var form template)
  (check-type maybe-var symbol)
  (if maybe-var
      `(let ((,maybe-var ,form))
         (declare (ignorable ,maybe-var))
         ,(funcall template maybe-var))
      (funcall template form)))

(defun %maybe-wrapper (wrapp template)
  (if wrapp
      template
      #'identity))

(defun %maybe-index-wrapper (index-var)
  (if index-var
      (let ((index 0))
        (lambda (forms)
          `(let ((,index-var ,(prog1 index (incf index))))
             (declare (ignorable ,index-var))
             ,@forms)))
      #'identity))

(defun %recursively (operator var forms)
  (if forms
      (reduce (lambda (form accumulated)
                (%maybe-binding var form
                                (lambda (value)
                                  `(,operator ,value ,accumulated))))
              forms :from-end t)
      (list operator)))

(define and (var) (&rest forms)
  (%recursively 'and var forms))

(define or (var) (&rest forms)
  (%recursively 'or var forms))

(define cond (test-var &key index) (&body clauses)
  (funcall (%maybe-wrapper test-var
                           (lambda (main)
                             `(let ((,test-var nil))
                                ,main)))
           (let ((update-wrapper
                  (%maybe-wrapper test-var
                                  (lambda (condition)
                                    `(setf ,test-var ,condition))))
                 (index-wrapper (%maybe-index-wrapper index)))
             `(cond
                ,@(map-bind
                   (mapcar) ((clause clauses))
                   (destructuring-bind (condition &body then) clause
                     `(,(funcall update-wrapper condition)
                        ,@(funcall index-wrapper then))))))))

(define if (var) (condition then &optional (else nil elsep))
  (%maybe-binding var condition
                  (lambda (value)
                    `(if ,value
                         ,then
                         ,@(when elsep (list else))))))

(define when (var) (condition &body forms)
  (%maybe-binding var condition
                  (lambda (value)
                    `(when ,value
                       ,@forms))))

(define unless (var) (condition &body forms)
  (%maybe-binding var condition
                  (lambda (value)
                    `(unless ,value
                       ,@forms))))

(define prog1 (var) (result &body body)
  (%maybe-binding var result
                  (lambda (value)
                    `(prog1 ,value
                       ,@body))))

(defun %listify (list-or-atom)
  (if (listp list-or-atom)
      list-or-atom
      (list list-or-atom)))

(defun %caselike (operator key-var test-var listify-keys-p index-var keyform cases)
  (check-type operator symbol)
  (check-type key-var symbol)
  (check-type test-var symbol)
  (check-type index-var symbol)
  (%maybe-binding
   key-var keyform
   (let* ((transform (lambda (keys forms)
                       `(,keys ,@forms)))
          (transform
           (if test-var
               (lambda (keys forms)
                 (funcall transform
                          keys
                          (list `(let ((,test-var
                                        ',(if listify-keys-p
                                              (%listify keys)
                                              keys)))
                                   (declare (ignorable ,test-var))
                                   ,@forms))))
               transform))
          (transform
           (if index-var
               (let ((index-wrapper (%maybe-index-wrapper index-var)))
                 (lambda (keys forms)
                   (funcall transform
                            keys
                            (list (funcall index-wrapper forms)))))
               transform)))
     (lambda (value)
       `(,operator ,value
                   ,@(map-bind
                      (mapcar) ((case cases))
                      (funcall transform (first case) (rest case))))))))


(define case (key &key keys index listify-keys-p) (keyform &body cases)
  (%caselike 'case key keys listify-keys-p index keyform cases))

(define ccase (key &key keys index listify-keys-p) (keyplace &body cases)
  (%caselike 'ccase key keys listify-keys-p index keyplace cases))

(define ecase (key &key keys index listify-keys-p) (keyform &body cases)
  (%caselike 'ecase key keys listify-keys-p index keyform cases))


(define typecase (key &key type index) (keyform &body cases)
  (%caselike 'typecase key type nil index keyform cases))

(define ctypecase (key &key type index) (keyplace &body cases)
  (%caselike 'ctypecase key type nil index keyplace cases))

(define etypecase (key &key type index) (keyform &body cases)
  (%caselike 'etypecase key type nil index keyform cases))
