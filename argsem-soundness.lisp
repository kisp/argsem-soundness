;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :argsem-soundness)

(defmacro with-member* ((list) &body body)
  (with-gensyms (hash)
    `(let ((,hash (make-hash-table)))
       (dolist (x ,list)
         (setf (gethash x ,hash) t))
       (macrolet ((member* (x) `(gethash ,x ,',hash)))
         ,@body))))

(defmacro member* (x)
  (declare (ignore x))
  (error "no member* env"))

(defun setp* (list)
  (let ((hash (make-hash-table)))
    (dolist (x list t)
      (if (gethash x hash)
          (return)
          (setf (gethash x hash) t)))))

(defun subsetp* (a b)
  (let ((hash (make-hash-table)))
    (dolist (x b) (setf (gethash x hash) t))
    (every (lambda (x) (gethash x hash)) a)))

(defmacro lambda* (args &body body)
  (with-gensyms (=args=)
    `(lambda (&rest ,=args=)
       (destructuring-bind ,args ,=args=
         ,@body))))

(defmacro implies (antecedent consequent)
  `(if ,antecedent ,consequent t))

(defun powerset (set)
  (if (null set)
      '(())
      (destructuring-bind (x . xs) set
        (let ((others (powerset xs)))
          (append others (mapcar (curry #'cons x) others))))))

(defun extension-p (graph extension)
  (and (setp* extension)
       (subsetp* extension (graph:nodes graph))))

(defun conflict-free-extension-p (graph extension)
  (with-member* (extension)
    (and (extension-p graph extension)
         (notany (lambda* ((a b))
                   (and (member* a)
                        (member* b)))
                 (graph:edges graph)))))

(defun acceptable-p (graph arguments argument)
  (every (lambda* ((b a))
           (implies (eql a argument)
                    (some (lambda (g)
                            (graph:has-edge-p graph (list g b)))
                          arguments)))
         (graph:edges graph)))

(defun admissible-extension-p (graph extension)
  (and (conflict-free-extension-p graph extension)
       (every (curry #'acceptable-p graph extension)
              extension)))

(defun complete-extension-p (graph extension)
  (with-member* (extension)
    (and (admissible-extension-p graph extension)
         (every (lambda (argument)
                  (implies (acceptable-p graph extension argument)
                           (member* argument)))
                (graph:nodes graph)))))

(defun grounded-extension-p (graph extension)
  (and (complete-extension-p graph extension)
       (let ((sub-extensions
               (remove extension (powerset extension) :test #'set-equal)))
         (notany (curry #'complete-extension-p graph) sub-extensions))))

(defun stable-extension-p (graph extension)
  (with-member* (extension)
    (and (conflict-free-extension-p graph extension)
         (every (lambda (argument)
                  (some (lambda* ((b a))
                          (and (eql a argument)
                               (member* b)))
                        (graph:edges graph)))
                (set-difference (graph:nodes graph) extension)))))

(defun preferred-extension-p (graph extension)
  (and (complete-extension-p graph extension)
       (every (lambda (set)
                (implies (and (not (set-equal extension set))
                              (subsetp* extension set))
                         (not (complete-extension-p graph set))))
              (powerset (graph:nodes graph)))))
