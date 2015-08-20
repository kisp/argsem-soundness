;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :argsem-soundness)

(defvar *edges-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))

(defun edges (graph)
  (multiple-value-bind (value hit)
      (gethash graph *edges-cache*)
    (if hit
        value
        (setf (gethash graph *edges-cache*)
              (graph:edges graph)))))

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

(defun count-extensions (graph predicate)
  "Count how many extensions in the powerset of the nodes of graph
satisfy predicate. Predicate is expected to be binary, taking as
arguments graph and extension."
  (count-if (lambda (extension)
              (funcall predicate graph extension))
            (powerset (graph:nodes graph))))

(defun list-extensions (graph predicate)
  "List extensions contained in the powersetof the nodes of graph that
satisfy predicate. Predicate is expected to be binary, taking as
arguments graph and extension."
  (remove-if-not (lambda (extension)
                   (funcall predicate graph extension))
                 (powerset (graph:nodes graph))))

(defun extension-p (graph extension)
  (and (setp* extension)
       (subsetp* extension (graph:nodes graph))))

(defun conflict-free-extension-p (graph extension)
  (with-member* (extension)
    (and (extension-p graph extension)
         (notany (lambda* ((a b))
                   (and (member* a)
                        (member* b)))
                 (edges graph)))))

(defun acceptable-p (graph extension argument)
  (every (lambda* ((b a))
           (implies (eql a argument)
                    (some (lambda (g)
                            (graph:has-edge-p graph (list g b)))
                          extension)))
         (edges graph)))

(defun characteristic-function (graph extension)
  (remove-if-not (curry #'acceptable-p graph extension)
                 (graph:nodes graph)))

(defun characteristic-function-fixpoint-p (graph extension)
  (set-equal extension (characteristic-function graph extension)))

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
                        (edges graph)))
                (set-difference (graph:nodes graph) extension)))))

(defun preferred-extension-p (graph extension)
  (and (complete-extension-p graph extension)
       (every (lambda (set)
                (implies (and (not (set-equal extension set))
                              (subsetp* extension set))
                         (not (complete-extension-p graph set))))
              (powerset (graph:nodes graph)))))
