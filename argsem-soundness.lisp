;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :argsem-soundness)

(defvar *edges-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))

(declaim (ftype (function (t) (values list)) edges))
(defun edges (graph)
  (multiple-value-bind (value hit)
      (gethash graph *edges-cache*)
    (if hit
        value
        (setf (gethash graph *edges-cache*)
              (graph:edges graph)))))

(defvar *nodes-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))

(declaim (ftype (function (t) (values list)) nodes))
(defun nodes (graph)
  (multiple-value-bind (value hit)
      (gethash graph *nodes-cache*)
    (if hit
        value
        (setf (gethash graph *nodes-cache*)
              (graph:nodes graph)))))

(defvar *precedents-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))

(defun precedents (graph node)
  (if-let ((hash (gethash graph *precedents-cache*)))
    (gethash node hash)
    (let ((hash (make-hash-table)))
      (dolist (node (nodes graph))
        (setf (gethash node hash)
              (graph:precedents graph node)))
      (setf (gethash graph *precedents-cache*) hash)
      (gethash node hash))))

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
  (declare (list a))
  (let ((hash (make-hash-table)))
    (dolist (x b) (setf (gethash x hash) t))
    (every (lambda (x) (gethash x hash)) a)))

(defmacro lambda* (args &body body)
  (declare (optimize (speed 0)))
  (assert (eql 1 (length args)))
  `(optima.extra:lambda-ematch
     ((list ,@ (first args))
      ,@body)))

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
       (subsetp* extension (nodes graph))))

(defun conflict-free-extension-p (graph extension)
  (with-member* (extension)
    (and (extension-p graph extension)
         (notany (lambda* ((a b))
                   (and (member* a)
                        (member* b)))
                 (edges graph)))))

(let ((list (list nil nil)))
  (defun acceptable-p (graph arguments argument)
    (declare (list arguments)
             (graph:graph graph))
    #+nil
    (every (lambda* ((b a))
             (implies (eql a argument)
                      (some (lambda (g)
                              (setf (first list) g
                                    (second list) b)
                              (graph:has-edge-p graph list))
                            arguments)))
           (edges graph))
    (every (lambda (b)
             (some (lambda (g)
                     (setf (first list) g
                           (second list) b)
                     (graph:has-edge-p graph list))
                   arguments))
           (the list (precedents graph argument)))))

(defun admissible-extension-p (graph extension)
  (declare (type list extension))
  (and (conflict-free-extension-p graph extension)
       (every (curry #'acceptable-p graph extension)
              extension)))

(defun complete-extension-p (graph extension)
  (with-member* (extension)
    (and (admissible-extension-p graph extension)
         (every (lambda (argument)
                  (implies (acceptable-p graph extension argument)
                           (member* argument)))
                (nodes graph)))))

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
                (set-difference (nodes graph) extension)))))

(defun preferred-extension-p (graph extension)
  (and (complete-extension-p graph extension)
       (every (lambda (set)
                (implies (and (not (set-equal extension set))
                              (subsetp* extension set))
                         (not (complete-extension-p graph set))))
              (powerset (nodes graph)))))
