;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :argsem-soundness)

(defvar *edges-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))
(defvar *nodes-cache*
  (trivial-garbage:make-weak-hash-table :weakness :key))
(defvar *setp-buffer* (make-array 20 :fill-pointer t))
(defvar *powersets* (make-hash-table :test #'equal))

(defmacro cached (fn-name key hash)
  (once-only (hash key)
    `(multiple-value-bind (value hit)
         (gethash ,key ,hash)
       (if hit
           value
           (setf (gethash ,key ,hash)
                 (,fn-name ,key))))))

(defstruct (af (:constructor make-af (arguments attacks)))
  "An abstract argumentation framework as plain data: a list of ARGUMENTS
and a list of ATTACKS, each attack a two-element list (attacker target)."
  (arguments '() :read-only t)
  (attacks '() :read-only t))

(defgeneric nodes-of (af)
  (:documentation "The arguments of AF, as a list."))

(defgeneric edges-of (af)
  (:documentation "The attacks of AF, as a list of (attacker target)."))

(defmethod nodes-of ((af af))
  (af-arguments af))

(defmethod edges-of ((af af))
  (af-attacks af))

(defun graph-reader (name object)
  "The GRAPH library's NAME reader, looked up at run time.

Only two things are ever asked of a framework -- its arguments and its
attacks -- so requiring the GRAPH library for two readers is a poor trade:
it is a package-inferred-system and pulls five more systems in behind it.
It is therefore supported without being depended on. Pass a graph object
and it is used if GRAPH happens to be loaded; pass an AF struct and nothing
outside alexandria and trivial-garbage is needed at all."
  (let* ((package (find-package "GRAPH"))
         (symbol (and package (find-symbol name package))))
    (unless (and symbol (fboundp symbol))
      (error "~s is not an argumentation framework: it is not an AF struct ~
              \(see MAKE-AF), and the GRAPH library, whose ~a it would ~
              otherwise be asked for, is not loaded."
             object name))
    (symbol-function symbol)))

(defmethod nodes-of (af)
  (funcall (graph-reader "NODES" af) af))

(defmethod edges-of (af)
  (funcall (graph-reader "EDGES" af) af))

(defun edges (af)
  (cached edges-of af *edges-cache*))

(defun nodes (af)
  (cached nodes-of af *nodes-cache*))

(defmacro with-member* ((list) &body body)
  `(macrolet ((member* (x) `(cl::member ,x ,',list)))
     ,@body))

(defmacro member* (x)
  (declare (ignore x))
  (error "no member* env"))

(defun member2 (a b list)
  "Is (member (list a b) list :test #'equal) true?"
  (dolist (x list nil)
    (when (and (eql (first x) a)
               (eql (second x) b))
      (return t))))

(defun %powerset (set)
  (if (null set)
      '(())
      (destructuring-bind (x . xs) set
        (let ((others (%powerset xs)))
          (append others (mapcar (curry #'cons x) others))))))

(defun powerset-cached (set)
  (cached %powerset set *powersets*))

(declaim (inline powerset))
(defun powerset (set)
  (%powerset set))

(defun setp* (list)
  (setf (fill-pointer *setp-buffer*) 0)
  (loop for i upfrom 0
        for x in list
        if (find x *setp-buffer*)
          do (return)
        else
          do (vector-push x *setp-buffer*)
        finally (return t)))

(defun subsetp* (a b)
  (cl::subsetp a b))

(defmacro lambda* (args &body body)
  (with-gensyms (x)
    `(lambda (,x)
       (destructuring-bind ,args ,x
         ,@body))))

(defmacro implies (antecedent consequent)
  `(if ,antecedent ,consequent t))

(defun count-extensions (graph predicate)
  "Count how many extensions in the powerset of the nodes of graph
satisfy predicate. Predicate is expected to be binary, taking as
arguments graph and extension."
  (count-if (lambda (extension)
              (funcall predicate graph extension))
            (powerset-cached (nodes graph))))

(defun list-extensions (graph predicate)
  "List extensions contained in the powersetof the nodes of graph that
satisfy predicate. Predicate is expected to be binary, taking as
arguments graph and extension."
  (remove-if-not (lambda (extension)
                   (funcall predicate graph extension))
                 (powerset-cached (nodes graph))))

(defun extension-p (graph extension)
  (and (setp* extension)
       (subsetp* extension (nodes graph))))

(defun conflict-free-extension-p (graph extension)
  (with-member* (extension)
    (and (extension-p graph extension)
         (notany (lambda* (a b)
                   (and (member* a)
                        (member* b)))
                 (edges graph)))))

(defun acceptable-p (graph extension argument)
  (let ((edges (edges graph)))
    (every (lambda* (b a)
             (implies (eql a argument)
                      (some (lambda (g)
                              (member2 g b edges))
                            extension)))
           edges)))

(defun characteristic-function (graph extension)
  (remove-if-not (curry #'acceptable-p graph extension)
                 (nodes graph)))

(defun characteristic-function-fixpoint-p (graph extension)
  (set-equal extension (characteristic-function graph extension)))

(defun admissible-extension-p (graph extension)
  (and (conflict-free-extension-p graph extension)
       (every (lambda (argument)
                (acceptable-p graph extension argument))
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
               (remove extension (powerset-cached extension) :test #'set-equal)))
         (notany (curry #'complete-extension-p graph) sub-extensions))))

(defun stable-extension-p (graph extension)
  (with-member* (extension)
    (and (conflict-free-extension-p graph extension)
         (every (lambda (argument)
                  (some (lambda* (b a)
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
              (powerset-cached (nodes graph)))))
