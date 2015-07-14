;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :argsem-soundness
  :name "argsem-soundness"
  :description "Given an Abstract Argumentation Framework and an Extension, check its soundness wrt different semantics."
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "package")
               (:file "argsem-soundness" :depends-on ("package"))
               )
  :depends-on (:alexandria :graph :trivial-garbage :optima))

(defmethod perform ((op test-op)
                    (system (eql (find-system :argsem-soundness))))
  (oos 'load-op :argsem-soundness-test)
  (funcall (intern "RUN!" "MYAM") :argsem-soundness-test))
