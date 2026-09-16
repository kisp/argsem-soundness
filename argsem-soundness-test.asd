;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :argsem-soundness-test
  :name "argsem-soundness-test"
  :description "Tests for argsem-soundness"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  ;; GRAPH is no longer a dependency of argsem-soundness itself; the
;; tests still exercise the graph-object path, so they ask for it.
  :depends-on (:argsem-soundness :myam :alexandria :graph))

(defmethod perform ((op test-op)
                    (system (eql (find-system :argsem-soundness-test))))
  (perform op (find-system :argsem-soundness)))
