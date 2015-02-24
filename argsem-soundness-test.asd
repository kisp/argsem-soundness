;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :argsem-soundness-test
  :name "argsem-soundness-test"
  :description "Tests for argsem-soundness"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package")))))
  :depends-on (:argsem-soundness :myam :alexandria))

(defmethod perform ((op test-op)
                    (system (eql (find-system :argsem-soundness-test))))
  (perform op (find-system :argsem-soundness)))
