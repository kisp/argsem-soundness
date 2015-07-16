;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage :argsem-soundness
  (:use :common-lisp :alexandria)
  (:export
   #:powerset
   #:extension-p
   #:conflict-free-extension-p
   #:admissible-extension-p
   #:complete-extension-p
   #:grounded-extension-p
   #:stable-extension-p
   #:preferred-extension-p
   #:acceptable-p
   #:characteristic-function)
  (:shadow #:setp #:subsetp #:member))
