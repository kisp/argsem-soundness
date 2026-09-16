;;;; run-tests.lisp -- the full suite, including the graph-object path.
;;;; MYAM's RUN! reports failures by returning NIL rather than signalling,
;;;; so check it: otherwise a failing suite would report a green build.
(require :asdf)
(asdf:load-system :argsem-soundness-test)
(unless (funcall (intern "RUN!" "MYAM") :argsem-soundness-test)
  (error "argsem-soundness: test suite failed"))
(format t "~&argsem-soundness: test suite passed~%")
