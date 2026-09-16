;;;; run-without-graph.lisp -- the property the library promises.
;;;;
;;;; argsem-soundness depends on alexandria and trivial-garbage only. This
;;;; runs in an image that has never heard of GRAPH: if someone puts the
;;;; dependency back, or reaches for a graph: symbol at load time, this
;;;; check goes red rather than the breakage surfacing as a Quicklisp
;;;; install failure on somebody else's machine.
(require :asdf)
(asdf:load-system :argsem-soundness)

(when (find-package "GRAPH")
  (error "the GRAPH package is loaded; argsem-soundness pulled it in again"))

(let* ((af (funcall (intern "MAKE-AF" "ARGSEM-SOUNDNESS") '(a b c) '((a b) (b c))))
       (grounded (funcall (intern "LIST-EXTENSIONS" "ARGSEM-SOUNDNESS")
                          af (intern "GROUNDED-EXTENSION-P" "ARGSEM-SOUNDNESS"))))
  ;; a attacks b, b attacks c: c is reinstated
  (unless (equal '((a c)) grounded)
    (error "expected ((a c)) as the grounded extension, got ~s" grounded))
  (format t "~&argsem-soundness: grounded extension ~s, with no GRAPH in the image~%"
          grounded))
