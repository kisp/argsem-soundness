;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :argsem-soundness-test)

(defsuite* :argsem-soundness-test)

(deftest extension-p.1
  (is-true (extension-p (populate (make-instance 'digraph) :nodes '(a b c))
                        '(a b c)))
  (is-false (extension-p (populate (make-instance 'digraph) :nodes '(a b c))
                         '(1 2)))
  (is-false (extension-p (populate (make-instance 'digraph) :nodes '(a b c))
                         '(a b b c))))

(deftest conflict-free-extension-p.1
  (is-true (conflict-free-extension-p
            (populate (make-instance 'digraph) :nodes '(a b c))
            '(a b c))))

(deftest conflict-free-extension-p.2
  (is-false (conflict-free-extension-p
             (populate (make-instance 'digraph) :nodes '(a b c))
             '(1 2))))

(deftest conflict-free-extension-p.3
  (is-false (conflict-free-extension-p
             (populate (make-instance 'digraph) :edges '((a b)))
             '(a b))))

(deftest conflict-free-extension-p.4
  (is-true (conflict-free-extension-p
            (populate (make-instance 'digraph) :edges '((a b)))
            '(a)))
  (is-true (conflict-free-extension-p
            (populate (make-instance 'digraph) :edges '((a b)))
            '(b)))
  (is-true (conflict-free-extension-p
            (populate (make-instance 'digraph) :edges '((a b)))
            '())))

(deftest admissible-extension-p.1
  (is-true (admissible-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '()))
  (is-false (admissible-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(a b)))
  (is-false (admissible-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(b)))
  (is-true (admissible-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '(a)))
  (is-true (admissible-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '(a c)))
  (is-false (admissible-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(c))))

(deftest complete-extension-p.1
  (is-true (complete-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '(a c)))
  (is-false (complete-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(b)))
  (is-false (complete-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(a))))

(deftest complete-extension-p.2
  (is-true (complete-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(d)))
  (is-true (complete-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(b d)))
  (is-true (complete-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(a c d)))
  (dolist (counter (powerset '(a b c d)))
    (unless (member counter '((d) (b d) (a c d)) :test #'set-equal)
      (is-false (complete-extension-p
                 (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
                 counter)))))

(deftest grounded-extension-p.1
  (is-true (grounded-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(d)))
  (is-false (grounded-extension-p
             (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
             '()))
  (is-false (grounded-extension-p
             (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
             '(b d)))
  (is-false (grounded-extension-p
             (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
             '(a c d))))

(deftest stable-extension-p.1
  (is-true (stable-extension-p
            (populate (make-instance 'digraph) :edges '((a b)))
            '(a)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b)))
             '(a b)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b a)))
             '())))

(deftest stable-extension-p.2
  (is-true (stable-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '(a c)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(a)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(b)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '(c)))
  (is-false (stable-extension-p
             (populate (make-instance 'digraph) :edges '((a b) (b c)))
             '())))

(deftest preferred-extension-p.1
  (is-true (preferred-extension-p
            (populate (make-instance 'digraph) :edges '((a b)))
            '(a)))
  (is-true (preferred-extension-p
            (populate (make-instance 'digraph) :edges '((a b) (b c)))
            '(a c)))
  (is-false (preferred-extension-p
             (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
             '(d)))
  (is-true (preferred-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(b d)))
  (is-true (preferred-extension-p
            (populate (make-instance 'digraph) :nodes '(d) :edges '((a b) (b a) (b c)))
            '(a c d))))

(deftest characteristic-function.1
  (is (set-equal '(a)
                 (characteristic-function
                  (populate (make-instance 'digraph) :nodes '(a)) '())))
  (is (set-equal '()
                 (characteristic-function
                  (populate (make-instance 'digraph)
                            :nodes '(a) :edges '((a a)))
                  '())))
  (is (set-equal '()
                 (characteristic-function
                  (populate (make-instance 'digraph)
                            :nodes '(a b) :edges '((a b) (b a)))
                  '()))))

(deftest characteristic-function-fixpoint-p.1
  (is-true (characteristic-function-fixpoint-p
            (populate (make-instance 'digraph)
                      :nodes '(a b) :edges '((a b) (b a)))
            '()))
  (is-false (characteristic-function-fixpoint-p
             (populate (make-instance 'digraph)
                       :nodes '(a b) :edges '((a b)))
             '())))
