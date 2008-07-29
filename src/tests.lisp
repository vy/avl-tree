;;; Copyright (c) 2008, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :avl-tree)

(let (tests)
  (def function def-test (caption method input output &key (eq-p #'equalp))
    "Introduces a new test to the TEST heap."
    (push (list caption method input output eq-p) tests))

  (def (function e) run-test-suite ()
    "Runs available test suites and reports success-failure statistics."
    (format t "Running test suite...~%~%")
    (let ((n-failure 0)
          (n-success 0)
          (n-tested  0))
      (dolist (test (reverse tests))
        (destructuring-bind (caption method input expected-output eq-p) test
          (format t "~,2d. ~a... " (incf n-tested) caption)
          (let ((produced-output (funcall method input)))
            (cond ((funcall eq-p expected-output produced-output)
                   (format t "done.~%")
                   (incf n-success))
                  (t
                   (format t "failed!~%")
                   (format t "  METHOD         : ~a~%" method)
                   (format t "  INPUT          : ~a~%" input)
                   (format t "  EXPECTED-OUTPUT: ~a~%" expected-output)
                   (format t "  PRODUCED-OUTPUT: ~a~%" produced-output)
                   (format t "  EQ-P           : ~a~%" eq-p)
                   (format t "~%")
                   (incf n-failure))))))
      (format
       t "~%Completed ~a of ~a tests successfully. (Success ratio: ~,1f%)~%"
       n-success (+ n-success n-failure)
       (* 100 (/ n-success (+ n-success n-failure)))))))

(def-test
 "Right rotation"
 (lambda (branch)
   (export-branch-to-sexp (rotate-right (import-branch-from-sexp branch))))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) (5 . E))
 '((1 . A) (2 . B) ((3 . C) (4 . D) (5 . E))))

(def-test
 "Left rotation"
 (lambda (branch)
   (export-branch-to-sexp (rotate-left (import-branch-from-sexp branch))))
 '((1 . A) (2 . B) ((3 . C) (4 . D) (5 . E)))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) (5 . E)))

(def-test
 "Balance branch for left-left case"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch-for-ll-case (import-branch-from-sexp branch))))
 '((((1 . A) (2 . B) (3 . C)) (4 . D) (5 . E)) (6 . F) (7 . G))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) ((5 . E) (6 . F) (7 . G))))

(def-test
 "Balance branch for right-right case"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch-for-rr-case (import-branch-from-sexp branch))))
 '((1 . A) (2 . B) ((3 . C) (4 . D) ((5 . E) (6 . F) (7 . G))))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) ((5 . E) (6 . F) (7 . G))))

(def-test
 "Balance branch for left-right case"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch-for-lr-case (import-branch-from-sexp branch))))
 '(((1 . A) (2 . B) ((3 . C) (4 . D) (5 . E))) (6 . F) (7 . G))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) ((5 . E) (6 . F) (7 . G))))

(def-test
 "Balance branch for right-left case"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch-for-rl-case (import-branch-from-sexp branch))))
 '((1 . A) (2 . B) (((3 . C) (4 . D) (5 . E)) (6 . F) (7 . G)))
 '(((1 . A) (2 . B) (3 . C)) (4 . D) ((5 . E) (6 . F) (7 . G))))

(def-test
 "Balance branch growing to right continuosly"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch (import-branch-from-sexp branch))))
 '(NIL (1 . A)
   (NIL (2 . B)
    (NIL (3 . C)
     (NIL (4 . D) (NIL (5 . E) (NIL (6 . F) (NIL (7 . G) (8 . H))))))))
 '(((NIL (1 . A) (2 . B)) (3 . C) (4 . D)) (5 . E) ((6 . F) (7 . G) (8 . H))))

(def-test
 "Balance branch growing with a zig-zag pattern"
 (lambda (branch)
   (export-branch-to-sexp
    (balance-branch (import-branch-from-sexp branch))))
 '((NIL (1 . A)
    ((NIL (2 . B)
          ((NIL (3 . C)
                (((4 . D) (5 . E) (6 . F)) (7 . G) ((8 . H) (9 . I) (10 . J))))
           (11 . K) NIL)) (12 . L) NIL)) (13 . M) NIL)
 '(((NIL (1 . A) (2 . B)) (3 . C) ((4 . D) (5 . E) (6 . F)))
   (7 . G) (((8 . H) (9 . I) (10 . J)) (11 . K) ((12 . L) (13 . M) NIL))))
