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


;;; Accessor Routines

(def (function e) branch-empty-p (branch)
  "Returns NIL if supplied BRANCH is not empty."
  (eql branch +branch-terminator+))

(def (function e) tree-empty-p (root)
  "Returns NIL if tree pointed by ROOT is not empty."
  (branch-empty-p (node-of root)))

(def function ensured-height-of (node)
  "Returns height of the node, if there is any. Otherwise 0 is returned."
  (if (branch-empty-p node)
      0
      (height-of node)))

(def function update-branch-height (branch)
  "Updates height of the supplied BRANCH by looking at its children and returns
updated BRANCH."
  (setf (height-of branch)
        (1+ (max (ensured-height-of (lbranch-of branch))
                 (ensured-height-of (rbranch-of branch)))))
  branch)


;;; Pretty Printing Routines

(def print-object tree-node (node stream)
  (format stream ":KEY ~a :HEIGHT ~a" (key-of node) (height-of node)))

(def print-object tree-root (root stream)
  (format stream "~%  :EQ-P ~s~%  :LT-P ~s~%  :NODE ~s"
          (eq-p-of root) (lt-p-of root) (node-of root)))

(def (function e) dump-branch (branch &optional (depth 0))
  "Dumps BRANCH recursively in a (relatively) human-readable form."
  (unless (branch-empty-p branch)
    (dump-branch (lbranch-of branch) (1+ depth))
    (loop repeat depth do (write-string "  "))
    (format t "~a~%" branch)
    (dump-branch (rbranch-of branch) (1+ depth))))

(def (function e) dump-tree (root)
  "Dumps tree pointed by ROOT recursively in a (relatively) human-readable
form."
  (format t "EQ-P: ~s~%LT-P: ~s~%~%" (eq-p-of root) (lt-p-of root))
  (dump-branch (node-of root)))


;;; Branch Balancing Routines

(def function rotate-right (branch)
  "Performs below figured transformation and appropriate node height updates for
the supplied BRANCH.

      D          B
     / \        / \
    B   E  =>  A   D
   / \            / \
  A   C          C   E"
  (let ((branch-d branch))
    (unless (branch-empty-p branch-d)
      (let ((branch-b (lbranch-of branch-d)))
        (cond
          ((branch-empty-p branch-b)
           ;; No `B' branch, no transformation.
           branch-d)
          (t
           ;; Compute heights.
           (setf (height-of branch-d)
                 (+ (ensured-height-of (rbranch-of branch-b))   ; of branch-c
                    (ensured-height-of (rbranch-of branch-d)))) ; of branch-e
           (setf (height-of branch-b)
                 (+ (ensured-height-of (lbranch-of branch-b))   ; of branch-a
                    (height-of branch-d)))
           ;; Swap branches.
           (setf (lbranch-of branch-d) (rbranch-of branch-b))
           (setf (rbranch-of branch-b) branch-d)
           ;; Return new branch root.
           (update-branch-height branch-b)))))))

(def function rotate-left (branch)
  "Performs below figured transformation and appropriate node height updates for
the supplied BRANCH.

    B              D
   / \            / \
  A   D    =>    B   E
     / \        / \
    C   E      A   C"
  (let ((branch-b branch))
    (unless (branch-empty-p branch-b)
      (let ((branch-d (rbranch-of branch-b)))
        (cond
          ((branch-empty-p branch-d)
           ;; No `D' branch, no transformation.
           branch-b)
          (t
           ;; Compute heights.
           (setf (height-of branch-b)
                 (+ (ensured-height-of (lbranch-of branch-b))   ; of branch-a
                    (ensured-height-of (lbranch-of branch-d)))) ; of branch-c
           (setf (height-of branch-d)
                 (+ (height-of branch-b)
                    (ensured-height-of (rbranch-of branch-d)))) ; of branch-e
           ;; Swap branches.
           (setf (rbranch-of branch-b) (lbranch-of branch-d))
           (setf (lbranch-of branch-d) branch-b)
           ;; Return new branch root.
           (update-branch-height branch-d)))))))

(def function balance-branch-for-ll-case (branch)
  "Balances supplied BRANCH for left-left case. Transformation gets performed
represented by below figure.

        F             D
       / \          /   \
      D   G        B     F
     / \     =>   / \   / \
    B   E        A   C E   G
   / \
  A   C"
  (rotate-right branch))

(def function balance-branch-for-rr-case (branch)
  "Balances supplied BRANCH for right-right case. Transformation gets performed
represented by below figure.

    B                 D
   / \              /   \
  A   D            B     F
     / \     =>   / \   / \
    C   F        A   C E   G
       / \
      E   G"
  (rotate-left branch))

(def function balance-branch-for-lr-case (branch)
  "Balances supplied BRANCH for left-right case. Transformation gets performed
reprensented by below figure.

      F                F
     / \              / \
    B   G            D   G
   / \       =>     / \    
  A   D            B   E
     / \          / \
    C   E        A   C

Finally branch gets transformed into a left-left case. Rest gets done by
BALANCE-BRANCH-FOR-LL-CASE."
  (setf (lbranch-of branch) (rotate-left (lbranch-of branch)))
  (balance-branch-for-ll-case branch))

(def function balance-branch-for-rl-case (branch)
  "Balances supplied BRANCH for right-left case. Transformation gets performed
represented by below figure.

    B              B
   / \            / \
  A   F          A   D
     / \     =>     / \
    D   G          C   F
   / \                / \
  C   E              E   G

Finally BRANCH gets transformed into a right-right case. Rest gets done by
BALANCE-BRANCH-FOR-RR-CASE."
  (setf (rbranch-of branch) (rotate-right (rbranch-of branch)))
  (balance-branch-for-rr-case branch))

(def function balance-branch (branch)
  "Balances supplied BRANCH according to children heights."
  (cond ((branch-empty-p branch) +branch-terminator+)
        (t
         ;; Balance children first.
         (setf (lbranch-of branch) (balance-branch (lbranch-of branch))
               (rbranch-of branch) (balance-branch (rbranch-of branch)))
         ;; Return branch after updating top level height.
         (update-branch-height
          ;; Balance branch pointed by current node.
          (let ((balance-factor
                 (- (ensured-height-of (rbranch-of branch))
                    (ensured-height-of (lbranch-of branch)))))
            (cond ((member balance-factor (list -1 0 1))
                   ;; Branch is already balanced.
                   branch)
                  ((< balance-factor -1)
                   ;; Left hand side appears to be heavier.
                   (balance-branch
                    (let ((lbranch (lbranch-of branch)))
                      (if (< (ensured-height-of (lbranch-of lbranch))
                             (ensured-height-of (rbranch-of lbranch)))
                          (balance-branch-for-lr-case branch)
                          (balance-branch-for-ll-case branch)))))
                  ;; Right hand side appears to be heavier.
                  (t (balance-branch
                      (let ((rbranch (rbranch-of branch)))
                        (if (> (ensured-height-of (lbranch-of rbranch))
                               (ensured-height-of (rbranch-of rbranch)))
                            (balance-branch-for-rl-case branch)
                            (balance-branch-for-rr-case branch)))))))))))


;;; Convenient Debugging Routines

(def (function e) export-branch-to-sexp (branch)
  "Exports data collected in the nodes of the supplied BRANCH into
IMPORT-BRANCH-FROM-SEXP parseable s-expression forms."
  (cond ((branch-empty-p branch) +branch-terminator+)
        ((and (branch-empty-p (lbranch-of branch))
              (branch-empty-p (rbranch-of branch)))
         (cons (key-of branch) (data-of branch)))
        (t (list (export-branch-to-sexp (lbranch-of branch))
                 (cons (key-of branch) (data-of branch))
                 (export-branch-to-sexp (rbranch-of branch))))))

(def (function e) import-branch-from-sexp (sexp)
  "Imports data supplied in s-expression forms into suitable TREE-NODE
structures."
  (cond ((null sexp) +branch-terminator+)
        ((atom (cdr sexp))
         (make-instance 'tree-node
                        :height (1+ (ensured-height-of +branch-terminator+))
                        :key (car sexp)
                        :data (cdr sexp)))
        (t (destructuring-bind (lbranch key-data-pair rbranch) sexp
             (let ((lbranch (import-branch-from-sexp lbranch))
                   (rbranch (import-branch-from-sexp rbranch)))
               (make-instance 'tree-node
                              :height (1+ (max (ensured-height-of lbranch)
                                               (ensured-height-of rbranch)))
                              :key (car key-data-pair)
                              :data (cdr key-data-pair)
                              :lbranch lbranch
                              :rbranch rbranch))))))

(def (function e) validate-branch-balance (branch)
  "Validates balance of the supplied BRANCH by looking at node heights."
  (unless (branch-empty-p branch)
    (let ((balance-factor
           (- (ensured-height-of (rbranch-of branch))
              (ensured-height-of (lbranch-of branch)))))
      (unless (member balance-factor (list -1 0 1))
        (error "Unexpected balance factor ~a at branch ~a."
               balance-factor branch)))
    (validate-branch-balance (lbranch-of branch))
    (validate-branch-balance (rbranch-of branch))))

(def (function e) validate-balance (root)
  "Validates balance of the supplied tree by looking at the heights of the nodes
collected under ROOT node."
  (validate-branch-balance (node-of root)))
