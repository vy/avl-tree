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


;;; Node Search Routines

(def function find-node-in-branch (branch key eq-p lt-p)
  "Returns node of the supplied KEY using given EQ-P and LT-P predicates. If
node is not found, +BRANCH-TERMINATOR+ is returned."
  (cond ((branch-empty-p branch) +branch-terminator+)
        ((funcall eq-p key (key-of branch)) branch)
        ((funcall lt-p key (key-of branch))
         (find-node-in-branch (lbranch-of branch) key eq-p lt-p))
        (t
         (find-node-in-branch (rbranch-of branch) key eq-p lt-p))))

(def (function e) find-node (root key &key default key-eq-p key-lt-p)
  "Returns TREE-NODE paired with supplied KEY in the tree pointed by ROOT. If no
such node is found, DEFAULT is returned."
  (let ((node
         (find-node-in-branch
          (node-of root)
          key
          (or key-eq-p (eq-p-of root))
          (or key-lt-p (lt-p-of root)))))
    (if (branch-empty-p node)
        (or default +branch-terminator+)
        node)))


;;; New Node Addition Routines

(def function insert-node-to-branch (branch node eq-p lt-p)
  "Inserts supplied NODE (actually can be a branch) into an appropriate place
under BRANCH using given EQ-P and LT-P comparison predicates. Function returns
the modified BRANCH."
  (cond ((branch-empty-p branch) node)
        ((funcall eq-p (key-of branch) (key-of node))
         (error 'duplicate-key :node branch))
        (t
         (balance-branch
          (prog1 branch
            (if (funcall lt-p (key-of node) (key-of branch))
                (setf
                 (lbranch-of branch)
                 (insert-node-to-branch (lbranch-of branch) node eq-p lt-p))
                (setf
                 (rbranch-of branch)
                 (insert-node-to-branch
                  (rbranch-of branch) node eq-p lt-p))))))))

(def (function e) insert-node (root key data &key key-eq-p key-lt-p)
  "Inserts a fresh TREE-NODE using supplied KEY and DATA into the tree pointed
by ROOT node. Finally function returns ROOT."
  (prog1 root
    (setf (node-of root)
          (insert-node-to-branch
           (node-of root)
           (make-instance 'tree-node :key key :data data)
           (or key-eq-p (eq-p-of root))
           (or key-lt-p (lt-p-of root))))))


;;; Node Removing Routines

(def function unsafe-insert-node-to-branch (branch node lt-p)
  "Inserts supplied NODE (actually can be a branch) into an appropriate place
under BRANCH using given LT-P comparison predicate. It's assumed that there
won't be any key duplication -- that's why there is no need for a EQ-P."
  (if (branch-empty-p branch)
      node
      (balance-branch
       (prog1 branch
         (if (funcall lt-p (key-of node) (key-of branch))
             (setf
              (lbranch-of branch)
              (unsafe-insert-node-to-branch (lbranch-of branch) node lt-p))
             (setf
              (rbranch-of branch)
              (unsafe-insert-node-to-branch (rbranch-of branch) node lt-p)))))))

(def function remove-root-from-branch (branch lt-p)
  "Removes root node from the supplied BRANCH and re-balances newly created
branch. For this purpose, we consider three different approaches in below
order.

  1. If there is only a single child, job is easy.
  2. If one of the branches have a single child, job is easy.
  3. If there are two children of each of the two branches,
  3.1. Let N be the branch with the lowest height.
  3.2. Let P be the child of N with the lowest height, and Q the other one.
  3.3. Lift N to up and apply steps from 1-3 while trying to append Q to P."
  (unless (branch-empty-p branch)
    (let ((lbranch (lbranch-of branch))
          (rbranch (rbranch-of branch)))
      (cond
        ;; 1. If there is only a single child,
        ((branch-empty-p lbranch) rbranch)
        ((branch-empty-p rbranch) lbranch)
        ;; 2.1.1. If there isn't a right child of left branch,
        ((and (not (branch-empty-p lbranch))
              (branch-empty-p (rbranch-of lbranch)))
         (setf (rbranch-of lbranch) rbranch)
         (balance-branch (update-branch-height lbranch)))
        ;; 2.1.2. If there isn't a left child of left branch,
        ((and (not (branch-empty-p lbranch))
              (branch-empty-p (lbranch-of lbranch)))
         (let ((r-of-lbranch (rbranch-of lbranch)))
           ;; Because of the branch is balanced, it *must* be guaranteed that
           ;; R-OF-LBRANCH doesn't have any children.
           (assert (and (branch-empty-p (lbranch-of r-of-lbranch))
                        (branch-empty-p (rbranch-of r-of-lbranch))))
           (setf (rbranch-of lbranch) +branch-terminator+
                 (lbranch-of r-of-lbranch) lbranch
                 (rbranch-of r-of-lbranch) rbranch)
           (balance-branch (update-branch-height r-of-lbranch))))
        ;; 2.2.1. If there isn't a left child of right branch,
        ((and (not (branch-empty-p rbranch))
              (branch-empty-p (lbranch-of rbranch)))
         (setf (lbranch-of rbranch) lbranch)
         (balance-branch (update-branch-height rbranch)))
        ;; 2.2.2. If there isn't a right child of right branch,
        ((and (not (branch-empty-p rbranch))
              (branch-empty-p (rbranch-of rbranch)))
         (let ((l-of-rbranch (lbranch-of rbranch))
               (r-of-lbranch (rbranch-of lbranch)))
           ;; Because of the branch is balanced, it *must* be guaranteed that
           ;; L-OF-RBRANCH doesn't have any children.
           (assert (and (branch-empty-p (lbranch-of l-of-rbranch))
                        (branch-empty-p (rbranch-of r-of-lbranch))))
           (setf (lbranch-of rbranch) +branch-terminator+
                 (lbranch-of l-of-rbranch) lbranch
                 (rbranch-of l-of-rbranch) rbranch)
           (balance-branch (update-branch-height l-of-rbranch))))
         ;; 3.1.1. If left child of the left branch and left branch itself have
         ;; the lowest height,
         ((and (< (height-of lbranch) (height-of rbranch))
               (< (height-of (lbranch-of lbranch))
                  (height-of (rbranch-of lbranch))))
          (setf
           (lbranch-of lbranch)
           (unsafe-insert-node-to-branch
            (lbranch-of lbranch) (rbranch-of lbranch) lt-p))
          (setf (rbranch-of lbranch) rbranch)
          (balance-branch (update-branch-height lbranch)))
         ;; 3.1.2. If the right child of the left branch and left branch itself
         ;; have the lowest height,
         ((< (height-of lbranch) (height-of rbranch))
          (let ((r-of-lbranch (rbranch-of lbranch)))
            (setf (rbranch-of lbranch) +branch-terminator+)
            (setf (lbranch-of r-of-lbranch)
                  (unsafe-insert-node-to-branch
                   (lbranch-of r-of-lbranch)
                   (update-branch-height lbranch)
                   lt-p))
            (setf (rbranch-of r-of-lbranch) rbranch)
            (balance-branch (update-branch-height r-of-lbranch))))
         ;; 3.2.1. If the left child of the right branch and right branch itself
         ;; have the lowest height,
         ((< (height-of (lbranch-of rbranch)) (height-of (rbranch-of rbranch)))
          (let ((l-of-rbranch (lbranch-of rbranch)))
            (setf (lbranch-of rbranch) +branch-terminator+)
            (setf (rbranch-of l-of-rbranch)
                  (unsafe-insert-node-to-branch
                   (rbranch-of l-of-rbranch)
                   (update-branch-height rbranch)
                   lt-p))
            (setf (lbranch-of l-of-rbranch) lbranch)
            (balance-branch (update-branch-height l-of-rbranch))))
         ;; 3.2.2. If the right child of the right branch and right branch
         ;; itself have the lowest height,
         (t
          (setf
           (rbranch-of rbranch)
           (unsafe-insert-node-to-branch
            (rbranch-of rbranch) (lbranch-of rbranch) lt-p))
          (setf (lbranch-of rbranch) lbranch)
          (balance-branch (update-branch-height rbranch)))))))

(def function remove-node-from-branch (branch key eq-p lt-p)
  "Removes node paired with supplied KEY nested under given BRANCH using
specified EQ-P and LT-P predicates. Returns the modified BRANCH."
  (cond ((branch-empty-p branch) +branch-terminator+)
        ((funcall eq-p key (key-of branch))
         (remove-root-from-branch branch lt-p))
        ((funcall lt-p key (key-of branch))
         (setf (lbranch-of branch)
               (remove-node-from-branch (lbranch-of branch) key eq-p lt-p))
         branch)
        (t
         (setf (rbranch-of branch)
               (remove-node-from-branch (rbranch-of branch) key eq-p lt-p))
         branch)))

(def (function e) remove-node (root key &key key-eq-p key-lt-p)
  "Removes TREE-NODE paired with supplied KEY nested under tree pointed by given
ROOT. Finally function returns ROOT."
  (prog1 root
    (setf (node-of root)
          (remove-node-from-branch
           (node-of root)
           key
           (or key-eq-p (eq-p-of root))
           (or key-lt-p (lt-p-of root))))))
