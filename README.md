         T   ##%## #% %%#~        AVL (Adelson-Velsky-Landis) Tree is a
       /   \   ##%## ##% %#~      self-balancing binary search tree
      V     E ###% %##~           implementation in Common Lisp.
     / \   / \  #####%## ##~
    A   L R   E  ####% %#% %#%%#~

# Installation

It's quite simple to install `AVL-TREE` via `ASDF`. (See [`ASDF`](http://cliki.net/ASDF) for more information and check if your Common Lisp implementation bundled with ASDF support.) For instance, in SBCL, you'll need to type

    CL-USER> (require :asdf)
    NIL
    CL-USER> (require :asdf-install)
    NIL
    CL-USER> (asdf-install:install :avl-tree)
    ...

`ASDF-INSTALL` will handle dependencies for you. In case of manual installation, you'll need to get `DEMACS` package to be able to install `AVL-TREE`.

After a successfull `ASDF-INSTALL`, you'll probably want to check the integrity of the supplied API functions. For this purpose, you can run test suits:

    CL-USER> (asdf:oos 'asdf:load-op :avl-tree)
    ...
    CL-USER> (avl-tree:run-test-suite)
    ...

# Documentation

Although the source code is quite well documented, below you can find a brief explanation of the exported functions & symbols by `AVL-TREE` package.

## Specials

    BRANCH-TERMINATOR+ [CONSTANT]

> Terminator to end branch pointers.

    DUPLICATE-KEY [CONDITION]

> Raised as an error when a node with an existing key tried to be inserted into a tree.
> Accessors: `NODE-OF`

    TREE-NODE-HEIGHT-TYPE [TYPE]

> Data type to represent height of a `TREE-NODE`.

    TREE-NODE [CLASS]

> Data component for node related data storage.
> Accessors: `KEY-OF`, `DATA-OF`, `HEIGHT-OF`, `LBRANCH-OF`, `RBRANCH-OF`

    TREE-ROOT [CLASS]

> Data component for tree related data storage.
> Accessors: `EQ-P-OF`, `LT-P-OF`, `NODE-OF`

## Utilities

    DUMP-BRANCH (BRANCH) [FUNCTION]

> Dumps branch recursively in a (relatively) human-readable form.

    DUMP-TREE (ROOT) [FUNCTION]

> Dumps tree pointed by `ROOT` recursively in a (relatively) human-readable form.

    EXPORT-BRANCH-TO-SEXP (BRANCH) [FUNCTION]

> Exports data collected in the nodes of the supplied `BRANCH` into `IMPORT-BRANCH-FROM-SEXP` parseable s-expression forms.

    IMPORT-BRANCH-FROM-SEXP (SEXP) [FUNCTION]

> Imports data supplied in s-expression forms into suitable `TREE-NODE` structures.

    TREE-BRANCH-P (BRANCH) [FUNCTION]

> Returns `NIL` if supplied `BRANCH` is not empty.

    TREE-EMPTY-P (ROOT) [FUNCTION]

> Returns `NIL` if tree pointed by `ROOT` is not empty.

    VALIDATE-BALANCE (ROOT) [FUNCTION]

> Validates balance of the nodes collected under the tree pointed by `ROOT`.

    VALIDATE-BRANCH-BALANCE (BRANCH) [FUNCTION]

> Validates balance of the nodes collected under the supplied `BRANCH`.

## Tree Routines

    FIND-NODE (ROOT KEY &KEY DEFAULT KEY-EQ-P KEY-LT-P) [FUNCTION]
    
> Returns `TREE-NODE` paired with supplied `KEY` in the tree pointed by `ROOT`. If no such node is found, `DEFAULT` is returned.

    INSERT-NODE (ROOT KEY DATA &KEY KEY-EQ-P KEY-LT-P) [FUNCTION]

> Inserts a fresh `TREE-NODE` using supplied `KEY` and `DATA` into the tree pointed by `ROOT` node. Finally function returns `ROOT`.

    REMOVE-NODE (ROOT KEY &KEY KEY-EQ-P KEY-LT-P) [FUNCTION]

> Removes `TREE-NODE` paired with supplied `KEY` nested under tree pointed by given `ROOT`. Finally function returns `ROOT`.

## Test Routines

    RUN-TEST-SUITE () [FUNCTION]

> Runs available test suite and reports success-failure statistics.


# Example Usage

Here is a small demonstration of the `AVL-TREE`.

    CL-USER> (asdf:oos 'asdf:load-op :avl-tree)
    ...
    CL-USER> (defpackage :test (:use :cl :avl-tree))
    #<PACKAGE "TEST">

    CL-USER> (in-package :test)
    #<PACKAGE "TEST">

    TEST> (defparameter *root* (make-instance 'tree-root :eq-p #'= :lt-p #'<))
    *ROOT*

    TEST> (dotimes (i 10)
            (insert-node *root* i (code-char (+ #.(char-code #\a) i))))
    NIL

    TEST> (dump-tree *root*)
    EQ-P: #<FUNCTION (SB-C::&OPTIONAL-DISPATCH =) {1000FF1469}>
    LT-P: #<FUNCTION (SB-C::&OPTIONAL-DISPATCH <) {1000B80C19}>
    
        #<TREE-NODE :KEY 0 :HEIGHT 1 {10037C2391}>
      #<TREE-NODE :KEY 1 :HEIGHT 2 {10037C3111}>
        #<TREE-NODE :KEY 2 :HEIGHT 1 {10037C5091}>
    #<TREE-NODE :KEY 3 :HEIGHT 4 {10037C5271}>
          #<TREE-NODE :KEY 4 :HEIGHT 1 {10037C53F1}>
        #<TREE-NODE :KEY 5 :HEIGHT 2 {10037C56C1}>
          #<TREE-NODE :KEY 6 :HEIGHT 1 {10037C5A81}>
      #<TREE-NODE :KEY 7 :HEIGHT 3 {10037C5DB1}>
        #<TREE-NODE :KEY 8 :HEIGHT 2 {10037C60B1}>
          #<TREE-NODE :KEY 9 :HEIGHT 1 {10037C6531}>
    NIL

    TEST> (data-of (find-node *root* 3))
    #\d

    TEST> (remove-node *root* 3)
    #<TREE-ROOT 
      :EQ-P #<FUNCTION (SB-C::&OPTIONAL-DISPATCH =) {1000FF1469}>
      :LT-P #<FUNCTION (SB-C::&OPTIONAL-DISPATCH <) {1000B80C19}>
      :NODE #<TREE-NODE :KEY 2 :HEIGHT 4 {1002534C11}> {1002FDE861}>

    TEST> (dump-tree *root*)
    EQ-P: #<FUNCTION (SB-C::&OPTIONAL-DISPATCH =) {1000FF1469}>
    LT-P: #<FUNCTION (SB-C::&OPTIONAL-DISPATCH <) {1000B80C19}>
    
        #<TREE-NODE :KEY 0 :HEIGHT 1 {1002504B81}>
      #<TREE-NODE :KEY 1 :HEIGHT 2 {1002505901}>
    #<TREE-NODE :KEY 2 :HEIGHT 4 {1002534C11}>
          #<TREE-NODE :KEY 4 :HEIGHT 1 {100267FC81}>
        #<TREE-NODE :KEY 5 :HEIGHT 2 {10027B2511}>
          #<TREE-NODE :KEY 6 :HEIGHT 1 {1002960B61}>
      #<TREE-NODE :KEY 7 :HEIGHT 3 {1002AB5C11}>
        #<TREE-NODE :KEY 8 :HEIGHT 2 {1002BEB201}>
          #<TREE-NODE :KEY 9 :HEIGHT 1 {1002DD59C1}>
    NIL
