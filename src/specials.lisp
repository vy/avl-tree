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

(def (type e) tree-node-height-type ()
  "Data type to represent height of a TREE-NODE."
  `fixnum)

(def (constant e :documentation "Terminator to end branch pointers.")
    +branch-terminator+ nil)

(def (class eanc) tree-node ()
  ((key)
   (data)
   (height :type tree-node-height-type :initform 0)
   (lbranch :type tree-node :initform +branch-terminator+)
   (rbranch :type tree-node :initform +branch-terminator+))
  (:documentation "Data component for node related data storage."))

(def (class eanc) tree-root ()
  ((eq-p :type function :initform (error "Missing EQ-P argument!"))
   (lt-p :type function :initform (error "Missing LT-P argument!"))
   (node :type tree-node :initform +branch-terminator+))
  (:documentation "Data component for tree related data storage."))

(def (condition eanr) duplicate-key ()
  ((node :documentation "A node already exists with the supplied key value.")))
