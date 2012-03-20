;;; -*- Mode: Lisp; Package: STELLA; Syntax: COMMON-LISP; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;; BEGIN LICENSE BLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; Version: MPL 1.1/GPL 2.0/LGPL 2.1                                          ;
;                                                                            ;
; The contents of this file are subject to the Mozilla Public License        ;
; Version 1.1 (the "License"); you may not use this file except in           ;
; compliance with the License. You may obtain a copy of the License at       ;
; http://www.mozilla.org/MPL/                                                ;
;                                                                            ;
; Software distributed under the License is distributed on an "AS IS" basis, ;
; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   ;
; for the specific language governing rights and limitations under the       ;
; License.                                                                   ;
;                                                                            ;
; The Original Code is the STELLA Programming Language.                      ;
;                                                                            ;
; The Initial Developer of the Original Code is                              ;
; UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          ;
; 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               ;
;                                                                            ;
; Portions created by the Initial Developer are Copyright (C) 1996-2003      ;
; the Initial Developer. All Rights Reserved.                                ;
;                                                                            ;
; Contributor(s):                                                            ;
;                                                                            ;
; Alternatively, the contents of this file may be used under the terms of    ;
; either the GNU General Public License Version 2 or later (the "GPL"), or   ;
; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),   ;
; in which case the provisions of the GPL or the LGPL are applicable instead ;
; of those above. If you wish to allow use of your version of this file only ;
; under the terms of either the GPL or the LGPL, and not to allow others to  ;
; use your version of this file under the terms of the MPL, indicate your    ;
; decision by deleting the provisions above and replace them with the notice ;
; and other provisions required by the GPL or the LGPL. If you do not delete ;
; the provisions above, a recipient may use your version of this file under  ;
; the terms of any one of the MPL, the GPL or the LGPL.                      ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END LICENSE BLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Version: cl-setup.lisp,v 1.52 2003/07/30 00:34:49 tar Exp

;;; Common-Lisp package setup and boot support.

(in-package "CL-USER")


(eval-when (compile load eval)
  (unless (find-package "STELLA")
    (make-package "STELLA" :use NIL))
  ;; Make it more convenient to call functions that take floats
  ;; from the Lisp prompt, by not requiring the double-float marker:
  (setq *READ-DEFAULT-FLOAT-FORMAT* 'DOUBLE-FLOAT))

(loop for symbol
      in '(CL:SETQ CL:SETF
           CL:PRINT-OBJECT
           CL:IN-PACKAGE CL:EXPORT
           CL:TRACE CL:UNTRACE CL:INSPECT CL:apropos
           ;; stuff necessary for ACL interface:
           #+allegro CL:COMPILE
           #+allegro CL:EVAL-WHEN
           #+allegro CL:*READTABLE*)
    do (shadowing-import symbol "STELLA"))


;; Load support libraries for TCP/IP

#+Allegro
(ignore-errors (require :socket))
#+(and MCL (not OPENMCL))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((ccl::*warn-if-redefine* nil)
	(ccl::*warn-if-redefine-kernel* nil))
    (require :OPENTRANSPORT)))
#+Lispworks
(require "comm")


(in-package "STELLA")

;; Boot-time kludges:

#|
(CL:defun cast (value type)
  (CL:declare (CL:ignore type))
  value)

(CL:defmacro cast (value type)
  (CL:declare (CL:ignore type))
  value)
|#

(CL:defun get-sgt (offset)
  (CL:declare (CL:ignore offset)))

(CL:declaim
 (CL:special
     NULL NULL-INTEGER TRUE FALSE
     ;; Avoid some warnings:
     NIL NIL-LIST NULL-STRING-WRAPPER NULL-CODE-WRAPPER
     NULL-FLOAT NULL-STRING NULL-CHARACTER NULL-NATIVE-VECTOR))

;; Set these via 'CL:setq' so we'll avoid multiple definitions:
(CL:setq NULL :null_value)
(CL:setq NULL-INTEGER CL:MOST-NEGATIVE-FIXNUM)
(CL:setq TRUE CL:t)
(CL:setq FALSE CL:nil)


  ;;
;;;;;; Low-level support for CLOS and CL-struct slot value access
  ;;

;;; The class prototype table links CLOS class prototypes to class
;;; names and vice versa.  Class prototypes are used by vector structs
;;; for method dispatch, and in an Allegro-specific "fast-slot hack"
;;; that allows fast CLOS slot access for the cost of two struct slot
;;; accesses.

(CL:defvar *clsys-class-prototype-table*
    (CL:make-hash-table :test #'CL:eq :size 200))

(CL:defun clsys-define-class-prototype (className)
  ;; Define a prototype for the class named 'className'.
  (CL:let ((prototype (CL:make-instance className)))
    ;; Make the prototype a defined key in the prototype table:
    (CL:setf (CL:gethash prototype *clsys-class-prototype-table*) className)
    ;; Link the prototype to its name:
    (CL:setf (CL:get className 'class-prototype) prototype)))

(CL:defmacro clsys-class-prototype (className)
  ;; Retrieve the prototype for the class named 'className'.
  ;; Define a prototype if necessary.
  `(CL:or (CL:get ,className 'class-prototype)
          (clsys-define-class-prototype ,className)))

(CL:defmacro clsys-class-prototype-p (thing)
  ;; True if 'thing' is a class prototype.
  `(CL:gethash ,thing *clsys-class-prototype-table*))

;;; For fast and generic access to slots in vector structs as well as for
;;; the Allegro fast-slot hack we define a vector struct that can be
;;; dynamically redefined to accomodate more slots.

(CL:defvar *clsys-fast-slots-number* CL:nil)

(CL:defun clsys-yield-fast-slot-name (index)
  (CL:intern (CL:format CL:nil "CLSYS-FAST-SLOT-~d" index) "STELLA"))

(CL:defun clsys-fast-slot-lookup (object slot-index)
  ;; A forward-declaration for the dynamically generated lookup function.
  (CL:declare (CL:ignore object slot-index)))

(CL:defun clsys-define-fast-slot-accessors (nofSlots)
  ;; Define a vector struct with 'nofSlots' and a corresponding
  ;;    'clsys-fast-slot-lookup' function.
  ;; The lookup function is only used for the Allegro fast-slot hack
  ;;    to disect CLOS instances.
  (CL:declare (CL:type CL:fixnum nofSlots))
  (CL:eval
   `(CL:defstruct (clsys-fast-slot-struct
                   (:type cl:vector) (:conc-name CL:nil))
      ,@(CL:loop for i from 0 to (CL:1- nofSlots)
            collect (clsys-yield-fast-slot-name i))))
  (CL:eval
   `(CL:defun clsys-fast-slot-lookup (object slot-index)
      (CL:declare
       (CL:optimize (CL:speed 3) (CL:safety 0) (CL:space 0) (CL:debug 0)))
      (CL:case slot-index
        ,@(CL:loop for i from 0 to (CL:1- nofSlots)
              collect `(,i (,(clsys-yield-fast-slot-name i) object)))
        (CL:otherwise
         (CL:error "clsys-fast-slot-lookup: 'slot-index' too high.")))))
  (CL:compile 'clsys-fast-slot-lookup)
  (CL:setq *clsys-fast-slots-number* nofSlots))

(CL:defun clsys-possibly-expand-fast-slot-accessors (minNofSlots)
  ;; Make sure the current version of 'clsys-fast-slot-struct' has
  ;;    at least 'minNofSlots' slots.
  (CL:declare (CL:type CL:fixnum minNofSlots))
  (CL:when (CL:< *clsys-fast-slots-number* minNofSlots)
    (clsys-define-fast-slot-accessors (CL:+ minNofSlots 10))))

(clsys-define-fast-slot-accessors 30)

;;; Support for Allegro-CL (ACL) fast-slot hack:

;;; If in ACL we compile a slot access to a vector struct with the highest
;;; possible optimization, all dynamic type checking is eliminated and we
;;; we can use that slot access to disect various Lisp data structures.
;;; For example, '(clsys-fast-slot-lookup i 0)' for some CLOS instance 'i'
;;; returns the data structure that points back from the instance to the
;;; class.  '(clsys-fast-slot-lookup i 1)' returns the actual vector of
;;; slot values of 'i'.  Since in Stella we know the type of the object
;;; on which a slot is accessed, we can access the slot of a CLOS object
;;; for the cost of two struct slot accesses, if we know the index of the
;;; slot in the slot-value vector.  Luckily, these slot indicies can be
;;; determined experimentally at compile time.

(CL:defun clsys-find-fast-slot-index (object slotValue)
  ;; Return the index of the slot on 'object' that contains 'slotValue',
  ;;    or CL:nil if no such slot exists.
  (CL:let ((nofSlots (CL:length object))
           (slotIndex CL:nil))
    (CL:declare (CL:type CL:fixnum nofSlots))
    (clsys-possibly-expand-fast-slot-accessors nofSlots)
    (CL:dotimes (i nofSlots)
      (CL:when (CL:eq (clsys-fast-slot-lookup object i) slotValue)
        (CL:setq slotIndex i)
        (CL:return)))
    slotIndex))

(CL:defvar *clsys-safe-to-use-fast-CLOS-slot-value-p* CL:nil)
(CL:defvar *clsys-fast-CLOS-slot-value-checked-for-pathname* CL:nil)

(CL:defun clsys-safe-to-use-fast-CLOS-slot-value-p ()
  ;; Return true if it is safe to use the ACL fast-slot hack.
  ;; It is safe if we are currently compiling code with the highest
  ;;    optimization setting.  The only way to reliably know whether we are
  ;;    currently compiling is to check whether 'CL:*compile-file-pathname*'
  ;;    is bound.  The only way to check whether we have the highest
  ;;    optimization setting is to compile some actual code and see whether
  ;;    its execution triggers an error or not.  Since this test is somewhat
  ;;    expensive, we make sure it is only executed once per compiled file.
  ;; NOTE: We use CL-USER as the package for '*compile-file-pathname*' to
  ;;       avoid importing that symbol into the CL package in Lisps that
  ;;       do not define 'CL:*compile-file-pathname*'.
  (CL:when (CL:and (CL:boundp 'CL-USER::*compile-file-pathname*)
                   CL-USER::*compile-file-pathname*
                   ;; Disable fast slots for now, since multiple
                   ;;    inheritance can trip us up:
                   (CL:not :disabled-p))
    (CL:if (CL:eq CL-USER::*compile-file-pathname*
                  *clsys-fast-CLOS-slot-value-checked-for-pathname*)
        *clsys-safe-to-use-fast-CLOS-slot-value-p*
      (CL:setq *clsys-safe-to-use-fast-CLOS-slot-value-p*
        (CL:handler-case
            (CL:and (CL:setq *clsys-fast-CLOS-slot-value-checked-for-pathname*
                      CL-USER::*compile-file-pathname*)
                    (CL:funcall
                     (CL:compile
                      CL:nil
                      '(CL:lambda ()
                        (clsys-fast-slot-0
                         (CL:find-class 'CL:standard-class)))))
                    CL:T)
          (CL:error CL:NIL))))))

(CL:defun clsys-yield-fast-CLOS-slot-value-tree (className slotName objectRef)
  ;; Yield a slot access tree to the slot 'className.slotName' on 'objectRef'
  ;;    via the ACL fast-slot hack if possible.
  #-(or allegro-v4.2 allegro-v4.3)
  (CL:declare (CL:ignore className slotName objectRef))
  #+(or allegro-v4.2 allegro-v4.3)
  (CL:when (clsys-safe-to-use-fast-CLOS-slot-value-p)
    (CL:handler-case
        (CL:when (CL:find-class className CL:nil)
          (CL:let ((prototype (clsys-class-prototype className))
                   (fastSlotIndex CL:nil))
            (CL:setf (CL:slot-value prototype slotName) slotName)
            (CL:setq fastSlotIndex
              (clsys-find-fast-slot-index
               (clsys-fast-slot-lookup prototype 1) slotName))
            (CL:when fastSlotIndex
              (CL:return-from
                  clsys-yield-fast-CLOS-slot-value-tree
                `(,(clsys-yield-fast-slot-name fastSlotIndex)
                  (clsys-fast-slot-1 ,objectRef))))))
      (CL:error CL:NIL))))

(CL:defun clsys-yield-CLOS-slot-accessor-name (className slotName)
  ;; Mirrors 'yield-CLOS-slot-accessor-name'.
  (CL:intern (CL:concatenate 'CL:string "%" (CL:symbol-name slotName))
             (CL:symbol-package className)))

(CL:defun clsys-yield-CLOS-slot-value-tree (className slotName objectRef)
  ;; Yield a slot access tree to the slot 'className.slotName' on 'objectRef'.
  ;; If the ACL fast-slot hack can be used use it, otherwise access the slot
  ;;    via its accessor method.
  (CL:let ((fastSlotValueTree
            (clsys-yield-fast-CLOS-slot-value-tree
             className slotName objectRef)))
    (CL:or fastSlotValueTree
           `(,(clsys-yield-CLOS-slot-accessor-name className slotName)
             ,objectRef))))

(CL:defmacro CLSYS-SVAL (CL:&rest arguments)
  ;; Calls to this macro are generated by 'yield-slot-value-reader-tree'
  ;;    to access native storage slots.
  ;; CLOS slots are accessed with the format
  ;;    '(CLSYS-SVAL <class-name> <slot-name> <object-ref>)'.
  ;; Vector struct slots are accessed with the format
  ;;    '(CLSYS-SVAL <slot-offset> <class-name> <slot-name> <object-ref>)'
  ;;    (currently, all but <slot-offset> are ignored).
  (CL:when (CL:and (CL:integerp (CL:first arguments))
                   (CL:not (clsys-use-structs-p))
                   (CL:not (clsys-use-vector-structs-p)))
    (CL:setq arguments (CL:rest arguments)))
  (CL:if (CL:integerp (CL:first arguments))
      (clsys-yield-struct-slot-value-tree
       (CL:first arguments) (CL:second arguments)
       (CL:third arguments) (CL:fourth arguments))
    (clsys-yield-CLOS-slot-value-tree
     (CL:first arguments) (CL:second arguments) (CL:third arguments))))


;;; Support for vector struct slot access and fast method call:

(CL:defun clsys-yield-struct-slot-accessor-name (className slotName)
  ;; Mirrors 'yield-struct-slot-accessor-name'.
  ;; Not used right now.
  (CL:intern (CL:concatenate 'CL:string
                             "%"
                             (CL:symbol-name className)
                             "."
                             (CL:symbol-name slotName))
             (CL:symbol-package className)))

(CL:defun clsys-yield-struct-slot-value-tree
    (slotOffset className slotName objectRef)
  ;; Yield a tree to access the vector struct slot with offset 'slotOffset'.
  (CL:declare (CL:type CL:fixnum slotOffset))
  (clsys-possibly-expand-fast-slot-accessors (CL:1+ slotOffset))
  (CL:if (clsys-use-structs-p)
      `(,(clsys-yield-struct-slot-accessor-name className slotName) ,objectRef)
    `(,(clsys-yield-fast-slot-name slotOffset) ,objectRef)))

(CL:defstruct (CLSYS-ROOT (:type CL:vector) (:conc-name CL:nil))
  ;; This class currently only exists for the purpose of defining
  ;;    the accessor 'clsys-prototype'.
  (clsys-prototype CL:nil))

;;; '*clsys-self*' is used to pass vector struct objects into 'print-object'
;;; methods, and to avoid multiple evaluation of complex first arguments:

(CL:defvar *clsys-self* CL:nil)

(CL:defmacro CLSYS-CALL (methodName firstArg CL:&rest otherArgs)
  ;; Calls to this macro are generated by 'cl-t-normal-call-method-tree'.
  ;; It expands into the actual method call appropriate for 'methodName'.
  (CL:if (CL:eq methodName 'print-object)
      `(CL:let ((*clsys-self* ,firstArg))
         (CL:print-object (clsys-prototype *clsys-self*) ,@otherArgs))
    (CL:if (CL:atom firstArg)
        `(,methodName (clsys-prototype ,firstArg) ,firstArg ,@otherArgs)
      `(,methodName (clsys-prototype (CL:setq *clsys-self* ,firstArg))
                    *clsys-self* ,@otherArgs))))

(CL:defmacro CLSYS-METHOD-CODE-CALL (methodCode firstArg CL:&rest otherArgs)
  ;; Calls to this macro are generated by `cl-t-call-function-code-tree'
  ;;    to translate methode code calls for vector structs.
  (CL:if (CL:atom firstArg)
      `(CL:funcall ,methodCode (clsys-prototype ,firstArg)
                   ,firstArg ,@otherArgs)
    `(CL:funcall ,methodCode (clsys-prototype (CL:setq *clsys-self* ,firstArg))
                 *clsys-self* ,@otherArgs)))

(CL:defmacro CLSYS-MAKE (className nofSlots)
  ;; Generate a vector struct for the class named 'className'
  ;;    with 'nofSlots' slots.  A class prototype object will
  ;;    be stored in slot 0 of the generated structure.
  (CL:declare (CL:type CL:fixnum nofSlots))
  `(CL:vector (clsys-class-prototype ',className)
              ,@(CL:make-list (CL:1- nofSlots))))

(CL:defun clsys-use-structs-p ()
  ;; Return true if the current instance of Stella uses standard CL structs.
  (CL:let* ((haveStandardListClassP
             (CL:eq (CL:type-of (CL:find-class 'OBJECT CL:nil))
                    'CL:STANDARD-CLASS)))
    (CL:not haveStandardListClassP)))

(CL:defun clsys-use-vector-structs-p ()
  ;; Return true if the current instance of Stella uses vector structs.
  (CL:let* ((haveStandardListClassP
             (CL:eq (CL:type-of (CL:find-class 'OBJECT CL:nil))
                    'CL:STANDARD-CLASS)))
    (CL:and haveStandardListClassP
            (CL:not (CL:slot-exists-p (CL:make-instance 'LIST) 'THE-CONS-LIST)))))

;;; Support for making 'print-object' work on vector structs:

(CL:defun clsys-vector-struct-p (thing)
  ;; Determine whether 'thing' is a vector struct by checking whether
  ;;    it is a vector whose first element is a class prototype.
  (CL:and (CL:vectorp thing)
          (CL:> (CL:length thing) 0)
          (clsys-class-prototype-p (CL:aref thing 0))))

(CL:defun clsys-print-vector-struct-as-vector (self stream)
  ;; Print the vector struct 'self' onto 'stream'.
  ;; For debugging.
  (CL:let ((length (CL:length self))
           (CL:*print-pretty* CL:nil))
    (CL:format stream "#CLSYS(")
    (CL:when (CL:> length 0)
      (CL:prin1 (CL:aref self 0) stream))
    (CL:loop for i from 1 to (CL:1- length)
        do (CL:format stream " ~s" (CL:aref self i)))
    (CL:format stream ")")))

(CL:defun clsys-print-vector-struct (self stream)
  ;; Print the vector struct 'self' onto 'stream' via a call to 'print-object'.
  ;; 'print-object' is dispatched on the class prototype, and the actual
  ;;    vector struct is passed in with help of '*clsys-self*'.
  (CL:let ((*clsys-self* self))
    (CL:print-object (CL:aref self 0) stream)))

#|
(CL:defmethod print-object :around ((self CL:vector) stream)
  ;; Advised version of 'print-object' that can handle vector structs.
  ;; This redefinition is actually performed by startup-time code of
  ;;    'cl-translate.ste', since it should only be done if the current
  ;;    instance of Stella actually uses vector structs.
  (CL:if (clsys-vector-struct-p self)
      (clsys-print-vector-struct self stream)
    (CL:call-next-method)))
|#

;;; Support for implementing STELLA conses as Lisp conses:

(CL:defmacro %%defconsmethod (name ((selfVar selfType) CL:&rest otherArgs)
                              CL:&body body)
  ;; Helper method to define STELLA methods on Lisp conses.
  ;; For CONS methods we always need an extra method on CL:nil.
  ;; For OBJECT/STANDARD-OBJECT we always need and extra method on
  ;;     CL:cons and CL:nil.
  `(CL:progn
     (CL:defmethod ,name ((,selfVar ,selfType) ,@otherArgs)
       ,@body)
     ,@(CL:when (CL:not (CL:eq selfType 'CL:CONS))
         `((CL:defmethod ,name ((,selfVar CL:CONS) ,@otherArgs)
       ,@body)))
     (CL:defmethod ,name ((,selfVar (CL:eql CL:nil)) ,@otherArgs)
       ,@body)))

(CL:defmacro %%value (x)
  ;; Read access macro for the `value' slot on a Lisp cons.
  ;; Slightly complicated, since it has to return NULL if applied to CL:nil.
  (CL:if (CL:symbolp x)
      `(CL:if ,x (CL:car (CL:the CL:cons ,x)) NULL)
    (CL:let ((var (CL:gensym)))
      `(CL:let ((,var ,x))
         (CL:if ,var (CL:car (CL:the CL:cons ,var)) NULL)))))

(CL:defsetf %%value (x) (new-value)
  ;; Write access macro for the `value' slot on a Lisp cons.
  ;; Slightly complicated, since it has to return NULL if applied to CL:nil.
  (CL:if (CL:symbolp x)
      `(CL:if ,x (CL:setf (CL:car (CL:the CL:cons ,x)) ,new-value) NULL)
    (CL:let ((var (CL:gensym)))
      `(CL:let ((,var ,x))
         (CL:if ,var (CL:setf (CL:car (CL:the CL:cons ,var)) ,new-value) NULL)))))

(CL:defmacro %%rest (x)
  ;; Access macro for the `rest' slot on a Lisp cons.
  `(CL:cdr (CL:the CL:list ,x)))

;;; %%print-stream:

(CL:defun %%print-object (stream object)
  ;; Print `object' onto `stream'.  Similar to `CL:print-object', but lets
  ;;    us work around a problem in CMUCL where CONSes are not printed via
  ;;    `CL:print-object'.
  ;; Also allows us to dynamically map the value of the STELLA variable
  ;;    `*printPretty?*' onto its CL analogue.
  ;; Currently, this uses run-time type tests, but conceivably we could
  ;;    supply type information from the translator for the price of
  ;;    some more elaborate/less readable `%%print-stream' syntax.
  (CL:declare (CL:special *printPretty?*))
  (CL:let ((CL:*print-pretty* *printPretty?*)
           ;; set various Lisp print control variables to safe values:
           (CL:*print-length* CL:nil)
           (CL:*print-level* CL:nil)
           (CL:*print-circle* CL:nil)
           (CL:*print-base* 10)
           (CL:*print-escape* CL:t))
    (CL:typecase object
      (CL:STRING
       (CL:write-string object stream))
      (CL:CONS
       (%%print-cons stream object))
      (CL:FLOAT
       (CL:let ((CL:*read-default-float-format* 'CL:double-float))
         (CL:princ object stream)))
      (CL:CHARACTER
       (CL:princ object stream))
      (CL:NULL
       (CL:write-string "()" stream))
      (CL:T
       (CL:prin1 object stream)))
    object))

(CL:defvar *%%print-cons-pprint-dispatch* CL:nil)

(CL:defun %%print-cons (stream list)
  ;; Print the `list' onto `stream'.  Make sure we pretty-print if necessary
  ;;    and that CL:NIL is printed as `()'.
  ;; Works around a CMUCL problem where redefining `CL:print-object' on CL:NIL
  ;;    doesn't do the right thing, and where CONSes are not printed by going
  ;;    through `CL:print-object'.
  (CL:cond (CL:*print-pretty*
            ;; Use some magic with the pprint dispatch table to make
            ;;    CL:NIL print as `()':
            (CL:when (CL:null *%%print-cons-pprint-dispatch*)
              ;; initialize it:
              (CL:setq *%%print-cons-pprint-dispatch*
                (CL:copy-pprint-dispatch CL:nil))
              (CL:set-pprint-dispatch
               'CL:null #'%%print-object 0.0 *%%print-cons-pprint-dispatch*))
            (CL:let ((CL:*print-pprint-dispatch* *%%print-cons-pprint-dispatch*))
              (CL:prin1 list stream)))
           (CL:t
            ;; Unpretty-print it by hand:
            (CL:write-string "(" stream)
            (CL:loop for sub on list
                do (CL:if (CL:listp (CL:car sub))
                       (%%print-cons stream (CL:car sub))
                     (CL:prin1 (CL:car sub) stream))
                   (CL:when (CL:rest sub)
                     (CL:write-string " " stream)))
            (CL:write-string ")" stream)))
  list)

(CL:defmacro %%print-stream (stream CL:&rest args)
  ;; Common-Lisp version of `print-stream': Print `args' onto `stream'.
  ;; Use `CL:write-string' whenever possible, `CL:terpri' for EOL, and
  ;;    `CL:force-output' for interactive streams (somehow, `CL:terpri'
  ;;    doesn't seem to do that in ACL 5.0.1).
  (CL:let ((streamVar (CL:gensym)))
    `(CL:let ((,streamVar ,stream))
       ,@(CL:loop for arg in args
             collect (CL:cond ((CL:stringp arg)
                               `(CL:write-string ,arg ,streamVar))
                              ((CL:and (CL:symbolp arg)
                                       (CL:equal (CL:symbol-name arg) "EOL"))
                               `(CL:terpri ,streamVar))
                              (CL:t
                               `(%%print-object ,streamVar ,arg))))
       (CL:when (CL:or (CL:eq ,streamVar CL:*terminal-io*)
                       (CL:eq ,streamVar CL:*standard-output*))
         (;; work around an Allegro 6+ problem when printing a backtrace:
          #+allegro
          CL:ignore-errors
          #-allegro
          CL:progn
          (CL:force-output ,streamVar))))))

;;; Read Line support.  Test return type of the READ-LINE function:

(CL:unless (cl:with-input-from-string (s "Test string")
             (cl:simple-string-p (cl:read-line s)))
    (cl:push :stella-coerce-readline cl:*features*))

;;; %%read-sequence  &  %%write-sequence

#.(CL:if (CL:fboundp (CL:find-symbol "READ-SEQUENCE" "COMMON-LISP"))
      '(CL:setf (CL:symbol-function '%%read-sequence)
        (CL:symbol-function (CL:find-symbol "READ-SEQUENCE" "COMMON-LISP")))
    '(CL:defun %%read-sequence (sequence stream CL:&key (start 0) end)
      ;; Emulate `CL:read-sequence' in those impoverished Lisps that don't have
      ;;    it (currently MCL <= 4.2 is a problem).
      ;; This uses `CL:read-char' which will be hideously slow.  We could do
      ;;    better using `CL:read-line', but since a line might be longer than
      ;;    `sequence' we'd need to do our own buffering and associate buffers
      ;;    with streams.
      ;; IMPORTANT: We require `sequence' to be a string instead of an arbitrary
      ;;    sequence (for speed).
      (CL:declare (CL:type CL:simple-string sequence))
      (CL:let (ch)
        (CL:when (CL:null end)
          (CL:setq end (CL:length sequence)))
        (CL:loop for i from start below end
            do (CL:setq ch (CL:read-char stream CL:nil :eof))
               (CL:when (CL:eq ch :eof)
                 (CL:setq end i)
                 (CL:return))
               (CL:setf (CL:aref sequence i) (CL:the CL:character ch)))
        end)))

#.(CL:if (CL:fboundp (CL:find-symbol "WRITE-SEQUENCE" "COMMON-LISP"))
      '(CL:setf (CL:symbol-function '%%WRITE-sequence)
        (CL:symbol-function (CL:find-symbol "WRITE-SEQUENCE" "COMMON-LISP")))
    '(CL:defun %%WRITE-sequence (sequence stream CL:&key (start 0) end)
      ;; Emulate `CL:WRITE-sequence' in those impoverished Lisps that don't have
      ;;    it (currently MCL <= 4.2 is a problem).
      ;; This uses `CL:write-string'.
      ;; IMPORTANT: We require `sequence' to be a string instead of an arbitrary
      ;;    sequence (for speed).
      (CL:declare (CL:type CL:simple-string sequence))
      (CL:when (CL:null end)
        (CL:setq end (CL:length sequence)))
      (CL:write-string sequence stream :start start :end end)))


;;; %%open-network-stream

(cl:defun %%open-network-stream (host port)
  ;; Opens a TCP/IP network stream connecting to `host' on `port'.
  #+allegro
  (socket:make-socket :remote-host host :remote-port port
		      :address-family :internet
		      :type :stream 
		      :connect :active
		      :format
                      #.(CL:read-from-string
                         "#+(version>= 5 0 1) :bivalent
                          #-(version>= 5 0 1) :text"))
  #+OPENMCL
  (ccl:make-socket :remote-host host :remote-port port
                   :address-family :internet
                   :type :stream 
                   :connect :active
                   :format :bivalent)
  #+(and MCL (not OPENMCL))
  (ccl::open-tcp-stream host port)
  #+Lispworks
  (comm:open-tcp-stream host port)
  #-(or :allegro :MCL :Lispworks)
  (CL:error "Don't know how to open a network stream in this Lisp dialect")
  )

;;; Condition message handling.
;; These are used because some Lisps, at least MCL <= 4.2 apparently
;;  implmented based on CLTL2 instead of the ANSI spec and used an
;;  older name for the :format-control argument.


#.(CL:if (CL:fboundp (CL:find-symbol "SIMPLE-CONDITION-FORMAT-CONTROL" "COMMON-LISP"))
    '(cl:defvar *condition-message-keyword* :FORMAT-CONTROL)
    '(cl:defvar *condition-message-keyword* :FORMAT-STRING))

#.(CL:if (CL:fboundp (CL:find-symbol "SIMPLE-CONDITION-FORMAT-CONTROL" "COMMON-LISP"))
    '(CL:setf (CL:symbol-function '%%simple-condition-format-control)
      (CL:symbol-function (CL:find-symbol "SIMPLE-CONDITION-FORMAT-CONTROL" "COMMON-LISP")))
    '(CL:setf (CL:symbol-function '%%simple-condition-format-control)
      (CL:symbol-function (CL:find-symbol "SIMPLE-CONDITION-FORMAT-STRING" "COMMON-LISP"))))

(cl:defun %%get-exception-message (condition)
  ;; Common Lisp has to run through format since some Lisp systems
  ;;  will add additional format directives to the string used to
  ;;  create the message.  Also, using the arguments allows interaction
  ;;  with native exceptions.
  ;; Note that the CL spec only requires defined message strings for
  ;;  subtypes of CL:SIMPLE-ERROR, not CL:ERROR in general.
  (cl:if (cl:typep condition 'cl:simple-condition)
    (cl:apply #'cl:format cl:nil 
	      (%%simple-condition-format-control condition)
	      (cl:simple-condition-format-arguments condition))
    ""))

(cl:defun %%print-exception-context (condition stream)
  ;; System dependent printing of error context
  (cl:declare (cl:ignore condition))
  (cl:let ((cl:*debug-io* stream))
    #+:EXCL (tpl::zoom-print-stack-1 stream 20)
    #+:MCL  (ccl:print-call-history)
    #+:CMU  (debug:backtrace)
    ))

;;; %%translate-logical-pathname

(CL:defun %%translate-logical-pathname (pathname)
  ;; Just like `CL:translate-logical-pathname' but doesn't break if `pathname'
  ;;    is already a physical pathname (it shouldn't, but CMUCL's version isn't
  ;;    quite as robust).
  (CL:setq pathname (CL:pathname pathname))
  (CL:if (CL:eq (CL:type-of pathname) 'CL:logical-pathname)
      (CL:translate-logical-pathname pathname)
    pathname))


  ;;
;;;;;; Object hash support in case `cl:sxhash' doesn't provide that
  ;;

;; Need to benchmark `cl:sxhash' at compile-time, so we can properly define
;;    CLSYS-ROOT-STRUCT and CLSYS-ROOT-OBJECT below:

(CL:eval-when (:compile-toplevel :load-toplevel)
  
(CL:defstruct clsys-test-struct slot-a slot-b)
(CL:defclass clsys-test-class () ((slot-a) (slot-b)))

(CL:defun clsys-compute-hash-code-statistics (hashCodes)
  ;; Compute distribution statistics for the list of `hashCodes' and
  ;;    return the result.
  (CL:declare (CL:type CL:cons hashCodes))
  (CL:let ((n (CL:length hashCodes))
           (mean 0.0d0)
           (median 0)
           (standardDeviation 0.0d0)
           (variance 0.0d0)
           (min CL:MOST-POSITIVE-FIXNUM)
           (max 0))
    (CL:declare (CL:type CL:double-float mean standardDeviation variance)
		(CL:type CL:fixnum n min max median))
    (CL:setq hashCodes (CL:sort hashCodes #'CL:<))
    (CL:setq median (CL:nth (CL:floor n 2) hashCodes))
    (CL:loop for code cl:fixnum in hashCodes
        do (CL:incf mean code)
           (CL:when (CL:< code min)
             (CL:setq min code))
           (CL:when (CL:> code max)
             (CL:setq max code)))
    (CL:setq mean (CL:/ mean n))
    (CL:loop for code cl:fixnum in hashCodes
        do (CL:incf variance (CL:* (CL:- code mean) (CL:- code mean))))
    (CL:setq variance (CL:/ variance n))
    (CL:setq standardDeviation (CL:sqrt variance))
    (CL:values min max mean median standardDeviation variance)))

(CL:defun clsys-test-sxhash-support (type)
  ;; Return true if `CL:sxhash' computes reasonable hash codes for objects
  ;;    of `type' (either :CLOS or :STRUCT).
  (CL:let* ((n 1000)
            (hashCodes
             (CL:loop for i from 1 to n
                 collect (CL:sxhash (CL:if (CL:eq type :CLOS)
                                        (CL:make-instance 'clsys-test-class)
					(make-clsys-test-struct))))))
    (cl:declare (cl:type cl:fixnum n))
    (CL:multiple-value-bind (min max mean median standardDeviation variance)
        (clsys-compute-hash-code-statistics hashCodes)
      (cl:declare (cl:ignore variance median)
		  (cl:type cl:fixnum min max)
		  (cl:type cl:double-float mean standardDeviation))
      (CL:and
       ;; test whether we get a reasonable number of different codes:
       (CL:> (CL:- max min) (CL:* n 10))
       ;; the mean should be somewhere in the middle of the interval:
       (CL:< (CL:* 0.4d0 (CL:- max min)) mean (CL:* 0.7d0 (CL:- max min)))
       ;; the standard deviation should be somewhere around 0.5 of the mean:
       (CL:< (CL:* 0.4d0 mean) standardDeviation (CL:* 0.7d0 mean))))))

(CL:defvar *clsys-sxhash-supported-object-types*
    `(,@(CL:and (clsys-test-sxhash-support :STRUCT) '(:STRUCT))
        ,@(CL:and (clsys-test-sxhash-support :CLOS) '(:CLOS))))
(cl:declaim (cl:type cl:cons *clsys-sxhash-supported-object-types*))

(CL:defun clsys-sxhash-supported-object-p (type)
  ;; Return true if `CL:sxhash' computes reasonable hash codes for objects
  ;;    of `type' (either :CLOS or :STRUCT).
  (CL:find type *clsys-sxhash-supported-object-types*))

) ;; eval-when

;;; The root struct/class below are necessary to optionally add a hash-code
;;;    slot in case the native CL:sxhash implementation isn't usable for
;;;    that type of object.  For structs we also add some printing magic
;;;    to make Lisps such as CMUCL happy.  We define both kinds of root objects
;;;    since at this point of the loading sequence we don't yet know whether
;;;    we are using CLOS objects or structs (conceivably, we could key in on
;;;    the value of `cl:user::*load-cl-struct-stella?*' defined in
;;;    `load-stella.ste' to determine that).

#.`(CL:defstruct (CLSYS-ROOT-STRUCT
                  (:conc-name CLSYS-ROOT-STRUCT.)
                  (:print-function
                   ;; Need this to make Lisp like CMUCL happy; just defining
                   ;;    a `print-object' method is not enough:
                   (CL:lambda (self stream depth)
                     (CL:declare (CL:ignore depth))
                     (CL:print-object self stream)))
                  (:constructor ())
                  (:copier ())
                  (:predicate ()))
     ,@(CL:unless (clsys-sxhash-supported-object-p :STRUCT)
         '((|%HaShCoDe| -1))))

#.`(CL:defclass CLSYS-ROOT-OBJECT ()
     (,@(CL:unless (clsys-sxhash-supported-object-p :CLOS)
          '((|%HaShCoDe| :allocation :instance
                         :accessor |%%HaShCoDe|
                         :initform -1)))))

(CL:defmacro %%object-hash-code (object)
  ;; Expand into a form that computes an EQ hash-code for `object' (assumed to
  ;;    be a STELLA OBJECT implemented via CLOS or structs or CL:conses).
  ;; Uses `CL:sxhash' if it is usable, otherwise it accesses and/or initializes
  ;;    the object's |%HaShCoDe| slot.
  ;; Guarantees the returned value is a CL:FIXNUM.
  (CL:let* ((mask
             (cl:1- (CL:expt 2 (CL:1- (cl:integer-length
                                       cl:most-positive-fixnum)))))
            (hashCodeForm
             `(CL:logand (CL:the CL:INTEGER (CL:sxhash ,object)) ,mask))
            (objectVar (CL:if (CL:atom object) object (CL:gensym)))
            (hashCodeSlot
             (CL:if (clsys-use-structs-p)
                 `(|CLSYS-ROOT-STRUCT.%HaShCoDe| ,objectVar)
               `(|%%HaShCoDe| ,objectVar))))
    (CL:when (CL:or (CL:and (clsys-use-structs-p)
                            (CL:not (clsys-sxhash-supported-object-p :STRUCT)))
                    (CL:and (CL:not (clsys-use-structs-p))
                            (CL:not (clsys-sxhash-supported-object-p :CLOS))))
      (CL:setq hashCodeForm
        `(CL:let* (,@(CL:when (CL:not (CL:atom object))
                       `((,objectVar ,object)))
                   ;; TRICKY: if we are using Lisp conses, we don't have a
                   ;; hash code slot and need to use `CL:sxhash' (which
                   ;; returns a non-negative fixnum so we don't have to test
                   ;; again to guard the slot initialization below):
                   (code (CL:if (CL:listp ,objectVar)
                             (CL:sxhash ,objectVar)
                           ,hashCodeSlot)))
           (CL:declare (CL:type CL:FIXNUM code))
           (CL:when (CL:= code -1)
             (CL:setq code (CL:random ,mask))
             (CL:setf ,hashCodeSlot code))
           code)))
    hashCodeForm))


  ;;
;;;;;; Synchronization Support:
  ;;

(cl:defun %make-process-lock ()
  ;; Returns a process lock object.  Conditionalized for supported Lisp
  ;; systems:
  #+Allegro   (MP:MAKE-PROCESS-LOCK)
  #+Lispworks (MP:MAKE-LOCK)
  #+MCL       (CCL:MAKE-LOCK)
  #+CMUCL     (MULTIPROCESSING:MAKE-LOCK)
  #-(or Allegro Lispworks MCL CMUCL) 'NO-LOCK)

(cl:defmacro with-process-lock (lock CL:&body forms)
  ;; Macro to synchronize a body of code based on a lock.  Conditionalized
  ;; for supported Lisp systems:
  `(#+Allegro   MP:WITH-PROCESS-LOCK
    #+Lispworks MP:WITH-LOCK
    #+MCL       CCL:WITH-LOCK-GRABBED
    #+CMUCL     MULTIPROCESSING:WITH-LOCK-HELD
    #+(or Allegro Lispworks MCL CMUCL) (,lock)
    #-(or Allegro Lispworks MCL CMUCL) CL:PROGN
    ,@forms))

  ;;
;;;;;; Compilation Warning Support:
  ;;

(cl:defmacro with-redefinition-warnings-suppressed (CL:&body forms)
  ;; Wrap form with code to suppress redefinition warnings
  `(cl:let (#+:MCL(CCL::*WARN-IF-REDEFINE* CL:NIL)
	    #+:EXCL(EXCL::*REDEFINITION-WARNINGS* CL:NIL)
	    #+:LUCID(USER::*REDEFINITION-ACTION* CL:NIL)
	    #+:TI(TICL::INHIBIT-FDEFINE-WARNINGS CL:T)
	    #+:LISPWORKS(LISPWORKS::*REDEFINITION-ACTION* CL:NIL))
     ,@forms ))

(cl:defmacro with-undefined-function-warnings-suppressed (CL:&body forms)
  ;; Wrap form with code to suppress undefined function warnings
  `(#-:EXCL Cl:with-compilation-unit #+:EXCL CL:handler-bind 
   (  #+:EXCL(EXCL:compiler-undefined-functions-called-warning
	      #'(Cl:lambda (c) (CL:declare (CL:ignore c))
			   (CL:muffle-warning)))
	     )
    ,@forms ))
