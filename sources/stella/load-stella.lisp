;;; -*- Mode: Lisp; Package: CL-USER; Syntax: COMMON-LISP; Base: 10 -*-

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
; Portions created by the Initial Developer are Copyright (C) 1996-2006      ;
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


;;; Version: load-stella.lisp,v 1.36 2006/05/16 18:28:19 tar Exp

;;; Load STELLA.

(in-package "CL-USER")



;;; These globals are here to facilitate bootstrapping (before STELLA is up):
;;; They need to include a directory separator at the end:
(defvar *stella-source-directory* "PL:sources;stella;")
(defvar *stella-native-directory* "PL:native;lisp;stella;")
(defvar *stella-binary-directory* "PL:bin;stella;")

;;; Various switches to customize the behavior of STELLA:

(defvar *load-cl-struct-stella?* NIL
  "If T load the version of STELLA that uses Lisp structs instead of
CLOS objects to implement STELLA objects.  This greatly improves slot
access speed for the price of less flexibility with class redefinition.
Use for production versions only.")

(defvar *load-vector-struct-stella?* NIL
  "Obsolete.")

(defvar *use-stella-hash-tables?* #+allegro T #-allegro NIL
  "If T use STELLA's implementation of hash tables instead of native
Lisp hash tables.  Useful if native hash tables are fraught with
performance problems (as happens in some versions of Allegro once
hash tables grow large).")

;;; Compiler optimization for translated Stella files:
(defvar *stella-compiler-optimization*
    '(optimize (speed 3) (safety 1) (space 0) (debug 1)))

(defvar *stella-source-extension* ".ste")
(defvar *stella-lisp-extension* ".lisp")
(defvar *stella-translated-extension*
    (cond (*load-vector-struct-stella?* ".vslisp")
          (*load-cl-struct-stella?* ".slisp")
          (t *stella-lisp-extension*)))
(defvar *stella-binary-extension*
    (format nil ".~a~a"
            (cond (*load-vector-struct-stella?* "vs")
                  (*load-cl-struct-stella?* "s")
                  (t ""))
	    (CL:pathname-type (cl:compile-file-pathname "f.lisp"))))
#+:lispworks
(pushnew (concatenate 'string "s" system:*binary-file-type*)
	 system::*binary-file-types* :test #'equal)
#+:lispworks
(pushnew (concatenate 'string "vs" system:*binary-file-type*)
	 system::*binary-file-types* :test #'equal)

;;; If this is T, Stella will compile/load/startup verbosely:
(defvar *stella-verbose?* *load-verbose*)

;;; This loading scheme still tries to deal with Lisps that do not support
;;; logical pathnames.  But, are there still any "healthy" Lisps like that?

(defparameter *stella-directory-separator*
    (char *stella-source-directory* (1- (length *stella-source-directory*))))

(defun stella-make-file-name (relativePath type &optional root)
  ;; Make an absolute file-name string from 'relativePath' (a string or list
  ;;    of strings ending in the extensionless file-name), 'type', and 'root'.
  ;; 'root' defaults to '*stella-binary-directory*' for binary files, and
  ;;    '*stella-source-directory*' for all other files.
; (format t "~%RELATIVE PATH: ~S   ~S   ~S~%" relativePath type root)
; (format t "TRUENAME ~S~% " (truename *stella-native-directory*))
  (let (file extension)
    (unless (consp relativePath)
      (setq relativePath (list relativePath)))
    (ecase type
      (:stella (setq extension *stella-source-extension*)
               (setq root (or root *stella-source-directory*)))
      (:lisp (setq extension *stella-lisp-extension*)
             (setq root (or root *stella-native-directory*)))
      (:translated (setq extension *stella-translated-extension*)
                   (setq root (or root *stella-native-directory*)))
      (:binary (setq extension *stella-binary-extension*)
               (setq root (or root *stella-binary-directory*)))
      (:directory (setq extension *stella-directory-separator*)
                  (setq root (or root *stella-source-directory*))))
    (setq file (car (last relativePath)))
    (setq relativePath (butlast relativePath))
    (format nil (format nil "~~a~~{~~a~a~~}~~a~~a"
                               *stella-directory-separator*)
                   root relativePath file extension)))

(defun stella-load-source (&rest file)
  ;; Load a Stella source 'file'.
  (let ((*load-verbose* *stella-verbose?*))
    (load (stella-make-file-name file :stella))))

(defun stella-load-lisp (&rest file)
  ;; Load a Lisp source 'file'.
  (let ((*load-verbose* *stella-verbose?*))
    (load (stella-make-file-name file :lisp))))

(defun stella-load-translated (&rest file)
  ;; Load a translated Lisp source 'file'.
  (let ((*load-verbose* *stella-verbose?*))
    (load (stella-make-file-name file :translated))))

(defun stella-load-binary (&rest file)
  ;; Load a binary 'file'.
  (let ((*load-verbose* *stella-verbose?*))
    (load (stella-make-file-name file :binary))))

(defun stella-need-to-compile? (sourceFile binaryFile)
  ;; T if 'sourceFile' needs to be compiled to yield 'binaryFile'.
  (or (not (probe-file binaryFile))
      (not (file-write-date sourceFile))
      (not (file-write-date binaryFile))
      (< (file-write-date binaryFile)
         (file-write-date sourceFile))))

(defun stella-c&l-source (&rest file)
  ;; Compile Stella sources into binaries and load binaries.
  ;; This is exclusively used to compile Stella files with the bootstrap
  ;;    walker and translator, and, since the generated binary files can't
  ;;    be loaded independently anyway, we give them different extensions
  ;;    so they won't interfere with compiled CL-translated Stella files.
  (let* ((*stella-binary-extension* ".bfasl")
         (stellaFileName (stella-make-file-name file :stella))
         (binaryFileName (stella-make-file-name file :binary))
         (*compile-verbose* *stella-verbose?*)
         (*load-verbose* *stella-verbose?*))
    (compile-file stellaFileName :output-file binaryFileName)
    (load binaryFileName)))

(defun stella-c&l-lisp (&rest file)
  ;; Compile Lisp sources into binaries and load binaries.
  ;; Do not compile if the compiled file is up-to-date.
  (let ((stellaFileName (stella-make-file-name file :lisp))
        (binaryFileName (stella-make-file-name file :binary))
        (*compile-verbose* *stella-verbose?*)
        (*load-verbose* *stella-verbose?*))
    (when (stella-need-to-compile? stellaFileName binaryFileName)
      (proclaim *stella-compiler-optimization*)
      (compile-file stellaFileName :output-file binaryFileName))
    (load binaryFileName)))

(defun stella-c&l-translated (&rest file)
  ;; Compile translated Stella sources into binaries and load binaries.
  ;; Do not compile if the compiled file is up-to-date.
  (let ((stellaFileName (CL:translate-logical-pathname
                         (stella-make-file-name file :translated)))
        (binaryFileName (CL:translate-logical-pathname
                         (stella-make-file-name file :binary)))
        (*compile-verbose* *stella-verbose?*)
        (*load-verbose* *stella-verbose?*))
    (when (stella-need-to-compile? stellaFileName binaryFileName)
      (proclaim *stella-compiler-optimization*)
      (compile-file stellaFileName :output-file binaryFileName))
    (load binaryFileName)))

#+allegro (setq excl:*print-nickname* t)
#+allegro (tpl:setq-default excl:*print-nickname* t)

#-:cmu19
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'with-compilation-unit)
    (defmacro with-compilation-unit ((&rest options) &body body)
      (declare (ignore options))
      `(progn ,@body))))

(cond
 ((and (find-package "STELLA")
       (fboundp (find-symbol "STARTUP" (find-package "STELLA"))))
  (format t "~%STELLA has already been loaded.~%"))
 (t
  (stella-c&l-translated "cl-lib" "cl-setup")
  (stella-load-translated "cl-lib" "make-stella")))
