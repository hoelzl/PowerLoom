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


;;; Version: translations.lisp,v 1.33 2010/10/16 17:18:23 hans Exp

;;; Pathname translations for PowerLoom and related systems.

(in-package "CL-USER")

;;  Use the pathname functions to avoid having to deal with
;;  different directory separators and other idiosyncratic
;;  handling of filenames across platforms.

(defparameter *powerloom-root-directory*
  (pathname-directory *load-truename*))


(defparameter *powerloom-binary-subdirectory*
  (first (list #+:ALLEGRO-V8.0   "acl8.0"
               #+:ALLEGRO-V7.0   "acl7.0"
               #+:ALLEGRO-V6.2   "acl6.2"
               #+(:AND :ALLEGRO-V6.2 :SUN)    "acl6.2-sun"
               #+:ALLEGRO-V6.1   "acl6.1"
               #+(:AND :ALLEGRO-V6.1 :SUN)    "acl6.1-sun"
               #+:ALLEGRO-V6.0   "acl6.0"
               #+(:AND :ALLEGRO-V6.0 :SUN)    "acl6.0-sun"
               #+:ALLEGRO-V5.1   "acl5.1"
               #+:ALLEGRO-V5.0.1 "acl5.0"
               #+(:AND :ALLEGRO-V5.0.1 :SUN)  "acl5.0-sun"
               #+:ALLEGRO-V5.0   "acl5.0"
               #+(:AND :ALLEGRO-V5.0 :SUN)    "acl5.0-sun"
               #+:ALLEGRO-V4.3   "acl4.3"
               #+:ALLEGRO-V4.2   "acl4.2"
	       #+:ALLEGRO-V4.1   "acl4.1"
               #+:ALLEGRO        "aclx.x"
               #+:LCL4.1         "lcl4.1"
               #+:LCL4.0         "lcl4.0"
               #+:LCL            "lclx.x"
               #+:CCL-5.1         "mcl5.1"
               #+:CCL-5.0         "mcl5.0"
               #+:MCL            "mcl"
               #+:LISPWORKS4.1   "lw4.1"
               #+:LISPWORKS4.0   "lw4.0"
               #+:CMU20          "cmu20"
               #+:CMU19          "cmu19"
               #+:CMU18          "cmu18"
               #+:CMU            "cmu"
	       #+:SBCL           "sbcl"
	       #+:CLISP          "clisp"
               "lisp")))

;; ACL 5.0, for example, doesn't allow :wild as a version
;; value.
(let* ((default-source *load-truename*)
       (wild-version-value
	(if (ignore-errors (make-pathname :version :wild
					  :defaults default-source))
	    :wild
	  nil)))
  (flet ((create-pathname-pattern (root &rest subs)
	   (make-pathname :directory (append root subs '(:wild-inferiors))
			  :name :wild :type :wild :version wild-version-value
			  :defaults default-source))
	 (create-directory-pattern (root &rest subs)
	   (make-pathname :directory (append root subs '(:wild-inferiors))
			  :name nil :type nil :version nil
			  :defaults default-source)))
    ;; try to figure out dynamically whether the current Lisp/OS
    ;;    combination needs a separate directory-only translation
    ;;    rule.
    (let* ((directory-test-temp
	    (ignore-errors
	      (progn (setf (logical-pathname-translations "PL")
			   `(("sources;**;*.*.*" 
			      ,(create-pathname-pattern
				*powerloom-root-directory* "sources"))))
		     (namestring (translate-logical-pathname "PL:sources;")))))
	   (needs-directory-rule?
	    (or (null directory-test-temp)
		(char= #\* (char directory-test-temp 
				 (1- (length directory-test-temp)))))))
      (setf (logical-pathname-translations "PL")
	    `(("sources;**;*.*.*" ,(create-pathname-pattern
				    *powerloom-root-directory* "sources"))
	      ("native;**;*.*.*" ,(create-pathname-pattern
				   *powerloom-root-directory* "native"))
	      ("bin;**;*.*.*" ,(create-pathname-pattern
				*powerloom-root-directory* "native" "lisp" "bin"
				*powerloom-binary-subdirectory*))
	      ("kbs;**;*.*.*" ,(create-pathname-pattern
				*powerloom-root-directory* "kbs"))
	      ("**;*.*.*" ,(create-pathname-pattern *powerloom-root-directory*))))
      (when needs-directory-rule?
	(setf (logical-pathname-translations "PL")
	      (append `(("sources;**;" ,(create-directory-pattern
					 *powerloom-root-directory* "sources"))
			("native;**;" ,(create-directory-pattern
					*powerloom-root-directory* "native"))
			("bin;**;" ,(create-directory-pattern
				     *powerloom-root-directory* "native" "lisp" "bin"
				     *powerloom-binary-subdirectory*))
			("kbs;**;" ,(create-directory-pattern
				     *powerloom-root-directory* "kbs"))
			("**;" ,(create-directory-pattern *powerloom-root-directory*)))
		      (logical-pathname-translations "PL")))) )))
