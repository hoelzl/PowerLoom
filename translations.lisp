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


;;; Version: translations.lisp,v 1.22 2003/04/18 21:14:23 hans Exp

;;; Pathname translations for PowerLoom and related systems.

(in-package "CL-USER")

;;  This goes through this whole rigamarole because some Lisps
;;  (like OpenMCL) will not put the trailing directory separator
;;  on the path unless there is a filename following it.  This
;;  strips the filename (being careful to call only NAMESTRING and
;;  not TRUENAME -- which can augment missing pieces).

(defparameter *powerloom-root-directory*
  (let* ((fullfilename (namestring (truename *load-pathname*)))
         (justfilename (namestring (make-pathname :host NIL
                                                  :directory NIL
                                                  :name (pathname-name *load-pathname*)
                                                  :type (pathname-type *load-pathname*)))))
    (subseq fullfilename 0 (- (length fullfilename) (length justfilename)))))


(defparameter *powerloom-binary-directory*
    (format nil "~Abin/~A/"
            *powerloom-root-directory*
            (symbol-name
             (read-from-string
               "#+:ALLEGRO-V4.1   |acl4.1|
                #+:ALLEGRO-V4.2   |acl4.2|
                #+:ALLEGRO-V4.3   |acl4.3|
                #+(:AND :ALLEGRO-V5.0 :SUN)    |acl5.0-sun|
                #+:ALLEGRO-V5.0   |acl5.0|
                #+(:AND :ALLEGRO-V5.0.1 :SUN)  |acl5.0-sun|
                #+:ALLEGRO-V5.0.1 |acl5.0|
                #+:ALLEGRO-V5.1   |acl5.1|
                #+(:AND :ALLEGRO-V6.0 :SUN)    |acl6.0-sun|
                #+:ALLEGRO-V6.0   |acl6.0|
                #+(:AND :ALLEGRO-V6.1 :SUN)    |acl6.1-sun|
                #+:ALLEGRO-V6.1   |acl6.1|
                #+:ALLEGRO        |aclx.x|
                #+:LCL4.1         |lcl4.1|
                #+:LCL4.0         |lcl4.0|
                #+:LCL            |lclx.x|
                #+:MCL            |mcl|
                #+:LISPWORKS4.0   |lw4.0|
                #+:LISPWORKS4.1   |lw4.1|
                                  |lisp|"))))

(let* ((directorySeparator
        (char *powerloom-root-directory*
              (1- (length *powerloom-root-directory*))))
       ;; try to figure out dynamically whether the current Lisp/OS
       ;;    combination wants a two or three-argument physical pattern,
       ;;    since that behavior seems to be somewhat unpredictable:
       (physicalPattern
        (or (ignore-errors
             (progn (setf (logical-pathname-translations "PL")
                      `(("sources;**;*.*.*"
                         ,(format nil "sources~a*.*.*" directorySeparator))))
                    (translate-logical-pathname "PL:sources;foo.ste")
                    "*.*.*"))
            "*.*"))
       (ppLength (length physicalPattern))
       (sourcesTranslation
        (substitute directorySeparator #\/
                    (format nil "~Asources/**/~a"
                            *powerloom-root-directory*
                            physicalPattern)))
       (binariesTranslation
	(substitute directorySeparator #\/
                    (format nil "~a**/~a"
                            *powerloom-binary-directory*
                            physicalPattern)))
       (nativesTranslation
        (substitute directorySeparator #\/
                    (format nil "~anative/**/~a"
                            *powerloom-root-directory*
                            physicalPattern)))
       (kbsTranslation
        (substitute directorySeparator #\/
                    (format nil "~akbs/**/~a"
                            *powerloom-root-directory*
                            physicalPattern)))
       ;; try to figure out dynamically whether the current Lisp/OS
       ;;    combination needs a separate directory-only translation
       ;;    rule.
       (directory-test-temp
        (ignore-errors
         (progn (setf (logical-pathname-translations "PL")
                      `(("sources;**;*.*.*" ,sourcesTranslation)))
                (namestring (translate-logical-pathname "PL:sources;")))))
       (needs-directory-rule?
        (or (null directory-test-temp)
            (char= #\* (char directory-test-temp (1- (length directory-test-temp)))))))
  (flet ((truncate-last (string n)
           ;; Returns STRING minus the last N characters.
           (subseq string 0 (- (length string) n))))
    
    (setf (logical-pathname-translations "PL")
          `(("sources;**;*.*.*" ,sourcesTranslation)
            ("native;**;*.*.*" ,nativesTranslation)
            ("bin;**;*.*.*" ,binariesTranslation)
            ("kbs;**;*.*.*" ,kbsTranslation)))
    (when needs-directory-rule?
      (setf (logical-pathname-translations "PL")
            (append `(("sources;**;" ,(truncate-last sourcesTranslation ppLength))
                      ("native;**;" ,(truncate-last nativesTranslation ppLength))
                      ("bin;**;" ,(truncate-last binariesTranslation ppLength))
                      ("kbs;**;" ,(truncate-last kbsTranslation ppLength)))
                    (logical-pathname-translations "PL")))) ))
