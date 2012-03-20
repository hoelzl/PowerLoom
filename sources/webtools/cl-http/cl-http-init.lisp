;;; -*- Mode: Lisp; Package: CL-USER -*-

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
; Portions created by the Initial Developer are Copyright (C) 2000-2010      ;
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


;;; Version: cl-http-init.lisp,v 1.3 2010/02/11 00:01:18 hans Exp

(in-package "CL-USER")


;;; NOTE: THIS SYSTEM NEEDS TO BE REWRITTEN FOR THE NEW HTTP-SERVER API.
;;;       IT IS CURRENTLY NOT FUNCTIONAL AND ONLY LEFT HERE FOR INSPIRATION.

;;; Need to either have loaded Cl-HTTP beforehand or else
;;; define a function of no arguments called LOAD-CL-HTTP visible in the
;;; CL-USER package that will load the Aserve code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "HTTP-USER")
    (let ((loader-function-symbol (find-symbol "LOAD-CL-HTTP")))
      (if (and loader-function-symbol 
               (fboundp loader-function-symbol))
        (funcall loader-function-symbol)
        (cerror "Continue trying to load file.  Requires CL-HTTP have been loaded."
                "Oops.  Looks like the required CL-HTTP system has not been loaded yet."))))
  (load "http:examples;configuration")
  ;(funcall (intern "ENABLE-HTTP-SERVICE" "HTTP") :on-ports '(8013))
  ;(funcall (intern "DISABLE-HTTP-SERVICE" "HTTP"))
  )
