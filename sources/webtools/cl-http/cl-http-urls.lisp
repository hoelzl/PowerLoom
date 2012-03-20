;;; -*- Mode: Lisp; Package: (HTTP-USER (:use HTTP CL)) -*-

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


;;; Version: cl-http-urls.lisp,v 1.5 2010/02/11 00:01:20 hans Exp

(in-package "HTTP-USER")


;;; NOTE: THIS SYSTEM NEEDS TO BE REWRITTEN FOR THE NEW HTTP-SERVER API.
;;;       IT IS CURRENTLY NOT FUNCTIONAL AND ONLY LEFT HERE FOR INSPIRATION.

(setf (logical-pathname-translations "ontosaurus")
  `(("ontosaurus:htdocs;**;*.*"
     ,(namestring
       (make-pathname :host (pathname-host (truename *load-pathname*))
		      :directory (append (butlast (pathname-directory
						   (translate-logical-pathname "pl:sources;")))
                                         '("htdocs" "ontosaurus" :wild-inferiors))
                      :name :wild
                      :type :wild)))))

(export-url #u"/ploom/" :directory
	    :pathname "ontosaurus:htdocs;"
	    :recursive-p t)

;(export-url #u"/ploom/control-panel.js" :javascript
;	    :pathname "/home/tar/powerloom/http/doc/control-panel.js")

(export-url #u"/ploom/shuttle-top.html"
            :html-computed-form
            :form-function #'serve-control-panel
	    :response-function #'respond-to-control-panel
;	    :authentication-realm :browser
;	    :capabilities nil
            :expiration '(:interval 10)
            :keywords '(:loom :browse)
            :documentation "Control panel for browsing PowerLoom objects.")

(export-url #u"/ploom/do-action?" :search
	    ; :authentication-realm :browser
	    ; :capabilities '((:default :browse :hack))
            :response-function #'respond-to-do-action
            :expiration `(:interval 0)
            :keywords '(:ploom :browse :show)
            :documentation
	    "Perform an parameterized ontosaurus action.
The parameters are ACTION,OBJECT-TYPE, MODULE-NAME, OBJECT-NAME.")


(export-url #u"/ploom/query.html" :html-file
            :pathname "ontosaurus:htdocs;query-form.html")

(export-url #u"/ploom/execute-ploom-query.html" :html-computed-form
            :form-function #'respond-to-get-query-form
	    :response-function #'respond-to-execute-query
	    ; :authentication-realm :browser
	    ; :capabilities nil
            :documentation "General query interface for PowerLoom")
