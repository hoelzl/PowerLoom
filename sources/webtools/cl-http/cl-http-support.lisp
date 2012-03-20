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


;;; Version: cl-http-support.lisp,v 1.4 2010/02/11 00:01:19 hans Exp

(in-package "HTTP-USER")


;;; NOTE: THIS SYSTEM NEEDS TO BE REWRITTEN FOR THE NEW HTTP-SERVER API.
;;;       IT IS CURRENTLY NOT FUNCTIONAL AND ONLY LEFT HERE FOR INSPIRATION.

(defmethod respond-to-do-action  ((url url:http-search) stream)
  (let* ((keys (url:search-keys url))
	 (action (first keys))
	 (objectType (second keys))
	 (moduleName (third keys))
	 (objectName (fourth keys))
	 (params (fifth keys))
	 depthValue user)
    (declare (ignore user depthValue params))
 #|
    (when params
      (when (setq depthvalue (find-integer-parameter "DEPTH" params))
	 (setq user (current-user-object))
	 (set-preference-value user :child-depth depthValue)
	 (write-preference-file (user-name user) user)))
 |#

 (if (>= (length keys) 4)
     (with-successful-response		; Was: with-conditional-get-response
	 (stream :html :expires (url:expiration-universal-time url))
       (stella::HTML-POWERLOOM-RESPONSE
	action objectType moduleName objectName stream))
   (with-successful-response		; Was: with-conditional-get-response
       (stream :html :expires (url:expiration-universal-time url))
     (write-string "ERROR: Four parameters to this search URL were expected, but only "
		   stream)
     (write (length keys) :stream stream)
     (if (= (length keys) 1)
	 (write-string " was supplied:" stream)
	 (write-string " were supplied:" stream))
     (loop for key in keys
	 initially (write-string "<BR><OL>" stream)
	 do (write-string "<LI>" stream)
	    (write-string key stream)
	    (write-string "</LI>" stream)
	    (terpri stream)
	 finally (write-string "</OL>" stream))))
   ))

(defun alist-to-kv-list (alist)
  (let ((kv-list (stella::new-key-value-list)))
    (loop for (key value) in alist
	do (stella::insert-at kv-list
			      (stella::wrap-string (string key))
			      (stella::wrap-string value)))
    kv-list ))

(defmethod respond-to-get-query-form ((url url:http-computed-form) stream)
  (with-successful-response
      (stream :html :expires (url:expiration-universal-time url))
    (stella::HTML-WRITE-QUERY-FORM-PAGE stream)))

(defmethod respond-to-get-query-form ((url url:http-computed-url) stream)
  (with-successful-response
      (stream :html :expires (url:expiration-universal-time url))
    (stella::HTML-WRITE-QUERY-FORM-PAGE stream)))

(defmethod respond-to-execute-query  ((url url:http-computed-form) stream alist)
  (with-successful-response
      (stream :html :expires (url:expiration-universal-time url))
    (handler-case 
	(stella::HTML-EXECUTE-QUERY (alist-to-kv-list alist) stream)
      (error (e) 
	(format stream "Error occurred:<BR>~%~A" e)))))


(defmethod serve-control-panel  ((url url:http-computed-form) stream)
  (with-successful-response
      (stream :html :expires (url:expiration-universal-time url))
    (stella::SERVE-CONTROL-PANEL-INTERNAL stream) ))
(defmethod serve-control-panel  ((url url:http-computed-url) stream)
  (with-successful-response
      (stream :html :expires (url:expiration-universal-time url))
    (stella::SERVE-CONTROL-PANEL-INTERNAL stream) ))

(defmethod respond-to-control-panel ((url url:http-computed-form) stream alist)
  ;; This function shouldn't be called, but if it is, just serve the form again.
  (declare (ignore alist))
  (with-conditional-get-response (stream :html :expires 
					 (url:expiration-universal-time url))
    (stella::SERVE-CONTROL-PANEL-NEEDS-JAVASCRIPT stream)) )

;;;;
;;;;  User Level Startup and Shutdown Code
;;;;

(defun stella::start-ontosaurus (port)
  (unless (= port http:*standard-http-port*)
    (url:remap-url-host HTTP:*LOCAL-HOST-DOMAIN-NAME* HTTP:*LOCAL-HOST-DOMAIN-NAME*
                        :new-port port))
  (enable-http-service :on-ports (list port)))

(defun stella::stop-ontosaurus ()
  (disable-http-service))
