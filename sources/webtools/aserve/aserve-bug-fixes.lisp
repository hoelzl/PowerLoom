;;;-*- Mode: Lisp; Package: NET.ASERVE -*-

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
; Portions created by the Initial Developer are Copyright (C) 2003-2010      ;
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


;;; Version: aserve-bug-fixes.lisp,v 1.10 2010/02/10 23:57:01 hans Exp

(in-package "NET.ASERVE")


;;;;
;;;; Fix some bugs that affect our ability to run Ontosaurus, mostly
;;;; on MCL.  The changed code is commented with "ontosaurus".
;;;; Some/all of this might be obsolete with the new HTTP API.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant $directory-separator$	; ontosaurus
    (let ((pn (namestring (make-pathname :directory '(:absolute "a")))))
      (schar pn (1- (length pn)))))
  ;; Constants for strings don't work in all lisps.  Make this a parameter instead.
  (defparameter $directory-separator-string$	; ontosaurus
    (make-string 1 :initial-element $directory-separator$)))

(defmacro fixup-directory-separators (namestring)       ; ontosaurus
  ;; Fixes directory separators for those operating systems that
  ;; don't use "/" as a valid directory separator character.  That
  ;; pretty much just means the Mac...
  ;; This is therefore a macro, since it becomes a no-op on the other
  ;; operating systems.
  (if (char= $directory-separator$ #\/)
      namestring
    `(substitute ,$directory-separator$ #\/ ,namestring)))


;;; Fix up directory separator characters
;;;
;;; From file publish.cl
#-allegro
(defmethod process-entity ((req http-request) (ent directory-entity))
  ;; search for a file in the directory and then create a file
  ;; entity for it so we can track last modified.
  
  ; remove the prefix and tack and append to the given directory
  
  (let* ((postfix (subseq (request-decoded-uri-path req)        ; ontosaurus
                          (length (prefix ent))))
	 (realname (concatenate 'string
		     (entity-directory ent)
                     (fixup-directory-separators postfix)))     ; ontosaurus
	 (redir-to)
	 (info)
	 (forbidden)
	 )
    (debug-format :info "directory request for ~s~%" realname)
    
    ; we can't allow the brower to specify a url with 
    ; any ..'s in it as that would allow the browser to 
    ; search outside the tree that's been published
    (if* (or #+mswindows (position #\\ postfix) ; don't allow windows dir sep
	     (match-regexp "\\.\\.[\\/]" postfix))
       then ; contains ../ or ..\  
	    ; ok, it could be valid, like foo../, but that's unlikely
	    ; Also on Windows don't allow \ since that's a directory sep
	    ; and user should be using / in http paths for that.
	    (return-from process-entity nil))

    #+allegro
    (if* sys:*tilde-expand-namestrings*
       then (setq realname (excl::tilde-expand-unix-namestring realname)))
    
    (multiple-value-setq (info forbidden)
      (read-access-files ent realname postfix))
    
    (if* forbidden
       then ; give up right away.
	    (return-from process-entity nil))
    
    (let ((type (#+:excl excl::filesys-type
		 #-:excl acl-compat.excl::filesys-type
			 realname)))
      (if* (null type)
	 then ; not present
	      (return-from process-entity nil)
       elseif (eq :directory type)
	 then ; Try the indexes (index.html, index.htm, or user-defined).
	      ; tack on a trailing slash if there isn't one already.
	      (if* (not (eql $directory-separator$
			     (schar realname (1- (length realname)))))
		 then (setq realname (concatenate 'string realname
						  $directory-separator-string$))) ;ontosaurus

	      (setf redir-to 
		(dolist (index (directory-entity-indexes ent) 
			  ; no match to index file, give up
			  (return-from process-entity nil))
		  (if* (eq :file (#+:excl excl::filesys-type
				  #-:excl acl-compat.excl::filesys-type
				  (concatenate 'string realname index)))
		     then (return index))))
	      
       elseif (not (eq :file type))
	 then  ; bizarre object
	      (return-from process-entity nil)))
    
    (if* redir-to
       then ; redirect to an existing index file
	    (with-http-response (req ent
				     :response *response-moved-permanently*)
	      (let ((path (uri-path (request-uri req))))
		(setf (reply-header-slot-value req :location) 
		  (concatenate 'string path
			       (if* (and path
					 (> (length path) 0)
					 (eq #\/ (aref path 
						       (1- (length path)))))
				  then ""
				  else "/")
			       redir-to))
			     
		(with-http-body (req ent))))
     elseif (and info (file-should-be-denied-p realname info))
       then ; we should ignore this file
	    (return-from process-entity nil)
     elseif (and (directory-entity-filter ent)
		 (funcall (directory-entity-filter ent) req ent 
			  realname info))
       thenret ; processed by the filter
       else ;; ok realname is a file.
	    ;; create an entity object for it, publish it, and dispatch on it
	    (return-from process-entity
	      (authorize-and-process 
	       req 
	       (funcall 
		(or (directory-entity-publisher ent)
		    #'standard-directory-entity-publisher)
				       
		req ent realname info))))
					   
    t))


;;; Convenience change:
;;; Until MCL supports chunking, set the default to NIL for it.
;;;
;;;  Some fixups for chunking.  These are done using ADVISE to make
;;;  them more robust over underlying Portable Aserve code changes.
;;;

#+:digitool
(ccl:advise net.aserve:start
            ;; Here we tell the server not to use chunking.
            (setq ccl::arglist (list* :chunking nil ccl::arglist))
            :when :before :name :hack-chunking-keyword)


#+:digitool
(ccl:advise net.aserve.client:do-http-request
            ;; Here we set the protocol to 1.0 so the reply won't use chunking.
            (setq ccl::arglist (list* (first ccl::arglist) 
				      :protocol :http/1.0
				      (rest ccl::arglist)))
            :when :before :name :hack-chunking-keyword)
