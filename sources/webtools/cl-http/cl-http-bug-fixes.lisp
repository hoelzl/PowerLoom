;;; -*- Mode: Lisp; Package: HTTP -*-

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


;;; Version: cl-http-bug-fixes.lisp,v 1.4 2010/02/11 00:01:17 hans Exp

(in-package "HTTP")


;;; NOTE: THIS SYSTEM NEEDS TO BE REWRITTEN FOR THE NEW HTTP-SERVER API.
;;;       IT IS CURRENTLY NOT FUNCTIONAL AND ONLY LEFT HERE FOR INSPIRATION.

(defmacro with-mime-multipart-block (multipart-keyword (stream &key last-block-p (force-output t) sleep-interval 
                                                               content-type content-length content-location last-modified
                                                               expires boundary) &body body)
  "Use this macro when BODY is writing blocks within a mime multipart document.
LAST-BLOCK-P should be non-null when writing the last block of a multipart mime message.
When FORCE-OUTPUT is non-null, output is forced on STREAM
after executing BODY. The HTTP 1.0 specification recommends including a CONTENT-LOCATION within multipart blocks.
Beware that CONTENT-LENGTH, CONTENT-LOCATION, LAST-MODIFIED, expires, and CONTENT-LOCATION can be multiply evaluated.
When SLEEP-INTERVAL is supplied, the process sleeps for SLEEP-INTERVAL seconds after executing the
body and forcing output if FORCE-OUTPUT is non-null."
  (let ((boundary-writer (if boundary
                             `(write-mime-multipart-boundary ,boundary ,stream)
                             `(write-mime-multipart-boundary 
                                (mime-multipart-boundary-string ,multipart-keyword) ,stream))))
    `(let ((headers `(:content-type ,,(typecase content-type
                                        (keyword `(quote ,(%mime-content-type-spec content-type)))
                                        (t `(%mime-content-type-spec ,content-type)))
                      ,,.(when content-length `(:content-length ,content-length))
                      ,,.(when content-location `(:content-location ,content-location))
                      ,,.(when last-modified `(:last-modified ,last-modified))
                      ,,.(when expires `(:expires ,expires)))))
       (declare (dynamic-extent headers))
       ,boundary-writer
       (write-headers ,stream headers t)
       (multiple-value-prog1
         (progn . ,body)
         ,.(when last-block-p `((,@boundary-writer t)))
         ,.(when force-output `((force-output ,stream)))
         ,.(when (and sleep-interval (not last-block-p)) `((sleep ,sleep-interval)))))))



(define nstring-unescape-special-chars (string &optional (start 0) (end (length string))
					       only-safe-characters-p (pad-char #\space) set-fill-pointer-p)
  "When any escaped characters are present, this returns a string with these characters unescaped.
STRING is destructively modified when escaped characters are present.
ONLY-SAFE-CHARACTERS-P set to non-nill skips reserved or unsafe URL characters.
When SET-FILL-POINTER-P is non-null, the fill-pointer on STRING is moved backwards
by two times the number of characters unescaped. Otherwise, that number of characters
are padded on the right using PAD-CHAR"
  (declare (values unescaped-string new-end chars-unescaped-p)
           (fixnum start end))
  (with-fast-array-references ((string string string))
    (loop with read-idx fixnum = start
	  with write-idx fixnum = start
	  and new-char and chars-unescaped-p 
          while (< read-idx end)
          for char = (aref string read-idx)
	  do (incf read-idx)
          when (escape-character-p char)
            do (setf new-char (unescape-character string read-idx (+ read-idx 2)))
               (cond  ;; Skip unescaping a char, just incf idx to skip the hex pair.
                 ((and only-safe-characters-p (uri-reserved-or-unsafe-char-p new-char))
		  (cond (chars-unescaped-p
			 (setf (aref string write-idx) char
			       (aref string (incf write-idx)) (aref string read-idx)
			       (aref string (incf write-idx)) (aref string (incf read-idx)))
			 (incf write-idx)
			 (incf read-idx))
			(t (incf write-idx 3)
			   (incf read-idx 2))))
                 ;; Escape a char, we have already started a new string.
                 ((or chars-unescaped-p (setq chars-unescaped-p t))
		  (setf (aref string write-idx) new-char)
		  (incf write-idx)
		  (incf read-idx 2)))
	  else do (when chars-unescaped-p
		    (setf (aref string write-idx) char))
		  (incf write-idx)
	  finally (return (cond (chars-unescaped-p
				 (if set-fill-pointer-p
				     (setf (fill-pointer string) write-idx)
				     (loop for idx upfrom write-idx below end
					   do (setf (aref string idx) pad-char)))	;pad out the end
				 (values string write-idx chars-unescaped-p))
				(t (values string read-idx)))))))


