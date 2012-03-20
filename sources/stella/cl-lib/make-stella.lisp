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


;;; Version: make-stella.lisp,v 1.44 2003/04/18 21:15:54 hans Exp

;;; Load STELLA from CL-translated files.

(in-package "STELLA")

(with-redefinition-warnings-suppressed
  (with-undefined-function-warnings-suppressed
      (CL:when CL-USER::*load-cl-struct-stella?*
	(CL-USER::stella-c&l-translated "stella-system-structs"))
    (CL-USER::stella-c&l-translated "hierarchy")
    (CL-USER::stella-c&l-translated "streams")
    (CL-USER::stella-c&l-translated "taxonomies")
    (CL-USER::stella-c&l-translated "primal")
    (CL-USER::stella-c&l-translated "cl-lib" "cl-primal")
    (CL-USER::stella-c&l-translated "startup")
    (CL-USER::stella-c&l-translated "type-predicates")
    (CL-USER::stella-c&l-translated "conses")
    (CL-USER::stella-c&l-translated "lists")
    (CL-USER::stella-c&l-translated "collections")
    (CL-USER::stella-c&l-translated "iterators")
    (CL-USER::stella-c&l-translated "symbols")
					;  (CL-USER::stella-c&l-translated "boot-symbols")
    (CL-USER::stella-c&l-translated "literals")
    (CL-USER::stella-c&l-translated "classes")
    (CL-USER::stella-c&l-translated "methods")
    (CL-USER::stella-c&l-translated "defclass")
    (CL-USER::stella-c&l-translated "date-time")
    (CL-USER::stella-c&l-translated "date-time-parser")
    (CL-USER::stella-c&l-translated "stella-in")
    (CL-USER::stella-c&l-translated "foreach")
    (CL-USER::stella-c&l-translated "walk")
    (CL-USER::stella-c&l-translated "dynamic-slots")
    (CL-USER::stella-c&l-translated "dynamic-literal-slots")
    (CL-USER::stella-c&l-translated "cl-translate")
    (CL-USER::stella-c&l-translated "macros")
    (CL-USER::stella-c&l-translated "memoize")
    (CL-USER::stella-c&l-translated "describe")
    (CL-USER::stella-c&l-translated "demons")
    (CL-USER::stella-c&l-translated "more-demons")
    (CL-USER::stella-c&l-translated "name-utility")
    (CL-USER::stella-c&l-translated "modules")
    (CL-USER::stella-c&l-translated "contexts")
    (CL-USER::stella-c&l-translated "read")
    (CL-USER::stella-c&l-translated "xml")
    (CL-USER::stella-c&l-translated "translate-file")
    (CL-USER::stella-c&l-translated "systems")
    (CL-USER::stella-c&l-translated "cl-translate-file")
    (CL-USER::stella-c&l-translated "cpp-translate")
    (CL-USER::stella-c&l-translated "cpp-translate-file")
    (CL-USER::stella-c&l-translated "cpp-class-out")
    (CL-USER::stella-c&l-translated "cpp-output")
    (CL-USER::stella-c&l-translated "java-translate")
    (CL-USER::stella-c&l-translated "java-translate-file")
    (CL-USER::stella-c&l-translated "java-class-out")
    (CL-USER::stella-c&l-translated "java-output")
    (CL-USER::stella-c&l-translated "idl-translate")
    (CL-USER::stella-c&l-translated "idl-translate-file")
    (CL-USER::stella-c&l-translated "idl-class-out")
    (CL-USER::stella-c&l-translated "idl-output")
    (CL-USER::stella-c&l-translated "cl-lib" "stella-to-cl")
    (CL-USER::stella-c&l-translated "startup-system")
    )) ;; with-redefinition-warnings-suppressed

(CL:WHEN CL-USER::*stella-verbose?*
  (CL:format CL:t "~&Initializing STELLA...~%"))
(startup-stella-system)
(startup-stella-to-cl)

(eval (if (use-vector-structs?)
          (print EOL "*** This Stella image uses vector structs "
                 "instead of CLOS instances ***" EOL EOL)
        (when (use-cl-structs?)
          (print EOL "*** This Stella image uses CL-structs "
                 "instead of CLOS instances ***" EOL EOL))))
