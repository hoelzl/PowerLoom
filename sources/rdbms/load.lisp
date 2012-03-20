;;; -*- Mode: Lisp; Package: CL-USER; Syntax: COMMON-LISP; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                          ;
;  COPYRIGHT (C) UNIVERSITY OF SOUTHERN CALIFORNIA, 2000-2002              ;
;  University of Southern California, Information Sciences Institute       ;
;  4676 Admiralty Way                                                      ;
;  Marina Del Rey, California 90292                                        ;
;                                                                          ;
;  This software was developed under the terms and conditions of Contract  ;
;  No. ???????????????? between the Defense Advanced Research Projects     ;
;  Agency and the University of Southern California, Information Sciences  ;
;  Institute.  Use and distribution of this software is further subject    ;
;  to the provisions of that contract and any other agreements developed   ;
;  between the user of the software and the University of Southern         ;
;  California, Information Sciences Institute.  It is supplied "AS IS",    ;
;  without any warranties of any kind.  It is furnished only on the basis  ;
;  that any party who receives it indemnifies and holds harmless the       ;
;  parties who furnish and originate it against any claims, demands, or    ;
;  liabilities connected with using it, furnishing it to others or         ;
;  providing it to a third party.  THIS NOTICE MUST NOT BE REMOVED FROM    ;
;  THE SOFTWARE, AND IN THE EVENT THAT THE SOFTWARE IS DIVIDED, IT SHOULD  ;
;  BE ATTACHED TO EVERY PART.                                              ;
;                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Version: load.lisp,v 1.4 2003/01/18 00:04:48 melz Exp

;;; Load file for (still experimental) Powerloom-to-legacy-RDBMS code.

;; Load powerloom:
(load "~/cvscheckout/powerloom/load-stella")
(in-package :stella)
(make-system "logic" :common-lisp)

;; required for persistence:
(make-system "ontomorph" :common-lisp)
;; Make sure mysql.ste and mysql-store.ste are included in the persistence system:
(make-system "persistence" :common-lisp)
;; For now I use Kojak for testing partial matching, but it's not essential:
(make-system "kojak" :common-lisp)
;; Foreign library for mysql
(cl:load "/usr/lib/libmysqlclient.so")
;; Powerloom-RDBMS connectivity:
(make-system "rdbms" :common-lisp :force-translation? TRUE)
(cc pl-user)

(defun ms () (make-system "rdbms" :common-lisp))