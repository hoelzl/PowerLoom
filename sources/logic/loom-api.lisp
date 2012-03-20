;;; -*- Mode: Lisp; Package: LOOM; Syntax: COMMON-LISP; Base: 10 -*-

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
; The Original Code is the PowerLoom KR&R System.                            ;
;                                                                            ;
; The Initial Developer of the Original Code is                              ;
; UNIVERSITY OF SOUTHERN CALIFORNIA, INFORMATION SCIENCES INSTITUTE          ;
; 4676 Admiralty Way, Marina Del Rey, California 90292, U.S.A.               ;
;                                                                            ;
; Portions created by the Initial Developer are Copyright (C) 1997-2006      ;
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


;;; Version: loom-api.lisp,v 1.12 2010/06/17 22:57:37 tar Exp

;; A Lisp API that enables PowerLoom to masquerade as Loom.

(cl:defpackage "LOOM" (:use "LOOM-API" "COMMON-LISP")
  (:import-from "LOOM-API"  ; Internal symbols of Loom-api
                #:named?)
  (:import-from "STELLA"
                #:setq
                #:lispify
                #:wrap-literal
                #:wrap-native-output-stream
                #:STANDARD-OUTPUT
                #:intern-symbol
                #:intern-keyword
                #:intern-surrogate
                #:class?
                #:relation?
                #:constant?
                #:get-class
                #:evaluate-logic-command
                #:translate-loom-statement
                #:translate-loom-definition
                #:translate-loom-description-to-kif
                #:define-relation-from-parse-tree
                #:get-self-or-prototype
                #:conceive-term
                #:find-direct-supers-and-subs
                #:pretty-print-logical-form
                #:finalize-objects
		)
  (:shadowing-import-from "STELLA"
			  #:assert
			  )
  (:export #:get-context
           #:find-context
           #:parent-contexts
           #:child-contexts
           #:context
           #:change-context
           #:change-kb
           #:cc
           #:list-context
           #:list-contexts
           #:clear-kb
           #:concept-p
           #:relation-p
           #:instance-p
           #:constant-p
           #:get-instance
           #:find-instance
           #:get-concept
           #:find-concept
           #:get-relation
           #:find-relation
           #:fi
           #:fc
           #:fr
           #:relation
           #:instance-of-p
           #:get-name
           #:name
           #:concept-names
           #:user-defined-p
           #:find-or-create-instance
           #:create
           #:createm
           #:destroy
           #:destroym
           #:delete-concept
           #:add-type
           #:remove-type
           #:get-types
           #:add-value
           #:remove-value
           #:get-value
           #:get-values
           #:get-roles
           #:get-role-values
           #:get-role-default-values
           #:list-role-names&values
           #:domain
           #:range
           #:get-role-min-cardinality
           #:get-role-max-cardinality
           #:compute-value-restriction
           #:get-instances
           #:subconcept-p
           #:get-superconcepts
           #:all-super-concepts
           #:get-subconcepts
           #:get-superrelations
           #:get-subrelations
           #:find-subsumers&subsumees
           #:compute-conjunction-concept
           #:most-specific-concepts
           #:defconcept
           #:defset
           #:defrelation
           #:defproperty
           #:define-concept
           #:define-relation
           #:tell
           #:tellm
           #:forget
           #:ask
           #:retrieve
           #:partitions
           #:exhaustive-partition-p
           #:partition-name
           #:partition-members
           #:disjoint-concepts-p
           #:initialize-network
           #:finalize-definitions
           #:pprint-object
           #:print-concept-definition
           #:source-definitions
           #:generate-object-definition
           #:set-feature
           #:unset-feature
           ))

(CL:in-package "LOOM")


  ;;
;;;;;; Support routines for converting betwee Lisp and Stella
  ;;

(CL:defun cl-surrogatify (objectRef)
  "Convert Lisp string or symbol into a STELLA surrogate."
  (CL:let ((string))
    (CL:typecase objectRef
      (CL:SYMBOL (setq string (CL:symbol-name objectRef)))
      (CL:STRING (setq string objectRef))
      (CL:t (CL:error "Can't surrogatify non-symbol and non-string: ~S" objectRef)))
    (intern-surrogate string)))

(CL:defun cl-stringify (objectRef)
  "Convert Lisp string or symbol into a STELLA string."
  (CL:typecase objectRef
    (CL:SYMBOL (CL:symbol-name objectRef))
    (CL:STRING objectRef)
    (CL:t (CL:error "Can't stringify non-symbol and non-string: ~S" objectRef))))

(CL:defun stellify (object)
  "Convert Lisp object into a STELLA object."
  (CL:typecase object
    (CL:NULL STELLA::NIL)
    (CL:CONS (stella::cons (stellify (CL:car object))
                           (stellify (CL:cdr object))))
    (CL:KEYWORD (intern-keyword (CL:symbol-name object)))
    (CL:SYMBOL (intern-symbol (CL:symbol-name object)))
    ((CL:or CL:NUMBER CL:STRING) (wrap-literal object))
    (STELLA::OBJECT object)
    (CL:t (CL:error "Can't stellify: ~S" object))))

;; DEFINED IN STELLA:
;(defun (stellafy OBJECT) ((thing LISP-CODE) (targetType TYPE))
;  :documentation "Partial inverse to 'lispify'.  Convert the Lisp object 
;'thing' into a Stella analogue of type 'targetType'."
;  :public? TRUE
;  (case targetType
;    ((@BOOLEAN @WRAPPED-BOOLEAN)
;     (return (wrap-boolean (not (eq? thing CL-NIL)))))
;    (@CONS
;     (return (permanentify (stella-code-to-cons-tree thing))))
;    (otherwise
;     (if (eq? thing CL-NIL)
;         (return NULL)
;       (return (permanentify (stella-code-to-cons-tree thing)))))))

(CL:defun lispify-boolean-value (boolean)
  "Convert a STELLA Boolean into its closest Lisp equivalent"
  (CL:case boolean
    (1 CL:t)
    (0 CL:NIL)
    (NULL-BOOLEAN CL:NIL)
    (TRUE-WRAPPER CL:t)
    (FALSE-WRAPPER CL:NIL)))
  

  ;;
;;;;;; Loom API
  ;;

(CL:defun loom:get-context (objectRef)
  "Return the context referred to by `objectRef'."
  (lispify (get-xcontext (stellify objectRef))) )

(CL:defun loom:find-context (objectRef &key error-p)
  "Return the context referred to by `objectRef'"
  (cl:declare (cl:ignore error-p))
  (loom:get-context objectRef))

(CL:defun loom:parent-contexts (contextRef)
  "Return the contexts above the context 'contextRef'."
  (lispify (get-parent-contexts (stellify contextRef))) )

(CL:defun loom:child-contexts (contextRef)
  "Return the contexts below the context 'contextRef'."
  (lispify (get-child-contexts (stellify contextRef))) )

(CL:defun loom:context (objectRef)
  "Return the context of an instance, or NIL if it is not context
sensitive."
  (lispify (context-of-instance (stellify objectRef))) )

(CL:defun loom:change-context (moduleName)
  "Switch to the PowerLoom module named 'moduleName'."
  (CL:let ((string (cl-stringify moduleName)))
    (CL:if string
      (stella::change-context string)
      (CL:warn "The argument ~S to 'change-context' is not a string."
               moduleName))))

(CL:defun loom::change-kb (moduleName)
  "Switch to the PowerLoom module named 'moduleName'."
  (loom:change-context moduleName) )

(CL:defmacro loom:cc (moduleName)
  "Switch to the PowerLoom module named 'moduleName'."
  `(stella::cc ,moduleName))

(CL:defun loom:list-contexts ()
  "Return a list of all contexts."
  (lispify (stella::all-modules)))

(CL:defun loom:list-context (cl:&rest options)
  (cl:cond ((cl:null options)
	    (lispify
             (loom-api::list-context stella::*context* (stellify :all))))
	   ((cl:eq (cl:first options) :partitions)
	    (lispify
             (loom-api::list-context
              stella::*context* (stellify (cl:second options)))))
	   ((cl:cdr options)
	    (lispify
             (loom-api::list-context
              (cl:first options) (stellify (cl:third options)))))
	   (cl:t
	    (lispify
             (loom-api::list-context (cl:first options) (stellify :all))))))

(CL:defun loom::clear-kb (CL:&optional kb)
  "Clear the context 'kb', or if its null, clear the current context."
  (CL:let ((context
            (CL:if (cl:null kb) stella::*module* (stella::get-module kb))))
    (stella::call-clear-module (stella::cons-list context))))

(CL:defun loom:concept-p (objectRef)
  "Return TRUE if 'objectRef' denotes a class."
  (class? objectRef))

(CL:defun loom:relation-p (objectRef)
  "Return TRUE if 'objectRef' denotes a relation or a class."
  (relation? objectRef))

(CL:defun loom:instance-p (objectRef)
  "Return TRUE if 'objectRef' is an instance or the
name of an instance."
  (instance? (stellify objectRef)))

(CL:defun loom:constant-p (objectRef)
  "Return TRUE if 'objectRef' denotes a constant."
  (constant? (stellify objectRef)))

(CL:defun loom:get-instance (instanceRef)
  "Return the nearest instance with name 'instanceRef'
visible from the current module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (lispify (stella::get-instance (stellify instanceRef))) )

(CL:defun loom:find-instance (instanceRef CL:&key no-warning-p)
  "Return the instance with name 'instanceRef' visible from the current
module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (CL:declare (CL:ignore no-warning-p))
  (lispify (stella::get-instance (stellify instanceRef))) )

(CL:defun loom:get-concept (conceptRef)
  "Return the nearest class with name 'instanceRef'
visible from the current module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (lispify (get-class (stellify conceptRef))) )

(CL:defun loom:find-concept (conceptRef CL:&key no-warning-p)
  "Return the nearest class with name 'instanceRef'
visible from the current module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (CL:declare (CL:ignore no-warning-p))
  (lispify (get-class (stellify conceptRef))) )

(CL:defun loom:get-relation (relationRef)
  "Return the nearest relation with name 'instanceRef'
visible from the current module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (lispify (stella::get-relation (stellify relationRef))) )

(CL:defun loom:find-relation (relationRef CL:&key no-warning-p)
  "Return the nearest relation with name 'instanceRef'
visible from the current module.  'instanceRef' can be a string, symbol,
or surrogate.  If 'instanceRef' is a surrogate, the search originates
in the module the surrogate was interned in."
  (CL:declare (CL:ignore no-warning-p))
  (lispify (stella::get-relation (stellify relationRef))) )

(CL:defmacro loom:fi (instanceRef)
  "Return the instance named 'instanceRef'."
  `(lispify (stella::get-instance (stellify ',instanceRef))))

(CL:defmacro loom:fc (conceptRef)
  "Return the instance named 'conceptRef'."
  `(lispify (stella::get-class (stellify ',conceptRef))))

(CL:defmacro loom:fr (relationRef)
  "Return the instance named 'relationRef'."
  `(lispify (stella::get-relation (stellify ',relationRef))))

(CL:defun loom:get-name (instanceRef)
  "Return the name (a symbol) of the instance referenced by 'instanceRef'."
  (CL:let ((instance (lispify (get-instance-name (stellify instanceRef)))))
    (CL:if (CL:atom instance)
      (CL:intern instance)
      CL:NIL)) )

(CL:defun loom:name (instanceRef)
  "Return the name (a symbol) of the instance referenced by 'instanceRef'."
  (loom:get-name instanceRef))

(CL:defun loom:concept-names (relationRef)
  "Return the name (a symbol) of the relation referenced by 'relationRef'."
  (lispify (get-equivalent-names (stellify relationRef))) )

(CL:defun loom:user-defined-p (instanceRef)
  "Return TRUE if the object returned by 'instanceRef'
has a name (is matched with a logical constant)."
  (named? (stellify instanceRef)))

(CL:defun loom:find-or-create-instance (instanceRef classRef)
  "Find an existing instance refenced by 'instanceRef'
of type 'classRef', or create of type 'classRef'."
  (find-or-create-instance (stellify instanceRef) (stellify classRef)) )

(CL:defun loom:create (name conceptRef CL:&key kb add-suffix-p clos-instance-p creation-policy)
  "Create an instance named 'name' of the class 'conceptRef'.
Keywords are not yet implemented."
  (CL:declare (CL:ignore clos-instance-p creation-policy))
  (lispify (create-instance (stellify name) (stellify conceptRef))))

(CL:defmacro loom:createm (name conceptRef CL:&key kb add-suffix-p clos-instance-p creation-policy)
  "Create an instance named 'name' of the class 'conceptRef'.
Keywords are not yet implemented."
  (CL:declare (CL:ignore clos-instance-p creation-policy))
  `(loom:create ,name ,conceptRef))

(CL:defun loom:destroy (instanceRef CL:&key dont-unintern-p)
  "Destroy the instance, concept, or relation 'instanceRef'."
  (CL:declare (CL:ignore dont-unintern-p))
  (lispify (delete-instance (stellify instanceRef))))

(CL:defun loom:destroym (instanceRef CL:&key dont-unintern-p)
  "Destroy the instance, concept, or relation 'instanceRef'.  The 'm' is ignored."
  (CL:declare (CL:ignore dont-unintern-p))
  (lispify (delete-instance (stellify instanceRef))))

(CL:defun loom:delete-concept (conceptRef CL:&key type delete-merged-concepts-p)
  "Destroy the instance, concept, or relation 'conceptRef'.  Keywords
are ignored."
  (CL:declare (CL:ignore type delete-merged-concepts-p))
  (lispify (delete-instance (stellify conceptRef))))

(CL:defun loom:instance-of-p (instanceRef classRef)
  "Return TRUE if the instance referenced by 'instanceRef'
belongs to the class referenced by 'classRef'."
  (instance-of? (stellify instanceRef) (stellify classRef)))

(CL:defun loom:add-type (instanceRef classRef CL:&key no-error-p)
  "Assert that the instance referenced by 'instanceRef'
is an instance of the class referenced by 'classRef'."
  (CL:declare (CL:ignore no-error-p))
  (add-type (stellify instanceRef) (stellify classRef)) )

(CL:defun loom:remove-type (instanceRef classRef CL:&key no-error-p)
  "Retract that the instance referenced by 'instanceRef'
is an instance of the class referenced by 'classRef'."
  (CL:declare (CL:ignore no-error-p))
  (remove-type (stellify instanceRef) (stellify classRef)) )

(CL:defun loom:get-types (instanceRef CL:&key direct-p)
  "Return a list of classes that 'instanceRef' belongs to."
  (lispify (loom-api::get-types (stellify instanceRef)
				(cl:if direct-p
				       (stellify :DIRECT) 
				       (stellify :ALL)))))

(CL:defun loom:add-value (instanceRef relationRef valueRef)
  "Assert the tuple '(relationRef instanceRef valueRef)'."
  (add-role-value (stellify instanceRef) (stellify relationRef) (stellify valueRef)) )

(CL:defun loom:remove-value (instanceRef relationRef valueRef)
  "Retract the tuple '(relationRef instanceRef valueRef)'."
  (remove-role-value (stellify instanceRef) (stellify relationRef) (stellify valueRef)) )

(CL:defun loom:get-value (instanceRef relationRef)
  "Return a value for the binary relation 'relationRef'
applied to 'instanceRef'."
  (lispify (get-role-value (stellify instanceRef) (stellify relationRef))) )

(CL:defun loom:get-values (instanceRef relationRef)
  "Return a list of values for the binary relation 'relationRef'
applied to 'instanceRef'."
  (lispify (get-role-values (stellify instanceRef) (stellify relationRef))) )

(CL:defun loom:get-instances (classRef)
  "Return a list of instances belonging to the class 'classRef'."
  (lispify (get-class-instances (stellify classRef) stella::FALSE)) )

(CL:defun loom:subconcept-p (subRelationRef superRelationRef)
  "Return TRUE if 'subrelationRef' specializes 'superrelationRef'."
  (subrelation? (stellify subRelationRef) (stellify superRelationRef)))

(CL:defun loom:get-superconcepts (conceptRef CL:&key direct-p)
  "Return a list of concepts that generalize 'conceptRef'. Non-reflexive."
  (loom:get-superrelations conceptRef :direct-p direct-p) )

(CL:defun loom:all-super-concepts (conceptRef)
  "Return a list of all concepts that generalize 'conceptRef'."
  (loom:get-superconcepts conceptRef :direct-p CL:NIL))

(CL:defun loom:get-subconcepts (conceptRef CL:&key direct-p)
  "Return a list of concepts that specialize 'conceptRef'. Non-reflexive."
  (loom:get-subrelations conceptRef :direct-p direct-p))

(CL:defun loom:get-superrelations (relationRef CL:&key direct-p)
  "Return a list of relations that generalize 'relationRef'.  Non-reflexive."
  (lispify (loom-api::get-superrelations (stellify relationRef) (cl:not direct-p))))

(CL:defun loom:get-subrelations (relationRef CL:&key direct-p)
  "Return a list of relations that specialize 'relationRef'. Non-reflexive."
  (lispify (loom-api::get-subrelations (stellify relationRef) (cl:not direct-p))))

(CL:defun loom:find-subsumers&subsumees (conceptExpression CL:&key supers-only-p)
  "Classify 'conceptExpression' and return three values, its direct
supers, direct subs, and either a class (if it is equivalent
to some class) or NULL.  Setting 'supers-only-p' will sometimes speed things up
quite a bit."
  (CL:multiple-value-bind (supers subs equivalents)
    (find-direct-supers-and-subs
     (conceive-term
      (translate-loom-description-to-kif (stellify conceptExpression)))
     (CL:not supers-only-p))
    (CL:values (lispify supers) (lispify subs)
               (CL:car (lispify equivalents)))) )

(CL:defun loom:most-specific-concepts (relationRefs)
  "Return a list of the most specific among the relations in 'relationRefs'."
  (lispify (most-specific-relations (stellify relationRefs) stella::TRUE)) )

(CL:defun loom:compute-conjunction-concept (conceptRefs)
  "Return a relation representing the conjunction of relations in 'relationRefs'."
  (lispify (compute-conjunction-relation (stellify conceptRefs))) )

(CL:defun loom:domain (relationRef)
  "Return the domain of the binary relation 'relationRef'."
  (lispify (get-domain (stellify relationRef))) )

(CL:defun loom:range (relationRef)
  "Return the range of the binary relation 'relationRef'."
  (lispify (get-range (stellify relationRef))) )

(CL:defun loom:get-role-min-cardinality (instanceRef relationRef)
  "Infer a minimum cardinality for the set of fillers of
the range of relation 'relationRef' applied to 'instanceRef'.
If 'instanceRef' denotes a class, inference is applied to a prototype
of that class."
  (lispify
   (get-min-cardinality 
    (get-self-or-prototype (stellify instanceRef)) (stellify relationRef))) )

(CL:defun loom:get-role-max-cardinality (instanceRef relationRef)
  "Infer a maximum cardinality for the set of fillers of
the range of relation 'relationRef' applied to 'instanceRef'.
If 'instanceRef' denotes a class, inference is applied to a prototype
of that class."
  (lispify
   (get-max-cardinality 
    (get-self-or-prototype (stellify instanceRef)) (stellify relationRef))) )

(CL:defun loom:compute-value-restriction (instanceRef relationRef)
  "Infer a type restriction on the set of fillers of
the range of relation 'relationRef' applied to 'instanceRef'.
If 'instanceRef' denotes a class, inference is applied to a prototype
of that class."
  (lispify
   (get-value-restriction 
    (get-self-or-prototype (stellify instanceRef)) (stellify relationRef))) )

(CL:defun loom:get-role-values (instanceRef relationRef)
  "Return a list of values for the binary relation 'relationRef'
applied to 'instanceRef'.  If 'instanceRef' denotes a class, inference
is applied to a prototype of that class."
  (lispify
   (get-role-values 
    (get-self-or-prototype (stellify instanceRef)) (stellify relationRef))) )

(CL:defun loom:get-role-default-values (instanceRef relationRef)
  "Return a list of default values for the binary relation 'relationRef'
applied to 'instanceRef'.  If 'instanceRef' denotes a class, inference
is applied to a prototype of that class."
  (lispify
   (get-role-default-values 
    (get-self-or-prototype (stellify instanceRef)) (stellify relationRef))) )

;; CAN'T RECALL HOW THIS WORKS, I.E., HOW DO LOOM ROLES GET CREATED???
(CL:defun loom:get-roles (instanceRef)
  (get-loom-roles instanceRef stella::TRUE))

(CL:defmethod relation ((self LOOM-ROLE))
  "Return the relation for the Loom role 'role'."
  (relation-of-loom-role self) )

(CL:defun loom:list-role-names&values (instanceRef)
  "Return a list of lists, with each sublist containing
a relation (role) name (a symbol) followed by one or more fillers of
that role on 'instanceRef'."
  (lispify
   (get-role-names-and-values 
    (get-self-or-prototype (stellify instanceRef)))) )

(CL:defmacro loom:defconcept (CL:&body body)
    "Define a PowerLoom concept."
    `(evaluate-logic-command
      (translate-loom-definition 
       (loom::stellify '(defconcept ,@ body)))
      stella::FALSE))

(CL:defmacro loom:defset (CL:&body body)
    "Define a PowerLoom concept."
    `(evaluate-logic-command
      (translate-loom-definition 
       (loom::stellify '(defset ,@ body)))
      stella::FALSE))

(CL:defmacro loom:defrelation (CL:&body body)
    "Define a PowerLoom relation."
    `(evaluate-logic-command
      (translate-loom-definition 
       (loom::stellify '(defrelation ,@ body)))
      stella::FALSE))

(CL:defmacro loom::defproperty (CL:&body body)
  `(evaluate-logic-command
    (translate-loom-definition 
     (loom::stellify '(defproperty ,@ body)))
    stella::FALSE))

(CL:defun loom:define-concept (definition)
  "Define a PowerLoom concept."
  (CL:let ((name (CL:getf definition :name)))
    (lispify
     (define-relation-from-parse-tree
       (translate-loom-definition 
        (stellify `(defconcept ,name ,@ definition)))))))

(CL:defun loom:define-relation (definition)
  "Define a PowerLoom concept."
  (CL:let ((name (CL:getf definition :name)))
    (lispify
     (define-relation-from-parse-tree
       (translate-loom-definition 
        (stellify `(defrelation ,name ,@ definition)))))))


(CL:defmacro loom:tell (CL:&body assertions)
  "Assert a list of Loom propositions"
  (lispify
   (translate-loom-statement (stellify `(tell ,@assertions)))))

(CL:defmacro loom:tellm (CL:&body assertions)
  "Make zero or more assertions (in Loom syntax); constraint
propagation will happen automatically (assuming :JUST-IN-TIME-INFERENCE
is enabled)."
  (CL:if assertions
    `(loom:tell ,@assertions)
    CL:NIL))

(CL:defmacro loom:forget (CL:&body assertions)
  "Retract a list of Loom propositions"
  (lispify
   (translate-loom-statement (stellify `(forget ,@assertions)))))

(CL:defmacro loom:ask (sentence CL:&key three-valued-p)
  "Assert a list of Loom propositions"
  (CL:let ((statement (CL:if three-valued-p
                        `(ask ,sentence :three-valued-p ,three-valued-p)
                        `(ask ,sentence))))
    `(lispify-boolean-value
      ,(lispify
        (translate-loom-statement (stellify statement))))))

(CL:defmacro loom:retrieve (variables query CL:&key kb generators)
  "Retrieve a list of values or tuples."
  (CL:declare (cl:ignore generators))
  `(lispify
    ,(lispify (translate-loom-statement 
               (stellify `(retrieve ,variables ,query
                                    ,@ (cl:when kb `(:kb ,kb))))))))

;; OOPS: THIS JUST PRINTS OUT THE NAME OF AN INSTANCE OR CLASS:
(CL:defun loom:pprint-object (instanceRef CL:&optional stream)
  "Pretty-print the instance referenced by 'instanceRef'.  If 'stream',
print to 'stream'."
  (pretty-print-logical-form 
   (stella::get-instance instanceRef) 
   (CL:if stream
     (wrap-native-output-stream stream)
     STANDARD-OUTPUT)) )

(CL:defun loom:print-concept-definition (collectionRef CL:&optional (stream CL:t))
  "Print a concept definition."
  (CL:let ((collection (stella::get-instance (stellify collectionRef))))
    (CL:typecase collection
      (stella::NAMED-DESCRIPTION
       (CL:format stream
                  (stella::cl-slot-value
                   collection "RELATION-STRINGIFIED-SOURCE" cl:nil))))))

(CL:defun loom:partitions (conceptRef)
  "Return a list of partitions that contain 'conceptRef'."
  (lispify (get-partitions (stellify conceptRef))) )

(CL:defun loom:exhaustive-partition-p (partitionRef)
  "Return T if the partition 'partitionRef' covers SOMETHING (can't
get an exact match with Loom semantics, since PowerLoom's mutually
disjoint collections have no inherent exhaustiveness property)."
  (lispify-boolean-value
   (covering? (stellify partitionRef) (stellify CL:NIL))))

;; BUG: 'lispify' RETURNS LONG NAME INSTEAD OF SHORT NAME:
(CL:defun loom:partition-name (partitionRef)
  "Return the name of the partition 'partitionRef'."
  (lispify (get-instance-name (stellify partitionRef))) )

(CL:defun loom:partition-members (partitionRef)
  "Return a list of concepts that belong to the partition 'partitionRef'."
  (lispify (collection-members (stellify partitionRef))) )

(CL:defun loom:disjoint-concepts-p (conceptRef1 conceptRef2)
  "Return T if 'conceptRef1' and 'conceptRef2' are disjoint."
  (CL:let ((concept1 (stellify conceptRef1))
           (concept2 (stellify conceptRef2)))
    (lispify-boolean-value (disjoint-relations? concept1 concept2))))



(CL:defun loom:generate-object-definition (instanceRef)
  "Return an s-expression representing the source expression for 'instanceRef'."
  (lispify (generate-source-expression (stellify instanceRef))) )

(CL:defun loom:initialize-network ()
  "Clear all user-defined modules, thereby reverting PowerLoom to a pristine state."
  (clear-user-contexts)
  (stella::change-context "PL-USER") )

(CL:defun loom:finalize-definitions ()
  "Finalize construction of user-defined classes and relations.  The
system automatically finalizes before a query.  Users can call this
to force any error messages to appear sooner."
  (finalize-objects) )


#|
loom:get-context
loom:find-context
loom:parent-contexts
loom:child-contexts
loom:context
loom:change-context
loom:cc
loom:concept-p
loom:get-concept
loom:find-concept
loom:get-relation
loom:find-relation
loom:relation-p
loom:instance-p
loom:constant-p
loom:find-instance
loom:get-name
loom:name
loom:concept-names
loom:user-defined-p
loom:find-or-create-instance
loom:create
loom:destroy
loom:destroym
loom:delete-concept
loom:instance-of-p
loom:add-type
loom:remove-type
loom:get-types
loom:add-value
loom:remove-value
loom:get-value
loom:get-values
loom:get-instances
loom:subconcept-p
loom:get-superconcepts
loom:all-super-concepts
loom:get-subconcepts
loom:get-superrelations
loom:get-subrelations
loom:find-subsumers&subsumees
loom:most-specific-concepts
loom:compute-conjunction-concept
loom:domain
loom:range
loom:get-role-min-cardinality
loom:get-role-max-cardinality
loom:compute-value-restriction
loom:get-role-values
loom:get-role-default-values
loom:get-roles
loom:relation                           ; maps a LOOM role to a relation
loom:list-role-names&values
loom:defconcept
loom:defrelation
loom:define-concept
loom:define-relation
loom:tell
loom:tellm
loom:forget
loom:ask
loom:retrieve
* loom:pprint-object
* loom:print-concept-definition
loom:partitions
loom:exhaustive-partition-p
loom:partition-name
loom:partition-members


;----<DONE TO HERE>----

loom:source-definitions
* loom::generate-object-definition
* loom:initialize-network
* loom:finalize-definitions
loom:set-feature
loom:unset-feature

http://www.isi.edu/isd/LOOM/documentation/manual/quickguide.html

|#

#|

;; STELLA BUG: THIS DOESN'T WORK:
(loom::change-context "LOOM-API")
(loom::change-context "/LOOM-API")

(loom:defconcept ANIMAL)
(loom:defconcept HUMAN :is-primitive ANIMAL)
(loom:defconcept DOG :is-primitive ANIMAL)
(loom:defconcept PET :is (and ANIMAL (at-least 1 owner-of)))
(loom:defrelation owner-of :domain ANIMAL :range HUMAN)
(loom:tell (DOG Fido))
(loom:tell (PERSON Hank))
(loom:tell (owner-of Fido Hank))
(loom:ask (owner-of Fido Hank))
;; BUG: THIS DOESN'T WORK:
(loom:ask (PET Fido))
(loom:forget (PET Fido))
(loom:forget (owner-of Fido Hank))

(eval (all-facts-of "FIDO"))

|#
