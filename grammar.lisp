;;; ***************************************************************************
;;; Construction and new construction rules defined for the Scone 
;;; construction grammar engine. 
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(defclass construction () (
	(ret-tag
		:initarg :ret-tag
		:type keyword
		:initform NIL
		:accessor construction-tag
		:documentation "The return tag that the construction produce.")
	(pattern
		:initarg :pattern
		:type cons
		:accessor construction-pattern
		:documentation "The pattern of the construction,which is represented
						as a list of components and each component is a list of 
						alternatives.")
	(variable-constraint
		:initarg :var-constraint
		:type cons
		:accessor construction-var-constraint
		:documentation "The list of constraints of the variables in the 
						construction method.")
	(modifier
		:initarg :modifier
		:type cons
		:initform NIL
		:accessor construction-modifier
		:documentation "The modifiers in the construction.")
	(action
		:initarg :action
		:type function
		:accessor construction-action
		:documentation "The function that put the construction into Scone.")
	(doc
		:initarg :doc
		:type string 
		:initform ""
		:accessor construction-doc
		:documentation "The documentation of the construction grammar"))
	(:documentation
		"A class that represent the construction grammar in the languange."))


(defun make-construction (ret-tag pattern var-constraint modifier action doc)
	"The function takes in the name, return type, return type,
	pattern and the function of a construction grammar and return
	a new construction-grammar type object."
	(make-instance 'construction :ret-tag ret-tag :pattern pattern 
		:var-constraint var-constraint :modifier modifier :action action :doc doc))

(defvar *constructions* nil)

(defun replace-variable-pattern (pattern variables) 
	"The function takes in a pattern and variables and replace 
	the variables in the pattern with the corresponding index
	in the list."
	(mapcar 
		(lambda (component) 
			(if (typep component 'symbol) 
				(loop for i from 0 to (- (length variables) 1) 
					when (equal component (car (nth i variables)))
					return i) component))
		 pattern))

(defun replace-variable-modifier (modifier variables) 
	"The function takes in a modifier and variables and replace 
	the variables in the modifier with the corresponding index
	in the list."
	(loop for i from 0 to (- (length variables) 1)
		collect (loop for component in modifier
					when (equal (car component) (car (nth i variables)))
					return (car (cdr component)))))

(defmacro new-construction (&key variables pattern ret-tag modifier action doc)
	"The macro takes in the variables, pattern, return-tag, modifier, action 
	and doc for a construction and add the new construction to the
	construction list."
	(let ((new-rule (gensym)))
		`(let ((,new-rule (make-construction ,ret-tag
				',(replace-variable-pattern pattern variables) 
				',(mapcar 'cdr variables)
				',(replace-variable-modifier modifier variables)
				#'(lambda ,(mapcar 'car variables) ,action)
				,doc)))

		(setf *constructions* (cons ,new-rule *constructions*)))))

;;; ------------------------------------------------------------------------
;;; NP

(new-construction 
	:variables ((?x :noun :type))
	:pattern (("a" "an") ?x)
	:ret-tag :noun
	:modifier NIL
	:action (let ((new-node (new-indv NIL ?x)))
			  	(add-np-to-referral new-node)
				new-node)
	:doc "np new individual")

(new-construction 
	:variables ((?x :adj) (?y :noun :type))
	:pattern (("a" "an") ?x ?y)
	:ret-tag :noun
	:modifier NIL
	:action (let ((new-node (new-indv NIL ?y)))
			  	(new-is-a new-node ?x)
			  	(add-np-to-referral new-node)
				new-node)
	:doc "np new individual with adj")

(new-construction 
	:variables ((?x {organization} :noun :indv) (?y {place} :noun))
	:pattern (?x ("based" "located") ("in" "at") ?y)
	:ret-tag :noun
	:modifier NIL
	:action (progn (x-is-the-y-of-z ?y {based location} ?x) ?x)
	:doc "np organization with location")

(new-construction 
	:variables ((?x {number}) (?y {tangible} :noun :type))
	:pattern (?x ?y)
	:ret-tag :noun
	:modifier NIL
	:action (let ((new-node (new-type NIL ?y)))
			  	(x-is-the-y-of-z ?x {count} ?y)
			  	(add-np-to-referral new-node)
				new-node)
	:doc "np new individual plural")

(new-construction 
	:variables ((?x {number}) (?z :adj) (?y {tangible} :noun :type))
	:pattern (?x ?z ?y)
	:ret-tag :noun
	:modifier NIL
	:action (let ((new-node (new-type NIL ?y)))
			  	(x-is-the-y-of-z ?x {count} new-node)
			  	(new-is-a new-node ?z)
			  	(add-np-to-referral new-node)
				new-node)
	:doc "np new individual plural with adjective")

(defvar *referral* NIL)

(defun ele-or-list-equal (ele1 ele2)
	"take in two elements where each one could be either scone element
	or a list of scone element, return if they are equal"
	(cond 
		((and (or (typep ele1 'element) (typep ele1 'element-iname)) 
			  (or (typep ele2 'element) (typep ele2 'element-iname)))  
		 (simple-is-x-eq-y? ele1 ele2))
		((and (typep ele1 'cons) (typep ele2 'cons)) 
		 (if (null (equal (length ele1) (length ele2))) NIL
				(not (loop for ele in ele1
					when (null (find ele ele2 :test #'simple-is-x-eq-y?))
					return T))))
		(t NIL)))

(defun remove-dup (ele ele-list)
	"take in an element and a list, remove the duplicate of the 
	element in the list if exist"
	(if (null ele-list) NIL
	(if (ele-or-list-equal ele (car ele-list))
		(cdr ele-list)
		(append (list (car ele-list)) (remove-dup ele (cdr ele-list))))))

(defun add-np-to-referral (np-ele)
	"The function takes in noun element and its children (new indv) element
	and add the mapping to the *referral* dict."
	(setf *referral* (append (list np-ele) (remove-dup np-ele *referral*))))
	; (setq try-find (assoc np *referral* :test #'simple-is-x-eq-y?))
	; (if (null try-find) 
	; 	(push (cons np (list np-ele)) *referral*)
	; 	(setf (cdr (assoc np *referral* :test #'simple-is-x-eq-y?)) 
	; 		(append (list np-ele) (copy-list (cdr try-find))))))

(new-construction 
	:variables ((?x :noun :type))
	:pattern (("the") ?x)
	:ret-tag :noun
	:modifier NIL
	:action ;; to-do: add ?y plural case
			(loop for np-ele in *referral*
				when (handler-case (simple-is-x-a-y? np-ele ?x) (t nil)) 
				return (progn 
					(add-np-to-referral np-ele)
					np-ele))
	:doc "np referral individual")

(new-construction
	:variables ((?x :noun) (?y :noun))
	:pattern (?x ("and") ?y)
	:ret-tag :noun
	:modifier NIL
	:action (list ?x ?y)
	:doc "noun parallel structure")

(new-construction
	:variables ((?x :noun) (?y :noun :list))
	:pattern (?x (",") ?y)
	:ret-tag :noun
	:modifier NIL
	:action (if (> (length ?y) 1) (append (list ?x) ?y) nil)
	:doc "noun parallel structure")

(new-construction
	:variables ((?x :adj) (?y :adj))
	:pattern (?x ("and") ?y)
	:ret-tag :adj
	:modifier NIL
	:action (list ?x ?y)
	:doc "adj parallel structure")

(new-construction
	:variables ((?x :adj) (?y :adj :list))
	:pattern (?x (",") ?y)
	:ret-tag :adj
	:modifier NIL
	:action (if (> (length ?y) 1) (append (list ?x) ?y) nil)
	:doc "adj parallel structure")

(new-construction
	:variables ((?x :noun :possessive) (?y :type-role))
	:pattern (?x ?y)
	:ret-tag :noun
	:modifier NIL
	:action ;; to-do: add ?y plural case
			(let ((prescan 
				(loop for np-ele in *referral*
					when (and (null (typep np-ele 'cons))
						(simple-is-x-a-y? np-ele (parent-element ?y))
						(not (null (find np-ele 
							(list-all-x-of-y ?y ?x) :test #'simple-is-x-eq-y?))))
					return np-ele)))
			(if (not (null prescan)) 
				(progn
					(new-is-a ?x (context-element ?y))
					(add-np-to-referral prescan)
					prescan)
				(let ((new-node (new-indv NIL (parent-element ?y))))
					(new-is-a ?x (context-element ?y))
					(x-is-a-y-of-z new-node ?y ?x)
					(add-np-to-referral new-node)
					new-node)))
	:doc "possessive type-role")


;;; ------------------------------------------------------------------------
;;; VP

; (new-construction
; 	:variables ((?x {animal} :noun) 
; 		(?y {kick} :verb)
; 		(?z {physical object} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action object} new-v)
; 				new-v)
; 	:doc "transitive action kick")

; (new-construction
; 	:variables ((?x {animal} :noun) 
; 		(?y {hit} :verb)
; 		(?z {physical object} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action object} new-v)
; 				new-v)
; 	:doc "transitive action hit")

; (new-construction
; 	:variables ((?x {animal} :noun) 
; 		(?y {eat} :verb)
; 		(?z {physical object} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action object} new-v)
; 				new-v)
; 	:doc "transitive action eat")

; (new-construction
; 	:variables ((?x {thing} :noun) 
; 		(?y {leave} :verb)
; 		(?z {place} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action object} new-v)
; 				new-v)
; 	:doc "transitive action leave")

; (new-construction
; 	:variables ((?x {person} :noun) 
; 		(?y {make} :verb)
; 		(?z {man-made object} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action object} new-v)
; 				new-v)
; 	:doc "transitive action person make")

; (new-construction
; 	:variables ((?x {animal} :noun) 
; 		(?y {give} :verb)
; 		(?z {animal} :noun)
; 		(?w {thing} :noun))
; 	:pattern (?x ?y ?z ?w)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 			  	(x-is-the-y-of-z ?z {action recipient} new-v)
; 			  	(x-is-the-y-of-z ?w {action object} new-v)
; 				new-v)
; 	:doc "transitive action with recipient give")

; (new-construction
; 	:variables ((?x {thing} :noun) 
; 		(?y {arrive} :verb))
; 	:pattern (?x ?y)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 				new-v)
; 	:doc "intransitive action arrive")

; (new-construction
; 	:variables ((?x {animal} :noun) 
; 		(?y {sit} :verb))
; 	:pattern (?x ?y)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (let ((new-v (new-indv NIL ?y)))
; 			  	(x-is-the-y-of-z ?x {action agent} new-v)
; 				new-v)
; 	:doc "intransitive action sit")

; (new-construction
; 	:variables (
; 		(?x {animal} :noun) 
; 		(?y {hate} :verb) 
; 		(?z {thing} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (new-statement ?x ?y ?z)
; 	:doc "state hate")

; (new-construction
; 	:variables (
; 		(?x {animal} :noun) 
; 		(?y {believe} :verb) 
; 		(?z {animal} :noun))
; 	:pattern (?x ?y ?z)
; 	:ret-tag :verb
; 	:modifier NIL
; 	:action (new-statement ?x ?y ?z)
; 	:doc "state believe")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :adj))
	:pattern (?x ?v ?y)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?x)
			(new-is-a ?x ?y))
	:doc "state verb adj")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :adj))
	:pattern (?x ?v ("not") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?x)
			(new-is-not-a ?x ?y))
	:doc "state verb adj")

(new-construction
	:variables ((?x {is-a link} :relation :list) (?y :adj :list))
	:pattern (?x (", not" "not") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((agent (a-element (car (last ?x))))
				  (ctx (context-element (car (last ?x)))))
				(in-context ctx)
				(append ?x 
					(loop for adj-ele in ?y
						  collect (new-is-not-a agent adj-ele))))
	:doc "state verb adj with not")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :noun :type))
	:pattern (?x ?v ?y)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?x)
			(new-is-a ?x ?y))
	:doc "state verb type")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :noun :type))
	:pattern (?x ?v ("not") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?x)
			(new-is-not-a ?x ?y))
	:doc "state verb type")

(new-construction
	:variables ((?x {is-a link} :relation :list) (?y :noun :type :list))
	:pattern (?x (", not" "not") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((agent (a-element (car (last ?x))))
				  (ctx (context-element (car (last ?x)))))
				(in-context ctx)
				(append ?x 
					(loop for type-ele in ?y
						  collect (new-is-not-a agent type-ele))))
	:doc "state verb type with not")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :noun :type))
	:pattern (?x ?v ("a" "an" "a kind of") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (if (find :plural (cdr ?v)) nil
			(progn
				(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
				(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
				(add-np-to-referral ?x)
				(new-is-a ?x ?y)))
	:doc "create new is a")

(new-construction 
	:variables ((?x :noun) (?v "is") (?y :noun :type))
	:pattern (?x ?v ("not a" "not an" "not a kind of") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (if (find :plural (cdr ?v)) nil
			(progn
				(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
				(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
				(add-np-to-referral ?x)
				(new-is-not-a ?x ?y)))
	:doc "create new is not a")

(new-construction
	:variables ((?x :type-role) (?v "is") (?z :noun))
	:pattern (("the") ?x ?v ?z)
	:ret-tag :relation
	:modifier NIL
	:action (let ((parent (context-element ?x)))
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?z)
			(loop for np-ele in *referral*
				when (handler-case (simple-is-x-a-y? np-ele parent) (t nil)) 
				return (x-is-the-y-of-z ?z ?x np-ele)))
	:doc "create the y of implicit z")

(new-construction
	:variables ((?x :type-role) (?y) (?v "is") (?z :noun))
	:pattern (("the") ?x ("of") ?y ?v ?z)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(if (find :past (cdr ?v)) (in-context (new-indv nil {past})))
			(if (find :future (cdr ?v)) (in-context (new-indv nil {future})))
			(add-np-to-referral ?z)
			(x-is-the-y-of-z ?z ?x ?y))
	:doc "create the y of z")

(new-construction
	:variables ((?x :noun) (?y :type-role))
	:pattern (?x ("is the") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((parent (context-element ?y)))
			(add-np-to-referral ?x)
			(loop for np-ele in *referral*
				when (handler-case (simple-is-x-a-y? np-ele parent) (t nil)) 
				return (x-is-the-y-of-z ?x ?y np-ele)))
	:doc "create the y of implicit z")

(new-construction
	:variables ((?x :noun) (?y :type-role) (?z))
	:pattern (?x ("is the") ?y ("of") ?z)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(add-np-to-referral ?x)
			(x-is-the-y-of-z ?x ?y ?z))
	:doc "create the y of z")

(new-construction
	:variables ((?x :noun) (?y :type-role))
	:pattern (?x ("is a" "is one of the") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((parent (context-element ?y)))
			(add-np-to-referral ?x)
			(loop for np-ele in *referral*
				when (handler-case (simple-is-x-a-y? np-ele parent) (t nil)) 
				return (x-is-a-y-of-z ?x ?y np-ele)))
	:doc "create a y of implicit z")

(new-construction
	:variables ((?x :noun) (?y :type-role) (?z))
	:pattern (?x ("is a" "is one of the") ?y ("of") ?z)
	:ret-tag :relation
	:modifier NIL
	:action (progn
			(add-np-to-referral ?x)
			(x-is-a-y-of-z ?x ?y ?z))
	:doc "create a y of z")

(new-construction
	:variables ((?x :noun :list) (?y :type-role))
	:pattern (?x ("are the") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((parent (context-element ?y)))
			(loop for np-ele in *referral*
				when (handler-case (simple-is-x-a-y? np-ele parent) (t nil)) 
				return (loop for x in ?x 
							collect (x-is-a-y-of-z x ?y np-ele))))
	:doc "create several y of z")

(new-construction
	:variables ((?x :noun :list) (?y :type-role) (?z))
	:pattern (?x ("are the") ?y ("of") ?z)
	:ret-tag :relation
	:modifier NIL
	:action (loop for x in ?x 
				collect (x-is-a-y-of-z x ?y ?z))
	:doc "create several y of z")

(new-construction
	:variables ((?x {person} :list :noun) (?y {friend} :type-role)) 
	:pattern (?x ("are") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (let ((len (length ?x)))
				(if (< len 2) (error 'grammar-error 
					:message "not enough agent to support the relation"))
				(loop for i from 0 to (- len 2)
		       append (loop for j from (+ i 1) to (- len 1)
		       		append (list (x-is-a-y-of-z (nth i ?x) ?y (nth j ?x))
		       					 (x-is-a-y-of-z (nth j ?x) ?y (nth i ?x))))))
	:doc "state verb relation teammate")

(new-construction
	:variables ((?x :noun) (?y {number} :noun) (?z :noun :type))
	:pattern (?x ("has" "have") ?y ?z)
	:ret-tag :relation
	:modifier NIL
	:action 
	;; check if this role type already exist
	(let ((new-node 
			(new-type-role NIL ?x ?z :n ?y)))
		(add-np-to-referral ?x)
		(add-np-to-referral new-node)
		new-node)
	:doc "has relation with number")

(new-construction
	:variables ((?x :noun) (?z :noun :type))
	:pattern (?x ("has" "have") ("a" "an") ?z)
	:ret-tag :relation
	:modifier NIL
	:action 
	;;to-do: check if this role type already exist
	(let ((new-node (new-type-role NIL ?x ?z :n {1})))
		(add-np-to-referral ?x)
		(add-np-to-referral new-node)
		new-node)
	:doc "has relation with number one")

(new-construction 
	:variables ((?x :relation) (?y {time reference} :noun))
	:pattern (?x ("in" "at" "on") ?y)
	:ret-tag :relation
	:modifier ((?x (new-indv nil {time reference})))
	:action (progn
				(new-is-a (context-element ?x) ?y)
				?x)
	:doc "time prepositional phrase")

(new-construction 
	:variables ((?x :relation) (?y {place} :noun))
	:pattern (?x ("in" "at" "on") ?y)
	:ret-tag :relation
	:modifier ((?x (new-indv nil {place})))
	:action (progn
				(new-is-a (context-element ?x) ?y)
				?x)
	:doc "location prepositional phrase")

(new-construction
	:variables ((?x :relation) (?y :relation))
	:pattern (?x ("and") ?y)
	:ret-tag :relation
	:modifier NIL
	:action (list ?x ?y)
	:doc "relation parallel structure")









