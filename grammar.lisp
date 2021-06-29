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


(defun make-construction (ret-tag pattern var-constraint action doc)
	"The function takes in the name, return type, return type,
	pattern and the function of a construction grammar and return
	a new construction-grammar type object."
	(make-instance 'construction :ret-tag ret-tag :pattern pattern 
		:var-constraint var-constraint :action action :doc doc))

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


(defmacro new-construction (&key variables pattern ret-tag action doc)
	"The macro takes in the variables, pattern, return-tag, action 
	and doc for a construction and add the new construction to the
	construction list."
	(let ((new-rule (gensym)))
		`(let ((,new-rule (make-construction ,ret-tag
				',(replace-variable-pattern pattern variables) 
				',(mapcar 'cdr variables)
				#'(lambda ,(mapcar 'car variables) ,action)
				,doc)))

		(setf *constructions* (cons ,new-rule *constructions*)))))

;;; ------------------------------------------------------------------------
;;; NP

(new-construction 
	:variables ((?x :noun))
	:pattern (("a" "an") ?x)
	:ret-tag :noun
	:action (let ((new_node (new-indv NIL ?x)))
			  	(add-np-to-referral ?x new_node)
				new_node)
	:doc "np new individual")

(new-construction 
	:variables ((?x :adj) (?y :noun))
	:pattern (("a" "an") ?x ?y)
	:ret-tag :noun
	:action (let ((new_node (new-indv NIL ?y)))
			  	(new-is-a new_node ?x)
			  	(add-np-to-referral ?y new_node)
				new_node)
	:doc "np new individual with adj")

(defvar *referral* NIL)

(defun add-np-to-referral (np np_ele)
	"The function takes in noun element and its children (new indv) element
	and add the mapping to the *referral* dict."
	(setq try-find (assoc np *referral* :test #'simple-is-x-eq-y?))
	(if (null try-find) 
		(push (cons np (list np_ele)) *referral*)
		(setf (cdr (assoc np *referral* :test #'simple-is-x-eq-y?)) 
			(append (list np_ele) (copy-list (cdr try-find))))))

(new-construction 
	:variables ((?x :noun))
	:pattern (("the") ?x)
	:ret-tag :noun
	:action (let ((try-find (assoc ?x *referral* :test #'simple-is-x-eq-y?)))
				(if (not (null try-find)) (car (cdr try-find))
				(error 'grammar-error :message "cannot find the referral noun")))
	:doc "np referral individual")

;;; ------------------------------------------------------------------------
;;; VP

(new-construction
	:variables ((?x {animal} :noun) 
		(?y {kick} :verb)
		(?z {physical object} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action object} new_v)
				new_v)
	:doc "transitive action kick")

(new-construction
	:variables ((?x {animal} :noun) 
		(?y {hit} :verb)
		(?z {physical object} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action object} new_v)
				new_v)
	:doc "transitive action hit")

(new-construction
	:variables ((?x {animal} :noun) 
		(?y {eat} :verb)
		(?z {physical object} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action object} new_v)
				new_v)
	:doc "transitive action eat")

(new-construction
	:variables ((?x {thing} :noun) 
		(?y {leave} :verb)
		(?z {place} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action object} new_v)
				new_v)
	:doc "transitive action leave")

(new-construction
	:variables ((?x {person} :noun) 
		(?y {make} :verb)
		(?z {man-made object} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action object} new_v)
				new_v)
	:doc "transitive action person make")

(new-construction
	:variables ((?x {animal} :noun) 
		(?y {give} :verb)
		(?z {animal} :noun)
		(?w {thing} :noun))
	:pattern (?x ?y ?z ?w)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
			  	(x-is-the-y-of-z ?z {action recipient} new_v)
			  	(x-is-the-y-of-z ?w {action object} new_v)
				new_v)
	:doc "transitive action with recipient give")

(new-construction
	:variables ((?x {thing} :noun) 
		(?y {arrive} :verb))
	:pattern (?x ?y)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
				new_v)
	:doc "intransitive action arrive")

(new-construction
	:variables ((?x {animal} :noun) 
		(?y {sit} :verb))
	:pattern (?x ?y)
	:ret-tag :verb
	:action (let ((new_v (new-indv NIL ?y)))
			  	(x-is-the-y-of-z ?x {action agent} new_v)
				new_v)
	:doc "intransitive action sit")

(new-construction
	:variables (
		(?x {animal} :noun) 
		(?y {hate} :verb) 
		(?z {thing} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (new-statement ?x ?y ?z)
	:doc "state hate")

(new-construction
	:variables (
		(?x {animal} :noun) 
		(?y {believe} :verb) 
		(?z {animal} :noun))
	:pattern (?x ?y ?z)
	:ret-tag :verb
	:action (new-statement ?x ?y ?z)
	:doc "state believe")

(new-construction 
	:variables ((?x :noun) (?y :noun :type))
	:pattern (?x ("is a" "is an") ?y)
	:ret-tag :relation
	:action (new-is-a ?x ?y)
	:doc "create new is a")

(new-construction 
	:variables ((?x :noun) (?y :adj))
	:pattern (?x ("is" "are") ?y)
	:ret-tag :relation
	:action (new-is-a ?x ?y)
	:doc "state verb adj")

(new-construction 
	:variables ((?x :noun) (?y :noun :type))
	:pattern (?x ("is" "are") ?y)
	:ret-tag :relation
	:action (new-is-a ?x ?y)
	:doc "state verb type")

(new-construction 
	:variables ((?x :noun) (?y :noun :indv))
	:pattern (?x ("is") ?y)
	:ret-tag :relation
	:action (new-eq ?x ?y)
	:doc "state verb indv")

(new-construction
	:variables ((?x {person} :noun) (?y {person} :noun) (?z {teammate of} :relation)) 
	:pattern (?x ("and") ?y ("are") ?z)
	:ret-tag :relation
	:action (new-statement ?x ?z ?y)
	:doc "state verb relation teammate")


