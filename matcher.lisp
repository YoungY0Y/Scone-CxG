;;; ***************************************************************************
;;; Construction matcher and scone element constructor for the Scone 
;;; construction grammar engine. 
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(defun sublst (lst idx len)
	"The function takes in a list, an index and a length,
	return the sublist of the list starting from the index and
	has length len."
    (cond
        ((null lst) nil)
        ((< 0  idx) (sublst (cdr lst) (1- idx) len))
        ((null len) lst)
        ((< 0  len) (cons (car lst) (sublst (cdr lst) idx (1- len))))))

(defun join-list-by-space (slist)
	"The function takes in a list and join the list of string by space."
	(format nil "~{~A~^ ~}" slist))

(defun get-syntax-tag (constraints)
	"The function takes in a list of variable constraints
	and returns the syntax tags in it."
	(loop for constraint in constraints
		when (not (null (find constraint (list :verb :noun :relation :adj))))
		collect constraint))

(defun get-parents (constraints)
	"The function takes in a list of variable constraints
	and returns the scone elements in it."
	(loop for constraint in constraints
		when (or (typep constraint 'element-iname) (typep constraint 'element))
		collect constraint))

(defun might-be-name (text constraints)
	"The function takes in a text and the constraints for a variable
	return if it could be a name."
	(if (null text) nil
	(and (not (loop for partial in (cl-ppcre:split "\\s+" text)
		when (not (upper-case-p (char partial 0)))
		return T))
		(null (loop for constraint in constraints
			when (and (or (typep constraint 'element-iname) (typep constraint 'element))
					(simple-is-x-a-y? constraint {intangible}))
			return T))
		(null (find :verb constraints))
		(null (find :relation constraints))
		(null (find :adj constraints))
		(null (find :type constraints))
		(null (find :type-role constraints)))))

(defun meet-constraint (element constraints parents ctx if-new-ctx verbose)
	"The function takes in an element, the constraints for the variable, 
	the parents for the variable, context of the element and whether
	the context need to be renewed when encountered a name and returns 
	nil if the element does not match the constraints, return the context
	if the element match."
	(if (null element) ctx
	
		(if (typep element 'cons) 
			(let ((next-ctx 
					(meet-constraint (car element) 
						(remove :list constraints) parents ctx if-new-ctx verbose)))
				(if (null next-ctx) nil 
					(if (and (null (simple-is-x-eq-y? next-ctx ctx)) if-new-ctx)
					(meet-constraint (cdr element) 
						constraints parents next-ctx nil verbose)
					(meet-constraint (cdr element) 
						constraints parents next-ctx if-new-ctx verbose))))

			(if (and (or (null (find :type constraints)) (type-node? element))
				 (or (null (find :type-role constraints)) (type-role-node? element))
			     (or (null (find :indv constraints)) (indv-node? element))
			     (not (loop for parent in parents
					    when (not (simple-is-x-a-y? element parent)) 
					  	return T))) ctx
				(if (might-be-name (iname element) constraints)
				(handler-case
					(let ((before-context ctx)
						  (new-ctx (if if-new-ctx (new-context NIL ctx) ctx)))
						(in-context new-ctx)
						(loop for parent in parents
							do (if verbose (commentary "Assume ~S is a ~S" element parent))
							do (new-is-a element parent))
						(in-context before-context)
						new-ctx) (t nil)))))))

(defun pronoun-match (text syntax-tag)
	"take in a text and return the noun it refers to if it is a pronoun"
	(if (not (and (null (find :verb syntax-tag)) (null (find :adj syntax-tag))
		(null (find :relation syntax-tag)))) NIL

	(cond ((or (equal text "he") (equal text "him"))
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(indv-node? ele)
							(not (equal (is-x-a-y? ele {male person}) :NO)))
					collect (let ((before-context *context*)
								  (new-ctx (new-context NIL *context*)))
								(in-context new-ctx)
								(new-is-a ele {male person})
								(in-context before-context)
								(list ele new-ctx))))
		((or (equal text "she") (equal text "her"))
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(indv-node? ele)
							(not (equal (is-x-a-y? ele {female person}) :NO)))
					collect (let ((before-context *context*)
								  (new-ctx (new-context NIL *context*)))
								(in-context new-ctx)
								(new-is-a ele {female person})
								(in-context before-context)
								(list ele new-ctx))))
		((equal text "it") 
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(indv-node? ele)
							(not (equal (is-x-a-y? ele {person}) :YES)))
					collect (let ((before-context *context*)
								  (new-ctx (new-context NIL *context*)))
								(in-context new-ctx)
								(new-is-not-a ele {person})
								(in-context before-context)
								(list ele new-ctx))))

		((or (equal text "they") (equal text "them"))
				(loop for ele in *referral*
					when (or (typep ele 'cons) (type-node? ele))
					collect (list ele *context*)))
		(t NIL))))

(defun variable-match (text morph-list constraints modifier partial verbose)
	"The function takes in a root text, morphology info, a list of constraints
	for the variable, returns the matched scone elements that
	satisfies the constraints, the after context
	and the after referral context."
	(if (let ((string-cons (loop for con in constraints
								when (typep con 'string)
								collect con)))
			(not (or (null string-cons) 
				(loop for con in string-cons
				 	when (string-equal text con)
				 	return T)))) nil 

	(let ((syntax-tag (get-syntax-tag constraints))
		  (parents (get-parents constraints)))
		(let ((result 
			(append 
				(loop for element-pair in (lookup-definitions text syntax-tag)
					for element = (car element-pair)
					for new-ctx = 
						(meet-constraint element constraints parents *context* t verbose)
					when (not (null new-ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(if (might-be-name text constraints)
							(list (cons element morph-list) new-ctx (append (list element) 
											(remove-dup element (copy-tree *referral*))))
							(list (cons element morph-list) new-ctx (copy-tree *referral*)))))
				(loop for element-pair in (constructor text (eval modifier) syntax-tag partial verbose)
					for element = (car element-pair)
					for context = (nth 2 element-pair)
					for ref-context = (nth 3 element-pair)
					for new-ctx = (meet-constraint element constraints parents context t verbose)
					when (not (null new-ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(if (and (typep element 'cons)
								(not (loop for ele in element
										when (null (or (indv-node? ele) (type-node? ele)))
										return T)))
							(list (cons element morph-list) new-ctx (append (list element) 
													(remove-dup element ref-context)))
							(list (cons element morph-list) new-ctx ref-context))))

				(loop for element-pair in (pronoun-match text syntax-tag)
					for element = (car element-pair)
					for context = (nth 1 element-pair)
					for new-ctx = 
						(meet-constraint element constraints parents context t verbose)
					when (not (null new-ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(list (cons element morph-list) new-ctx (copy-tree *referral*)))))))

			(if (not (null result)) result
				(if (might-be-name text constraints)
					(handler-case 
					(let ((new-node (new-indv text {new thing}))
						  (before-context *context*)
						  (new-ctx (new-context NIL *context*)))
						(commentary "Create new name ~S" new-node)
						(in-context new-ctx)
						(loop for parent in parents
							do (new-is-a new-node parent))
						(in-context before-context)
						(list (list (cons new-node morph-list) new-ctx (append (list new-node) 
											(remove-dup new-node (copy-tree *referral*))))))
					(t nil))))))))


(defun one-ele-match (text pattern var-constraint modifier partial verbose)
	"The function takes in a raw text, a single pattern, a list of 
	variable constraints and check if the textmatch the pattern."
	(if (typep pattern 'integer)
		(variable-match (car (simple-morphology text)) (cdr (simple-morphology text)) (nth pattern var-constraint) 
									(nth pattern modifier) partial verbose)
		(find text (mapcar (lambda (x) (join-list-by-space (tokenizer x))) pattern) 
					:test #'string-equal)))

(defun construction-match-checker (wordlist pattern-list var-constraint modifier partial)
	"The function takes in a list of words, a pattern list, 
	a list of variable constraints return if the list of word 
	could match the pattern."
	(let ((before-ref-context (copy-tree *referral*))
		  (before-context *context*))
	(let ((result
	(cond 
		((and (null pattern-list) (null wordlist)) T)
		((or (null pattern-list) (null wordlist) 
			(< (length wordlist) (length pattern-list))) NIL)
		(T (loop for i from 1 to 
					(+ (- (length wordlist) (length pattern-list)) 1)
			for text = (join-list-by-space (sublst wordlist 0 i))
			for first-element-result = (progn 
				(setf *referral* before-ref-context)
				(in-context before-context)
				(one-ele-match text (car pattern-list) var-constraint modifier partial NIL))
			when (progn
			(and 
				(not (null first-element-result)) 
				(if (typep first-element-result 'string) 
					(construction-match-checker 
						(sublst wordlist i (- (length wordlist) i)) 
						(cdr pattern-list) var-constraint modifier partial)
					(loop for ele-pair in first-element-result
						for ele = (car ele-pair)
						for ref-ctx = (nth 2 ele-pair)
						for ctx = (nth 1 ele-pair)
						when (progn 
							(setf *referral* ref-ctx)
							(in-context ctx)
							(construction-match-checker 
								(sublst wordlist i (- (length wordlist) i)) 
											(cdr pattern-list) var-constraint modifier partial))
						return T))))
			return T)))))
	(setf *referral* before-ref-context)
	(in-context before-context)
	result)))

(defun base-case-matcher (len)
	"the function takes in the length, and return the base case
	for the construction matcher, which is a list of nil and 
	context and referral context at the end"
	(append (loop for i from 1 to len collect nil) 
		(list *context*) (list (copy-tree *referral*))))

(defun change-ith-list (l i new-content)
	"the function takes in a list, an index i and a new content,
	changes the ith element to the new content and returns the list"
	(setf (nth i l) new-content)
	l)

(defun remove-null (l)
	"the function takes in a list, remove the null element and 
	return the new list"
	(loop for ele in l 
		when (not (null ele))
		collect ele))

(defun construction-matcher (wordlist pattern-list var-constraint modifier partial verbose)
	"The main matcher function that takes in a wordlist, a pattern list and 
	a list of variable constraints and returns all possible combination
	of the elements for the variables, the after context
	and the after referral context."
	(let ((before-ref-context (copy-tree *referral*))
		  (before-context *context*))
	(let ((result
	(if (or (null pattern-list) (null wordlist) 
			(< (length wordlist) (length pattern-list))) 
		(list (base-case-matcher (length var-constraint)))

		(loop for i from 1 to (+ (- (length wordlist) (length pattern-list)) 1)
			for text = (join-list-by-space (sublst wordlist 0 i))
			for first-element-result = (progn
				(setf *referral* before-ref-context)
				(in-context before-context)
				(one-ele-match text (car pattern-list) var-constraint modifier partial verbose))
			when (not (null first-element-result))
			append 
				(if (typep first-element-result 'string) 
					(if (construction-match-checker 
							(sublst wordlist i (- (length wordlist) i)) 
							(cdr pattern-list) var-constraint modifier partial)
					(construction-matcher (sublst wordlist i (- (length wordlist) i)) 
						(cdr pattern-list) var-constraint modifier partial verbose))
		
					(loop for ele-pair in first-element-result
						for ele = (car ele-pair)
						for ref-ctx = (nth 2 ele-pair)
						for ctx = (nth 1 ele-pair)
						append (progn 
							(setf *referral* ref-ctx)
							(in-context ctx)
							(if (construction-match-checker 
									(sublst wordlist i (- (length wordlist) i)) 
									(cdr pattern-list) var-constraint modifier partial)
								(loop for rest-ele in 
									(construction-matcher 
										(sublst wordlist i (- (length wordlist) i)) 
										(cdr pattern-list) var-constraint modifier partial verbose)
								collect (change-ith-list 
											rest-ele (car pattern-list) ele))))))))))
	(setf *referral* before-ref-context)
	(in-context before-context)
	(remove-null result))))

;;; -----------------------------------------------------------------------------------
;;; Partial Match

(defun remove-ith (i l)
	"The function takes in an index and a list, return a new copy of the list 
	without the ith element"
	(copy-tree (append (sublst l 0 i) (sublst l (+ i 1) nil))))

(defun construction-partial-match-checker (wordlist pattern-list var-constraint modifier)
	"The function that supports partial match and takes in 
	a list of words, a pattern list, a list of variable constraints 
	return if the list of word 
	could match the pattern."
	(if (construction-match-checker wordlist pattern-list var-constraint modifier t) t
		(if (< (length pattern-list) 3) nil
		(loop for i from 0 to (- (length pattern-list) 1)
			when (and (not (typep (nth i pattern-list) 'integer))
				(construction-match-checker wordlist (remove-ith i pattern-list)
					var-constraint modifier t))
			return t))))

(defun construction-partial-matcher (wordlist pattern-list var-constraint modifier verbose)
	"The matcher function that supports partial match and 
	takes in a wordlist, a pattern list and a list of variable constraints 
	and returns all possible combinationof the elements for the variables,
	the after context and the after referral context."
	(if (construction-match-checker wordlist pattern-list var-constraint modifier t) 
		(construction-matcher wordlist pattern-list var-constraint modifier t verbose)
		(if (> (length pattern-list) 2)
		(loop for i from 0 to (- (length pattern-list) 1)
			when (and (not (typep (nth i pattern-list) 'integer))
				(construction-match-checker wordlist (remove-ith i pattern-list)
					var-constraint modifier t))
			append (construction-matcher wordlist (remove-ith i pattern-list)
					var-constraint modifier t verbose)))))


;;; -----------------------------------------------------------------------------------
;;; Constructor

(defun tokenizer (text)
	"a simple tokenizer that takes a raw text and split by space
	keep comma as a separate token."
	(let ((temp-list (cl-ppcre:split "\\s+" text))) 
		(loop for str in temp-list 
			append (if (and (> (length str) 1) 
							(equal (char str (- (length str) 1)) #\,))
						(list (subseq str 0 (- (length str) 1)) ",")
						(list str)))))

(defun flatten-variable (variable-value constraint)
	"the function takes all possible values for each variable
	and the constraints for the variable, returns all possible combination
	for all of the variables."
	(if (null variable-value) (list NIL) 
		(let ((rest-result 
				(flatten-variable (cdr variable-value) (cdr constraint)))
			  (first-ele 
			  	(if (and (null (find :list (car constraint))) (typep (car (car variable-value)) 'cons))
			  		(mapcar 
			  			(lambda (subl) (cons subl (cdr (car variable-value)))) (car (car variable-value)))
			  		(car variable-value))))
			(if (find :list (car constraint))
				(if (null (typep (car first-ele) 'cons))
					(mapcar 
						(lambda (subl) 
							(append (list (list first-ele)) subl)) 
						rest-result)
					(mapcar 
						(lambda (subl) (append (list first-ele) subl)) 
						rest-result))
				(if (null (typep (car first-ele) 'cons))
					(mapcar 
						(lambda (subl) (append (list first-ele) subl)) 
						rest-result)
					(loop for val in first-ele
						append (mapcar 
								(lambda (subl) (append (list val) subl)) 
								rest-result)))))))

(defun multiple-apply (action variable-value-list verbose)
	"The function takes in an action and a list of all possible values
	for the variable, apply the action on each possible values and collect
	the results."
	(setq *construction-verbose* verbose)
	(if (= (length variable-value-list) 1)
		(apply action (car variable-value-list))
		(loop for var-value in variable-value-list
			collect (apply action var-value))))

(defun context-occurance (ctx match-results)
	"The function takes in a context and a list of match results from
	the matcher and count the occurance of the context"
	(if (null match-results) 0
		(if (simple-is-x-eq-y? ctx (car (cdr (reverse (car match-results)))))
			(+ 1 (context-occurance ctx (cdr match-results)))
			(context-occurance ctx (cdr match-results)))))

(defun pre-selection-pattern (text pattern)
	"the function takes in a raw text and a pattern list of a construction
	and returns if the text has the strings in the pattern"
	(let ((pad-text (concatenate 'string " " 
				(join-list-by-space (tokenizer text)) " ")))
		(if (null pattern) T
			(and 
			(if (typep (car pattern) 'cons) 
				(loop for phrase in (car pattern)
					when (not (null (search (concatenate 'string " " phrase " ") pad-text)))
					return T)
				T)
			(pre-selection-pattern text (cdr pattern))))))

(defun pre-selection-constraint (text constraint)
	"the function takes in a d raw text and a constraint list of a construction
	and returns if the text has the strings in the string constraint"
	(if (null constraint) T
		(and 
		(if (typep (car (car constraint)) 'string) 
			(loop for root in (car constraint)
				when (loop for word in (tokenizer text)
						when (string-equal (car (simple-morphology word)) root)
						return T)
				return T)
			T)
		(pre-selection-constraint text (cdr constraint)))))

(defun merge-modifier (context modifiers)
	"the function takes in a context for a construction, the modifier, 
	return the new modifier for the subparts of the construction"
	(loop for modifier in modifiers
		collect (let ((ctx_modifier 
						(loop for ele in modifier
							when (or (typep ele 'element) (typep ele 'element-iname))
							return ele)))
				(if (null ctx_modifier) context ctx_modifier))))

(defun tree-constructor (construction-tree text context taglist partial verbose)
	"the function takes in a construction tree, raw text, optional 
	syntax tags and applies matched constructions actions on it, 
	return the output scone element, syntax tag, the context and the 
	referral context after the construction."
	(let ((pattern (construction-pattern (car construction-tree)))
		  (constraint (construction-var-constraint  (car construction-tree)))
		  (modifier (merge-modifier context (construction-modifier (car construction-tree))))
		  (tag (construction-tag (car construction-tree))))
		(if (and (or (pre-selection-pattern text pattern) partial)
				(pre-selection-constraint text constraint)
				(if (null partial) (not (null (construction-match-checker 
					(tokenizer text) pattern constraint modifier nil)))
					(not (null (construction-partial-match-checker 
					(tokenizer text) pattern constraint modifier))))
				(or (null taglist) (find tag taglist)))

			(progn
				(if verbose (commentary "Match ~S with construction ~S pattern ~{~a~^, ~}" 
								text (construction-doc (car construction-tree)) pattern))
				(let ((match-results 
						(if (null partial) (construction-matcher (tokenizer text) pattern constraint modifier nil verbose)
							(construction-partial-matcher (tokenizer text) pattern constraint modifier verbose))))
				(loop for match-result in match-results
					for variable-value = (reverse (cdr (cdr (reverse match-result))))
					for cur-ctx = (car (cdr (reverse match-result)))
					for ctx = (if (= (context-occurance cur-ctx match-results) 1) 
										cur-ctx (new-context nil cur-ctx))
					do (in-context (if (null context) ctx context))
					do (setf *referral* (car (last match-result)))
					append 
						(let ((res (handler-case 
							(multiple-apply (construction-action (car construction-tree)) 
								(flatten-variable variable-value (construction-modifier (car construction-tree))) verbose) (t nil))))
							(if (not (null res))

								(let ((new-ctx *context*)
									  (new-ref (copy-tree *referral*)))
								(let ((new-res
									(loop for cons-tree in (cdr construction-tree)
										do (in-context new-ctx)
										do (setf *referral* new-ref)
										append (tree-constructor cons-tree text context taglist partial verbose))))
									(if (null new-res) (list (list res tag ctx new-ref)) new-res)))))))))))


(defun constructor (text context &optional taglist partial verbose)
	"the function takes in raw text and optional syntax tags 
  	and applies matched constructions actions on it, return the
  	output scone element, syntax tag, the context and the referral 
  	context after the construction."
	(let ((before-ref-context (copy-tree *referral*))
		  (before-context *context*))
	(let ((result
		(loop for construction-tree in *constructions*
			do (in-context before-context)
			do (setf *referral* before-ref-context)
			append (tree-constructor construction-tree text context taglist partial verbose))

		))
	(setf *referral* before-ref-context)
 	(in-context before-context)
 	(remove-null result))))









