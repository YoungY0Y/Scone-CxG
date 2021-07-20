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

(defun join_list_by_space (slist)
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
	(and (not (loop for partial in (cl-ppcre:split "\\s+" text)
		when (not (upper-case-p (char partial 0)))
		return T))
		(null (find :list constraints))
		(null (find :verb constraints))
		(null (find :relation constraints))
		(null (find :adj constraints))
		(null (find :type constraints))))

(defun meet-constraint (element constraints parents ctx if_new_ctx verbose)
	"The function takes in an element, the constraints for the variable, 
	the parents for the variable, context of the element and whether
	the context need to be renewed when encountered a name and returns 
	nil if the element does not match the constraints, return the context
	if the element match."
	(if (null element) ctx
	(if (and (find :list constraints) (null (typep element 'cons))) nil 
		(if (typep element 'cons) 
			(let ((next_ctx 
					(meet-constraint (car element) 
						(remove :list constraints) parents ctx if_new_ctx verbose)))
				(if (null next_ctx) nil 
					(if (and (null (simple-is-x-eq-y? next_ctx ctx)) if_new_ctx)
					(meet-constraint (cdr element) 
						constraints parents next_ctx nil verbose)
					(meet-constraint (cdr element) 
						constraints parents next_ctx if_new_ctx verbose))))

			(if (and (or (null (find :type constraints)) (type-node? element))
			     (or (null (find :indv constraints)) (indv-node? element))
			     (not (loop for parent in parents
					    when (not (simple-is-x-a-y? element parent)) 
					  	return T))) ctx
				(if (might-be-name (iname element) constraints)
				(handler-case
					(let ((before_context ctx)
						  (new_ctx (if if_new_ctx (new-context NIL ctx) ctx)))
						(in-context new_ctx)
						(loop for parent in parents
							do (if verbose (commentary "Assume ~S is a ~S" element parent))
							do (new-is-a element parent))
						(in-context before_context)
						new_ctx) (t nil))))))))

(defun pronoun-match (text syntax_tag)
	"take in a text and return the noun it refers to if it is a pronoun"
	(if (not (and (null (find :verb syntax_tag)) (null (find :adj syntax_tag))
		(null (find :relation syntax_tag)))) NIL

	(cond ((or (equal text "he") (equal text "him"))
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(not (equal (is-x-a-y? ele {male person}) :NO)))
					collect (let ((before_context *context*)
								  (new_ctx (new-context NIL *context*)))
								(in-context new_ctx)
								(new-is-a ele {male person})
								(in-context before_context)
								(list ele new_ctx))))
		((or (equal text "she") (equal text "her"))
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(not (equal (is-x-a-y? ele {female person}) :NO)))
					collect (let ((before_context *context*)
								  (new_ctx (new-context NIL *context*)))
								(in-context new_ctx)
								(new-is-a ele {female person})
								(in-context before_context)
								(list ele new_ctx))))
		((equal text "it") 
				(loop for ele in *referral*
					when (and (or (typep ele 'element) (typep ele 'element-iname))
							(not (equal (is-x-a-y? ele {person}) :YES)))
					collect (let ((before_context *context*)
								  (new_ctx (new-context NIL *context*)))
								(in-context new_ctx)
								(new-is-not-a ele {person})
								(in-context before_context)
								(list ele new_ctx))))

		((or (equal text "they") (equal text "them"))
				(loop for ele in *referral*
					when (or (typep ele 'cons) (type-node? ele))
					collect (list ele *context*)))
		(t NIL))))

(defun variable-match (text constraints verbose)
	"The function takes in a raw text and a list of constraints
	for the variable, returns the matched scone elements that
	satisfies the constraints, the after context
	and the after referral context."
	(let ((syntax_tag (get-syntax-tag constraints))
		  (parents (get-parents constraints)))
		(let ((result 
			(append 
				(loop for element_pair in (lookup-definitions text syntax_tag)
					for element = (car element_pair)
					for new_ctx = 
						(meet-constraint element constraints parents *context* t verbose)
					when (not (null new_ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(if (might-be-name text constraints)
							(list element new_ctx (append (list element) 
											(remove-dup element (copy-tree *referral*))))
							(list element new_ctx (copy-tree *referral*)))))
				(loop for element_pair in (constructor text syntax_tag verbose)
					for element = (car element_pair)
					for context = (nth 2 element_pair)
					for ref-context = (nth 3 element_pair)
					for new_ctx = 
						(meet-constraint element constraints parents context t verbose)
					when (not (null new_ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(if (and (typep element 'cons)
								(not (loop for ele in element
										when (null (indv-node? ele))
										return T)))
							(list element new_ctx (append (list element) 
													(remove-dup element ref-context)))
							(list element new_ctx ref-context))))

				(loop for element_pair in (pronoun-match text syntax_tag)
					for element = (car element_pair)
					for context = (nth 1 element_pair)
					for new_ctx = 
						(meet-constraint element constraints parents context t verbose)
					when (not (null new_ctx))
					collect (progn
						(if verbose (commentary "Match ~S with ~S" text element))
						(list element new_ctx (copy-tree *referral*)))))))

			(if (not (and (null result) (null (lookup-definitions text)))) result
				(if (might-be-name text constraints)
					(handler-case 
					(let ((new_node (new-indv text {thing}))
						  (before_context *context*)
						  (new_ctx (new-context NIL *context*)))
						(commentary "Create new name ~S" new_node)
						(in-context new_ctx)
						(loop for parent in parents
							do (new-is-a new_node parent))
						(in-context before_context)
						(list (list new_node new_ctx (append (list new_node) 
											(remove-dup new_node (copy-tree *referral*))))))
					(t nil)))))))

(defun one-ele-match (text pattern var_constraint verbose)
	"The function takes in a raw text, a single pattern, a list of 
	variable constraints and check if the textmatch the pattern."
	(if (typep pattern 'integer) 
		(variable-match text (nth pattern var_constraint) verbose)
		(find text pattern :test #'string-equal)))

(defun construction-match-checker (wordlist pattern_list var_constraint)
	"The function takes in a list of words, a pattern list, 
	a list of variable constraints return if the list of word 
	could match the pattern."
	(let ((before_ref_context (copy-tree *referral*))
		  (before_context *context*))
	(let ((result
	(cond 
		((and (null pattern_list) (null wordlist)) T)
		((or (null pattern_list) (null wordlist) 
			(< (length wordlist) (length pattern_list))) NIL)
		(T (loop for i from 1 to 
					(+ (- (length wordlist) (length pattern_list)) 1)
			for text = (join_list_by_space (sublst wordlist 0 i))
			for first_element_result = (progn 
				(setf *referral* before_ref_context)
				(in-context before_context)
				(one-ele-match text (car pattern_list) var_constraint NIL))
			when (and 
				(not (null first_element_result)) 
				(if (typep first_element_result 'string) 
					(construction-match-checker 
						(sublst wordlist i (- (length wordlist) i)) 
						(cdr pattern_list) var_constraint)
					(loop for ele_pair in first_element_result
						for ele = (car ele_pair)
						for ref-ctx = (nth 2 ele_pair)
						for ctx = (nth 1 ele_pair)
						when (progn 
							(setf *referral* ref-ctx)
							(in-context ctx)
							(construction-match-checker 
								(sublst wordlist i (- (length wordlist) i)) 
											(cdr pattern_list) var_constraint))
						return T)))
			return T)))))
	(setf *referral* before_ref_context)
	(in-context before_context)
	result)))

(defun base-case-matcher (len)
	"the function takes in the length, and return the base case
	for the construction matcher, which is a list of nil and 
	context and referral context at the end"
	(append (loop for i from 1 to len collect nil) 
		(list *context*) (list (copy-tree *referral*))))

(defun change-ith-list (l i new_content)
	"the function takes in a list, an index i and a new content,
	changes the ith element to the new content and returns the list"
	(setf (nth i l) new_content)
	l)

(defun remove-null (l)
	"the function takes in a list, remove the null element and 
	return the new list"
	(loop for ele in l 
		when (not (null ele))
		collect ele))

(defun construction-matcher (wordlist pattern_list var_constraint verbose)
	"The main matcher function that takes in a wordlist, a pattern list and 
	a list of variable constraints and returns all possible combination
	of the elements for the variables, the after context
	and the after referral context."
	(let ((before_ref_context (copy-tree *referral*))
		  (before_context *context*)
		(result
	(if (or (null pattern_list) (null wordlist) 
			(< (length wordlist) (length pattern_list))) 
		(list (base-case-matcher (length var_constraint)))

		(loop for i from 1 to (+ (- (length wordlist) (length pattern_list)) 1)
			for text = (join_list_by_space (sublst wordlist 0 i))
			for first_element_result = 
				(one-ele-match text (car pattern_list) var_constraint verbose)
			when (not (null first_element_result)) 
			append 
				(if (typep first_element_result 'string) 
					(if (construction-match-checker 
							(sublst wordlist i (- (length wordlist) i)) 
							(cdr pattern_list) var_constraint)
					(construction-matcher (sublst wordlist i (- (length wordlist) i)) 
						(cdr pattern_list) var_constraint verbose))
		
					(loop for ele_pair in first_element_result
						for ele = (car ele_pair)
						for ref-ctx = (nth 2 ele_pair)
						for ctx = (nth 1 ele_pair)
						append (progn 
							(setf *referral* ref-ctx)
							(in-context ctx)
							(if (construction-match-checker 
									(sublst wordlist i (- (length wordlist) i)) 
									(cdr pattern_list) var_constraint)
								(loop for rest_ele in 
									(construction-matcher 
										(sublst wordlist i (- (length wordlist) i)) 
										(cdr pattern_list) var_constraint verbose)
								collect (change-ith-list 
											rest_ele (car pattern_list) ele))))))))))
	(setf *referral* before_ref_context)
	(in-context before_context)
	(remove-null result)))

;;; -----------------------------------------------------------------------------------
;;; Constructor

(defun tokenizer (text)
	"a simple tokenizer that takes a raw text and split by space
	keep comma as a separate token."
	(let ((temp_list (cl-ppcre:split "\\s+" text))) 
		(loop for str in temp_list 
			append (if (and (> (length str) 1) 
							(equal (char str (- (length str) 1)) #\,))
						(list (subseq str 0 (- (length str) 1)) ",")
						(list str)))))

(defun flatten-variable (variable_value constraint)
	"the function takes all possible values for each variable
	and the constraints for the variable, returns all possible combination
	for all of the variables."
	(if (null variable_value) (list NIL) 
		(let ((rest_result 
				(flatten-variable (cdr variable_value) (cdr constraint))))
			(if (or (find :list (car constraint)) 
					(null (typep (car variable_value) 'cons)))
				(mapcar 
					(lambda (subl) (append (list (car variable_value)) subl)) 
					rest_result)
				(loop for val in (car variable_value)
					append (mapcar 
							(lambda (subl) (append (list val) subl)) 
							rest_result))))))

(defun multiple-apply (action variable_value_list)
	"The function takes in an action and a list of all possible values
	for the variable, apply the action on each possible values and collect
	the results."
	(if (= (length variable_value_list) 1)
		(apply action (car variable_value_list))
		(loop for var_value in variable_value_list
			collect (apply action var_value))))

(defun context-occurance (ctx match_results)
	"The function takes in a context and a list of match results from
	the matcher and count the occurance of the context"
	(if (null match_results) 0
		(if (simple-is-x-eq-y? ctx (car (cdr (reverse (car match_results)))))
			(+ 1 (context-occurance ctx (cdr match_results)))
			(context-occurance ctx (cdr match_results)))))

(defun pre-selection (text pattern)
	"the function takes in a d raw text and a pattern list of a construction
	and returns if the text has the strings in the pattern"
	(let ((pad_text (concatenate 'string " " text " ")))
		(if (null pattern) T
			(and 
			(if (typep (car pattern) 'cons) 
				(loop for phrase in (car pattern)
					when (not (null (search (concatenate 'string " " phrase " ") pad_text)))
					return T)
				T)
			(pre-selection text (cdr pattern))))))

(defun constructor (text &optional taglist verbose)
	"the function takes in raw text and optional syntax tags 
	and applies matched constructions actions on it, return the
	output scone element, syntax tag, the context and the referral 
	context after the construction."
	(let ((before-ref-context (copy-tree *referral*))
		  (before-context *context*))
	(let ((result (loop for construction in *constructions*
		for pattern = (construction-pattern construction)
		for constraint = (construction-var-constraint construction)
		for modifier = (construction-modifier construction)
		for tag = (construction-tag construction)
		do (setf *referral* before-ref-context)
		do (in-context before-context)
		when (and (pre-selection text pattern)
			(not (null (construction-match-checker 
						(tokenizer text) pattern constraint)))
			(or (null taglist) (find tag taglist)))
		append 
			(progn
			(if verbose (commentary "Match ~S with construction ~S pattern ~{~a~^, ~}" 
								text (construction-doc construction) pattern))
			(let ((match_results 
						(construction-matcher (tokenizer text) pattern constraint verbose)))
			(loop for match_result in match_results
					for variable_value = (reverse (cdr (cdr (reverse match_result))))
					for cur_ctx = (car (cdr (reverse match_result)))
					for ctx = (if (= (context-occurance cur_ctx match_results) 1) 
										cur_ctx (new-context nil cur_ctx))
					do (in-context ctx)
					do (setf *referral* (car (last match_result)))
					collect (handler-case (list 
						(multiple-apply (construction-action construction) 
							(flatten-variable variable_value constraint))   
						tag *context* (copy-tree *referral*)) (t nil))))))))
		(setf *referral* before-ref-context)
		(in-context before-context)
		(loop for r in result
			when (not (null r))
			collect r))))

