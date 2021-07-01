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
        ((< 0  len) (cons (car lst) (sublst (cdr lst) idx (1- len))))
 	))

(defun join_list_by_space (slist)
	"The function takes in a list and join the list of string by space."
	(format nil "~{~A~^ ~}" slist))

(defun get-syntax-tag (constraints)
	"The function takes in a list of variable constraints
	and returns the syntax tags in it."
	(loop for constraint in constraints
		when (not (null (find constraint (list :verb :noun :verb :relation :adj))))
		collect constraint))

(defun get-parents (constraints)
	"The function takes in a list of variable constraints
	and returns the scone elements in it."
	(loop for constraint in constraints
		when (typep constraint 'element-iname)
		collect constraint))

(defun meet-constraint (element constraints parents)
	""
	(if (and (find :list constraints) (null (typep element 'cons))) nil 
		(if (typep element 'cons) 
			(not (loop for ele in element
				when (null (meet-constraint ele 
							(remove :list constraints) parents))
				return T))
			(and (or (null (find :type constraints)) (type-node? element))
					  (or (null (find :indv constraints)) (indv-node? element))
					  (not (loop for parent in parents
					  		when (not (simple-is-x-a-y? element parent)) 
					  		return T))))))

(defun variable-match (text constraints)
	"The function takes in a raw text and a list of constraints
	for the variable, returns the matched scone elements that
	satisfies the constraints and the after referral context."
	(let ((syntax_tag (get-syntax-tag constraints))
		  (parents (get-parents constraints)))
		(append 
		(loop for element_pair in (lookup-definitions text syntax_tag)
			for element = (car element_pair)
			when (meet-constraint element constraints parents)
			collect (list element (copy-tree *referral*)))
		(loop for element_pair in (constructor text syntax_tag)
			for element = (car element_pair)
			for context = (nth 2 element_pair)
			when (meet-constraint element constraints parents)
			collect (list element context)))))

(defun one-ele-match (text pattern var_constraint)
	"The function takes in a raw text, a single pattern, a list of 
	variable constraints and check if the textmatch the pattern."
	(if (typep pattern 'integer) 
		(variable-match text (nth pattern var_constraint))
		(find text pattern :test #'string-equal)))

(defun construction-match-checker (wordlist pattern_list var_constraint)
	"The function takes in a list of words, a pattern list, 
	a list of variable constraints return if the list of word 
	could match the pattern."
	(let ((before-context (copy-tree *referral*))
		(result
	(cond 
		((and (null pattern_list) (null wordlist)) T)
		((or (null pattern_list) (null wordlist) 
			(< (length wordlist) (length pattern_list))) NIL)
		(T (loop for i from 1 to 
					(+ (- (length wordlist) (length pattern_list)) 1)
			for text = (join_list_by_space (sublst wordlist 0 i))
			when (and 
				(not (null (one-ele-match text (car pattern_list) var_constraint))) 
				(construction-match-checker 
					(sublst wordlist i (- (length wordlist) i)) 
								(cdr pattern_list) var_constraint))
			return T)))
	))
	(setf *referral* before-context)
	result))

(defun base-case-matcher (len ref)
	""
	(append (loop for i from 1 to len collect nil) (list ref)))

(defun change-ith-list (l i new_content)
	""
	(setf (nth i l) new_content)
	l)

(defun construction-matcher (wordlist pattern_list var_constraint)
	"The main matcher function that takes in a wordlist, a pattern list and 
	a list of variable constraints and returns all possible combination
	of the elements for the variables and the after referral context."
	(let ((before-context (copy-tree *referral*))
		(result
	(if (or (null pattern_list) (null wordlist) 
			(< (length wordlist) (length pattern_list))) 
		(list (base-case-matcher (length var_constraint) (copy-tree *referral*)))

		(loop for i from 1 to (+ (- (length wordlist) (length pattern_list)) 1)
			for text = (join_list_by_space (sublst wordlist 0 i))
			when (and 
				(not (null (one-ele-match text (car pattern_list) var_constraint))) 
				(construction-match-checker (sublst wordlist i (- (length wordlist) i)) 
					(cdr pattern_list) var_constraint))
			append 
			(let ((first_element_result 
				(one-ele-match text (car pattern_list) var_constraint)))

				(if (typep first_element_result 'string) 
					(construction-matcher (sublst wordlist i (- (length wordlist) i)) 
						(cdr pattern_list) var_constraint)
		
					(loop for ele_pair in first_element_result
						for ele = (car ele_pair)
						for ctx = (nth 1 ele_pair)
						append (progn 
							(setf *referral* ctx)
							(loop for rest_ele in (construction-matcher 
							  		(sublst wordlist i (- (length wordlist) i)) 
									(cdr pattern_list) var_constraint)
							  	collect (change-ith-list rest_ele (car pattern_list) ele))))))))))
	(setf *referral* before-context)
	result))

;;; -----------------------------------------------------------------------------------
;;; Constructor

(defun tokenizer (text)
	"a simple tokenizer that takes a raw text and split by space
	keep comma as a separate token."
	(let ((temp_list (cl-ppcre:split "\\s+" text))) 
		(loop for str in temp_list 
			append (if (equal (char str (- (length str) 1)) #\,)
						(list (subseq str 0 (- (length str) 1)) ",")
						(list str)))))

(defun flatten-variable (variable_value constraint)
	""
	(if (null variable_value) (list NIL) 
		(let ((rest_result 
				(flatten-variable (cdr variable_value) (cdr constraint))))
			(if (or (find :list (car constraint)) (null (typep (car variable_value) 'cons)))
				(mapcar (lambda (subl) (append (list (car variable_value)) subl)) rest_result)
				(loop for val in (car variable_value)
					append (mapcar (lambda (subl) (append (list val) subl)) rest_result))
				))))

(defun multiple-apply (action variable_value_list)
	""
	(if (= (length variable_value_list) 1)
		(apply action (car variable_value_list))
		(loop for var_value in variable_value_list
			collect (apply action var_value))))

(defun constructor (text &optional taglist)
	"the function takes in raw text and optional syntax tags 
	and applies matched constructions actions on it, return the
	output scone element, syntax tag and the referral context
	after the construction."
	(let ((before-context (copy-tree *referral*)))
	(let ((result (loop for construction in *constructions*
		for pattern = (construction-pattern construction)
		for constraint = (construction-var-constraint construction)
		for modifier = (construction-modifier construction)
		for tag = (construction-tag construction)
		do (setf *referral* before-context)
		when (and (not (null (construction-match-checker 
						(tokenizer text) pattern constraint)))
			(or (null taglist) (find tag taglist)))
		append (loop for match_result in 
				(construction-matcher (tokenizer text) pattern constraint)
					for variable_value = (reverse (cdr (reverse match_result)))
					do (setf *referral* (car (last match_result)))
					collect (handler-case (list 
						(multiple-apply (construction-action construction) 
							(flatten-variable variable_value constraint))   
						tag (copy-tree *referral*)) (t nil))))))
		(setf *referral* before-context)
		(loop for r in result
			when (not (null r))
			collect r))))

