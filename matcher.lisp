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

(defun variable-match (text constraints)
	"The function takes in a raw text and a list of constraints
	for the variable, returns the matched scone elements that
	satisfies the constraints and the after referral context."
	(let ((syntax_tag (get-syntax-tag constraints))
		  (parents (get-parents constraints)))
		(append 
		(loop for element_pair in (lookup-definitions text syntax_tag)
			for element = (car element_pair)
			when (and (or (null (find :type constraints)) (type-node? element))
					  (or (null (find :indv constraints)) (indv-node? element))
					  (not (loop for parent in parents
					  		when (not (simple-is-x-a-y? element parent)) 
					  		return T)))
			collect (list element (copy-tree *referral*)))
		(loop for element_pair in (constructor text syntax_tag)
			for element = (car element_pair)
			for context = (nth 2 element_pair)
			when (and (or (null (find :type constraints)) (type-node? element))
					  (or (null (find :indv constraints)) (indv-node? element))
					  (not (loop for parent in parents
					  		when (not (simple-is-x-a-y? element parent)) 
					  		return T)))
			collect (list element context)))))


(defun one-ele-match (text pattern var_constraint)
	"The function takes in a raw text, a single pattern, a list of 
	variable constraints and check if the textmatch the pattern."
	(if (typep pattern 'integer) (variable-match text (nth pattern var_constraint))
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

(defun construction-matcher (wordlist pattern_list var_constraint)
	"The main matcher function that takes in a wordlist, a pattern list and 
	a list of variable constraints and returns all possible combination
	of the elements for the variables and the after referral context."
	(let ((before-context (copy-tree *referral*))
		(result
	(if (or (null pattern_list) (null wordlist) 
			(< (length wordlist) (length pattern_list))) 
		(list (list (copy-tree *referral*)))
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
							  	collect (cons ele rest_ele))))))))))
	(setf *referral* before-context)
	result))

;;; -----------------------------------------------------------------------------------
;;; Constructor

(defun tokenizer (text)
	"a simple tokenizer that takes a raw text and split by space."
	(setq temp_list (cl-ppcre:split "\\s+" text))
	(loop for x in temp_list collect x))

(defun constructor (text &optional taglist)
	"the function takes in raw text and optional syntax tags 
	and applies matched constructions actions on it, return the
	output scone element, syntax tag and the referral context
	after the construction."
	(let ((before-context (copy-tree *referral*)))
	(let ((result (loop for construction in *constructions*
		for pattern = (construction-pattern construction)
		for constraint = (construction-var-constraint construction)
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
						(apply (construction-action construction) variable_value)   
						tag (copy-tree *referral*)) (t nil))))))
		(setf *referral* before-context)
		(loop for r in result
			when (not (null r))
			collect r))))

