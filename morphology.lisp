;;; ***************************************************************************
;;; A toy version of morphology parser for the Scone construction grammar engine. 
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(defun is_simple_past (text)
	"the function takes in a verb and return if it is a simple past form"
	(if (and (> (length text) 2)
		(string-equal (subseq text (- (length text) 2)) "ed"))
		(if (not (null (lookup-definitions (subseq text 0 (- (length text) 2)) '(:verb))))
			(subseq text 0 (- (length text) 2))
		(if (not (null (lookup-definitions (subseq text 0 (- (length text) 1)) '(:verb))))
			(subseq text 0 (- (length text) 1))))))

(defvar *special-past*
		'(("ate" . "eat")
		  ("left" . "leave")
		  ("made" . "make")
		  ("gave" . "give")
		  ("sat" . "sit")))

(defun is_simple_singular (text)
	"the function takes in a verb and return if it is a simple singular form"
	(if (and (> (length text) 1)
		(string-equal (subseq text (- (length text) 1)) "s")
		(not (null (lookup-definitions (subseq text 0 (- (length text) 1)) '(:verb)))))
		(subseq text 0 (- (length text) 1))
	(if (and (> (length text) 2)
		(string-equal (subseq text (- (length text) 2)) "es")
		(not (null (lookup-definitions (subseq text 0 (- (length text) 2)) '(:verb)))))
		(subseq text 0 (- (length text) 2))
	(if (and (> (length text) 3)
		(string-equal (subseq text (- (length text) 3)) "ies")
		(not (null (lookup-definitions 
			(concatenate 'string (subseq text 0 (- (length text) 3)) "y") '(:verb)))))
		(concatenate 'string (subseq text 0 (- (length text) 3)) "y")
		))))

(defun is_simple_future (text)
	"the function takes in a verb and return if it is a simple singular form"
	(if (and (> (length text) 5)
		(string-equal (subseq text 0 5) "will ")
		(not (null (lookup-definitions (subseq text 5) '(:verb)))))
		(subseq text 5)))

(defun simple-morphology (text)
	"a simple mopholorgy function that takes in a text, return
	the root text and the morphology tags"
	(cond 
		((string-equal text "are") (list "are"))
		((string-equal text "is") (list "are" :singular))
		((string-equal text "was") (list "are" :past :singular))
		((string-equal text "were") (list "are" :past))
		((string-equal text "will be") (list "is" :future))
		((not (null (assoc text *special-past* :test #'string-equal))) 
						(list (cdr (assoc text *special-past* :test #'string-equal)) :past))
		((not (null (is_simple_past text))) (list (is_simple_past text) :past))
		((not (null (is_simple_singular text))) (list (is_simple_singular text) :singular))
		((not (null (is_simple_future text))) (list (is_simple_future text) :future))
		(t (list text))
		))

