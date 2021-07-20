;;; ***************************************************************************
;;; Core construction grammar engine for Scone knowledge base
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(defvar *text-record* nil)
(defvar *result-record* nil)

(defun refresh-context ()
	"The function sets all saved contexts and text to nil."
	(setf *referral* NIL)
	(setf *text-record* nil)
	(setf *result-record* nil)
	(in-context {general}))


(defun read-text-helper (ctx ref-ctx text-list) 
	"The function serves as helper function for the read-text 
	function, which takes in a ctx and a list of text and returns
	whether the list of texts make sense under the given referral
	context"
	(if (null text-list) T
	(let ((before-ref-context (copy-tree *referral*))
		  (before-context *context*))
		(setf *referral* ref-ctx)
		(in-context ctx)
		(let ((result 
			(let ((construct-result (constructor (car text-list))))
				(loop for con in construct-result
					for next-ctx = (nth 2 con)
					for next-ref-ctx = (nth 3 con)
					when (read-text-helper next-ctx next-ref-ctx (cdr text-list))
					return T))))
			(setf *referral* before-ref-context)
			(in-context before-context)
			result))))

(defun backtrack (unread-text verbose) 
	"The function takes in a list of text and backtrack the context 
	record to a state that all the input text make sense and return
	the unfilled text."
	(if verbose (commentary "Backtracking to the text ~S" (car (car (last *text-record*)))))
	(if (read-text-helper *context* *referral* unread-text) unread-text
		(if (null (car (last *result-record*)))
			(if (null *text-record*) nil 
				(let ((new-unfilled (nconc unread-text 
						(list (car (car (reverse *text-record*)))))))
				(setf *result-record* (reverse (cdr (reverse *result-record*))))
				(setf *text-record* (reverse (cdr (reverse *text-record*))))
				(backtrack new-unfilled verbose)))
			
			(let ((next (car (car (last *result-record*)))))
				(if verbose (commentary "Taking ~{~a~^, ~} as new state" next))
				(setf (car (last *result-record*)) (cdr (car (last *result-record*))))
				(if (read-text-helper (nth 2 next) (nth 3 next) unread-text) 
					(progn 
						(if verbose (commentary "This state works"))
						(in-context (nth 2 next))
						(setf *referral* (nth 3 next))
						(setf (nth 1 (car (last *text-record*))) (nth 0 next))
						(setf (nth 2 (car (last *text-record*))) (nth 1 next))
						unread-text)
					(backtrack unread-text verbose))))))

(defun read-text (text &optional verbose)
	"The function takes in a piece of text, run the constructor on the text 
	and combine the output with previously collected infomation, return current
	result of the construction engine for all of the input texts."
	(commentary "System reading ~S" text)
	(let ((construction-result (constructor text NIL verbose)))
		(if (not (null construction-result))
			(progn
				(setq ele (nth 0 (car construction-result)))
				(commentary "Take result ~S" ele)
				(setq tag (nth 1 (car construction-result)))
				(setq ctx (nth 2 (car construction-result)))
				(setq ref-ctx (nth 3 (car construction-result)))
				(setf *text-record* (nconc *text-record* (list (list text ele tag))))
				(setf *result-record* 
					(nconc *result-record* (list (cdr construction-result))))
				(in-context ctx)
				(setf *referral* ref-ctx)
				*text-record*)
			(progn
			(if verbose (commentary "No construction matched in current context"))
			(let ((new-unfilled (backtrack (list text) verbose)))
				(if (null new-unfilled) nil
					(progn
						(loop for new-text in new-unfilled
							do (read-text new-text NIL))
						*text-record*)))))))

