;;; ***************************************************************************
;;; Scone Construction grammar engine tests
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(read-text "Clyde, George and Tony are elephants" t)
(read-text "they are grey" t)
(read-text "Wesley and Yang are friends" t)
(read-text "Wesley hates elephants" t)
(statement-true? {Wesley} {hate} {Clyde})
