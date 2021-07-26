;;; ***************************************************************************
;;; Scone Construction grammar engine tests
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(read-text "Facebook is an American technology company" t)
(read-text "Facebook is an American technology company based in California" t)
(read-text "Mark Zuckerberg is the founder of it" t)
(read-text "Mark Zuckerberg is the founder" t)
(read-text "Mark Zuckerberg, Eduardo and Andrew are the founders" t)

(read-text "it is founded by Mark Zuckerberg" t)
(read-text "it was founded by Mark Zuckerberg in 2004" t)
(read-text "it was founded by Mark Zuckerberg, Eduardo, Andrew, Dustin and Chris" t)
(read-text "it was founded by Mark Zuckerberg, along with his roommates" t)
(read-text "it was founded by Mark Zuckerberg, along with his roommates at Harvard" t)

(read-text "it is one of the most valuable companies" t)

(read-text "Google is an American technology company" t)
(read-text "Google is an American technology company that specializes in internet-related product" t)
(read-text "Google was founded by Larry Page and Sergey Brin" t)
(read-text "Google was founded by Larry Page and Sergey Brin while they were students at Stanford" t)