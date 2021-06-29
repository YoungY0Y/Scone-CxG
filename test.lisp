;;; ***************************************************************************
;;; Scone Construction grammar engine tests
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(print (constructor "Clyde is an elephant"))
; (({Eq 0-3024} :RELATION (({elephant} {elephant 0-3022})))
;  ({Is-A 0-3027} :RELATION NIL))

(print (constructor "the elephant"))
; NIL

(print (constructor "a red apple"))
; (({apple 0-3042} :NOUN (({apple} {apple 0-3042})))) 
; (simple-is-x-a-y? {apple 0-3042} {red thing})
; T

(print (constructor "a mouse kicks an apple"))
; (({kick 0-3359} :VERB (({apple} {apple 0-3349}) ({mouse} {mouse 0-3341})))) 
; (list-all-x-of-y {action agent} {kick 0-3359})
; ({mouse 0-3341})
; (list-all-x-of-y {action object} {kick 0-3359})
; ({apple 0-3349})

(print (constructor "an elephant kicks a mouse"))
; (({kick 0-3539} :VERB
;   (({mouse} {mouse 0-3531}) ({elephant} {elephant 0-3521})))
;  ({kick 0-3545} :VERB
;   (({computer mouse} {computer mouse 0-3533}) ({elephant} {elephant 0-3521})))) 

(print (constructor "elephant hates dog"))
; (({hate 0-3557} :VERB NIL)) 
; (a-element {hate 0-3557})
; {elephant}
; (b-element {hate 0-3557})
; {dog}

(print (constructor "Clara is smart"))
; (({Is-A 0-3558} :RELATION NIL)) 
; (simple-is-x-a-y? {Clara} {smart thing})
; T
; (a-element {Is-A 0-3558})
; {Clara}
; (b-element {Is-A 0-3558})
; {smart thing}

(read-text "Clyde is an elephant")
(print *text-record*)
; (("Clyde is an elephant" {Eq 0-3570} :RELATION)) 
(print *result-record*)
; ((({Is-A 0-3573} :RELATION NIL))) 
(print *referral*)
; (({elephant} {elephant 0-3568})) 

(read-text "Clyde kicks a mouse")
(print *text-record*)
; (("Clyde is an elephant" {Eq 0-3570} :RELATION)
;  ("Clyde kicks a mouse" {kick 0-3594} :VERB)) 
(print *result-record*)
; ((({Is-A 0-3573} :RELATION NIL))
;  (({kick 0-3600} :VERB
;    (({computer mouse} {computer mouse 0-3592})
;     ({elephant} {elephant 0-3568}))))) 
(print *referral*)
; (({mouse} {mouse 0-3590}) ({elephant} {elephant 0-3568})) 

(read-text "Yang made the mouse")
(print *text-record*)
; (("Clyde is an elephant" {Eq 0-3570} :RELATION)
;  ("Clyde kicks a mouse" {kick 0-3600} :VERB)
;  ("Yang made the mouse" {make 0-3612} :VERB)) 
(print *result-record*)
; ((({Is-A 0-3573} :RELATION NIL)) NIL NIL) 
(print *referral*)
; (({computer mouse} {computer mouse 0-3592}) ({elephant} {elephant 0-3568})) 

