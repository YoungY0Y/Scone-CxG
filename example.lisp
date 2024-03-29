;;; ***************************************************************************
;;; Scone Construction grammar engine examples
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

;;; ------------------------------------------------------------------------
;;; Noun Phrase

(read-text "an elephant" nil t)
; System reading "an elephant"
; Match "an elephant" with construction "np new individual" pattern (a an), 0
; Match "elephant" with {elephant}
; Create new individual {elephant 0-2698}
; Add {elephant 0-2698} to referral context
; Take result {elephant 0-2698}
; (("an elephant" {elephant 0-2698} :NOUN))

(refresh-context)
(read-text "some elephants" nil t)
; System reading "some elephants"
; Match "some elephants" with construction "np new individual plural" pattern 0, 1
; Match "some" with {some}
; Match "elephants" with {elephant}
; Create new type {elephant 0-2698}
; Assert {some} is the {count} of {elephant 0-2698}
; Add {elephant 0-2698} to referral context
; Take result {elephant 0-2698}
; (("some elephants" {elephant 0-2698} :NOUN))

(refresh-context)
(read-text "a smart person" nil t)
; System reading "a smart person"
; Match "a smart person" with construction "np new individual with adj" pattern (a
;                                                                                an), 0, 1
; Match "smart" with {smart thing}
; Match "person" with {person}
; Take result {person 0-2697}
; (("a smart person" {person 0-2697} :NOUN))
(simple-is-x-a-y? {person 0-2697} {smart thing})
; T

(refresh-context)
(read-text "Clyde is an elephant" nil t)
(read-text "the elephant" nil t)
; System reading "Clyde is an elephant"
; Create new name {Clyde}
; Match "Clyde is an elephant" with construction "create new is a" pattern 0, 1, (a
;                                                                                 an
;                                                                                 a kind of), 2
; Match "Clyde" with {Clyde}
; Match "elephant" with {elephant}
; Take result {Is-A 0-2706}
; System reading "the elephant"
; Match "the elephant" with construction "np referral individual" pattern (the), 0
; Match "elephant" with {elephant}
; Take result {Clyde}
; (("Clyde is an elephant" {Is-A 0-2706} :RELATION)
;  ("the elephant" {Clyde} :NOUN))

(refresh-context)
(read-text "Yang's friend" nil t)
; System reading "Yang's friend"
; Create new name {Yang}
; Match "Yang's friend" with construction "possessive type-role" pattern 0, 1
; Match "Yang" with {Yang}
; Match "friend" with {friend}
; Create new name {Yang's}
; Take result {person 0-2722}
; (("Yang's friend" {person 0-2722} :NOUN))
(list-all-x-of-y {friend} {Yang})
; ({person 0-2722})
(read-text "Yang and Jack are friends" nil nil)
; System reading "Yang and Jack are friends"
; Create new name {Yang}
; Create new name {Jack}
; Take result ({Is-A 0-2713} {Is-A 0-2715})
; (("Yang and Jack are friends" ({Is-A 0-2713} {Is-A 0-2715}) :RELATION))
(read-text "Yang's friend" nil t)
; System reading "Yang's friend"
; Match "Yang's friend" with construction "possessive type-role" pattern 0, 1
; Match "Yang" with {Yang}
; Match "friend" with {friend}
; Find {Jack} in referral context
; where {Jack} is a {person}
; Assert {Yang} is a {person}
; Add {Jack} to referral context
; Take result {Jack}
; (("Yang and Jack are friends" ({Is-A 0-2713} {Is-A 0-2715}) :RELATION)
;  ("Yang's friend" {Jack} :NOUN))

;;; ------------------------------------------------------------------------
;;; Parallel structure

(refresh-context)
(read-text "an elephant and a mouse" nil t)
(read-text "red and smart" nil t)
; System reading "an elephant and a mouse"
; Match "an elephant and a mouse" with construction "noun parallel structure" pattern 0, (and), 1
; Match "an elephant" with construction "np new individual" pattern (a an), 0
; Match "elephant" with {elephant}
; Match "an elephant" with {elephant 0-3029}
; Match "a mouse" with construction "np new individual" pattern (a an), 0
; Match "mouse" with {mouse}
; Match "mouse" with {computer mouse}
; Match "a mouse" with {mouse 0-3044}
; Match "a mouse" with {computer mouse 0-3047}
; Take result ({elephant 0-3029} {mouse 0-3044})
; System reading "red and smart"
; Match "red and smart" with construction "adj parallel structure" pattern 0, (and), 1
; Match "red" with {red thing}
; Match "smart" with {smart thing}
; Take result ({red thing} {smart thing})
; (("an elephant and a mouse" ({elephant 0-3029} {mouse 0-3044}) :NOUN)
;  ("red and smart" ({red thing} {smart thing}) :ADJ))

;;; ------------------------------------------------------------------------
;;; State Verb

(refresh-context)
(read-text "Yang hates elephants" nil t)
; System reading "Yang hates elephants"
; Match "Yang hates elephants" with construction "state hate" pattern 0, 1, 2
; Assume {Yang} is a {animal}
; Match "Yang" with {Yang}
; Match "elephants" with {elephant}
; Take result {hate 0-2732}
; (("Yang hates elephants" {hate 0-2732} :VERB))
(statement-true? {Yang} {hate} {elephant})
; T

;;; ------------------------------------------------------------------------
;;; Static Description

(refresh-context)
(read-text "elephant is a kind of mammal" nil t)
(read-text "elephants are grey" nil t)
(read-text "Clyde is an elephants" nil t)
(read-text "it is red, not grey" nil t)
; System reading "elephant is a kind of mammal"
; Match "elephant is a kind of mammal" with construction "create new is a" pattern 0, 1, (a
;                                                                                         an
;                                                                                         a kind of), 2
; Match "elephant" with {elephant}
; Match "mammal" with {mammal}
; Take result {Is-A 0-5400}
; System reading "elephants are grey"
; Match "elephants are grey" with construction "state verb adj" pattern 0, 1, 2
; Match "elephants" with {elephant}
; Match "grey" with {gray thing}
; Take result {Is-A 0-5401}
; System reading "Clyde is an elephants"
; Match "Clyde is an elephants" with construction "create new is a" pattern 0, 1, (a
;                                                                                  an
;                                                                                  a kind of), 2
; Match "Clyde" with {Clyde}
; Match "elephants" with {elephant}
; Take result {Is-A 0-5402}
; System reading "it is red, not grey"
; Match "it is red, not grey" with construction "state verb adj with not" pattern 0, (, not
;                                                                                     not), 1
; Match "it is red" with construction "state verb adj" pattern 0, 1, 2
; Match "it" with {Clyde}
; Match "it" with {elephant}
; Match "red" with {red thing}
; Match "red" with {red thing}
; Match "it is red" with {Is-A 0-5811}
; Match "it is red" with {Is-A 0-5812}
; Match "grey" with {gray thing}
; Match "grey" with {gray thing}
; Take result ({Is-A 0-5811} {Is-Not-A 0-5943})
; (("elephant is a kind of mammal" {Is-A 0-5400} :RELATION)
;  ("elephants are grey" {Is-A 0-5401} :RELATION)
;  ("Clyde is an elephants" {Is-A 0-5402} :RELATION)
;  ("it is red, not grey" ({Is-A 0-5811} {Is-Not-A 0-5943}) :RELATION))
(list-all-x-of-y {predominant color} {Clyde})
; ({red})
(list-all-x-of-y {predominant color} {elephant})
; ({gray})

(refresh-context)
(read-text "Facebook is an American technology company based in California" nil t)
(read-text "Mark Zuckerberg and his roommates are the founders" nil t)
; System reading "Facebook is an American technology company based in California"
; Match "Facebook is an American technology company based in California" with construction "state verb indv" pattern 0, 1, 2
; Match "Facebook" with {Facebook}
; Match "an American technology company" with construction "np new individual with adj" pattern (a
;                                                                                                an), 0, 1
; Match "American" with {American thing}
; Match "technology company" with {technology company}
; Match "an American technology company" with {technology company 0-82567}
; Match "an American technology company based in California" with construction "np organization with location" pattern 0, (based
;                                                                                                                          located), (in
;                                                                                                                                     at), 1
; Match "an American technology company" with construction "np new individual with adj" pattern (a
;                                                                                                an), 0, 1
; Match "American" with {American thing}
; Match "technology company" with {technology company}
; Match "an American technology company" with {technology company 0-84831}
; Match "California" with {California}
; Match "an American technology company based in California" with {technology company 0-84831}
; Take result {Eq 0-88537}
; System reading "Mark Zuckerberg and his roommates are the founders"
; Create new name {Mark}
; Create new name {Zuckerberg}
; Create new name {Mark Zuckerberg}
; Match "Mark Zuckerberg and his roommates are the founders" with construction "create several y of z" pattern 0, (are the), 1
; Match "Mark" with {Mark}
; Match "Mark Zuckerberg" with {Mark Zuckerberg}
; Match "Mark Zuckerberg and his roommates" with construction "noun parallel structure" pattern 0, (and), 1
; Match "Mark" with {Mark}
; Match "Mark Zuckerberg" with {Mark Zuckerberg}
; Match "his roommates" with construction "possessive type-role" pattern 0, 1
; Match "he" with {Mark Zuckerberg}
; Match "roommates" with {roommate}
; Match "his roommates" with {person 0-120751}
; Match "Mark Zuckerberg and his roommates" with ({Mark Zuckerberg}
;                                                 {person 0-120751})
; Match "founders" with {founder}
; Take result ({Is-A 0-122618} {Is-A 0-122619})
; (("Facebook is an American technology company based in California" {Eq 0-88537}
;   :RELATION)
;  ("Mark Zuckerberg and his roommates are the founders"
;   ({Is-A 0-122618} {Is-A 0-122619}) :RELATION))
(the-x-of-y {based location} {Facebook})
; {California}
(the-x-of-y {belonged country} {Facebook})
; {United States}
(list-all-x-of-y {founder} {Facebook})
; ({person 0-120751} {Mark Zuckerberg})
(list-all-x-of-y {roommate} {Mark Zuckerberg})
; ({person 0-120751})

(refresh-context)
(read-text "Yang has two dogs" nil t)
; System reading "Yang has two dogs"
; Create new name {Yang}
; Match "Yang has two dogs" with construction "has relation with number" pattern 0, (has
;                                                                                    have), 1, 2
; Match "Yang" with {Yang}
; Match "two" with {two}
; Match "dogs" with {dog}
; Take result {dog 0-159013}
; (("Yang has two dogs" {dog 0-159013} :RELATION))

;;; ------------------------------------------------------------------------
;;; Tense
(refresh-context)
(read-text "dogs were smart" nil t)
; System reading "dogs were smart"
; Match "dogs were smart" with construction "state verb adj" pattern 0, 1, 2
; Match "dogs" with {dog}
; Match "smart" with {smart thing}
; Take result {Is-A 0-2745}
; (("dogs were smart" {Is-A 0-2745} :RELATION))
(context-element {Is-A 0-2745})
; {past 0-2743}

;;; ------------------------------------------------------------------------
;;; Prepositional Phrase

(refresh-context)
(read-text "elephants are grey in Africa" nil t)
; System reading "elephants are grey in Africa"
; Match "elephants are grey in Africa" with construction "location prepositional phrase" pattern 0, (in
;                                                                                                    at
;                                                                                                    on), 1
; Match "elephants are grey" with construction "state verb adj" pattern 0, 1, 2
; Match "elephants" with {elephant}
; Match "are" with {are}
; Match "grey" with {gray thing}
; Add {elephant} to referral context
; Create new is-a link between {elephant} and {gray thing}
; Match "elephants are grey" with {Is-A 0-2699}
; Match "Africa" with {Africa}
; Change context of {Is-A 0-2699} to {Africa}
; Take result {Is-A 0-2699}
; (("elephants are grey in Africa" {Is-A 0-2699} :RELATION))

;;; ------------------------------------------------------------------------
;;; Example for Context Back-tracking
(refresh-context)
(read-text "Yang has a mouse" nil t)
(read-text "the mouse is a tool" nil t)
; System reading "Yang has a mouse"
; Create new name {Yang}
; Match "Yang has a mouse" with construction "has relation with number one" pattern 0, (has
;                                                                                       have), (a
;                                                                                               an), 1
; Match "Yang" with {Yang}
; Match "mouse" with {mouse}
; Match "mouse" with {computer mouse}
; Take result {mouse 0-2847}
; System reading "the mouse is a tool"
; Match "the mouse is a tool" with construction "create new is a" pattern 0, 1, (a
;                                                                                an
;                                                                                a kind of), 2
; Match "the mouse" with construction "np referral individual" pattern (the), 0
; Match "mouse" with {mouse}
; Match "mouse" with {computer mouse}
; Match "the mouse" with {mouse 0-2847}
; Match "tool" with {tool}
; Match "the mouse is a tool" with construction "state verb indv" pattern 0, 1, 2
; Match "the mouse" with construction "np referral individual" pattern (the), 0
; Match "mouse" with {mouse}
; Match "mouse" with {computer mouse}
; Match "the mouse" with {mouse 0-2847}
; Match "a tool" with construction "np new individual" pattern (a an), 0
; Match "tool" with {tool}
; Match "a tool" with {tool 0-2888}
; No construction matched in current context
; Backtracking to the text "Yang has a mouse"
; Taking {computer mouse 0-2850}, RELATION, {general 0-2849}, ({computer mouse 0-2850}
;                                                              {Yang}) as new state
; This state works
; System reading "the mouse is a tool"
; Take result {Is-A 0-3085}
; (("Yang has a mouse" {computer mouse 0-2850} :RELATION)
;  ("the mouse is a tool" {Is-A 0-3085} :RELATION))
