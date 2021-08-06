;;; ***************************************************************************
;;; Scone Construction grammar engine tests
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(read-text "elephant is a kind of mammal" t)
(read-text "elephants are grey" t)
(read-text "Clyde, George and Tony are elephants" t)
(read-text "they are red, not grey" t)
(read-text "Wesley and Yang are friends" t)
(read-text "Wesley is smart and he is a basketball player" t)
; (read-text "he hates elephants" t)
(statement-true? {Wesley} {hate} {Clyde})

; System reading "elephant is a kind of mammal"
; Match "elephant is a kind of mammal" with construction "create new is a" pattern 0, (is a
;                                                                                      is an
;                                                                                      is a kind of), 1
; Match "elephant" with {elephant}
; Match "mammal" with {mammal}
; Take result {Is-A 0-72422}
; System reading "elephants are grey"
; Match "elephants are grey" with construction "state verb adj" pattern 0, (is
;                                                                           are), 1
; Match "elephants" with {elephant}
; Match "grey" with {gray thing}
; Take result {Is-A 0-72423}
; System reading "Clyde, George and Tony are elephants"
; Create new name {Clyde}
; Create new name {George}
; Create new name {Tony}
; Match "Clyde, George and Tony are elephants" with construction "state verb type" pattern 0, (is
;                                                                                              are), 1
; Match "Clyde" with {Clyde}
; Match "Clyde , George and Tony" with construction "noun parallel structure" pattern 0, (,), 1
; Match "Clyde" with {Clyde}
; Match "George and Tony" with construction "noun parallel structure" pattern 0, (and), 1
; Match "George" with {George}
; Match "Tony" with {Tony}
; Match "George and Tony" with ({George} {Tony})
; Match "Clyde , George and Tony" with ({Clyde} {George} {Tony})
; Match "elephants" with {elephant}
; Take result ({Is-A 0-86628} {Is-A 0-86629} {Is-A 0-86630})
; System reading "they are red, not grey"
; Match "they are red, not grey" with construction "state verb adj" pattern 0, (is
;                                                                               are), 1, (, not
;                                                                                         not), 2
; Match "they" with ({Clyde} {George} {Tony})
; Match "they" with ({George} {Tony})
; Match "red" with {red thing}
; Match "grey" with {gray thing}
; Match "red" with {red thing}
; Match "grey" with {gray thing}
; Take result (({Is-A 0-100044} {Is-Not-A 0-100045})
;              ({Is-A 0-100046} {Is-Not-A 0-100047})
;              ({Is-A 0-100048} {Is-Not-A 0-100049}))
; System reading "Wesley and Yang are friends"
; Create new name {Yang}
; Match "Wesley and Yang are friends" with construction "state verb relation friend" pattern 0, (are), 1
; Match "Wesley and Yang" with construction "noun parallel structure" pattern 0, (and), 1
; Match "Wesley" with {Wesley}
; Match "Yang" with {Yang}
; Assume {Wesley} is a {person}
; Assume {Yang} is a {person}
; Match "Wesley and Yang" with ({Wesley} {Yang})
; Match "friends" with {friend of}
; Take result ({friend of 0-104429})
; System reading "Wesley is smart and he is a basketball player"
; Match "Wesley is smart and he is a basketball player" with construction "relation parallel structure" pattern 0, (and), 1
; Match "Wesley is smart" with construction "state verb adj" pattern 0, (is are), 1
; Match "Wesley" with {Wesley}
; Match "smart" with {smart thing}
; Match "Wesley is smart" with {Is-A 0-644515}
; Match "he is a basketball" with construction "create new is a" pattern 0, (is a
;                                                                            is an
;                                                                            is a kind of), 1
; Match "he" with {Wesley}
; Match "he" with {Yang}
; Match "basketball" with {basketball}
; Match "basketball" with {basketball}
; Match "he is a basketball" with construction "state verb indv" pattern 0, (is), 1
; Match "he" with {Wesley}
; Match "he" with {Yang}
; Match "a basketball" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "a basketball" with {basketball 0-658848}
; Match "a basketball" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "a basketball" with {basketball 0-658854}
; Match "he is a basketball player" with construction "create new is a" pattern 0, (is a
;                                                                                   is an
;                                                                                   is a kind of), 1
; Match "he" with {Wesley}
; Match "he" with {Yang}
; Match "basketball" with {basketball}
; Match "basketball player" with {basketball player}
; Match "basketball" with {basketball}
; Match "basketball player" with {basketball player}
; Match "he is a basketball player" with construction "state verb indv" pattern 0, (is), 1
; Match "he" with {Wesley}
; Match "he" with {Yang}
; Match "a basketball" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "a basketball" with {basketball 0-660222}
; Match "a basketball player" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "basketball player" with {basketball player}
; Match "a basketball player" with {basketball player 0-660230}
; Match "a basketball" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "a basketball" with {basketball 0-660252}
; Match "a basketball player" with construction "np new individual" pattern (a an), 0
; Match "basketball" with {basketball}
; Match "basketball player" with {basketball player}
; Match "a basketball player" with {basketball player 0-660260}
; Match "he is a basketball player" with {Is-A 0-660182}
; Match "he is a basketball player" with {Is-A 0-660183}
; Match "he is a basketball player" with {Eq 0-660404}
; Match "he is a basketball player" with {Eq 0-660405}
; Take result ({Is-A 0-644515} {Is-A 0-660182})
; System reading "he hates elephants"
; Match "he hates elephants" with construction "state hate" pattern 0, 1, 2
; Match "he" with {Wesley}
; Match "he" with {Yang}
; Match "hates" with {hate}
; Match "elephants" with {elephant}
; Match "hates" with {hate}
; Match "elephants" with {elephant}
; Take result {hate 0-854595}
; T