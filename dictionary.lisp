;;; ***************************************************************************
;;; A sample dictionary where new scone elements are defined for the
;;; purpose of the Scone construction grammar engine.  
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

(new-type {apple} {natural object} :english '("apples"))
(new-type {banana} {natural object} :english '("bananas"))
(new-type {leaf} {natural object} :english '("leaves"))
(new-type {basketball} {man-made object} :english '("basketballs"))

(new-type {tool} {man-made object})
(new-type {telescope} {tool} :english '("telescopes"))

(new-type {cloth} {physical object})
(new-type {pajama} {cloth} :english '("pajamas"))

(new-type {mouse} {animal} :english '("mouse" "mice"))
(new-type {computer mouse} {man-made object} :english '("mouse" "mice"))

(new-type {technology company} {company})
(new-type-role {founder} {organization} {person} :english '("founders"))

(english {mammal} :iname "mammals")
(english {elephant} :iname "elephants")
(english {dog} :iname "dogs")

;;; ------------------------------------------------------------------------
;;; noun count

(new-type-role {count} {tangible} {number})

(new-type {integer range} {inexact number})
(new-type-role {lower bound} {integer range} {integer})
(new-type-role {upper bound} {integer range} {integer})

(defun new-integer-range (name lowerbound upperbound)
	"takes in the name, lowerbound and upperbound and return an
	integer range node"
	(let ((rangeNode (new-indv name {integer range})))
		(if (not (null lowerbound)) (x-is-the-y-of-z lowerbound {lower bound} rangeNode))
  	 	(if (not (null upperbound)) (x-is-the-y-of-z upperbound {upper bound} rangeNode))
	 	rangeNode))

(new-integer-range "some" 2 NIL)

(defun new-string-number (name number-node)
	"the function takes in a string name and a number node
	then creates a new number node with that name."
	(let ((new-node (new-indv name {exact number})))
		(new-eq new-node number-node)
		new-node))

(new-string-number "one" {1})
(new-string-number "two" {2})
(new-string-number "three" {3})
(new-string-number "four" {4})
(new-string-number "a dozen of" {12})

;;; ------------------------------------------------------------------------
;;; action verb

(new-type {transitive action} {action})
(new-type {transitive action with recipient} {transitive action})
(new-type {intransitive action} {action})

(new-indv {kick} {transitive action} :english '(:verb "kicked" "kicks" "kicking"))
(new-indv {hit} {transitive action} :english '(:verb "hits" "hitting"))
(new-indv {eat} {transitive action} :english '(:verb "eats" "ate" "eating"))
(new-indv {make} {transitive action} :english '(:verb "makes" "made" "making"))

(new-indv {give} {transitive action with recipient}
	:english '(:verb "gives" "gave" "giving"))

(new-indv {arrive} {intransitive action} :english 
	'(:verb "arrives" "arrived" "arriving"))
(new-indv {sit} {intransitive action} :english '(:verb "sits" "sat" "sitting"))
(new-indv {leave} {intransitive action} :english '(:verb "leaves" "left" "leaving"))

;;; ------------------------------------------------------------------------
;;; state verb

(new-relation {hate}
	      :a-inst-of {animal}
	      :b-inst-of {thing}
	      :english '(:verb "hates" "hated"))

(new-relation {believe}
		  :a-inst-of {animal}
		  :b-inst-of {animal}
		  :english '(:verb "believes" "believed"))

;;; ------------------------------------------------------------------------
;;; Auxiliary verb

(setq auxiliary-verb-dict (list "is" "are"))

(new-type-role {teammate} {person} {person} :english '("teammate" "teammates"))
(new-type-role {roommate} {person} {person} :english '("roommate" "roommates"))
(new-type-role {friend} {person} {person} :english '("friend" "friends"))

(new-type {basketball player} {person}
		  :english '("basketball player" "basketball players"))

;;; ------------------------------------------------------------------------
;;; Adj

(new-type {smart thing} {thing} :english '(:no-iname :adj "smart"))
(new-type {tall animal} {animal} :english '(:no-iname :adj "tall"))

;;; Color

(new-type {color} {tangible})
(new-type {colored thing} {tangible} :english '(:no-iname :adj "colored"))
(new-type-role {predominant color} {colored thing} {color} :english '("color"))

(new-type {red thing} {colored thing} :english '(:no-iname :adj "red"))
(new-indv {red} {color})
(x-is-the-y-of-z {red} {predominant color} {red thing})

(new-type {gray thing} {colored thing} :english '(:no-iname :adj "gray" "grey"))
(new-indv {gray} {color})
(x-is-the-y-of-z {gray} {predominant color} {gray thing})

;;; Country

(new-type-role {belonged country} {thing} {country} :english '("country"))

(new-indv {United States} {country with states}
	  :english '("United States"
		     "United States of America"
		     "USA"
		     "America"))
(new-type {American thing} {thing} :english '(:no-iname :adj "american" "American"))
(x-is-the-y-of-z {United States} {belonged country} {american thing})
(new-indv {California} {land area})
(x-is-a-y-of-z {California} {state} {United States})

(new-indv {China} {country with provinces})
(new-type {Chinese thing} {thing} :english '(:no-iname :adj "chinese" "Chinese"))
(x-is-the-y-of-z {China} {belonged country} {Chinese thing})

;;; ------------------------------------------------------------------------
;;; prepositional phrase

(new-type-role {action tool} {action} {physical object})
(new-type-role {person outwear} {person} {cloth})

(new-type-role {based location} {organization} {place})

(new-indv {Shanghai} {city})
(new-type {park} {place})
(new-type {hammer} {tool})
(new-indv {now} {time reference})
 
(setq vp-prepositional-phrase
      '(("at" {event location} {event time})
        ("in" {event location} {event time})
        ("by" {action tool})
        ("to" {action recipient})))

(setq np-prepositional-phrase
      '(("in" {person outwear})))
