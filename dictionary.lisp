;;; ***************************************************************************
;;; A sample dictionary where new scone elements are defined for the
;;; purpose of the Scone construction grammar engine.  
;;;
;;; Author: Yang Yang
;;; ***************************************************************************

;;; names
(new-indv {Yang} {person})
(new-indv {Wesley} {person})
(new-indv {Clara} {person})
(new-indv {Tony} {thing})
(new-indv {Clyde} {thing})

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

(defun new-string-number (name number_node)
	""
	(let ((new_node (new-indv name {exact number})))
		(new-eq new_node number_node)
		new_node))

(new-string-number "one" {1})
(new-string-number "two" {2})
(new-string-number "a dozon of" {12})

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

(new-indv {arrive} {intransitive active} :english 
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
(new-relation {teammate of}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :symmetric t
	      :english '("teammate" "teammates"))

(new-type {basketball player} {person}
		  :english '("basketball player" "basketball players"))

(new-type {red thing} {thing} :english '(:no-iname :adj "red"))
(new-type {smart thing} {thing} :english '(:no-iname :adj "smart"))
(new-type {tall animal} {animal} :english '(:no-iname :adj "tall"))

;;; ------------------------------------------------------------------------
;;, prepositional phrase

(new-type-role {action tool} {action} {physical object})
(new-type-role {person outwear} {person} {cloth})

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
