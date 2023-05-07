;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: September 11, 2019.

;Allegro Prolog uses Lisp syntax. Rule head is first expression following "<--" symbol. Rule
;body is rest of expressions. Subsequent definitions of rule use "<-" symbol to avoid overwrite.
;Code filed in Allegro Editor as "2019 Prolog MEE".

;After this code has been entered into Allegro Editor window and compiled, it is necessary to
;similarly enter and compile a mission. File "2019 Man Overboard Mission Orders" stored in
;Allegro Editor files provides an example. Entering "(run)" to Lisp prompt then runs both files together.

;Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)

;Facts
(<-- (current_phase 0)) ;Starting phase.
(<-- (current_phase_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_phase) (done) !)
(<-- (initialize_mission) (abolish current_phase 1) (asserta ((current_phase 1))))
(<-- (execute_current_phase) (current_phase ?x) (execute_phase ?x) !)
(<-- (done) (current_phase 'mission_complete))
(<- (done) (current_phase 'mission_abort))

;Human external agent communication functions

(<-- (negative nil)) (<- (negative n))
(<-- (affirmative ?x) (not (negative ?x)))
(<-- (report ?C) (princ ?C) (princ ".") (nl))
(<-- (command ?C) (princ ?C) (princ "!") (nl))
(<-- (ask ?Q ?A) (princ ?Q) (princ "?") (read ?A))
(<-- (ask_outcome ?A) (ask "Did goal succeed (s), fail (f), or abort (x)" ?A))

;Utility functions
(<-- (change_phase ?old ?new) (retract ((current_phase ?old))) (asserta ((current_phase ?new))))
(<-- (update_outcome) (ask_outcome ?A) (abolish current_phase_outcome 1) (asserta ((current_phase_outcome ?A))))


;Test functions (illustrate format for calling predicates from Lisp)
(defun run () (?- (execute_mission)))
(defun update () (?- (update_outcome)))
(defun mission-phase () (?- (current_phase ?X)))
(defun outcome () (?- (current_phase_outcome ?X)))
