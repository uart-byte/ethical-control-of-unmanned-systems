;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: 23 September 2019.

(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog) ;Start Prolog.

;Mission specification, expanding phase 1 to phases 1.1, 1.2 and 1.3

(<-- (execute_phase 1.1) (command "Phase 1.1: Choose Tube and Launch") (update_outcome)
                        (current_phase_outcome s) (change_phase 1.1 1.2))
(<- (execute_phase 1.1) (current_phase_outcome f) (change_phase 1.1 'mission_abort) (report "Launch failure"))
(<- (execute_phase 1.1) (current_phase_outcome x) (change_phase 1.1 'mission_abort) (report "Launch abort"))

(<- (execute_phase 1.2) (command "Phase 1.2: Enter Water and Get GPS Fix") (update_outcome)
                        (current_phase_outcome s) (change_phase 1.2 1.3))
(<- (execute_phase 1.2) (current_phase_outcome f) (change_phase 1.2 'mission_abort) (report "Phase 1.2 report: No GPS"))
(<- (execute_phase 1.2) (current_phase_outcome x) (change_phase 1.2 'mission_abort) (report "Phase 1.2 report: GPS aborted"))

(<- (execute_phase 1.3) (command "Phase 1.3: Descend to Search Depth") (update_outcome)
                        (current_phase_outcome s) (change_phase 1.3 2))
(<- (execute_phase 1.3) (current_phase_outcome f) (change_phase 1.3 'mission_abort) (report "Phase 1.3 report: Descent failed"))
(<- (execute_phase 1.3) (current_phase_outcome x) (change_phase 1.3 'mission_abort) (report "Phase 1.3 report: Descent failed"))

(<- (execute_phase 2) (command "Phase 2: Search for Sailor") (update_outcome)
                      (current_phase_outcome s) (change_phase 2 3))
(<- (execute_phase 2) (current_phase_outcome f) (change_phase 2 4))
(<- (execute_phase 2) (current_phase_outcome x) (change_phase 2 4))

(<- (execute_phase 3) (command "Phase 3: Track Sailor Afloat Until Safe") (update_outcome)
                      (current_phase_outcome s) (change_phase 3 4))
(<- (execute_phase 3) (current_phase_outcome f) (change_phase 3 4))
(<- (execute_phase 3) (current_phase_outcome x) (change_phase 3 4))

(<- (execute_phase 4) (command "Phase 4: Proceed to recovery") (update_outcome)
                      (current_phase_outcome s) (change_phase 4 'mission_complete) (report "Phase 4 report: Vehicle recovered"))
(<- (execute_phase 4) (current_phase_outcome f) (change_phase 4 'mission_abort)    (report "Phase 4 report: Vehicle lost"))
(<- (execute_phase 4) (current_phase_outcome x) (change_phase 4 'mission_abort)    (report "Phase 4 report: Awaiting orders"))


;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: September 22, 2019.

;Allegro Prolog uses Lisp syntax. Rule head is first expression following "<--" symbol. Rule
;body is rest of expressions. Subsequent definitions of rule use "<-" symbol to avoid overwrite.
;Code filed in Allegro Editor as "2019 Prolog MEE".

;After this code has been entered into Allegro Editor window and compiled, it is necessary to
;similarly enter and compile a mission. File "2019 Man Overboard Mission Orders" stored in
;Allegro Editor files provides an example. Entering "(run)" to Lisp prompt then runs both files together.

;Facts
(<-- (current_phase 0)) ;Starting phase.
(<-- (current_phase_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_phase) (done) !)
(<-- (initialize_mission) (abolish current_phase 1) (asserta ((current_phase 1.1))))
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
