;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)


;Mission specification

;;;;;;;;;;;;;;;;;;;;;; Begin HospitalShipEmDecoy1.Opponent.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

(<-- (execute_goal 'HSEMD.Foe.1) (command "Goal HSEMD.Foe.1: Search, Observe - Find ship, surveil for weaknesses") (update_outcome) ; initial goal, by default
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.1 'HSEMD.Foe.2))
(<- (execute_goal 'HSEMD.Foe.1) (current_goal_outcome f) (change_goal 'HSEMD.Foe.1 'HSEMD.Foe.2))
(<- (execute_goal 'HSEMD.Foe.1) (current_goal_outcome x) (change_goal 'HSEMD.Foe.1 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.2) (command "Goal HSEMD.Foe.2: Assess, approach - Surreptitious entry, harbor or anchorage") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.2 'HSEMD.Foe.3))
(<- (execute_goal 'HSEMD.Foe.2) (current_goal_outcome f) (change_goal 'HSEMD.Foe.2 'HSEMD.Foe.3))
(<- (execute_goal 'HSEMD.Foe.2) (current_goal_outcome x) (change_goal 'HSEMD.Foe.2 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.3) (command "Goal HSEMD.Foe.3: Covertly Board - EM spoofing devices attached to topside") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.3 'HSEMD.Foe.4))
(<- (execute_goal 'HSEMD.Foe.3) (current_goal_outcome f) (change_goal 'HSEMD.Foe.3 'HSEMD.Foe.4))
(<- (execute_goal 'HSEMD.Foe.3) (current_goal_outcome x) (change_goal 'HSEMD.Foe.3 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.4) (command "Goal HSEMD.Foe.4: Standoff, Observe - Fall back to safe vantage point") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.4 'HSEMD.Foe.5))
(<- (execute_goal 'HSEMD.Foe.4) (current_goal_outcome f) (change_goal 'HSEMD.Foe.4 'HSEMD.Foe.5))
(<- (execute_goal 'HSEMD.Foe.4) (current_goal_outcome x) (change_goal 'HSEMD.Foe.4 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.5) (command "Goal HSEMD.Foe.5: Initiate Fake Attack - Light off false EM spoofing signals") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.5 'HSEMD.Foe.6))
(<- (execute_goal 'HSEMD.Foe.5) (current_goal_outcome f) (change_goal 'HSEMD.Foe.5 'HSEMD.Foe.6))
(<- (execute_goal 'HSEMD.Foe.5) (current_goal_outcome x) (change_goal 'HSEMD.Foe.5 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.6) (command "Goal HSEMD.Foe.6: Observe Reaction - Monitor response, assess damage") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.6 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.6) (current_goal_outcome f) (change_goal 'HSEMD.Foe.6 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.6) (current_goal_outcome x) (change_goal 'HSEMD.Foe.6 'HSEMD.Foe.7))
(<- (execute_goal 'HSEMD.Foe.7) (command "Goal HSEMD.Foe.7: Evade, Escape - Avoid detection, depart locale - Terminal condition ") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.Foe.7 'mission_complete))
(<- (execute_goal 'HSEMD.Foe.7) (current_goal_outcome f) (change_goal 'HSEMD.Foe.7 'mission_abort))
(<- (execute_goal 'HSEMD.Foe.7) (current_goal_outcome x) (change_goal 'HSEMD.Foe.7 'mission_abort))

;;;;;;;;;;;;;;;;;;;;;; End HospitalShipEmDecoy1.Opponent.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

;Facts
(<-- (current_goal 'HSEMD.Foe.1)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal 'HSEMD.Foe.1) (asserta ((current_goal 'HSEMD.Foe.1)))) ; initialGoal
(<-- (execute_current_goal) (current_goal ?x) (execute_goal ?x) !)
(<-- (done) (current_goal 'mission_complete))
(<- (done) (current_goal 'mission_abort))

; Human external agent communication functions

(<-- (negative nil)) (<- (negative n))
(<-- (affirmative ?x) (not (negative ?x)))
(<-- (report ?C) (princ ?C) (princ ".") (nl))
(<-- (command ?C) (princ ?C) (princ "!") (nl))
(<-- (ask ?Q ?A) (princ ?Q) (princ "?") (read ?A))
(<-- (ask_outcome ?A) (ask "Did goal succeed (s), fail (f), or abort (x)" ?A))

; Utility functions
(<-- (change_goal ?old ?new) (retract ((current_goal ?old))) (asserta ((current_goal ?new))))
(<-- (update_outcome) (ask_outcome ?A) (abolish current_goal_outcome 1) (asserta ((current_goal_outcome ?A))))

; Test functions (illustrate format for calling predicates from Lisp)
(defun run () (?- (execute_mission)))
(defun update () (?- (update_outcome)))
(defun mission-goal () (?- (current_goal ?X)))
(defun outcome () (?- (current_goal_outcome ?X)))
