;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)


;Mission specification

;;;;;;;;;;;;;;;;;;;;;; Begin HospitalShipEmDecoy2.Defender.SenseDecideAct.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

(<-- (execute_goal 'HSEMD.reflex.11) (command "Goal HSEMD.reflex.11: Attack Response Thresholds Set - Signal strength needed for close-proximity activation") (update_outcome) ; initial goal, by default
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.11 'HSEMD.reflex.12))
(<- (execute_goal 'HSEMD.reflex.11) (current_goal_outcome f) (change_goal 'HSEMD.reflex.11 'HSEMD.reflex.12))
(<- (execute_goal 'HSEMD.reflex.11) (current_goal_outcome x) (change_goal 'HSEMD.reflex.11 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.12) (command "Goal HSEMD.reflex.12: Enable Robot Swarm - Close-in weapon system activated") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.12 'HSEMD.reflex.13))
(<- (execute_goal 'HSEMD.reflex.12) (current_goal_outcome f) (change_goal 'HSEMD.reflex.12 'HSEMD.reflex.13))
(<- (execute_goal 'HSEMD.reflex.12) (current_goal_outcome x) (change_goal 'HSEMD.reflex.12 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.13) (command "Goal HSEMD.reflex.13: Threat Signals Received - Above response threshold") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.13 'HSEMD.reflex.14))
(<- (execute_goal 'HSEMD.reflex.13) (current_goal_outcome f) (change_goal 'HSEMD.reflex.13 'HSEMD.reflex.14))
(<- (execute_goal 'HSEMD.reflex.13) (current_goal_outcome x) (change_goal 'HSEMD.reflex.13 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.14) (command "Goal HSEMD.reflex.14: Move to Threat - Group response") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.14 'HSEMD.reflex.17))
(<- (execute_goal 'HSEMD.reflex.14) (current_goal_outcome f) (change_goal 'HSEMD.reflex.14 'HSEMD.reflex.17))
(<- (execute_goal 'HSEMD.reflex.14) (current_goal_outcome x) (change_goal 'HSEMD.reflex.14 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.17) (command "Goal HSEMD.reflex.17: Robot Swarm Counterattack - Lethal force authorized") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.17 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.17) (current_goal_outcome f) (change_goal 'HSEMD.reflex.17 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.17) (current_goal_outcome x) (change_goal 'HSEMD.reflex.17 'HSEMD.reflex.99.0))
(<- (execute_goal 'HSEMD.reflex.99.0) (command "Goal HSEMD.reflex.99.0: Proceed to recovery - Mission complete, prepare for pickup. Terminal condition.") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.reflex.99.0 'mission_complete))
(<- (execute_goal 'HSEMD.reflex.99.0) (current_goal_outcome f) (change_goal 'HSEMD.reflex.99.0 'mission_abort))
(<- (execute_goal 'HSEMD.reflex.99.0) (current_goal_outcome x) (change_goal 'HSEMD.reflex.99.0 'mission_abort))

;;;;;;;;;;;;;;;;;;;;;; End HospitalShipEmDecoy2.Defender.SenseDecideAct.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

;Facts
(<-- (current_goal 'HSEMD.reflex.11)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal 'HSEMD.reflex.11) (asserta ((current_goal 'HSEMD.reflex.11)))) ; initialGoal
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
