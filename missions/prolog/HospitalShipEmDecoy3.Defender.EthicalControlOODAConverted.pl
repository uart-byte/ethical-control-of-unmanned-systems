;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)


;Mission specification

;;;;;;;;;;;;;;;;;;;;;; Begin HospitalShipEmDecoy3.Defender.EthicalControlOODA.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

(<-- (execute_goal 'HSEMD.OODA.21) (command "Goal HSEMD.OODA.21: Attack Response Thresholds Set - Signal strength needed for close-proximity activation") (update_outcome) ; initial goal, by default
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.21 'HSEMD.OODA.22))
(<- (execute_goal 'HSEMD.OODA.21) (current_goal_outcome f) (change_goal 'HSEMD.OODA.21 'HSEMD.OODA.22))
(<- (execute_goal 'HSEMD.OODA.21) (current_goal_outcome x) (change_goal 'HSEMD.OODA.21 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.22) (command "Goal HSEMD.OODA.22: Enable Robot Swarm - Close-in weapon system activated") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.22 'HSEMD.OODA.23))
(<- (execute_goal 'HSEMD.OODA.22) (current_goal_outcome f) (change_goal 'HSEMD.OODA.22 'HSEMD.OODA.23))
(<- (execute_goal 'HSEMD.OODA.22) (current_goal_outcome x) (change_goal 'HSEMD.OODA.22 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.23) (command "Goal HSEMD.OODA.23: Threat Signals Received - Above response threshold") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.23 'HSEMD.OODA.24))
(<- (execute_goal 'HSEMD.OODA.23) (current_goal_outcome f) (change_goal 'HSEMD.OODA.23 'HSEMD.OODA.24))
(<- (execute_goal 'HSEMD.OODA.23) (current_goal_outcome x) (change_goal 'HSEMD.OODA.23 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.24) (command "Goal HSEMD.OODA.24: Move to Threat - Group response") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.24 'HSEMD.OODA.27))
(<- (execute_goal 'HSEMD.OODA.24) (current_goal_outcome f) (change_goal 'HSEMD.OODA.24 'HSEMD.OODA.27))
(<- (execute_goal 'HSEMD.OODA.24) (current_goal_outcome x) (change_goal 'HSEMD.OODA.24 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.25) (command "Goal HSEMD.OODA.25: IFFNU - Identify Friend Foe Neutral Unknown") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.25 'HSEMD.OODA.26))
(<- (execute_goal 'HSEMD.OODA.25) (current_goal_outcome f) (change_goal 'HSEMD.OODA.25 'HSEMD.OODA.26))
(<- (execute_goal 'HSEMD.OODA.25) (current_goal_outcome x) (change_goal 'HSEMD.OODA.25 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.26) (command "Goal HSEMD.OODA.26: Confirm In-Port Counterattack? Rapid-response human checkpoint") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.26 'HSEMD.OODA.27))
(<- (execute_goal 'HSEMD.OODA.26) (current_goal_outcome f) (change_goal 'HSEMD.OODA.26 'HSEMD.OODA.36))
(<- (execute_goal 'HSEMD.OODA.26) (current_goal_outcome x) (change_goal 'HSEMD.OODA.26 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.27) (command "Goal HSEMD.OODA.27: Robot Swarm versus Terrorists - Lethal force authorized") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.27 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.27) (current_goal_outcome f) (change_goal 'HSEMD.OODA.27 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.27) (current_goal_outcome x) (change_goal 'HSEMD.OODA.27 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.36) (command "Goal HSEMD.OODA.36: Hospital Ship Attack Denied - Lethal force NOT authorized") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.36 'HSEMD.OODA.37))
(<- (execute_goal 'HSEMD.OODA.36) (current_goal_outcome f) (change_goal 'HSEMD.OODA.36 'HSEMD.OODA.37))
(<- (execute_goal 'HSEMD.OODA.36) (current_goal_outcome x) (change_goal 'HSEMD.OODA.36 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.37) (command "Goal HSEMD.OODA.37: Search for Intruders - All defense forces alerted") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.37 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.37) (current_goal_outcome f) (change_goal 'HSEMD.OODA.37 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.37) (current_goal_outcome x) (change_goal 'HSEMD.OODA.37 'HSEMD.OODA.99.0))
(<- (execute_goal 'HSEMD.OODA.99.0) (command "Goal HSEMD.OODA.99.0: Proceed to recovery - Mission complete, prepare for pickup. Terminal condition.") (update_outcome)
                           (current_goal_outcome s) (change_goal 'HSEMD.OODA.99.0 'mission_complete))
(<- (execute_goal 'HSEMD.OODA.99.0) (current_goal_outcome f) (change_goal 'HSEMD.OODA.99.0 'mission_abort))
(<- (execute_goal 'HSEMD.OODA.99.0) (current_goal_outcome x) (change_goal 'HSEMD.OODA.99.0 'mission_abort))

;;;;;;;;;;;;;;;;;;;;;; End HospitalShipEmDecoy3.Defender.EthicalControlOODA.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

;Facts
(<-- (current_goal 'HSEMD.OODA.21)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal 'HSEMD.OODA.21) (asserta ((current_goal 'HSEMD.OODA.21)))) ; initialGoal
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
