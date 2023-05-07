;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)


;Mission specification

;;;;;;;;;;;;;;;;;;;;;; Begin LifeboatTracking.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

(<-- (execute_goal 'LBT1.0) (command "Goal LBT1.0: Deploy, Launch - Commit to robot support") (update_outcome) ; initial goal, by default
                           (current_goal_outcome s) (change_goal 'LBT1.0 'LBT2.0))
(<- (execute_goal 'LBT1.0) (current_goal_outcome f) (change_goal 'LBT1.0 'LBT99.0))
(<- (execute_goal 'LBT1.0) (current_goal_outcome x) (change_goal 'LBT1.0 'LBT99.0))
(<- (execute_goal 'LBT2.0) (command "Goal LBT2.0: Transit to search area - Proceed to estimated position") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT2.0 'LBT3.0))
(<- (execute_goal 'LBT2.0) (current_goal_outcome f) (change_goal 'LBT2.0 'LBT99.0))
(<- (execute_goal 'LBT2.0) (current_goal_outcome x) (change_goal 'LBT2.0 'LBT99.0))
(<- (execute_goal 'LBT3.0) (command "Goal LBT3.0: Locate Lifeboat - Follow best search pattern") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT3.0 'LBT3.1))
(<- (execute_goal 'LBT3.0) (current_goal_outcome f) (change_goal 'LBT3.0 'LBT2.0))
(<- (execute_goal 'LBT3.0) (current_goal_outcome x) (change_goal 'LBT3.0 'LBT99.0))
(<- (execute_goal 'LBT3.1) (command "Goal LBT3.1: Report position - Alerts updated") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT3.1 'LBT3.2))
(<- (execute_goal 'LBT3.1) (current_goal_outcome f) (change_goal 'LBT3.1 'LBT4.0))
(<- (execute_goal 'LBT3.1) (current_goal_outcome x) (change_goal 'LBT3.1 'LBT99.0))
(<- (execute_goal 'LBT3.2) (command "Goal LBT3.2: Mark with Beacon - Monitor wind effects and ocean current") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT3.2 'LBT4.0))
(<- (execute_goal 'LBT3.2) (current_goal_outcome f) (change_goal 'LBT3.2 'LBT4.0))
(<- (execute_goal 'LBT3.2) (current_goal_outcome x) (change_goal 'LBT3.2 'LBT99.0))
(<- (execute_goal 'LBT4.0) (command "Goal LBT4.0: Track Lifeboat - Monitor and communicate") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT4.0 'LBT4.1))
(<- (execute_goal 'LBT4.0) (current_goal_outcome f) (change_goal 'LBT4.0 'LBT4.1))
(<- (execute_goal 'LBT4.0) (current_goal_outcome x) (change_goal 'LBT4.0 'LBT99.0))
(<- (execute_goal 'LBT4.1) (command "Goal LBT4.1: Maintain proximity - Overhead or afloat nearby") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT4.1 'LBT4.2))
(<- (execute_goal 'LBT4.1) (current_goal_outcome f) (change_goal 'LBT4.1 'LBT4.2))
(<- (execute_goal 'LBT4.1) (current_goal_outcome x) (change_goal 'LBT4.1 'LBT99.0))
(<- (execute_goal 'LBT4.2) (command "Goal LBT4.2: Periodic reports - Popup or float to report, also recharge") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT4.2 'LBT4.3))
(<- (execute_goal 'LBT4.2) (current_goal_outcome f) (change_goal 'LBT4.2 'LBT4.3))
(<- (execute_goal 'LBT4.2) (current_goal_outcome x) (change_goal 'LBT4.2 'LBT99.0))
(<- (execute_goal 'LBT4.3) (command "Goal LBT4.3: Continue - Repeat until conditions change") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT4.3 'LBT5.0))
(<- (execute_goal 'LBT4.3) (current_goal_outcome f) (change_goal 'LBT4.3 'LBT7.0))
(<- (execute_goal 'LBT4.3) (current_goal_outcome x) (change_goal 'LBT4.3 'LBT99.0))
(<- (execute_goal 'LBT5.0) (command "Goal LBT5.0: Check relieved by other asset - Task update received?") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT5.0 'LBT99.0))
(<- (execute_goal 'LBT5.0) (current_goal_outcome f) (change_goal 'LBT5.0 'LBT4.0))
(<- (execute_goal 'LBT5.0) (current_goal_outcome x) (change_goal 'LBT5.0 'LBT99.0))
(<- (execute_goal 'LBT6.0) (command "Goal LBT6.0: Low Fuel - Make best effort possible") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT6.0 'LBT6.1))
(<- (execute_goal 'LBT6.0) (current_goal_outcome f) (change_goal 'LBT6.0 'LBT6.1))
(<- (execute_goal 'LBT6.0) (current_goal_outcome x) (change_goal 'LBT6.0 'LBT99.0))
(<- (execute_goal 'LBT6.1) (command "Goal LBT6.1: Remain with lifeboat? Choices: land on boat, attach to boat, or adrift nearby") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT6.1 'LBT6.2))
(<- (execute_goal 'LBT6.1) (current_goal_outcome f) (change_goal 'LBT6.1 'LBT6.2))
(<- (execute_goal 'LBT6.1) (current_goal_outcome x) (change_goal 'LBT6.1 'LBT99.0))
(<- (execute_goal 'LBT6.2) (command "Goal LBT6.2: Beacon? While power remains") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT6.2 'LBT99.0))
(<- (execute_goal 'LBT6.2) (current_goal_outcome f) (change_goal 'LBT6.2 'LBT99.0))
(<- (execute_goal 'LBT6.2) (current_goal_outcome x) (change_goal 'LBT6.2 'LBT99.0))
(<- (execute_goal 'LBT7.0) (command "Goal LBT7.0: Request Guidance? Need updated position") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT7.0 'LBT2.0))
(<- (execute_goal 'LBT7.0) (current_goal_outcome f) (change_goal 'LBT7.0 'LBT99.0))
(<- (execute_goal 'LBT7.0) (current_goal_outcome x) (change_goal 'LBT7.0 'LBT99.0))
(<- (execute_goal 'LBT99.0) (command "Goal LBT99.0: Proceed to recovery - Mission complete, prepare for pickup") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT99.0 'LBT99.1))
(<- (execute_goal 'LBT99.0) (current_goal_outcome f) (change_goal 'LBT99.0 'LBT99.2))
(<- (execute_goal 'LBT99.0) (current_goal_outcome x) (change_goal 'LBT99.0 'LBT99.3))
(<- (execute_goal 'LBT99.1) (command "Goal LBT99.1: Halt and prepare for recovery - Operations completed, final success state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT99.1 'mission_complete))
(<- (execute_goal 'LBT99.1) (current_goal_outcome f) (change_goal 'LBT99.1 'mission_abort))
(<- (execute_goal 'LBT99.1) (current_goal_outcome x) (change_goal 'LBT99.1 'mission_abort))
(<- (execute_goal 'LBT99.2) (command "Goal LBT99.2: Halt and deploy recovery beacon - Unable to operate, final failure state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT99.2 'mission_complete))
(<- (execute_goal 'LBT99.2) (current_goal_outcome f) (change_goal 'LBT99.2 'mission_abort))
(<- (execute_goal 'LBT99.2) (current_goal_outcome x) (change_goal 'LBT99.2 'mission_abort))
(<- (execute_goal 'LBT99.3) (command "Goal LBT99.3: Halt and await further orders - Unplanned failure, final exception state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'LBT99.3 'mission_complete))
(<- (execute_goal 'LBT99.3) (current_goal_outcome f) (change_goal 'LBT99.3 'mission_abort))
(<- (execute_goal 'LBT99.3) (current_goal_outcome x) (change_goal 'LBT99.3 'mission_abort))

;;;;;;;;;;;;;;;;;;;;;; End LifeboatTracking.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

;Facts
(<-- (current_goal 'LBT1.0)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal 'LBT1.0) (asserta ((current_goal 'LBT1.0)))) ; initialGoal
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
