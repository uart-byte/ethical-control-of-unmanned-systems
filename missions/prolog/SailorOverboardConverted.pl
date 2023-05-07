;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)


;Mission specification

;;;;;;;;;;;;;;;;;;;;;; Begin SailorOverboard.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

(<-- (execute_goal 'Goal1) (command "Goal1: Deploy, Launch - Sailor Overboard Immediate Action") (update_outcome) ; initial goal, by default
                           (current_goal_outcome s) (change_goal 'Goal1 'Goal2))
(<- (execute_goal 'Goal1) (current_goal_outcome f) (change_goal 'Goal1 'Goal7))
(<- (execute_goal 'Goal1) (current_goal_outcome x) (change_goal 'Goal1 'Goal8))
(<- (execute_goal 'Goal2) (command "Goal2: Rendezvous with Sailor - Go directly to best known location") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal2 'Goal4))
(<- (execute_goal 'Goal2) (current_goal_outcome f) (change_goal 'Goal2 'Goal3))
(<- (execute_goal 'Goal2) (current_goal_outcome x) (change_goal 'Goal2 'Goal5))
(<- (execute_goal 'Goal3) (command "Goal3: Search for Sailor  - Sailor position not known, intermittent") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal3 'Goal4))
(<- (execute_goal 'Goal3) (current_goal_outcome f) (change_goal 'Goal3 'Goal5))
(<- (execute_goal 'Goal3) (current_goal_outcome x) (change_goal 'Goal3 'Goal5))
(<- (execute_goal 'Goal4) (command "Goal4: Track Sailor afloat until safe - Watch closely, not to interfere with rescue operations") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal4 'Goal5))
(<- (execute_goal 'Goal4) (current_goal_outcome f) (change_goal 'Goal4 'Goal5))
(<- (execute_goal 'Goal4) (current_goal_outcome x) (change_goal 'Goal4 'Goal5))
(<- (execute_goal 'Goal5) (command "Goal5: Proceed to Recovery - Mission complete, prepare for pickup") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal5 'Goal6))
(<- (execute_goal 'Goal5) (current_goal_outcome f) (change_goal 'Goal5 'Goal7))
(<- (execute_goal 'Goal5) (current_goal_outcome x) (change_goal 'Goal5 'Goal8))
(<- (execute_goal 'Goal6) (command "Goal6: Halt and prepare for recovery - Operations complete, final success state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal6 'mission_complete))
(<- (execute_goal 'Goal6) (current_goal_outcome f) (change_goal 'Goal6 'mission_abort))
(<- (execute_goal 'Goal6) (current_goal_outcome x) (change_goal 'Goal6 'mission_abort))
(<- (execute_goal 'Goal7) (command "Goal7: Halt and deploy recovery beacon - Unable to continue, final failure state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal7 'mission_complete))
(<- (execute_goal 'Goal7) (current_goal_outcome f) (change_goal 'Goal7 'mission_abort))
(<- (execute_goal 'Goal7) (current_goal_outcome x) (change_goal 'Goal7 'mission_abort))
(<- (execute_goal 'Goal8) (command "Goal8: Halt and await further orders - Unexpected problem, final exception state") (update_outcome)
                           (current_goal_outcome s) (change_goal 'Goal8 'mission_complete))
(<- (execute_goal 'Goal8) (current_goal_outcome f) (change_goal 'Goal8 'mission_abort))
(<- (execute_goal 'Goal8) (current_goal_outcome x) (change_goal 'Goal8 'mission_abort))

;;;;;;;;;;;;;;;;;;;;;; End SailorOverboard.xml Mission Orders ;;;;;;;;;;;;;;;;;;;;;;

;Facts
(<-- (current_goal 'Goal1)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal 'Goal1) (asserta ((current_goal 'Goal1)))) ; initialGoal
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
