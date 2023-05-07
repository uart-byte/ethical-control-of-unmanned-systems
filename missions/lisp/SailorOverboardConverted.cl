;This code written in ANSI Common Lisp
;
;Original design pattern:
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.
            
; Generator: AvclToLisp.xslt

;This code facilitates human-robot cooperation and involves three entities. At
;the top level, the "mission commander" is always human, at least at this point in the
;evolution of RBM software. The "mission controller" is usually assumed to be electronic,
;but could be a human "executive officer" in the case of a manned vehicle. The mission
;agent is always a vehicle. If the vehicle has an onboard computer incorporating the
;mission controller, then the vehicle is a robot. If not, it is an "ROV" or a "drone".

;;;;;;;;;;;;;;;;;;;;;;;;;Begin Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;

(defclass mission-goal ()
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-goal).

(defclass mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (current-execution-goal :accessor current-execution-goal :initform 'goalGoal1)
   (successor-list-index :accessor successor-list-index :initform 0)))

(defmethod initialize-goal ((goal mission-goal) new-command new-successor-list)
  (setf (command goal) new-command
    (successor-list goal) new-successor-list))

(defmethod issue-command ((MEE mission-execution-engine))
  (let* ((goal (current-execution-goal MEE))
         (new-command (command (eval goal))))
    (issue-order new-command)))

(defmethod ask-result ((MEE mission-execution-engine))
  (let* ((result (ask-outcome)))
    (setf (external-agent-response MEE) result)))

(defmethod set-next-goal ((MEE mission-execution-engine))
  (let* ((goal (current-execution-goal MEE))
         (new-successor-list (successor-list (eval goal)))
         (new-index (successor-list-index MEE))
         (next-goal (second (nth new-index new-successor-list))))
    (setf (current-execution-goal MEE) next-goal)))

(defmethod set-successor-list-index ((MEE mission-execution-engine))
  (let* ((index (convert-outcome-to-index (external-agent-response MEE))))
    (setf (successor-list-index MEE) index)))

(defvar mission-controller (make-instance 'mission-execution-engine)) ; instantiate the class as an object

; discussion for future work: naming conventions

(defun execute-goal ()
  (issue-command mission-controller)
  (ask-result mission-controller)
  (set-successor-list-index mission-controller)
  (set-next-goal mission-controller))

(defun execute-terminal-goal ()
  (issue-command mission-controller))

(defun create-mission-orders ()
  (initialize-mission)
  (setf (current-execution-goal mission-controller) 'goalGoal1))

(defun issue-order (command)
  (format t "~A" command)
  (terpri)) ; terminate printing, newline

(defun ask-outcome ()
  (format t "... determine goal execution: success (s), failure (f), or exception (x)? ")
  (terpri)
  (let ((value (read)))
       (if (not(member value '(s f x)))
           (format t " ? unknown response, please retry" (terpri)))
       (if (member value '(s f x))
           value
           (ask-outcome))))

(defun run ()
  (create-mission-orders)
  (execute-mission))

; execute starting at a new goal, also enables testing and refinement resuming in the middle of a mission
(defun rerun (new-start-goal)
  (setf (current-execution-goal mission-controller) new-start-goal)
  (execute-mission))

(defun convert-outcome-to-index (r)
  (cond ((equal r 's) (format t " =success")   (terpri) 0)
        ((equal r 'f) (format t " =failure")   (terpri) 1)
        ((equal r 'x) (format t " =exception") (terpri) 2)))

(defvar terminal-goal-list nil)

(defun execute-mission ()
  (cond ((member (current-execution-goal mission-controller) terminal-goal-list) (execute-terminal-goal))
        (t (execute-goal)(execute-mission))))

;;;;;;;;;;;;;;;;;;;;;;;;;End Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;Begin SailorOverboard.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar goalGoal1 (make-instance 'mission-goal)) ; initial goal, by default
  (defvar goalGoal2 (make-instance 'mission-goal))
  (defvar goalGoal3 (make-instance 'mission-goal))
  (defvar goalGoal4 (make-instance 'mission-goal))
  (defvar goalGoal5 (make-instance 'mission-goal))
  (defvar goalGoal6 (make-instance 'mission-goal))
  (defvar goalGoal7 (make-instance 'mission-goal))
  (defvar goalGoal8 (make-instance 'mission-goal))

(defun initialize-mission ()
  (setf terminal-goal-list '(goalGoal6 goalGoal7 goalGoal8))

  (initialize-goal goalGoal1 "Goal1: Deploy, Launch - Sailor Overboard Immediate Action"
                    '(("Success." goalGoal2) ("Failed." goalGoal7) ("Exception." goalGoal8)))

  (initialize-goal goalGoal2 "Goal2: Rendezvous with Sailor - Go directly to best known location"
                    '(("Success." goalGoal4) ("Failed." goalGoal3) ("Exception." goalGoal5)))

  (initialize-goal goalGoal3 "Goal3: Search for Sailor  - Sailor position not known, intermittent"
                    '(("Success." goalGoal4) ("Failed." goalGoal5) ("Exception." goalGoal5)))

  (initialize-goal goalGoal4 "Goal4: Track Sailor afloat until safe - Watch closely, not to interfere with rescue operations"
                    '(("Success." goalGoal5) ("Failed." goalGoal5) ("Exception." goalGoal5)))

  (initialize-goal goalGoal5 "Goal5: Proceed to Recovery - Mission complete, prepare for pickup"
                    '(("Success." goalGoal6) ("Failed." goalGoal7) ("Exception." goalGoal8)))

  (initialize-goal goalGoal6 "Goal6: Halt and prepare for recovery - Operations complete, final success state" nil)

  (initialize-goal goalGoal7 "Goal7: Halt and deploy recovery beacon - Unable to continue, final failure state" nil)

  (initialize-goal goalGoal8 "Goal8: Halt and await further orders - Unexpected problem, final exception state" nil)
)
;;;;;;;;;;;;;;;;;;;;;;End SailorOverboard.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
