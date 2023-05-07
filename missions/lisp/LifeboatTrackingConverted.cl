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
   (current-execution-goal :accessor current-execution-goal :initform 'goalLBT1.0)
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
  (setf (current-execution-goal mission-controller) 'goalLBT1.0))

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

;;;;;;;;;;;;;;;;;;;;;;Begin LifeboatTracking.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar goalLBT1.0 (make-instance 'mission-goal)) ; initial goal, by default
  (defvar goalLBT2.0 (make-instance 'mission-goal))
  (defvar goalLBT3.0 (make-instance 'mission-goal))
  (defvar goalLBT3.1 (make-instance 'mission-goal))
  (defvar goalLBT3.2 (make-instance 'mission-goal))
  (defvar goalLBT4.0 (make-instance 'mission-goal))
  (defvar goalLBT4.1 (make-instance 'mission-goal))
  (defvar goalLBT4.2 (make-instance 'mission-goal))
  (defvar goalLBT4.3 (make-instance 'mission-goal))
  (defvar goalLBT5.0 (make-instance 'mission-goal))
  (defvar goalLBT6.0 (make-instance 'mission-goal))
  (defvar goalLBT6.1 (make-instance 'mission-goal))
  (defvar goalLBT6.2 (make-instance 'mission-goal))
  (defvar goalLBT7.0 (make-instance 'mission-goal))
  (defvar goalLBT99.0 (make-instance 'mission-goal))
  (defvar goalLBT99.1 (make-instance 'mission-goal))
  (defvar goalLBT99.2 (make-instance 'mission-goal))
  (defvar goalLBT99.3 (make-instance 'mission-goal))

(defun initialize-mission ()
  (setf terminal-goal-list '(goalLBT99.1 goalLBT99.2 goalLBT99.3))

  (initialize-goal goalLBT1.0 "Goal LBT1.0: Deploy, Launch - Commit to robot support"
                    '(("Success." goalLBT2.0) ("Failed." goalLBT99.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT2.0 "Goal LBT2.0: Transit to search area - Proceed to estimated position"
                    '(("Success." goalLBT3.0) ("Failed." goalLBT99.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT3.0 "Goal LBT3.0: Locate Lifeboat - Follow best search pattern"
                    '(("Success." goalLBT3.1) ("Failed." goalLBT2.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT3.1 "Goal LBT3.1: Report position - Alerts updated"
                    '(("Success." goalLBT3.2) ("Failed." goalLBT4.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT3.2 "Goal LBT3.2: Mark with Beacon - Monitor wind effects and ocean current"
                    '(("Success." goalLBT4.0) ("Failed." goalLBT4.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT4.0 "Goal LBT4.0: Track Lifeboat - Monitor and communicate"
                    '(("Success." goalLBT4.1) ("Failed." goalLBT4.1) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT4.1 "Goal LBT4.1: Maintain proximity - Overhead or afloat nearby"
                    '(("Success." goalLBT4.2) ("Failed." goalLBT4.2) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT4.2 "Goal LBT4.2: Periodic reports - Popup or float to report, also recharge"
                    '(("Success." goalLBT4.3) ("Failed." goalLBT4.3) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT4.3 "Goal LBT4.3: Continue - Repeat until conditions change"
                    '(("Success." goalLBT5.0) ("Failed." goalLBT7.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT5.0 "Goal LBT5.0: Check relieved by other asset - Task update received?"
                    '(("Success." goalLBT99.0) ("Failed." goalLBT4.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT6.0 "Goal LBT6.0: Low Fuel - Make best effort possible"
                    '(("Success." goalLBT6.1) ("Failed." goalLBT6.1) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT6.1 "Goal LBT6.1: Remain with lifeboat? Choices: land on boat, attach to boat, or adrift nearby"
                    '(("Success." goalLBT6.2) ("Failed." goalLBT6.2) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT6.2 "Goal LBT6.2: Beacon? While power remains"
                    '(("Success." goalLBT99.0) ("Failed." goalLBT99.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT7.0 "Goal LBT7.0: Request Guidance? Need updated position"
                    '(("Success." goalLBT2.0) ("Failed." goalLBT99.0) ("Exception." goalLBT99.0)))

  (initialize-goal goalLBT99.0 "Goal LBT99.0: Proceed to recovery - Mission complete, prepare for pickup"
                    '(("Success." goalLBT99.1) ("Failed." goalLBT99.2) ("Exception." goalLBT99.3)))

  (initialize-goal goalLBT99.1 "Goal LBT99.1: Halt and prepare for recovery - Operations completed, final success state" nil)

  (initialize-goal goalLBT99.2 "Goal LBT99.2: Halt and deploy recovery beacon - Unable to operate, final failure state" nil)

  (initialize-goal goalLBT99.3 "Goal LBT99.3: Halt and await further orders - Unplanned failure, final exception state" nil)
)
;;;;;;;;;;;;;;;;;;;;;;End LifeboatTracking.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
