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
   (current-execution-goal :accessor current-execution-goal :initform 'goalHSEMD.reflex.11)
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
  (setf (current-execution-goal mission-controller) 'goalHSEMD.reflex.11))

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

;;;;;;;;;;;;;;;;;;;;;;Begin HospitalShipEmDecoy2.Defender.SenseDecideAct.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar goalHSEMD.reflex.11 (make-instance 'mission-goal)) ; initial goal, by default
  (defvar goalHSEMD.reflex.12 (make-instance 'mission-goal))
  (defvar goalHSEMD.reflex.13 (make-instance 'mission-goal))
  (defvar goalHSEMD.reflex.14 (make-instance 'mission-goal))
  (defvar goalHSEMD.reflex.17 (make-instance 'mission-goal))
  (defvar goalHSEMD.reflex.99.0 (make-instance 'mission-goal))

(defun initialize-mission ()
  (setf terminal-goal-list '(goalHSEMD.reflex.99.0))

  (initialize-goal goalHSEMD.reflex.11 "Goal HSEMD.reflex.11: Attack Response Thresholds Set - Signal strength needed for close-proximity activation"
                    '(("Success." goalHSEMD.reflex.12) ("Failed." goalHSEMD.reflex.12) ("Exception." goalHSEMD.reflex.99.0)))

  (initialize-goal goalHSEMD.reflex.12 "Goal HSEMD.reflex.12: Enable Robot Swarm - Close-in weapon system activated"
                    '(("Success." goalHSEMD.reflex.13) ("Failed." goalHSEMD.reflex.13) ("Exception." goalHSEMD.reflex.99.0)))

  (initialize-goal goalHSEMD.reflex.13 "Goal HSEMD.reflex.13: Threat Signals Received - Above response threshold"
                    '(("Success." goalHSEMD.reflex.14) ("Failed." goalHSEMD.reflex.14) ("Exception." goalHSEMD.reflex.99.0)))

  (initialize-goal goalHSEMD.reflex.14 "Goal HSEMD.reflex.14: Move to Threat - Group response"
                    '(("Success." goalHSEMD.reflex.17) ("Failed." goalHSEMD.reflex.17) ("Exception." goalHSEMD.reflex.99.0)))

  (initialize-goal goalHSEMD.reflex.17 "Goal HSEMD.reflex.17: Robot Swarm Counterattack - Lethal force authorized"
                    '(("Success." goalHSEMD.reflex.99.0) ("Failed." goalHSEMD.reflex.99.0) ("Exception." goalHSEMD.reflex.99.0)))

  (initialize-goal goalHSEMD.reflex.99.0 "Goal HSEMD.reflex.99.0: Proceed to recovery - Mission complete, prepare for pickup. Terminal condition." nil)
)
;;;;;;;;;;;;;;;;;;;;;;End HospitalShipEmDecoy2.Defender.SenseDecideAct.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
