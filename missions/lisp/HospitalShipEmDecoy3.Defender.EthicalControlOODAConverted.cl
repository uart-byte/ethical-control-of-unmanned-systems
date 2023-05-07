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
   (current-execution-goal :accessor current-execution-goal :initform 'goalHSEMD.OODA.21)
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
  (setf (current-execution-goal mission-controller) 'goalHSEMD.OODA.21))

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

;;;;;;;;;;;;;;;;;;;;;;Begin HospitalShipEmDecoy3.Defender.EthicalControlOODA.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar goalHSEMD.OODA.21 (make-instance 'mission-goal)) ; initial goal, by default
  (defvar goalHSEMD.OODA.22 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.23 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.24 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.25 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.26 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.27 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.36 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.37 (make-instance 'mission-goal))
  (defvar goalHSEMD.OODA.99.0 (make-instance 'mission-goal))

(defun initialize-mission ()
  (setf terminal-goal-list '(goalHSEMD.OODA.99.0))

  (initialize-goal goalHSEMD.OODA.21 "Goal HSEMD.OODA.21: Attack Response Thresholds Set - Signal strength needed for close-proximity activation"
                    '(("Success." goalHSEMD.OODA.22) ("Failed." goalHSEMD.OODA.22) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.22 "Goal HSEMD.OODA.22: Enable Robot Swarm - Close-in weapon system activated"
                    '(("Success." goalHSEMD.OODA.23) ("Failed." goalHSEMD.OODA.23) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.23 "Goal HSEMD.OODA.23: Threat Signals Received - Above response threshold"
                    '(("Success." goalHSEMD.OODA.24) ("Failed." goalHSEMD.OODA.24) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.24 "Goal HSEMD.OODA.24: Move to Threat - Group response"
                    '(("Success." goalHSEMD.OODA.27) ("Failed." goalHSEMD.OODA.27) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.25 "Goal HSEMD.OODA.25: IFFNU - Identify Friend Foe Neutral Unknown"
                    '(("Success." goalHSEMD.OODA.26) ("Failed." goalHSEMD.OODA.26) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.26 "Goal HSEMD.OODA.26: Confirm In-Port Counterattack? Rapid-response human checkpoint"
                    '(("Success." goalHSEMD.OODA.27) ("Failed." goalHSEMD.OODA.36) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.27 "Goal HSEMD.OODA.27: Robot Swarm versus Terrorists - Lethal force authorized"
                    '(("Success." goalHSEMD.OODA.99.0) ("Failed." goalHSEMD.OODA.99.0) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.36 "Goal HSEMD.OODA.36: Hospital Ship Attack Denied - Lethal force NOT authorized"
                    '(("Success." goalHSEMD.OODA.37) ("Failed." goalHSEMD.OODA.37) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.37 "Goal HSEMD.OODA.37: Search for Intruders - All defense forces alerted"
                    '(("Success." goalHSEMD.OODA.99.0) ("Failed." goalHSEMD.OODA.99.0) ("Exception." goalHSEMD.OODA.99.0)))

  (initialize-goal goalHSEMD.OODA.99.0 "Goal HSEMD.OODA.99.0: Proceed to recovery - Mission complete, prepare for pickup. Terminal condition." nil)
)
;;;;;;;;;;;;;;;;;;;;;;End HospitalShipEmDecoy3.Defender.EthicalControlOODA.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
