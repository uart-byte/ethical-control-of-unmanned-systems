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

(defclass mission-phase ()
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-phase).

(defclass mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (current-execution-phase :accessor current-execution-phase :initform 'phaseGoal1)
   (successor-list-index :accessor successor-list-index :initform 0)))

(defmethod initialize-phase ((phase mission-phase) new-command new-successor-list)
  (setf (command phase) new-command
    (successor-list phase) new-successor-list))

(defmethod issue-command ((MEE mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (new-command (command (eval phase))))
    (issue-order new-command)))

(defmethod ask-result ((MEE mission-execution-engine))
  (let* ((result (ask-outcome)))
    (setf (external-agent-response MEE) result)))

(defmethod set-next-phase ((MEE mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (new-successor-list (successor-list (eval phase)))
         (new-index (successor-list-index MEE))
         (next-phase (second (nth new-index new-successor-list))))
    (setf (current-execution-phase MEE) next-phase)))

(defmethod set-successor-list-index ((MEE mission-execution-engine))
  (let* ((index (convert-outcome-to-index (external-agent-response MEE))))
    (setf (successor-list-index MEE) index)))

(defvar mission-controller (make-instance 'mission-execution-engine)) ; instantiate the class as an object

; discussion for future work: naming conventions

(defun execute-phase ()
  (issue-command mission-controller)
  (ask-result mission-controller)
  (set-successor-list-index mission-controller)
  (set-next-phase mission-controller))

(defun execute-terminal-phase ()
  (issue-command mission-controller))

(defun create-mission-orders ()
  (initialize-mission)
  (setf (current-execution-phase mission-controller) 'phaseGoal1))

(defun issue-order (command)
  (format t "~A" command)
  (terpri))

(defun ask-outcome ()
  (format t "Did goal succeed (s), fail (f), or abort (x)?")
  (let ((value (read)))
       (if (member value '(s f x))
           value
          (ask-outcome))))

(defun run ()
  (create-mission-orders)
  (execute-mission))

; execute starting at a new phase, also enables testing and refinement resuming in the middle of a mission
(defun rerun (new-start-phase)
  (setf (current-execution-phase mission-controller) new-start-phase)
  (execute-mission))

(defun convert-outcome-to-index (x)
  (cond ((equal x 's) 0)
        ((equal x 'f) 1)
        ((equal x 'x) 2)))

(defvar terminal-phase-list nil)

(defun execute-mission ()
  (cond ((member (current-execution-phase mission-controller) terminal-phase-list) (execute-terminal-phase))
        (t (execute-phase)(execute-mission))))

;;;;;;;;;;;;;;;;;;;;;;;;;End Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;Begin SailorOverboard.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar phaseGoal1 (make-instance 'mission-phase)) ; initial goal, by default
  (defvar phaseGoal2 (make-instance 'mission-phase))
  (defvar phaseGoal3 (make-instance 'mission-phase))
  (defvar phaseGoal4 (make-instance 'mission-phase))
  (defvar phaseGoal5 (make-instance 'mission-phase))
  (defvar phaseGoal6 (make-instance 'mission-phase))
  (defvar phaseGoal7 (make-instance 'mission-phase))
  (defvar phaseGoal8 (make-instance 'mission-phase))

(defun initialize-mission ()
  (setf terminal-phase-list '(phaseGoal6 phaseGoal7 phaseGoal8))

  (initialize-phase phaseGoal1 "Deploy, Launch - Sailor Overboard Immediate Action"
                    '(("Success." phaseGoal2) ("Failed." phaseGoal2) ("Exception." phaseGoal5)))

  (initialize-phase phaseGoal2 "Rendezvous with Sailor - Go directly to best known location"
                    '(("Success." phaseGoal4) ("Failed." phaseGoal3) ("Exception." phaseGoal5)))

  (initialize-phase phaseGoal3 "Search for Sailor  - Sailor position not known, intermittent"
                    '(("Success." phaseGoal4) ("Failed." phaseGoal5) ("Exception." phaseGoal5)))

  (initialize-phase phaseGoal4 "Track Sailor afloat until safe - Watch closely, not to interfere with rescue operations"
                    '(("Success." phaseGoal5) ("Failed." phaseGoal5) ("Exception." phaseGoal5)))

  (initialize-phase phaseGoal5 "Proceed to Recovery - Mission complete, prepare for pickup"
                    '(("Success." phaseGoal6) ("Failed." phaseGoal2) ("Exception." phaseGoal6)))

  (initialize-phase phaseGoal6 "Halt and prepare for recovery - Operations complete, final success state" nil)

  (initialize-phase phaseGoal7 "Halt and deploy recovery beacon - Unable to continue, final failure state" nil)

  (initialize-phase phaseGoal8 "Halt and await further orders - Unexpected problem, final exception state" nil)
)
;;;;;;;;;;;;;;;;;;;;;;End SailorOverboard.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
