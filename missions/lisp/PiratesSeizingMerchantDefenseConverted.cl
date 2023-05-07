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
   (current-execution-goal :accessor current-execution-goal :initform 'goalPSMD11.0)
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
  (setf (current-execution-goal mission-controller) 'goalPSMD11.0))

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

;;;;;;;;;;;;;;;;;;;;;;Begin PiratesSeizingMerchantDefense.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar goalPSMD11.0 (make-instance 'mission-goal)) ; initial goal, by default
  (defvar goalPSMD12.0 (make-instance 'mission-goal))
  (defvar goalPSMD13.0 (make-instance 'mission-goal))
  (defvar goalPSMD13.1 (make-instance 'mission-goal))
  (defvar goalPSMD13.2 (make-instance 'mission-goal))
  (defvar goalPSMD14.0 (make-instance 'mission-goal))
  (defvar goalPSMD14.1 (make-instance 'mission-goal))
  (defvar goalPSMD14.2 (make-instance 'mission-goal))
  (defvar goalPSMD14.3 (make-instance 'mission-goal))
  (defvar goalPSMD14.9 (make-instance 'mission-goal))
  (defvar goalPSMD21.0 (make-instance 'mission-goal))
  (defvar goalPSMD22.0 (make-instance 'mission-goal))
  (defvar goalPSMD22.1 (make-instance 'mission-goal))
  (defvar goalPSMD22.2 (make-instance 'mission-goal))
  (defvar goalPSMD30.0 (make-instance 'mission-goal))
  (defvar goalPSMD31.0 (make-instance 'mission-goal))
  (defvar goalPSMD32.0 (make-instance 'mission-goal))
  (defvar goalPSMD32.1 (make-instance 'mission-goal))
  (defvar goalPSMD32.2 (make-instance 'mission-goal))
  (defvar goalPSMD32.3 (make-instance 'mission-goal))
  (defvar goalPSMD33.0 (make-instance 'mission-goal))
  (defvar goalPSMD33.1 (make-instance 'mission-goal))
  (defvar goalPSMD34.0 (make-instance 'mission-goal))
  (defvar goalPSMD34.1 (make-instance 'mission-goal))
  (defvar goalPSMD34.2 (make-instance 'mission-goal))
  (defvar goalPSMD34.3 (make-instance 'mission-goal))
  (defvar goalPSMD34.4 (make-instance 'mission-goal))
  (defvar goalPSMD35.0 (make-instance 'mission-goal))
  (defvar goalPSMD35.1 (make-instance 'mission-goal))
  (defvar goalPSMD35.2 (make-instance 'mission-goal))
  (defvar goalPSMD35.3 (make-instance 'mission-goal))
  (defvar goalPSMD35.4 (make-instance 'mission-goal))
  (defvar goalPSMD35.5 (make-instance 'mission-goal))
  (defvar goalPSMD36.0 (make-instance 'mission-goal))
  (defvar goalPSMD37.0 (make-instance 'mission-goal))
  (defvar goalPSMD40.0 (make-instance 'mission-goal))
  (defvar goalPSMD41.0 (make-instance 'mission-goal))
  (defvar goalPSMD41.1 (make-instance 'mission-goal))
  (defvar goalPSMD41.2 (make-instance 'mission-goal))
  (defvar goalPSMD41.3 (make-instance 'mission-goal))
  (defvar goalPSMD41.4 (make-instance 'mission-goal))
  (defvar goalPSMD41.5 (make-instance 'mission-goal))
  (defvar goalPSMD42.0 (make-instance 'mission-goal))
  (defvar goalPSMD43.0 (make-instance 'mission-goal))
  (defvar goalPSMD90.0 (make-instance 'mission-goal))
  (defvar goalPSMD99.0 (make-instance 'mission-goal))
  (defvar goalPSMD99.1 (make-instance 'mission-goal))
  (defvar goalPSMD99.2 (make-instance 'mission-goal))
  (defvar goalPSMD99.3 (make-instance 'mission-goal))

(defun initialize-mission ()
  (setf terminal-goal-list '(goalPSMD99.1 goalPSMD99.2 goalPSMD99.3))

  (initialize-goal goalPSMD11.0 "Goal PSMD11.0: Deploy, Launch - Commit to robot support"
                    '(("Success." goalPSMD12.0) ("Failed." goalPSMD99.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD12.0 "Goal PSMD12.0: Transit to hostile area - Proceed to estimated position"
                    '(("Success." goalPSMD13.0) ("Failed." goalPSMD99.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD13.0 "Goal PSMD13.0: Locate Merchant - Use best search pattern, then follow moving ship and boats"
                    '(("Success." goalPSMD13.1) ("Failed." goalPSMD21.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD13.1 "Goal PSMD13.1: Report position - Alerts updated"
                    '(("Success." goalPSMD13.2) ("Failed." goalPSMD13.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD13.2 "Goal PSMD13.2: Mark with Beacon - Monitor wind effects and ocean current"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD21.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD14.0 "Goal PSMD14.0: Track Merchant - Monitor and communicate"
                    '(("Success." goalPSMD14.1) ("Failed." goalPSMD14.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD14.1 "Goal PSMD14.1: Maintain proximity - Beyond range of small arms"
                    '(("Success." goalPSMD14.2) ("Failed." goalPSMD14.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD14.2 "Goal PSMD14.2: Periodic reports - Situation reports, changing status"
                    '(("Success." goalPSMD14.3) ("Failed." goalPSMD14.3) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD14.3 "Goal PSMD14.3: Continue until further orders - Repeat until conditions change"
                    '(("Success." goalPSMD14.9) ("Failed." goalPSMD21.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD14.9 "Goal PSMD14.9: Check posture changed? Task update received?"
                    '(("Success." goalPSMD30.0) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD21.0 "Goal PSMD21.0: Request Guidance? Need updated position"
                    '(("Success." goalPSMD12.0) ("Failed." goalPSMD99.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD22.0 "Goal PSMD22.0: Low Fuel - Make best effort possible"
                    '(("Success." goalPSMD22.1) ("Failed." goalPSMD22.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD22.1 "Goal PSMD22.1: Remain with Merchant? Choices: land on boat, attach to boat, or adrift nearby"
                    '(("Success." goalPSMD22.2) ("Failed." goalPSMD22.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD22.2 "Goal PSMD22.2: Beacon? While power remains"
                    '(("Success." goalPSMD99.0) ("Failed." goalPSMD99.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD30.0 "Goal PSMD30.0: Warning Phase - Close interaction with pirates, merchant ship"
                    '(("Success." goalPSMD31.0) ("Failed." goalPSMD31.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD31.0 "Goal PSMD31.0: Confirm Mission Authorities - Check constraints and defensive responses"
                    '(("Success." goalPSMD32.0) ("Failed." goalPSMD32.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD32.0 "Goal PSMD32.0: IFFNU - Identify Friend Foe Neutral Unknown"
                    '(("Success." goalPSMD32.1) ("Failed." goalPSMD32.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD32.1 "Goal PSMD32.1: Photograph all close contacts - Pass within range of small arms"
                    '(("Success." goalPSMD32.2) ("Failed." goalPSMD32.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD32.2 "Goal PSMD32.2: Classify, send contact reports - Based on target behavior or signal/image match"
                    '(("Success." goalPSMD32.3) ("Failed." goalPSMD32.3) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD32.3 "Goal PSMD32.3: Confirm IFFNU classifications - Requires approval by human commander before proceeding further"
                    '(("Success." goalPSMD33.0) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD33.0 "Goal PSMD33.0: Commence Warnings - May provoke pirate response"
                    '(("Success." goalPSMD33.1) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD33.1 "Goal PSMD33.1: Notify Merchant - Keep crew informed, even if they cannot transmit"
                    '(("Success." goalPSMD34.0) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD34.0 "Goal PSMD34.0: Send Warning Messages - Communicate to pirates, all parties"
                    '(("Success." goalPSMD34.1) ("Failed." goalPSMD34.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD34.1 "Goal PSMD34.1: Send multiple message paths - Loudspeaker, flashing light, siren, drop smoke, bridge-bridge radio"
                    '(("Success." goalPSMD34.2) ("Failed." goalPSMD34.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD34.2 "Goal PSMD34.2: Maintain proximity - Just outside range of small arms"
                    '(("Success." goalPSMD34.3) ("Failed." goalPSMD34.3) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD34.3 "Goal PSMD34.3: Periodic reports - Situation reports, changing status"
                    '(("Success." goalPSMD34.4) ("Failed." goalPSMD34.4) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD34.4 "Goal PSMD34.4: Continue until further orders - First repeat for all designated pirates"
                    '(("Success." goalPSMD35.0) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.0 "Goal PSMD35.0: Observe pirate response - Monitor and report back to own ship"
                    '(("Success." goalPSMD35.1) ("Failed." goalPSMD35.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.1 "Goal PSMD35.1: Pirates retreat? Stay with merchant, report if response changes"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD35.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.2 "Goal PSMD35.2: Pirates attack? Stay with merchant, counterattack if response changes"
                    '(("Success." goalPSMD40.0) ("Failed." goalPSMD35.3) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.3 "Goal PSMD35.3: Maintain proximity - Just outside range of small arms"
                    '(("Success." goalPSMD35.4) ("Failed." goalPSMD35.4) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.4 "Goal PSMD35.4: Periodic reports - Situation reports, changing status - note ship may have EMCON radio silence"
                    '(("Success." goalPSMD35.5) ("Failed." goalPSMD35.5) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD35.5 "Goal PSMD35.5: Continue until further orders - Repeat until conditions change"
                    '(("Success." goalPSMD36.0) ("Failed." goalPSMD36.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD36.0 "Goal PSMD36.0: Check posture changed? Task update received"
                    '(("Success." goalPSMD37.0) ("Failed." goalPSMD35.0) ("Exception." goalPSMD14.0)))

  (initialize-goal goalPSMD37.0 "Goal PSMD37.0: Fire Warning Shot - Warning shots remain an available option for human commanders. Lethal force is authorized."
                    '(("Success." goalPSMD31.0) ("Failed." goalPSMD31.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD40.0 "Goal PSMD40.0: Attack - Close interaction with pirates, merchant ship"
                    '(("Success." goalPSMD41.0) ("Failed." goalPSMD41.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD41.0 "Goal PSMD41.0: Attack Pirate Boats in priority order - Rapidly engage, shoot to disable or kill. Lethal force still authorized."
                    '(("Success." goalPSMD41.1) ("Failed." goalPSMD41.1) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD41.1 "Goal PSMD41.1: Pirates retreat? Stay with merchant, report if response changes"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD41.2) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD41.2 "Goal PSMD41.2: Pirates attacking? Stay with merchant, counterattack if response changes"
                    '(("Success." goalPSMD41.3) ("Failed." goalPSMD41.3) ("Exception." goalPSMD42.0)))

  (initialize-goal goalPSMD41.3 "Goal PSMD41.3: Maintain proximity - Just outside range of small arms"
                    '(("Success." goalPSMD41.4) ("Failed." goalPSMD41.4) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD41.4 "Goal PSMD41.4: Periodic reports - Situation reports, changing status - note ship may have EMCON radio silence"
                    '(("Success." goalPSMD41.5) ("Failed." goalPSMD41.5) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD41.5 "Goal PSMD41.5: Continue until further orders - Repeat until conditions change"
                    '(("Success." goalPSMD36.0) ("Failed." goalPSMD36.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD42.0 "Goal PSMD42.0: Low or no ammunition: need to disengage? Hold ammunition in reseve, or else fight to the finish"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD41.3) ("Exception." goalPSMD90.0)))

  (initialize-goal goalPSMD43.0 "Goal PSMD43.0: Check for guidance, resume tracking or continue attacking? Humans monitoring attack can interrupt or override"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD41.0) ("Exception." goalPSMD90.0)))

  (initialize-goal goalPSMD90.0 "Goal PSMD90.0: Check relieved by other asset - Continue tracking merchant unless further task update recieved"
                    '(("Success." goalPSMD14.0) ("Failed." goalPSMD14.0) ("Exception." goalPSMD99.0)))

  (initialize-goal goalPSMD99.0 "Goal PSMD99.0: Proceed to recovery - Mission complete, prepare for pickup"
                    '(("Success." goalPSMD99.1) ("Failed." goalPSMD99.2) ("Exception." goalPSMD99.3)))

  (initialize-goal goalPSMD99.1 "Goal PSMD99.1: Halt and prepare for recovery - Operations completed, final success state" nil)

  (initialize-goal goalPSMD99.2 "Goal PSMD99.2: Halt and deploy recovery beacon - Unable to operate, final failure state" nil)

  (initialize-goal goalPSMD99.3 "Goal PSMD99.3: Halt and await further orders - Unplanned failure, final exception state" nil)
)
;;;;;;;;;;;;;;;;;;;;;;End PiratesSeizingMerchantDefense.xml Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
