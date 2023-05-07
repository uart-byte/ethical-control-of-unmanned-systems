;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: 10 August 2020.

;Comment1: As of the time of this writing, the default Allegro IDE editor window print
;font is oversized, and does not print a full-width copy of the IDE Debug or Editor
;windows. To fix this, go to the top of the Allegro opening screen and click "Tools".
;Then, from the drop-down menu, select "Options". Next, on the resulting "Options"
;pop-up screen, select, "Fonts". Then click on "Editor Printouts". When a window pops up
;asking "Revert to the default fixed-width font?", answer "No". A "Font" window will
;pop up. In this window, scroll down fonts to "Courier New". Scroll "Print Size" to "8".
;Click "OK" at bottom of this window, and it will disappear. Finally, click "OK" in
;"Options" window, and it too will disappear. The result of all this manipulation will
;be that clicking "Print" on the drop-down menu from the "File" tab of the IDE opening
;screen will print an exact copy of what you see on the IDE editor or debug windows.
;You will be glad you did all this.

;Comment2: The reason that "Courier New" is recommended above is that it is a "fixed-
;width" font. This means that when Lisp code is viewed on screen or in a printout,
;characters, especially parentheses, line up vertically, facilitating human
;comprehension.

;Comment3: This code facilitates human-robot cooperation and involves three entities. At
;the top level, the "mission commander" is always human, at least at this point in the
;evolution of RBM software. The "mission controller" is usually assumed to be electronic,
;but could be a human "executive officer" in the case of a manned vehicle. The mission
;agent is always a vehicle. If the vehicle has an onboard computer incorporating the
;mission controller, then the vehicle is a robot. If not, it is an "ROV" or a 
;remotely operated "drone".


;;;;;;;;;;;;;;;;;;;;;;;;;Begin Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;

(defclass mission-phase ()
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-phase).

(defclass mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (previous-execution-phase :accessor previous-execution-phase :initform nil)
   (current-execution-phase :accessor current-execution-phase :initform 'phase1)
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
    (setf (previous-execution-phase MEE) phase (current-execution-phase MEE) next-phase)))

(defmethod set-successor-list-index ((MEE mission-execution-engine))
  (let* ((index (convert-outcome-to-index (external-agent-response MEE))))
    (setf (successor-list-index MEE) index)))

(defvar mission-controller (make-instance 'mission-execution-engine))

(defun execute-phase ()
  (issue-command mission-controller)
  (ask-result mission-controller)
  (set-successor-list-index mission-controller)
  (set-next-phase mission-controller))

(defun execute-terminal-phase ()
  (issue-command mission-controller))

(defun create-mission-orders ()
  (initialize-mission)
  (setf (current-execution-phase mission-controller) 'phase1))

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

(defun repeat ()
  (rerun (previous-execution-phase mission-controller)))

(defun refine-last-command ()
  (let* ((previous-phase (previous-execution-phase mission-controller)))
    (cond ((equal previous-phase 'phase1) (rerun 'phase1.1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;End Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;Begin 8-phase Sailor Overboard Mission Orders;;;;;;;;;;;;;;;;;;;;

(defvar phase1 (make-instance 'mission-phase))
(defvar phase1.1 (make-instance 'mission-phase))
(defvar phase1.2 (make-instance 'mission-phase))
(defvar phase1.3 (make-instance 'mission-phase))      
(defvar phase2 (make-instance 'mission-phase))
(defvar phase3 (make-instance 'mission-phase))
(defvar phase4 (make-instance 'mission-phase))
(defvar phase5 (make-instance 'mission-phase))
(defvar phase6 (make-instance 'mission-phase))
(defvar phase7 (make-instance 'mission-phase))
(defvar phase8 (make-instance 'mission-phase))

(defun initialize-mission ()
  (setf terminal-phase-list '(phase6 phase7 phase8))

  (initialize-phase phase1.1 "Choose Tube and Launch!"  
                    '(("Success." phase1.2) ("Failed." phase8) ("Exception." phase8)))

  (initialize-phase phase1.2 "Enter Water and Get GPS Fix!"
                    '(("Success." phase1.3) ("Failed." phase8) ("Exception." phase8)))

  (initialize-phase phase1.3 "Descend to Search Depth!"  
                    '(("Success." phase2) ("Failed." phase8) ("Exception." phase8)))

  (initialize-phase phase1 "Deploy!"  
                    '(("Success." phase2) ("Failed." phase8) ("Exception." phase8)))

; deviation from phase4/phase3/phase5, see SailorOverboardMission8StateNoHumanSupervision.cl for alternative
  (initialize-phase phase2 "Rendezvous with Sailor!"
                    '(("Success." phase4) ("Failed." phase3) ("Exception." phase8)))

  (initialize-phase phase3 "Search for Sailor!"  
                    '(("Success." phase4) ("Failed." phase8) ("Exception." phase8)))

  (initialize-phase phase4 "Track Sailor Afloat Until Safe!"  
                    '(("Success." phase5) ("Failed." phase5) ("Exception." phase8)))

  (initialize-phase phase5 "Proceed to recovery!"
                    '(("Success." phase6) ("Failed." phase7) ("Exception." phase8)))

  (initialize-phase phase6 "Halt and prepare for recovery!" nil)

  (initialize-phase phase7 "Halt and deploy recovery beacon!" nil)

  (initialize-phase phase8 "Halt and await further orders!" nil))
 
;;;;;;;;;;;;;;;;;;;;;;End 8-phase Sailor Overboard Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;
