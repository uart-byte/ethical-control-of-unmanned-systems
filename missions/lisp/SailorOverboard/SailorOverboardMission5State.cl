;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: 19 October 2019.

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

(defvar commander1)
(defvar phase1)
(defvar phase2)
(defvar phase3)
(defvar phase4)
(defvar phase5)
(defvar 2019-SOB-mission-orders)

;Above declarations have no function other than to stop compiler complaints about
;use of global ("special") variables.

(defclass mission-phase ()
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-phase).

(defclass human-agent-mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (current-execution-phase :accessor current-execution-phase :initform 'phase1)
   (successor-list-index :accessor successor-list-index :initform 0)))

(defun create-5-mission-phases ()
  (setf phase1 (make-instance 'mission-phase)
        phase2 (make-instance 'mission-phase)
        phase3 (make-instance 'mission-phase)
        phase4 (make-instance 'mission-phase)
        phase5 (make-instance 'mission-phase)))

(defmethod initialize-phase ((phase mission-phase) command successor-list)
  (setf (command phase) command
    (successor-list phase) successor-list))

(defun initialize-2019-SOB-mission-phases ()
  ; Exception means no longer able to continue with mission, so try to recover robot
  (initialize-phase phase1 "Deploy!"  
                    '(("Success." phase2) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase2 "Rendezvous with Sailor!"
                    '(("Success." phase4) ("Failed." phase3) ("Exception." phase5))) 
  (initialize-phase phase3 "Search for Sailor!"  
                    '(("Success." phase4) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase4 "Track Sailor Afloat Until Safe!"  
                    '(("Success." phase5) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase5 "Proceed to recovery!"
                    '(("Success." "Phase 6 report: Vehicle recovered.") ("Failed." "Phase 7 report: Vehicle lost.")
                      ("Exception." "Phase 8 report: Maintaining standoff distance while awaiting orders."))))

(defmethod issue-command ((MEE human-agent-mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (command (command (eval phase))))
    (issue-order command)))

(defmethod ask-result ((MEE human-agent-mission-execution-engine))
  (let* ((result (ask-outcome)))
    (setf (external-agent-response MEE) result)))

(defmethod set-next-phase ((MEE human-agent-mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (successor-list (successor-list (eval phase)))
         (index (successor-list-index MEE))
         (next-phase (second (nth index successor-list))))
    (setf (current-execution-phase MEE) next-phase)))

(defmethod set-successor-list-index ((MEE human-agent-mission-execution-engine))
  (let* ((index (convert-outcome-to-index (external-agent-response MEE))))
    (setf (successor-list-index MEE) index)))

(defun execute-phase ()
  (issue-command commander1)
  (ask-result commander1)
  (set-successor-list-index commander1)
  (set-next-phase commander1))

(defun create-2019-SOB-mission-orders ()
  (setf 2019-SOB-mission-orders (create-5-mission-phases))
  (initialize-2019-SOB-mission-phases))

(defun start ()
  (create-2019-SOB-mission-orders)
  (setf commander1 (make-instance 'human-agent-mission-execution-engine)))

(defun issue-order (command)
  (format t "~A" command)
  (terpri))

(defun ask-outcome ()
  (format t "Did goal succeed (s), fail (f), or abort (x)?")
  (let ((value (read)))
       (if (member value '(s f x))
           value
          (ask-outcome))))

(defun execute-mission ()
  (if (equal (current-execution-phase commander1) 'phase5) (execute-phase)
    (and (execute-phase)(execute-mission))))

(defun run ()
  (start)
  (execute-mission))

(defun convert-outcome-to-index (x)
  (cond ((equal x 's) 0)
        ((equal x 'f) 1)
        ((equal x 'x) 2)))
