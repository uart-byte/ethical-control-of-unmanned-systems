;This code is written in ANSI Common Lisp, Allegro 10.1, complementary copy available
;at Franz.com. All code written by Prof. Robert B. McGhee (robertbmcghee@gmail.com) at 
;the Naval Postgraduate School, Monterey, CA 93943. Date of latest update: 1 April 2019.

; Invocation: first click on dump truck to compile, then Debug Window: (run)

; N.B. This code is developmental, buggy and not recommended for further use.
; It is archived for documentation purposes and superseded by later versions.

;Mission coded below is taken from Fig. 7, pg. 434, in "Ethical Mission Definition and 
;Execution for Maritime Robots Under Human Supervision", IEEE Journal of Oceanic 
;Engineering (JOE), Vol. 43, No. 2, April, 2018.

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

;It should be noted that any other 5-phase mission can be exhaustively tested by 
;appropriately editing the phase initialization function below. Evidently, missions of
;fewer than five phases can be tested in the same way by simply not initializing unused
;phases. Creation of mission orders with more than five phases is not recommended since 
;the list of all possible dialogs between mission orders agent and mission execution 
;agent is likely to be too long for accurate human validation. Instead, as discussed in
;referenced JOE paper, "phase abstraction" can be used to combine phases and thereby
;reduce the number of phases in top level mission orders.


(defvar mission-controller1)
(defvar phase1)
(defvar phase2)
(defvar phase3)
(defvar phase4)
(defvar phase5)
(defvar 2018-JOE-orders)

;Above declarations have no function other than to stop compiler complaints about 
;use of global ("special") variables.

(defclass mission-phase () 
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-phase).

(defclass robot-agent-mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (current-execution-phase :accessor current-execution-phase :initform 'phase1)
   (successor-list-index :accessor successor-list-index :initform 0)
   (mission-log :accessor mission-log :initform nil)))

(defun create-5-mission-phases ()
  (setf phase1 (make-instance 'mission-phase)
        phase2 (make-instance 'mission-phase)
        phase3 (make-instance 'mission-phase)
        phase4 (make-instance 'mission-phase)
        phase5 (make-instance 'mission-phase)))

(defmethod initialize-phase ((phase mission-phase) command successor-list)
  (setf (command phase) command
    (successor-list phase) successor-list))

(defun initialize-2018-mission-phases ()
  (initialize-phase phase1 "Search Area A!"  
                    '(("Success." phase2) ("Failed." phase3) ("Constraint." phase4)))
  (initialize-phase phase2 "Sample environment!" 
                    '(("Success." phase3) ("Failed." phase5) ("Constraint." phase4)))
  (initialize-phase phase3 "Search Area B!"  
                    '(("Success." phase4) ("Failed." phase4) ("Constraint." phase4)))
  (initialize-phase phase4 "Rendezvous with Vehicle2!"  
                    '(("Success." phase5) ("Failed." phase5) ("Constraint." phase5)))
  (initialize-phase phase5 "Proceed to recovery!" 
                    '(("Success." "Mission complete.") ("Failed." nil) 
                      ("Constraint." "Mission aborted. Vehicle recovered"))))

(defmethod update-mission-log ((MEE robot-agent-mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (command (command (eval phase))))
    (setf (mission-log mission-controller1)
          (append (list phase command (mission-log mission-controller1))))))

(defmethod ask-result ((MEE robot-agent-mission-execution-engine))
  (let* ((result (random 3)))
    (setf (external-agent-response MEE) result)))

(defmethod set-next-phase ((MEE robot-agent-mission-execution-engine))
  (let* ((phase (current-execution-phase MEE))
         (successor-list (successor-list (eval phase)))
         (index (successor-list-index MEE))
         (next-phase (second (nth index successor-list))))
    (setf (current-execution-phase MEE) next-phase)))

(defmethod set-successor-list-index ((MEE robot-agent-mission-execution-engine))
  (let* ((index (random 3)))
    (setf (successor-list-index MEE) index)))

(defun execute-phase ()
  (ask-result mission-controller1)
  (update-mission-log mission-controller1)
  (set-successor-list-index mission-controller1)
  (set-next-phase mission-controller1))

(defun create-2018-JOE-orders ()
  (setf 2018-JOE-orders (create-5-mission-phases))
  (initialize-2018-mission-phases))

(defun start ()
  (create-2018-JOE-orders)
  (setf mission-controller1 (make-instance 'robot-agent-mission-execution-engine)))

(defun execute-mission ()
  (if (equal (current-execution-phase mission-controller1) 'phase5) (execute-phase)
    (and (execute-phase)(execute-mission))))

(defun run ()
  (start)
  (execute-mission)
  (results))

(defun results ()
  (mission-log mission-controller1))

