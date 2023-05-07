
;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
; , CA 93943. Date of latest update: 30 June 2020.

;The mission coded below is taken from Fig. 7, pg. 434, in "Ethical Mission Definition
;and Execution for Maritime Robots Under Human Supervision", IEEE Journal of Oceanic
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
;You will be glad you did all of this.

;Comment2: The reason that "Courier New" is recommended above is that it is a "fixed-
;width" font. This means that when Lisp code is viewed on screen or in a printout,
;characters, especially parentheses, line up vertically, facilitating human
;comprehension.

;Comment3: At some point, after you install Allegro 10.1, the compiler will ask you "Do
;you want to start a project". Answer "No." Saying "Yes" will introduce you to needless
;complexity not needed to run the attached code. Instead, after installing and opening
;Allegro, starting with an electronic copy of this report, highlight the code you want
;to run and then use ctrl-c keys to copy. Next, place the mouse cursor in the editor window and
;paste with ctrl-v. Now using the "File" menu, select "save as" and enter a file name of your
;choosing in pop-up menu. Next, click on the dumptruck icon at top of screen, select your
;file from popup menu,and double click on it. The debug window will show some system files
;loading and will then present a prompt as shown in Fig. 1 above. Now type "(start)" then "(run)"
:to prompt and respond to queries as in Fig. 1. Type "(run" again and try you own responses to
;see if the results correspond to the mission flow graph.

;It should be noted that any other 5-phase mission can be exhaustively tested by
;appropriately editing the phase initialization function below. Evidently, missions of
;fewer than five phases can be tested in the same way by simply not initializing unused
;phases. Creation of mission orders with more than five phases is not recommended since
;the list of all possible dialogs between the mission orders agent and the mission execution
;agent is likely to be too long for accurate human validation. Instead, as explained in the
;referenced JOE paper, "phase abstraction" can be used to combine phases and thereby
;reduce the number of phases in top level mission orders.

(defvar *current-paths-to-goal* nil)
(defvar *path-to-goal* '(s s s s))
(defvar *all-algorithmic-paths* nil)
(defvar *mission-execution-scenario* nil)
(defvar *all-human-directed-paths* '((X X) (X F) (X S) (F X X) (F X F) (F X S) (F F X) (F F F) (F F S) (F S X) (F S F)
  (F S S) (S X X) (S X F) (S X S) (S F) (S S X X) (S S X F) (S S X S) (S S F X) (S S F F) (S S F S) (S S S X) (S S S F)
  (S S S S)))    ;From 6/22/20 results emailed to NPS team.
(defvar agent1)
(defvar agent2)
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

(defclass human-search-agent ()
  ((agent-response :accessor agent-response :initform nil)
   (current-execution-phase :accessor current-execution-phase :initform 'phase1)
   (successor-list-index :accessor successor-list-index :initform 1)
   (current-outcome-sequence :accessor current-outcome-sequence :initform nil)))

(defclass DAG-find-all-paths-agent ()
  ((current-search-phase :accessor current-search-phase :initform 'phase1)
   (successor-list :accessor successor-list :initform nil)
   (successor-list-index :accessor successor-list-index :initform 0)
   (all-paths-to-frontier :accessor all-paths-to-frontier :initform nil)
   (all-paths-to-goal :accessor all-paths-to-goal :initform nil)
   (new-paths-list-length :accessor new-paths-list-length :initform nil)
   (new-paths-list :accessor new-paths-list :initform nil)
   (new-path-segments-list :accessor new-path-segments-list :initform nil)))

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
                    '(("Success." phase2) ("Failure." phase3) ("Exception." phase4)))
  (initialize-phase phase2 "Sample environment!"
                    '(("Success." phase3) ("Failure." phase5) ("Exception." phase4)))
  (initialize-phase phase3 "Search Area B!"  
                    '(("Success." phase4) ("Failure." phase4) ("Exception." phase4)))
  (initialize-phase phase4 "Rendezvous with Vehicle2!"  
                    '(("Success." phase5) ("Failure." phase5) ("Exception." phase5)))
  (initialize-phase phase5 "Proceed to recovery!"
                    '(("Success." "Mission complete.") ("Failed." "Mission aborted.")
                      ("Exception." "Mission aborted."))))



;;;;;;;;;;;;;;;;;;;;;;;;;Human Mission Testing and Validation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;The system function "eval", as used below, obtains pointer (address) for a global object.

(defmethod issue-command ((agent human-search-agent))
  (let* ((phase (current-execution-phase agent))
         (command (command (eval phase))))
    (issue-order command)))

(defmethod ask-result ((agent human-search-agent))
  (let* ((result (ask-outcome))
         (outcome-sequence (current-outcome-sequence agent)))
    (setf (agent-response agent) result
      (current-outcome-sequence agent) (cons result outcome-sequence))))

(defmethod ask-list-result ((agent human-search-agent))
  (let* ((result (ask-list-outcome))
         (outcome-sequence (current-outcome-sequence agent)))
    (setf (agent-response agent) result
          (current-outcome-sequence agent) (cons result outcome-sequence))))

(defmethod set-next-phase ((agent human-search-agent))
  (let* ((phase (current-execution-phase agent))
         (successor-list (successor-list (eval phase)))
         (index (successor-list-index agent))
         (next-phase (second (nth index successor-list))))
    (setf (current-execution-phase agent) next-phase)))

(defmethod set-successor-list-index ((agent human-search-agent))
  (let* ((index (convert-outcome-to-index (agent-response agent))))
    (setf (successor-list-index agent) index)))

(defun execute-phase ()
  (issue-command agent1)
  (ask-result agent1)
  (set-successor-list-index agent1)
  (set-next-phase agent1))

(defun list-execute-phase ()
  (issue-command agent1)
  (ask-list-result agent1)             ;Note that "ask-list-result" accesses outcome sequence
  (set-successor-list-index agent1)    ;list rather than human single digit keyboard entry.
  (set-next-phase agent1))

(defun create-2018-JOE-orders ()
  (setf 2018-JOE-orders (create-5-mission-phases))
  (initialize-2018-mission-phases))

(defun start1 ()
  (create-2018-JOE-orders)
  (setf agent1 (make-instance 'human-search-agent)))

(defun issue-order (command)
  (format t "~A" command)
  (terpri))

(defun ask-outcome ()
  (format t "Is execution outcome success (s), failure (f), or exception (x)?:")
  (let ((value (read)))
       (if (member value '(s f x))  
           value
         (ask-outcome))))

(defun ask-list-outcome ()
  (let ((value (pop *path-to-goal*)))
       (if (member value '(s f x))  
           value)))

(defun execute-mission ()
  (execute-phase)
  (if (equal (current-execution-phase agent1) 'phase5) (issue-command agent1)
    (execute-mission)))

(defun list-execute-mission ()
  (list-execute-phase)
  (if (equal (current-execution-phase agent1) 'phase5) (issue-command agent1)
    (list-execute-mission)))

(defun start ()
  (setf *current-paths-to-goal* nil)
  (start1)
  (start-all)
  'ready)

(defun run1 ()
  (start1)
  (execute-mission)
  (reverse (current-outcome-sequence agent1)))

(defun list-run1 ()
  (start1)
  (list-execute-mission)
  (reverse (current-outcome-sequence agent1)))

(defun run ()
  (pprint (increment-paths-to-goal)))

(defun list-run (outcome-list)
  (setf *path-to-goal* outcome-list)
  (pprint (list-increment-paths-to-goal)))

(defun run-all ()
  (terpri)
  (list-run (pop *current-paths-to-goal*))
  (pop *current-paths-to-goal*)
  (if (not (equal *current-paths-to-goal* nil)) (run-all) 'done))

(defun increment-paths-to-goal ()
  (setf (current-outcome-sequence agent1) (run1))
  (push (reverse (current-outcome-sequence agent1)) *current-paths-to-goal*))

(defun list-increment-paths-to-goal ()
  (setf (current-outcome-sequence agent1) (list-run1))
  (push (reverse (current-outcome-sequence agent1)) *current-paths-to-goal*))

(defun convert-outcome-to-index (x)
  (cond ((equal x 's) 0)
        ((equal x 'f) 1)
        ((equal x 'x) 2)))

(defun cycle-left (x) (append (rest x) (list (first x))))



;;;;;;;;;;;;;;;;;;;;;;Algorithmic Proof of Correctness by Breadth First Exhaustive Search;;;;;;;;;;;;;;;;;;;;;



;DAG means "Directed Acyclic Graph".

(defun update-successor-list-index (index modulus) (mod (1+ index) modulus))
   
(defmethod get-new-path-segment ((agent DAG-find-all-paths-agent))
  (let* ((phase (current-search-phase agent))
         (command (command (eval phase)))
         (successor-list (successor-list (eval phase)))
         (index (successor-list-index agent))
         (outcome (first (nth index successor-list)))
         (next-phase (second (nth index successor-list)))
         (path-segment (list phase command outcome next-phase)))
    (setf (successor-list-index agent) (update-successor-list-index index 3))
    (push path-segment (new-path-segments-list agent))))

(defmethod update-new-path-segments-list ((agent DAG-find-all-paths-agent))
  (setf (new-path-segments-list agent) nil)
  (dotimes (i 3) (get-new-path-segment agent))
  (new-path-segments-list agent))  
 
(defmethod extend-path ((agent DAG-find-all-paths-agent))
  (let* ((all-paths (all-paths-to-frontier agent))
         (new-segment (pop (new-path-segments-list agent)))
         (path (first all-paths)))
    (push (connect path new-segment) (new-paths-list agent))))

(defmethod start-mission-execution-tree ((agent DAG-find-all-paths-agent))
  (setf (new-paths-list agent) (update-new-path-segments-list agent))
  (dotimes (i 3) (store-new-path agent))
  (setf (current-search-phase agent) (flf (all-paths-to-frontier agent)))
  (all-paths-to-frontier agent))  

(defmethod expand-frontier-node ((agent DAG-find-all-paths-agent))
  (update-new-path-segments-list agent)
  (dotimes (i 3) (extend-path agent))
  (pop (all-paths-to-frontier agent))
  (setf (current-search-phase agent) (flf (all-paths-to-frontier agent)))
  (new-paths-list agent))

(defmethod store-new-path ((agent DAG-find-all-paths-agent))
  (let* ((new-path (pop (new-paths-list agent))))
    (if new-path (if (not (equal 'phase5 (fl new-path)))
                     (push new-path (all-paths-to-frontier agent))
                     (push new-path (all-paths-to-goal agent))))))
 
(defmethod extend-all-paths-to-frontier ((agent DAG-find-all-paths-agent))
  (let* ((n (length (all-paths-to-frontier agent))))
    (progn (dotimes (i n) (expand-frontier-node agent))
           (setf (new-paths-list-length agent) (length (new-paths-list agent)))
           (all-paths-to-frontier agent))))

(defmethod advance-execution-tree-frontier ((agent DAG-find-all-paths-agent))
  (extend-all-paths-to-frontier agent)
  (store-all-new-paths agent)
  (setf (current-search-phase agent) (flf (all-paths-to-frontier agent))))

(defmethod store-all-new-paths ((agent DAG-find-all-paths-agent))
  (dotimes (i (new-paths-list-length agent)) (store-new-path agent)))
 
(defun create-agent2 ()
  (setf agent2 (make-instance 'DAG-find-all-paths-agent)))

(defun start-all ()
  (create-2018-JOE-orders)
  (create-agent2)
  (start-mission-execution-tree agent2))

(defun connect (list1 list2)
  (if (equal (first (last list1)) (first list2))
      (append list1 (rest list2))
      list1))

(defun flf (list) (first (last (first list))))

(defun fl (list) (first (last list)))

(defun find-all-paths-to-goal ()
  (start)
  (dotimes (i 3) (advance-execution-tree-frontier agent2)) 'done)

(defun all-paths ()
  (find-all-paths-to-goal))

(defun results ()  
  (pprint (all-paths-to-goal agent2)))

(defun save-results ()  
  (setf *all-algorithmic-paths* (all-paths-to-goal agent2)
        *mission-execution-scenario* (first *all-algorithmic-paths*)))

(defun convert-path-element-to-symbol (x)
  (cond ((equal x "Success.") 's)
        ((equal x "Failure.") 'f)
        ((equal x "Exception.") 'x)))

(defun abbreviate-scenario-element ()
  (let* ((element (pop *mission-execution-scenario*))
         (symbol (convert-path-element-to-symbol element)))
    (list element symbol)))