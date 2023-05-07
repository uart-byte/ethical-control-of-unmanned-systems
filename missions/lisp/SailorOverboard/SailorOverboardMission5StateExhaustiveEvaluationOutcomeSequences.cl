;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: 21 October 2019.

;Comment1: As of the time of this writing, the default Allegro IDE editor window print
;font is oversized, and does not print a full-width copy of the IDE Debug or Editor
;windows. To fix this, go to the top of the Allegro opening screen and click "Tools".
;Then, from the drop-down menu, select "Options". Next, on the resulting "Options"
;pop-up screen, select, "Fonts". Then click on "Editor Printouts". When a window pops up
;asking "Revert to the default fixed-width font?", answer "No". A "Font" window will
;pop up. In this window, scroll down fonts to "Courier New". Scroll "Print Size" to "9".
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


(defvar agent1)
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

(defun create-5-mission-phases ()
  (setf phase1 (make-instance 'mission-phase)
        phase2 (make-instance 'mission-phase)
        phase3 (make-instance 'mission-phase)
        phase4 (make-instance 'mission-phase)
        phase5 (make-instance 'mission-phase)))

(defun create-2019-SOB-mission-orders ()
  (setf 2019-SOB-mission-orders (create-5-mission-phases))
  (initialize-2019-SOB-mission-phases))
 
(defclass DAG-find-all-paths-agent ()
  ((current-search-phase :accessor current-search-phase :initform 'phase1)
   (successor-list :accessor successor-list :initform nil)
   (successor-list-index :accessor successor-list-index :initform 0)
   (all-paths-to-frontier :accessor all-paths-to-frontier :initform nil)
   (all-paths-to-goal :accessor all-paths-to-goal :initform nil)
   (new-paths-list-length :accessor new-paths-list-length :initform nil)
   (new-paths-list :accessor new-paths-list :initform nil)
   (new-path-segments-list :accessor new-path-segments-list :initform nil)))

;DAG means "Directed Acyclic Graph".

(defun update-successor-list-index (index modulus) (mod (1+ index) modulus))
   
(defmethod initialize-phase ((phase mission-phase) command successor-list)
  (setf (command phase) command
        (successor-list phase) successor-list))

;The system function "eval", as used below, obtains pointer (address) for a global object.

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
 
(defun create-agent1 ()
  (setf agent1 (make-instance 'DAG-find-all-paths-agent)))

(defun initialize-2019-SOB-mission-phases ()
  (initialize-phase phase1 "Deploy!"  
                    '(("Success." phase2) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase2 "Rendezvous with Sailor!"
;;;                 '(("Success." phase4) ("Failed." phase3) ("Exception." phase4))) ; note initial mistake phase4 vice phase5
                    '(("Success." phase4) ("Failed." phase3) ("Exception." phase5))) ; note correctly defined phase5
  (initialize-phase phase3 "Search for Sailor!"  
                    '(("Success." phase4) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase4 "Track Sailor Afloat Until Safe!"  
                    '(("Success." phase5) ("Failed." phase5) ("Exception." phase5)))
  (initialize-phase phase5 "Proceed to recovery!"
                    '(("Success." "Mission complete.") ("Failed." "Mission aborted.")
                      ("Exception." "Mission aborted."))))

(defun start ()
  (create-2019-SOB-mission-orders)
  (create-agent1)
  (start-mission-execution-tree agent1))

(defun connect (list1 list2)
  (if (equal (first (last list1)) (first list2))
      (append list1 (rest list2))
      list1))

(defun flf (list) (first (last (first list))))

(defun fl (list) (first (last list)))

(defun find-all-paths-to-goal ()
  (start)
  (dotimes (i 3) (advance-execution-tree-frontier agent1))
  (pprint (all-paths-to-goal agent1)))

(defun run ()
  (find-all-paths-to-goal))
