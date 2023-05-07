;This code written in ANSI Common Lisp, Allegro 10.1 enhancement, from Franz, Inc., by
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School,
;Monterey, CA 93943. Date of latest update: 7 October 2019.

(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog) ;Start Prolog.

;Mission specification

(<-- (execute_phase 1) (command "Phase 1 report: Deploy") (update_outcome)
                      (current_phase_outcome s) (change_phase 1 2))
(<- (execute_phase 1) (current_phase_outcome f) (change_phase 1 5))
(<- (execute_phase 1) (current_phase_outcome x) (change_phase 1 5))

(<- (execute_phase 2) (command "Phase 2 report: Rendezvous with Sailor") (update_outcome)
                      (current_phase_outcome s) (change_phase 2 4))
(<- (execute_phase 2) (current_phase_outcome f) (change_phase 2 3))
(<- (execute_phase 2) (current_phase_outcome x) (change_phase 2 4))

(<- (execute_phase 3) (command "Phase 3 report: Search for Sailor") (update_outcome)
                      (current_phase_outcome s) (change_phase 3 4))
(<- (execute_phase 3) (current_phase_outcome f) (change_phase 3 5))
(<- (execute_phase 3) (current_phase_outcome x) (change_phase 3 5))

(<- (execute_phase 4) (command "Phase 4 report: Track Sailor Afloat Until Safe") (update_outcome)
                      (current_phase_outcome s) (change_phase 4 5))
(<- (execute_phase 4) (current_phase_outcome f) (change_phase 4 5))
(<- (execute_phase 4) (current_phase_outcome x) (change_phase 4 5))

(<- (execute_phase 5) (command "Phase 5 report: Proceed to recovery") (update_outcome)
                      (current_phase_outcome s) (change_phase 5 'mission_complete) (report "Phase 6 report: Vehicle recovered"))
(<- (execute_phase 5) (current_phase_outcome f) (change_phase 5 'mission_abort)    (report "Phase 7 report: Vehicle lost"))
(<- (execute_phase 5) (current_phase_outcome x) (change_phase 5 'mission_abort)    (report "Phase 8 report: Awaiting orders"))
 