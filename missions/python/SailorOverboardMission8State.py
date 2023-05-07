from __future__ import annotations # this is necessary to avoid this issue:  https://stackoverflow.com/questions/33837918/type-hints-solve-circular-dependency
# Please use the latest python version (which as of this writing is 3.8.5) to make sure you have all of the security patches!
import sys;  assert sys.version_info >= (3, 8, 5)
import typing
from typing import Set, List
from copy import deepcopy
# import typeguard  # TODO add the required method @decorators to activate typeguard


"""

This is a python translation of the August 13th version of Prof. McGhee's lisp file "SailorOverboardMission8State.cl".
This August 13th version corresponds to git commit fd098c05805d9f4262438695e55cf634963c68b0

"""




# ---------------------------------------------------------------------------------------- #
#                        Begin Universal Mission Execution Engine                          #
# ---------------------------------------------------------------------------------------- #


# The idea with this Command class is that currently it only contains a human readable order for a human to execute manually,
# but in the future it could contain an instruction to a robot instead.
# This can be achieved by replacing the internal details of the Command class without having to affect the code of the MissionExecutionEngine.
class Command:
  human_readable_order: str = None
  def __init__(self, human_readable_order: str) -> None:
    self.human_readable_order = human_readable_order

class Outcome:
  name: str  # "s", "f", or "x"
  index: int #  0 ,  1 , or  2  respectively.
  def __init__(self, name: str, index: int) -> None:
    self.name = name
    self.index = index
#
OUTCOME_SUCCESS: Outcome   = Outcome("s", 0)
OUTCOME_FAILURE: Outcome   = Outcome("f", 1)
OUTCOME_EXCEPTION: Outcome = Outcome("x", 2)

# An allowed transition between one MissionPhase and a valid subsequent phase.
class PhaseTransition:
  description: str = None
  next_phase: MissionPhase = None
  def __init__(self, description: str, next_phase: MissionPhase) -> None:
    self.description = description
    self.next_phase = next_phase

SuccessorList = List[PhaseTransition]
def SuccessorListCtr() -> SuccessorList: # Ctr means Constructor
  # We need a length of exactly 3, corresponding (in order) to OUTCOME_SUCCESS, OUTCOME_FAILURE, and OUTCOME_EXCEPTION.
  template: SuccessorList = [None, None, None]
  return deepcopy(template)

class MissionPhase:
  command: Command = None
  succesor_list: SuccessorList = None
  
  # We need to use a custom "initialize" method instead of the actual constructor ("__init__")
  # because we are passing in subsequent MissionPhases as parameters,
  # which means that if we passed the parameters in the constructor,
  # the user would have to specify the mission in reverse, starting with the terminal_phases and
  # working backward to the first_phase, in order to have object handles to pass into constructors as needed.
  # Using our own "initialize" method instead allows the user to construct the MissionPhase objects in forward order,
  # and then initialize all of them in forward order.
  def initialize(self, command: Command, upon_success: MissionPhase, upon_failure: MissionPhase, upon_exception: MissionPhase) -> None:
    self.command = command
    success_phase_transition: PhaseTransition = PhaseTransition("Success.",   upon_success)
    failure_phase_transition: PhaseTransition = PhaseTransition("Failed.",    upon_failure)
    exception_phase_transition: PhaseTransition = PhaseTransition("Exception.", upon_exception)
    self.succesor_list = SuccessorListCtr()
    self.succesor_list[OUTCOME_SUCCESS.index] = success_phase_transition
    self.succesor_list[OUTCOME_FAILURE.index] = failure_phase_transition
    self.succesor_list[OUTCOME_EXCEPTION.index] = exception_phase_transition

# TODO this is a new data structure; document it.
class MissionOrders:
  all_phases: Set[MissionPhase] = None
  first_phase: MissionPhase = None
  terminal_phases: Set[MissionPhase] = None
  
  # If validation fails, this method should throw an exception.
  def validate(self) -> None:
    # Validation Step 1:
    # Validate that all terminal phases have no outgoing transitions,
    # and that all non-terminal phases have three outgoing transitions.
    p: MissionPhase
    for p in self.all_phases:
      if p in self.terminal_phases:
        assert p.succesor_list[0].next_phase is None
        assert p.succesor_list[1].next_phase is None
        assert p.succesor_list[2].next_phase is None
      else:
        # non-terminal phase
        assert p.succesor_list[0].next_phase is not None
        assert p.succesor_list[1].next_phase is not None
        assert p.succesor_list[2].next_phase is not None
    
    # Validation Step 2:
    # TODO additional forms of validation, such as ensuring there are no cycles.

class MissionExecutionEngine:
  mission_orders: MissionOrders
  previous_execution_phase: MissionPhase
  current_execution_phase: MissionPhase
  current_phase_outcome: Outcome
  successor_list_index: int
  
  def __init__(self, mission_orders: MissionOrders) -> None:
    self.mission_orders = mission_orders
    self.previous_execution_phase = None
    self.current_execution_phase = mission_orders.first_phase
    self.current_phase_outcome = None
    self.successor_list_index = -1
  
  def execute_phase(self) -> None:
    assert self.current_execution_phase not in self.mission_orders.terminal_phases
    
    was_able_to_issue_command = self.issue_command()
    # TODO in the future, was_able_to_issue_command could be false to indicate a failure to communicate the order, e.g. due to an intranet error.
    outcome: Outcome
    if was_able_to_issue_command:
      outcome = self.ask_outcome()
    else:
      outcome = OUTCOME_EXCEPTION
    self.current_phase_outcome = outcome
    self.set_successor_list_index(outcome)
    self.set_next_phase()
  
  def execute_terminal_phase(self) -> None:
    assert self.current_execution_phase in self.mission_orders.terminal_phases
    self.issue_command()
  
  def set_successor_list_index(self, external_agent_response: Outcome) -> None:
    self.successor_list_index = external_agent_response.index
  
  def set_next_phase(self) -> None:
    succesor_list: SuccessorList = self.current_execution_phase.succesor_list
    phase_transition: PhaseTransition = succesor_list[self.successor_list_index]
    next_phase: MissionPhase = phase_transition.next_phase
    self.previous_execution_phase = self.current_execution_phase
    self.current_execution_phase = next_phase
    self.current_phase_outcome = None
    self.successor_list_index = -1

  def execute_mission(self) -> None:
    self.mission_orders.validate()
    while self.current_execution_phase not in self.mission_orders.terminal_phases:
      self.execute_phase()
    self.execute_terminal_phase()
  
  ###
  # BEGIN INTERFACE BETWEEN MissionExecutionEngine AND ROBOT (OR HUMAN STANDING IN AS THE ROBOT)
  
  # Returns whether the command was successfully issued.
  def issue_command(self):
    # Currently a human is standing in for the robot, so we will issue the order to them.
    english_text_order = self.current_execution_phase.command.human_readable_order
    success = self.issue_order_to_human(english_text_order)
    return success
  
  # Returns whether the order was successfully delivered to the human.  TODO this should return false if the screen is off (if order is conveyed by screen) or the speaker is muted (if the order is conveyed by speaker).
  def issue_order_to_human(self, human_readable_order: str) -> None:
    # Currently the "robot" is a human, so we will issue the order to them by printing it on the screen.
    print(human_readable_order)
    success = True
    return success
  
  def ask_outcome(self) -> Outcome:
    while True:
      user_input: str = input("Did goal succeed (s), fail (f), or abort (x)?")
      possible_outcome: Outcome
      for possible_outcome in [OUTCOME_SUCCESS, OUTCOME_FAILURE, OUTCOME_EXCEPTION]:
        if user_input == possible_outcome.name:
          return possible_outcome
  
  # END INTERFACE BETWEEN MissionExecutionEngine AND ROBOT (OR HUMAN STANDING IN AS THE ROBOT)
  ###
  

"""
TODO:  Implement the following optional convenience commands which help the human who is tasked with validating the MissionOrders

(defun rerun (new-start-phase)
  (setf (current-execution-phase mission-controller) new-start-phase)
  (execute-mission))


(defun repeat ()
  (rerun (previous-execution-phase mission-controller)))

(defun refine-last-command ()
  (let* ((previous-phase (previous-execution-phase mission-controller)))
    (cond ((equal previous-phase 'phase1) (rerun 'phase1.1)))))

"""

# ---------------------------------------------------------------------------------------- #
#                         End Universal Mission Execution Engine                           #
# ---------------------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------------------- #
#                      Begin 8-phase Sailor Overboard Mission Orders                       #
# ---------------------------------------------------------------------------------------- #

def run_sailor_overboard_mission():
  all_phases: Set[MissionPhase] = set()
  p: MissionPhase
  p = phase1   = MissionPhase();  all_phases.add(p)
  p = phase1_1 = MissionPhase();  all_phases.add(p)
  p = phase1_2 = MissionPhase();  all_phases.add(p)
  p = phase1_3 = MissionPhase();  all_phases.add(p)
  p = phase2   = MissionPhase();  all_phases.add(p)
  p = phase3   = MissionPhase();  all_phases.add(p)
  p = phase4   = MissionPhase();  all_phases.add(p)
  p = phase5   = MissionPhase();  all_phases.add(p)
  p = phase6   = MissionPhase();  all_phases.add(p)
  p = phase7   = MissionPhase();  all_phases.add(p)
  p = phase8   = MissionPhase();  all_phases.add(p)
  
  
  # First Phase
  
  phase1.initialize( command = Command(
        human_readable_order = "Deploy!" ),
                upon_success = phase2,
                upon_failure = phase8,
              upon_exception = phase8 )
  
  # Intermediate Phases
  
  phase1_1.initialize( command = Command(
          human_readable_order = "Choose Tube and Launch!" ),
                  upon_success = phase1_2,
                  upon_failure = phase8,
                upon_exception = phase8 )
  
  phase1_2.initialize( command = Command(
          human_readable_order = "Enter Water and Get GPS Fix!" ),
                  upon_success = phase1_3,
                  upon_failure = phase8,
                upon_exception = phase8 )
  
  phase1_3.initialize( command = Command(
          human_readable_order = "Descend to Search Depth!" ),
                  upon_success = phase2,
                  upon_failure = phase8,
                upon_exception = phase8 )
  
  phase2.initialize( command = Command(
        human_readable_order = "Rendezvous with Sailor!" ),
                upon_success = phase4,
                upon_failure = phase3,
              upon_exception = phase8 )    # deviation from phase4/phase3/phase5
  
  phase3.initialize( command = Command(
        human_readable_order = "Search for Sailor!" ),
                upon_success = phase4,
                upon_failure = phase8,
              upon_exception = phase8 )
  
  phase4.initialize( command = Command(
        human_readable_order = "Track Sailor Afloat Until Safe!" ),
                upon_success = phase5,
                upon_failure = phase5,
              upon_exception = phase8 )
  
  phase5.initialize( command = Command(
        human_readable_order = "Proceed to recovery!" ),
                upon_success = phase6,
                upon_failure = phase7,
              upon_exception = phase8 )

  # Terminal Phases
  
  phase6.initialize( command = Command(
        human_readable_order = "Halt and prepare for recovery!" ),
                upon_success = None,
                upon_failure = None,
              upon_exception = None )
  
  phase7.initialize( command = Command(
        human_readable_order = "Halt and deploy recovery beacon!" ),
                upon_success = None,
                upon_failure = None,
              upon_exception = None )
  
  phase8.initialize( command = Command(
        human_readable_order = "Halt and await further orders!" ),
                upon_success = None,
                upon_failure = None,
              upon_exception = None )
  
  
  first_phase = phase1
  terminal_phases = set([phase6, phase7, phase8])
  
  mission_orders = MissionOrders()
  mission_orders.all_phases = all_phases
  mission_orders.first_phase = first_phase
  mission_orders.terminal_phases = terminal_phases
  
  engine: MissionExecutionEngine = MissionExecutionEngine( mission_orders )
  engine.execute_mission()
  
run_sailor_overboard_mission()
