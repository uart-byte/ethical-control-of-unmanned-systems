from __future__ import annotations # this is necessary to avoid this issue:  https://stackoverflow.com/questions/33837918/type-hints-solve-circular-dependency
# Please use the latest python version (which as of this writing is 3.9.0) to make sure you have all of the security patches!
import sys; assert sys.version_info >= (3, 0, 0) # checks python version (major, minor, patch)
import os
import argparse
from copy import deepcopy
from xml.etree import ElementTree as ET
import typing
from typing import Set, List, Dict, Optional
# import typeguard  # TODO add the required method @decorators to activate typeguard

# Warning about unsafe XML files:
# https://docs.python.org/3/library/xml.html#xml-vulnerabilities
# TODO consider using defusedxml instead of xml.etree:  https://pypi.org/project/defusedxml/

# TODO better checking of illegal characters within input files, report changes explicitly,
# be careful automatically changing without user permission!

# ---------------------------------------------------------------------------------------- #
#                        Begin Universal Mission Execution Engine                          #
# ---------------------------------------------------------------------------------------- #


# The idea with this Command class is that currently it only contains a human readable order for a human to execute manually,
# but in the future it could contain an instruction to a robot instead.
# This can be achieved by replacing the internal details of the Command class without having to affect the code of the MissionExecutionEngine.
class Command:
  human_readable_order: str
  def __init__(self, human_readable_order: str) -> None:
    self.human_readable_order = human_readable_order
  def human_readable_order_indented(self, line_prefix: str = "") -> str:
    return "\n".join([ line_prefix + line for line in self.human_readable_order.splitlines() ])


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
  description: str
  next_phase: Optional[MissionPhase] # next_phase is of type Optional[MissionPhase] because it can be null.  It's null if the current phase is a terminal phase.
  def __init__(self, description: str, next_phase: Optional[MissionPhase]) -> None:
    self.description = description
    self.next_phase = next_phase


class SuccessorList(List[PhaseTransition]):
  NULL_PLACEHOLDER: PhaseTransition = PhaseTransition("NULL", None)
  
  def __init__(self):
    self.append(self.NULL_PLACEHOLDER)
    self.append(self.NULL_PLACEHOLDER)
    self.append(self.NULL_PLACEHOLDER)
    assert len(self) == 3
  
  def validate(self):
    # We need the SuccessorList to have length of exactly 3, corresponding (in order) to OUTCOME_SUCCESS, OUTCOME_FAILURE, and OUTCOME_EXCEPTION.
    assert len(self) == 3
    assert self.NULL_PLACEHOLDER not in self # Make sure the NULL_PLACEHOLDER is not still an element of the list.
    
    is_valid_nonterminal_phase = (self[0].next_phase is not None)\
                             and (self[1].next_phase is not None)\
                             and (self[2].next_phase is not None)
    is_valid_terminal_phase = (self[0].next_phase is None)\
                          and (self[1].next_phase is None)\
                          and (self[2].next_phase is None)
    assert is_valid_nonterminal_phase or is_valid_terminal_phase


class MissionPhase:
  name: str
  command: Command
  succesor_list: SuccessorList
  
  # We need to use a custom "initialize" method instead of the actual constructor ("__init__")
  # because we are passing in subsequent MissionPhases as parameters,
  # which means that if we passed the parameters in the constructor,
  # the user would have to specify the mission in reverse, starting with the terminal_phases and
  # working backward to the first_phase, in order to have object handles to pass into constructors as needed.
  # Using our own "initialize" method instead allows the user to construct the MissionPhase objects in forward order,
  # and then initialize all of them in forward order.
  def initialize(self,   name: str,   command: Command, 
                 upon_success: Optional[MissionPhase],
                 upon_failure: Optional[MissionPhase],
                 upon_exception: Optional[MissionPhase]) -> None:
    self.name = name
    self.command = command
    success_phase_transition: PhaseTransition   = PhaseTransition("Success.",   upon_success)
    failure_phase_transition: PhaseTransition   = PhaseTransition("Failed.",    upon_failure)
    exception_phase_transition: PhaseTransition = PhaseTransition("Exception.", upon_exception)
    self.succesor_list = SuccessorList()
    self.succesor_list[OUTCOME_SUCCESS.index]   = success_phase_transition
    self.succesor_list[OUTCOME_FAILURE.index]   = failure_phase_transition
    self.succesor_list[OUTCOME_EXCEPTION.index] = exception_phase_transition
    self.succesor_list.validate()


# TODO this is a new data structure; document it.
class MissionOrders:
  all_phases: Set[MissionPhase]
  first_phase: MissionPhase
  terminal_phases: Set[MissionPhase]
  
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
    # TODO additional forms of validation, such as:
    #   - Ensuring that there are no cycles. (optional)
    #   - Ensuring that there are no unreachable phases.  (optional but HIGHLY recommended, because an unreachable phase is likely a mistake.)


# TODO document this class.
class RBM:
  strategic_level: MissionExecutionEngine
  tactical_level:  TacticalEngine
  execution_level: ExecutionEngine
  strat_tac_link_busy: bool
  tac_exec_link_busy:  bool
  
  def __init__(self, strategic_level: MissionExecutionEngine, tactical_level: TacticalEngine, execution_level: ExecutionEngine) -> None:
    self.strategic_level = strategic_level
    self.tactical_level  = tactical_level
    self.execution_level = execution_level
    self.strat_tac_link_busy = False
    self.tac_exec_link_busy  = False
  
  def ExecuteMission(self) -> None:
    self.strategic_level.execute_mission()
  
  # Returns whether the command was successfully sent.
  # After calling this function to issue a Command,
  # the Strategic Level of the RBM must next call StratLvl_WaitForOutcomeOfCommand.
  def StratLvl_SendCommandTo_TacLvl(self, command: Command, phase: MissionPhase) -> bool:
    assert self.strat_tac_link_busy == False
    self.strat_tac_link_busy = True
    command_transmitted_successfully: bool = self.tactical_level.carry_out_command(command, phase)
    if not command_transmitted_successfully:
      self.strat_tac_link_busy = False
    return command_transmitted_successfully
  
  # Blocks until the Command issued with StratLvl_SendCommandTo_TacLvl has finished running.
  # Then, returns the Outcome.
  def StratLvl_WaitForOutcomeOfCommand(self) -> Outcome:
    assert self.strat_tac_link_busy == True
    self.strat_tac_link_busy = False
    outcome: Outcome = self.tactical_level.wait_for_outcome_of_command()
    return outcome

  """ Link between tactical_level and execution_level not implemented. """
# end class RBM


# This represents the Strategic Level of the RBM.
class MissionExecutionEngine:
  rbm: RBM
  mission_orders: MissionOrders
  previous_execution_phase: Optional[MissionPhase]  # Optional because it is initially None when the mission first starts, and there is no previous phase yet.
  current_execution_phase: MissionPhase
  current_phase_outcome: Optional[Outcome]
  successor_list_index: int
  
  def __init__(self, mission_orders: MissionOrders) -> None:
    self.mission_orders = mission_orders
    self.previous_execution_phase = None
    self.current_execution_phase = mission_orders.first_phase
    self.current_phase_outcome = None
    self.successor_list_index = -1
  
  def connect_to_RBM(self, rbm: RBM) -> None:
    self.rbm = rbm
  
  def execute_phase(self, is_terminal: bool) -> None:
    if is_terminal: assert self.current_execution_phase in self.mission_orders.terminal_phases
    else:           assert self.current_execution_phase not in self.mission_orders.terminal_phases
    
    command_acknowledged: bool = \
    self.rbm.StratLvl_SendCommandTo_TacLvl(self.current_execution_phase.command,
                                           self.current_execution_phase)
    
    outcome: Outcome
    if command_acknowledged:
      outcome = self.rbm.StratLvl_WaitForOutcomeOfCommand()
    else:
      outcome = OUTCOME_EXCEPTION
    self.current_phase_outcome = outcome
    
    if not is_terminal:
      self.set_successor_list_index(outcome)
      self.set_next_phase()
  
  def set_successor_list_index(self, external_agent_response: Outcome) -> None:
    self.successor_list_index = external_agent_response.index
  
  def set_next_phase(self) -> None:
    succesor_list: SuccessorList = self.current_execution_phase.succesor_list
    phase_transition: PhaseTransition = succesor_list[self.successor_list_index]
    self.previous_execution_phase = self.current_execution_phase
    
    assert phase_transition.next_phase is not None
    self.current_execution_phase  = phase_transition.next_phase
    self.current_phase_outcome = None
    self.successor_list_index = -1

  def execute_mission(self) -> None:
    self.mission_orders.validate()
    while self.current_execution_phase not in self.mission_orders.terminal_phases:
      self.execute_phase(is_terminal=False)
    self.execute_phase(is_terminal=True)
  
# end class MissionExecutionEngine


class TacticalEngine: # Abstract base class.
  def connect_to_RBM(self, rbm: RBM) -> None:
    raise NotImplementedError()
  def carry_out_command(self, command: Command, phase: MissionPhase) -> bool: # Return true if the Command was successfully received & understood.
    raise NotImplementedError()
  def wait_for_outcome_of_command(self) -> Outcome:
    raise NotImplementedError()


class HumanInTheLoopTacticalEngine(TacticalEngine):
  rbm: RBM
  current_execution_phase: MissionPhase
  
  def connect_to_RBM(self, rbm: RBM) -> None:
    self.rbm = rbm
  
  def carry_out_command(self, command: Command, phase: MissionPhase) -> bool: # Return true if the Command was successfully received & understood.
    self.current_execution_phase = phase
    print("\n-----------------------------------------------------------------------\n")
    print(command.human_readable_order)
    print() # avoid special characters for portability
    return True # command received & acknowledged
  
  def wait_for_outcome_of_command(self) -> Outcome:
    while True:
      user_input: str = input("Did " + self.current_execution_phase.name + " succeed (s), fail (f), or abort (x)?  ")
      possible_outcome: Outcome
      for possible_outcome in [OUTCOME_SUCCESS, OUTCOME_FAILURE, OUTCOME_EXCEPTION]:
        if user_input == possible_outcome.name:
          return possible_outcome


# Uses depth-first search to exhaustively test the mission.
class ExhaustiveTestingMockTacticalEngine(TacticalEngine):
  rbm: RBM
  path_currently_being_explored: str
  fully_explored_paths: Dict[str, bool]
  did_explore_all_paths_in_this_mission: bool
  phases_leading_to_potential_cycle: List[MissionPhase] # For detecting cycles (a.k.a. infinite loops)
  this_path_is_cycle: bool
  phase_just_before_cycle_gets_closed: Optional[MissionPhase]
  phase_closing_cycle: Optional[MissionPhase]
  cycle_closing_point_step_number: Optional[int]
  all_paths_count: int
  infinite_loop_paths_count: int
  max_runs_limit: Optional[int]
  printing_function: function
  
  def __init__(self, printing_function: function, max_runs_limit: Optional[int]):
    self.path_currently_being_explored = ""
    self.fully_explored_paths = {}
    self.did_explore_all_paths_in_this_mission = False
    self.phases_leading_to_potential_cycle = []
    self.this_path_is_cycle = False
    self.phase_just_before_cycle_gets_closed = None
    self.phase_closing_cycle = None
    self.cycle_closing_point_step_number = None
    self.all_paths_count = 0
    self.infinite_loop_paths_count = 0
    self.max_runs_limit = max_runs_limit
    self.printing_function = printing_function
  
  def connect_to_RBM(self, rbm: RBM) -> None:
    self.rbm = rbm
  
  def carry_out_command(self, command: Command, phase: MissionPhase) -> bool: # Return true if the Command was successfully received & understood.
    if self.this_path_is_cycle:
      pass # Do not append phase to phases_leading_to_potential_cycle because we want to leave phases_leading_to_potential_cycle ending with the phase that closes the cycle.
    else:
      if phase in self.phases_leading_to_potential_cycle:
        self.this_path_is_cycle = True
        self.phase_closing_cycle = phase
        self.phase_just_before_cycle_gets_closed = self.phases_leading_to_potential_cycle[-1] # We have not yet appended phase to phases_leading_to_potential_cycle (that happens in the next line below), so currently the phase right below the cycle gets closed is the last element of phases_leading_to_potential_cycle
        i: int
        for i in range(0, len(self.phases_leading_to_potential_cycle)):
          if self.phases_leading_to_potential_cycle[i] == phase:
            self.cycle_closing_point_step_number = i
            break
      self.phases_leading_to_potential_cycle.append(phase)
    
    # self.printing_function(command.human_readable_order)
    return True # command received & acknowledged
  
  def wait_for_outcome_of_command(self) -> Outcome:
    outcome: Outcome
    chose_a_new_outcome: bool = False
    
    if self.this_path_is_cycle:
      outcome = OUTCOME_EXCEPTION  # Keep returning OUTCOME_EXCEPTION to get out of the infinite loop and escape the mission.  If even the damn exception handler is an infinite loop we are well and truly screwed and the program will hang, but God help us if someone wrote a mission that bad.
      chose_a_new_outcome = True
    else:
      possible_outcome: Outcome
      for possible_outcome in [OUTCOME_SUCCESS, OUTCOME_FAILURE, OUTCOME_EXCEPTION]:
        
        theoretical_path: str = self.path_currently_being_explored + possible_outcome.name
        if self._did_fully_explore_path(theoretical_path):
          continue # try the next possible_outcome
        else:
          outcome = possible_outcome
          chose_a_new_outcome = True
          break
    
    assert chose_a_new_outcome
    self.path_currently_being_explored += outcome.name
    
    return outcome
  
  # Special methods used for the depth-first-search based exhaustive testing:
  
  def did_complete_all_runs(self):
    return self.did_explore_all_paths_in_this_mission
    
  def should_abort(self):
    return self.max_runs_limit is not None \
    and    self.all_paths_count >= self.max_runs_limit
  
  def on_abort(self):
    # This should be called when the program is going to stop running this mission any more times, but did_complete_all_runs is False.
   self.printing_function("Aborted early, after " + str(self.all_paths_count) + " paths tested, of which " + str(self.infinite_loop_paths_count) + " contain infinite loops.")
    
  def mission_terminated(self):
    path_just_explored: List[Outcome] = self.path_currently_being_explored
    if self.this_path_is_cycle:
      # Truncate path_just_explored so it ends right at the point where the cycle wrapped around.
      path_just_explored = path_just_explored[:len(self.phases_leading_to_potential_cycle) - 1]
    self.fully_explored_paths[path_just_explored] = True

    self.all_paths_count += 1
    if self.this_path_is_cycle:
      self.infinite_loop_paths_count += 1
      
      last_outcome_letter: str = path_just_explored[-1]  # the last outcome which resulted in closing the loop.  A lower-case "s", "f", or "x".
      inf_loop_description: str = ""
      i: int = 0
      for i in range(0, len(path_just_explored)):
        letter: str = path_just_explored[i]
        if i >= self.cycle_closing_point_step_number:  letter = letter.upper()  # Make the looping part of the sequence be rendered in upper case, so it's easy to visualize.
        if i > 0:  inf_loop_description += " "
        inf_loop_description += letter
      self.printing_function("\nINFINITE LOOP DETECTED, CAUSED BY OUTCOME SEQUENCE:  " + inf_loop_description + "\n" + \
                             self.phase_just_before_cycle_gets_closed.command.human_readable_order_indented("    => ") + \
                             '\nUPON OUTCOME ' + last_outcome_letter + ', LOOPS BACK TO:\n' +
                             self.phase_closing_cycle.command.human_readable_order_indented("    <= ") + \
                             "\nWHICH (counting starting with initial phase = step # 1) was previously visited at step # " + str(1 + self.cycle_closing_point_step_number))
    
    # Backtrack & mark any fully-explored levels of the paths tree as having been explored.
    up: str = path_just_explored
    while len(up) > 0:
      # assert len(OUTCOME_SUCCESS.name) == 1 and len(OUTCOME_FAILURE.name) == 1 and len(OUTCOME_EXCEPTION.name) == 1
      up = up[:-1]
      if  self._did_fully_explore_path(up + OUTCOME_SUCCESS.name)  \
      and self._did_fully_explore_path(up + OUTCOME_FAILURE.name)  \
      and self._did_fully_explore_path(up + OUTCOME_EXCEPTION.name):
        #
        self.fully_explored_paths[up] = True
        # Try to mitigate the issue of running out of memory and crashing as the fully_explored_paths dictionary grows:
        del self.fully_explored_paths[up + OUTCOME_SUCCESS.name]
        del self.fully_explored_paths[up + OUTCOME_FAILURE.name]
        del self.fully_explored_paths[up + OUTCOME_EXCEPTION.name]
      else:
        break
    # end while
    if len(up) == 0:
      self.did_explore_all_paths_in_this_mission = True
      if self.infinite_loop_paths_count > 0:
        self.printing_function("\n" + str(self.all_paths_count) + " paths tested, of which " + str(self.infinite_loop_paths_count) + " contain infinite loops.")
      else:
        self.printing_function(str(self.all_paths_count) + " paths tested; no infinite loops.")
    
    # Erase mission state, in preparation for the next time (if any) that the mission is run.
    self.path_currently_being_explored = ""
    self.phases_leading_to_potential_cycle = []
    self.this_path_is_cycle = False
    self.phase_just_before_cycle_gets_closed = None
    self.phase_closing_cycle = None
    self.cycle_closing_point_step_number = None
    # done.
  
  # Private
  def _did_fully_explore_path(self, path: str):
    if path in self.fully_explored_paths and self.fully_explored_paths[path] == True:
      return True
    else:
      return False



class ExecutionEngine: # Abstract base class.
  rbm: RBM
  def connect_to_RBM(self, rbm: RBM) -> None:
    self.rbm = rbm

# NullExecutionEngine is a placeholder to use for now since we have not implemented any ExecutionEngines.
# A real ExecutionEngine would be e.g. a drone flight controller.
# For more information about this layer of the RBM stack, read this paper: https://calhoun.nps.edu/handle/10945/44438
class NullExecutionEngine(ExecutionEngine):
  rbm: RBM
  def connect_to_RBM(self, rbm: RBM) -> None:
    self.rbm = rbm


# ---------------------------------------------------------------------------------------- #
#                         End Universal Mission Execution Engine                           #
# ---------------------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------------------- #
#               Begin AVCL(XML) Mission Specification File Loading and Parsing             #
# ---------------------------------------------------------------------------------------- #


RoutableFilename = typing.NewType("RoutableFilename", str) # A path to a file we can locate. (A relative path or absolute path)
FileBasename = typing.NewType("FileBasename", str)         # Unlike RoutableFilename, FileBasename doesn't tell you how to get to the file (doesn't tell you what folder it's in).  The file could live anywhere on any disk or server.


# USER SHOULD PROVIDE THE AVCL_MISSIONS_FOLDER PATH BASED ON THEIR SYSTEM'S FILE LAYOUT:
PYTHON_SCRIPT_DIR:    RoutableFilename = RoutableFilename(os.path.dirname(os.path.realpath(__file__)))
AVCL_MISSIONS_FOLDER: RoutableFilename = RoutableFilename(os.path.abspath(\
os.path.join(PYTHON_SCRIPT_DIR, "..", "avcl")))
# ^ This is the path to the folder where the AVCL mission definition files are located.




# Speed optimization
printing_buffer: str = ""
printing_buffer_line_ct = 0
PRINTING_BUFFER_FLUSH_AFTER_LINES: int = 100
def buffered_print(s: str):
  global printing_buffer, printing_buffer_line_ct
  printing_buffer += (s + "\n")
  printing_buffer_line_ct += 1
  if printing_buffer_line_ct >= PRINTING_BUFFER_FLUSH_AFTER_LINES:
    flush_buffered_print()
def flush_buffered_print():
  global printing_buffer
  print(printing_buffer, end="")
  # print(str(printing_buffer.encode("utf8")), end="") # TODO check how to best do this
  printing_buffer = ""
  printing_buffer_line_ct = 0


# Returns a string of the path to the selected AVCL(XML) file.
def ask_user_for_mission_file() -> RoutableFilename:
  print("Looking in this folder for AVCL files:")
  print(AVCL_MISSIONS_FOLDER)
  print("If you want to look in a different folder, please change the variable AVCL_MISSIONS_FOLDER in MissionExecutionEngine.py \n")
  
  available_missions: List[FileBasename] = \
  [FileBasename(x) for x in os.listdir(AVCL_MISSIONS_FOLDER) if os.path.splitext(x)[1].lower() == ".xml"]
  
  n_missions: int = len(available_missions)
  
  print("FYI:  Instead of using this menu, you can pass the mission filename as a command line argument.")
  print("\nAVAILABLE MISSIONS:")
  for i in range(0, n_missions):
    print("[" + str(i + 1) + "]  " + available_missions[i])
  
  selected_mission: Optional[FileBasename] = None
  while selected_mission is None:
    print("\nSelect a mission.\nInput the number or the filename:  ")
    user_input: str = input()
    try:
      int_selection: int = int(user_input)
      if int_selection in range(1, n_missions + 1):
        selected_mission = available_missions[int_selection - 1]
        break
    except ValueError: pass   # user_input is not an int.  That's fine, keep going; we'll see if it's a filename instead.
    possible_mission_name: FileBasename
    for possible_mission_name in available_missions:
      if user_input.lower() == possible_mission_name.lower() \
      or user_input.lower() + ".xml" == possible_mission_name.lower():
        selected_mission = possible_mission_name
        break
    if selected_mission is None:
      print("\nInvalid selection.")
  print()
  
  assert selected_mission in available_missions
  selected_mission_path: RoutableFilename = RoutableFilename(os.path.join(AVCL_MISSIONS_FOLDER, selected_mission))
  return selected_mission_path


class FailedToStartRBM(Exception):
  pass # This is a catch-all exception which means that the RBM cannot start for any reason.

class InvalidAVCL(FailedToStartRBM):
  pass # This exception is thrown if the parser is told to parse an invalid AVCL file which is not compliant with the file format spec or which represents an incorrectly defined mission.

# Raises InvalidAVCL if requested node is missing.
def traverse_AVCL_XML_tree(parent_element: ET.Element, *node_names: str) -> ET.Element:
  node: Optional[ET.Element] = parent_element
  for name in node_names:
    assert node is not None # This is to make the type checker happy about us calling .find
    node = node.find(name)
    if node is None:  raise InvalidAVCL("Element " + name + " is missing.")
  assert node is not None # This is to make the type checker happy about us returning a non-optional ET.Element.
  return node

# If the file can successfully be parsed, returns a MissionOrders object.
# If not, prints debug information to stdout and throws an InvalidAVCL exception.
def load_mission_orders_from_avcl_file(avcl_mission_filename: RoutableFilename, validate: bool) -> MissionOrders:
  # Keywords from the file format spec: https://savage.nps.edu/Savage/AuvWorkbench/AVCL/AVCL.html
  NODE_BODY: str = "body"
  NODE_MISSION_PREPARATION: str = "MissionPreparation"
  NODE_AGENDA_MISSIONS: str = "AgendaMission"
  NODE_GOAL_LIST: str = "GoalList"
  ATTRIB_NAME: str = "id"
  ATTRIB_NEXT_ON_SUCCESS: str = "nextOnSuccess"
  ATTRIB_NEXT_ON_FAILURE: str = "nextOnFailure"
  ATTRIB_NEXT_ON_EXCEPTION: str = "nextOnException"
  ATTRIB_ORDERS_TITLE: str = "title"
  ATTRIB_ORDERS_DESCRIPTION: str = "description"
  ATTRIB_MISSION_SEGMENT: str = "phase"  # The use of the word "phase" here in the AVCL file format spec is a misnomer.  This attribute is not unique to a particular mission phase, but rather is shared between a group of similar phases (what we are calling a "mission segment").
  
  # Parse the file.
  try:
    xml_tree: ET.ElementTree = ET.parse(avcl_mission_filename)
    xml_root: ET.Element = xml_tree.getroot()
    xml_goal_list: ET.Element = traverse_AVCL_XML_tree(\
      xml_root, NODE_BODY, NODE_MISSION_PREPARATION, NODE_AGENDA_MISSIONS, NODE_GOAL_LIST)
    goal_list: List[ET.Element] = list(xml_goal_list)  # This syntax gets xml_goal_list's child Elements
    phases_by_name: Dict[str, MissionPhase] = {}
    
    # Outputs from parsing
    all_phases:      Set[MissionPhase] = set()
    terminal_phases: Set[MissionPhase] = set()
    first_phase:         MissionPhase

    # Create blank new phases
    xml_phase: ET.Element
    for xml_phase in goal_list:
      p1: MissionPhase = MissionPhase()
      p1_name: str = xml_phase.attrib[ATTRIB_NAME]
      if p1_name in phases_by_name:
        exception1_description: str = "Error:  Mission file is invalid; it contains multiple phases with the same name: \n"
        exception1_description += p1_name + "\n\n"
        print(exception1_description, end="")
        # NOT TESTED
        raise InvalidAVCL(exception1_description)
      else:
        phases_by_name[ p1_name ] = p1
    
    # Configure the phases and wire up the directed acyclic graph of PhaseTransitions
    first_phase = phases_by_name[ goal_list[0].attrib[ATTRIB_NAME] ]
    #
    for xml_phase in goal_list:
      p2_attribs: Dict[str, str] = xml_phase.attrib
      p2_name: str = xml_phase.attrib[ATTRIB_NAME]
      p2: MissionPhase = phases_by_name[p2_name]
      
      # Attempt to figure out what type of phase this phase is intended to be, even if the user misconfigured the phase
      PHASE_TYPE_UNKNOWN     = 0
      PHASE_TYPE_NONTERMINAL = 1
      PHASE_TYPE_TERMINAL    = 2
      phase_type: int        = PHASE_TYPE_UNKNOWN
      next_phase_upon_success:   Optional[MissionPhase] = None
      next_phase_upon_failure:   Optional[MissionPhase] = None
      next_phase_upon_exception: Optional[MissionPhase] = None
      #
      if ATTRIB_NEXT_ON_SUCCESS in p2_attribs:
        phase_type = PHASE_TYPE_NONTERMINAL
        next_phase_upon_success = phases_by_name[ p2_attribs[ATTRIB_NEXT_ON_SUCCESS] ]
        #
        if ATTRIB_NEXT_ON_FAILURE in p2_attribs:
          next_phase_upon_failure = phases_by_name[ p2_attribs[ATTRIB_NEXT_ON_FAILURE] ]
        else:
          if validate:
            print("\n||===========================================================================================||")
            print("!!! SEVERE WARNING !!!")
            print("Phase:")
            print("  " + p2_name)
            print("IS MISSING A FAILURE HANDLER!")
            print("The AVCL specification says that if a failure occurs in this phase,")
            print("it will be SILENTLY treated as a success, which can be EXTREMELY DANGEROUS!!!!!!!!")
            print("E.g., if a phase called \"Turn off circuit breakers\" fails but is silently treated as success,")
            print("and the next phase is \"Tell lineman to repair frayed power wire\",")
            print("the lineman could get killed.")
            print("Please consider fixing your AVCL mission definition file to have explicit failure handlers!")
            print("^^^ SEVERE WARNING ^^^")
            print("Are you sure you want to proceed?")
            yn = None
            while yn not in ["y", "n"]:
              yn = input("(y/n) ").lower()
            print("||===========================================================================================||\n")
            if yn != "y":
              print("\n\n\nThank you.\nPlease open your AVCL file \"" + os.path.basename(avcl_mission_filename) + "\" in a mission editor (e.g. \"AUV Workbench\" from the Naval Postgraduate School) or a text editor.")
              print("Please define the attribute:")
              print("  " + ATTRIB_NEXT_ON_FAILURE)
              print("for each one of your mission phases.\n\nThank you.\n\n")
              raise InvalidAVCL("Phase \"" + p2_name + "\" does not have a failure handler defined, and user elects not to proceed.  Please repair the AVCL mission definition!  For safe behavior, all non-terminal phases should have a failure handler.")
          next_phase_upon_failure = next_phase_upon_success
        #
        if ATTRIB_NEXT_ON_EXCEPTION in p2_attribs:
          next_phase_upon_exception = phases_by_name[ p2_attribs[ATTRIB_NEXT_ON_EXCEPTION] ]
        else:
          if ATTRIB_NEXT_ON_FAILURE in p2_attribs:
            # Exception handler is not defined by Failure handler is defined,
            # so we will use the Failure handler to handle Exceptions as well.
            # This is fine and is safe, so we will not give a warning about this.
            next_phase_upon_exception = next_phase_upon_failure
          else:
            # Both Failure and Exception handlers are undefined.
            # We already gave the user a SEVERE WARNING about the lack of a failure handler.
            # Here we will give a smaller warning about the exception handler being missing,
            # since the missing failure handler is the really bad issue.
            if validate:
              print("\nWarning:")
              print("Phase \"" + p2_name + "\" does not have an exception handler defined,")
              print("and because it is also missing a failure hander (see severe warning above),")
              print("then the AVCL specification says that if an exception occurs in this phase,")
              print("it will be SILENTLY treated as a success.")
              print("Please consider fixing your AVCL mission definition file to have explicit failure and exception handlers!")
              print("If you are only going to do one or the other,")
              print("the MOST IMPORTANT thing is to at least define FAILURE handlers for every phase.  Thank you.\n")
            next_phase_upon_exception = next_phase_upon_success
      #
      else:
        # ATTRIB_NEXT_ON_SUCCESS not in p_attribs
        phase_type = PHASE_TYPE_TERMINAL
        
        if ATTRIB_NEXT_ON_FAILURE in p2_attribs \
        or ATTRIB_NEXT_ON_EXCEPTION in p2_attribs:
          exception2_description: str = "Error:  Mission file is invalid.\n"
          exception2_description += "This phase:  " + p2_name + "\n"
          exception2_description +="is a terminal phase (does not have a success handler defined),\n"
          exception2_description += "but has failure and/or exception handlers defined, which a terminal phase is not allowed to have.\n\n"
          print(exception2_description, end="")
          raise InvalidAVCL(exception2_description)
      
      
      # Make sure we have properly defined the phase transitions:
      assert phase_type != PHASE_TYPE_UNKNOWN
      if phase_type == PHASE_TYPE_NONTERMINAL:
        assert next_phase_upon_success is not None
        assert next_phase_upon_failure is not None
        assert next_phase_upon_exception is not None
      else: # PHASE_TYPE_TERMINAL
        assert next_phase_upon_success is None
        assert next_phase_upon_failure is None
        assert next_phase_upon_exception is None
      
      # Wire up the phase object.
      all_phases.add(p2)
      if phase_type == PHASE_TYPE_TERMINAL:  terminal_phases.add(p2)
      
      human_readable_order = "GOAL ID: " + p2_name + "\n"
      if ATTRIB_MISSION_SEGMENT in p2_attribs:  # Optional
        human_readable_order += "MISSION SEGMENT: " + p2_attribs[ATTRIB_MISSION_SEGMENT] + "\n"
      human_readable_order += "ORDERS: \n"
      human_readable_order += "  " + p2_attribs[ATTRIB_ORDERS_TITLE] + "\n"
      human_readable_order += "  " + p2_attribs[ATTRIB_ORDERS_DESCRIPTION]
      
      p2.initialize( name=    p2_name,
                     command= Command(human_readable_order= human_readable_order),
                     upon_success=   next_phase_upon_success,
                     upon_failure=   next_phase_upon_failure,
                     upon_exception= next_phase_upon_exception )
    
    # end for xml_phase in goal_list
    # Done parsing.
    
    # Construct and return MissionOrders object.
    mission_orders = MissionOrders()
    mission_orders.all_phases = all_phases
    mission_orders.first_phase = first_phase
    mission_orders.terminal_phases = terminal_phases
    
    return mission_orders
    
  except (ET.ParseError, AttributeError) as e:
    exception3_description: str = "Error:  Mission file is not a valid AVCL (Autonomous Vehicle Control Language) file. \n"
    exception3_description += "See AVCL file format spec here: \n"
    exception3_description += "https://savage.nps.edu/Savage/AuvWorkbench/AVCL/AVCL.html \n"
    exception3_description += "Internal details of exception inside xml.etree library: \n"
    exception3_description += str(e) + "\n\n"
    print(exception3_description, end="")
    raise InvalidAVCL(exception3_description)

 
# Creates a new RBM stack with the provided avcl_mission_filename as the mission.
# If any issue prevents the creation or running of the RBM stack,
# FailedToStartRBM will be raised.
def run_RBM_with_mission_orders(mission_orders: MissionOrders,
                                # The strategic_level_engine is constructed by run_RBM_with_avcl_mission_file, so we do not allow the caller to provide it.
                                optional_tactical_level_engine_instance:  Optional[TacticalEngine],
                                optional_execution_level_engine_instance: Optional[ExecutionEngine],
                                tactical_level_engine_class:  type,
                                execution_level_engine_class: type) -> None:

  strategic_level_engine:   MissionExecutionEngine    = MissionExecutionEngine(mission_orders)
  
  tactical_level_engine:  TacticalEngine
  if optional_tactical_level_engine_instance is not None:
    assert type(optional_tactical_level_engine_instance) == tactical_level_engine_class
    tactical_level_engine = optional_tactical_level_engine_instance
  else:
    tactical_level_engine = tactical_level_engine_class()
  
  execution_level_engine: ExecutionEngine
  if optional_execution_level_engine_instance is not None:
    assert type(optional_execution_level_engine_instance) == execution_level_engine_class
    execution_level_engine = optional_execution_level_engine_instance
  else:
    execution_level_engine = execution_level_engine_class()
  
  ## CONSTRUCT RBM.
  rational_behavior_model:  RBM  
  rational_behavior_model = RBM(strategic_level_engine, tactical_level_engine, execution_level_engine)
  strategic_level_engine   .connect_to_RBM(rational_behavior_model)
  tactical_level_engine    .connect_to_RBM(rational_behavior_model)
  execution_level_engine   .connect_to_RBM(rational_behavior_model)

  ## RUN RBM.
  rational_behavior_model.ExecuteMission()
  return


# Can raise InvalidAVCL exception which is a subclass of FailedToStartRBM exception.
def exhaustively_test_mission(avcl_mission_filename: RoutableFilename, validate: bool, max_runs_limit: Optional[int]) -> bool:
  print("\nExhaustively testing mission:  " + os.path.abspath(avcl_mission_filename))

  mission_orders: MissionOrders
  try:
    mission_orders = load_mission_orders_from_avcl_file(avcl_mission_filename, validate)
  except InvalidAVCL as e:
    raise e
  
  depth_first_search_exhaustive_tester: ExhaustiveTestingMockTacticalEngine \
  = ExhaustiveTestingMockTacticalEngine(printing_function=buffered_print, \
                                        max_runs_limit=max_runs_limit)
  
  null_execution_engine: NullExecutionEngine  = NullExecutionEngine() # Keeping & reusing one of these to avoid the performance hit from re-instantiating the class over and over during the exhaustive test.
  
  try:
    while (not depth_first_search_exhaustive_tester.did_complete_all_runs()) \
    and   (not depth_first_search_exhaustive_tester.should_abort()):
      run_RBM_with_mission_orders(mission_orders                           =mission_orders,
                                  optional_tactical_level_engine_instance  =depth_first_search_exhaustive_tester,
                                  optional_execution_level_engine_instance =null_execution_engine,
                                  tactical_level_engine_class              =ExhaustiveTestingMockTacticalEngine,
                                  execution_level_engine_class             =NullExecutionEngine)
      # Done running mission.
      depth_first_search_exhaustive_tester.mission_terminated()
    # end while
    if not depth_first_search_exhaustive_tester.did_complete_all_runs():
      depth_first_search_exhaustive_tester.on_abort()
  #
  except KeyboardInterrupt:
    # If the user is doing --exhaustively-test-all-standard-library-missions, it's convenient to be able to KeyboardInterrupt just one mission that is taking forever to test, without quitting the whole program, so that the program can go on to test the other missions.
    buffered_print("KeyboardInterrupt")
    depth_first_search_exhaustive_tester.on_abort()
  
  # Done with all runs.
  flush_buffered_print()
  successfully_completed_exhaustive_test = depth_first_search_exhaustive_tester.did_complete_all_runs()
  return successfully_completed_exhaustive_test


def main():
  
  parser: argparse.ArgumentParser             = argparse.ArgumentParser(      allow_abbrev=False,  description="Mission Execution Engine for the Rational Behavior Model system for ethical control of autonomy.  See project website at:  https://savage.nps.edu/EthicalControl/")
  group1: argparse._MutuallyExclusiveGroup    = parser.add_mutually_exclusive_group()
  parser .add_argument("--validate",            action="store_true",          help="Warn the user if the mission is dangerously under-specified.")
  parser .add_argument("--exhaustive-testing-max-runs-per-mission",           type=int,            help="Sets the exhaustive tester to abort testing a mission after testing a certain number of runs through the mission.  This can be useful if your mission is large and would take days, years, or decades to exhaustively test.")
  parser .add_argument("--exhaustively-test",   action="store_true",          help="Exhaustively test the selected mission file, using depth-first search to exhaustively run through all possible mission flows.")
  group1 .add_argument("--exhaustively-test-all-standard-library-missions",   action="store_true", help="Exhaustively test every mission in the standard mission library folder " + AVCL_MISSIONS_FOLDER + ".  If you use this option you should not specify a specific mission filename.") 
  group1 .add_argument("mission_file_path",     nargs="?")    # If no default value is specified,  the default is None.  The default type is essentially Optional[str], because either a str or None will be returned.
  
  args:   argparse.Namespace                =   parser.parse_args()
  validate: bool                            = ( args.validate == True )
  exhaustive_test: bool                     = ( args.exhaustively_test == True ) or ( args.exhaustively_test_all_standard_library_missions == True )
  loop_over_all_standard_lib_missions: bool = ( args.exhaustively_test_all_standard_library_missions == True )
  exhaustive_testing_max_runs_per_mission: Optional[int] =  args.exhaustive_testing_max_runs_per_mission
  avcl_mission_filename: RoutableFilename
  
  print()
  print("For help, run:\npython MissionExecutionEngine.py --help\n")

  
  if loop_over_all_standard_lib_missions:
    assert exhaustive_test == True
    
    all_mission_file_basenames: List[FileBasename] = \
    [FileBasename(x) for x in os.listdir(AVCL_MISSIONS_FOLDER) if os.path.splitext(x)[1].lower() == ".xml"]
    
    all_mission_file_paths: List[RoutableFilename] = \
    [RoutableFilename(os.path.join(AVCL_MISSIONS_FOLDER, x)) for x in all_mission_file_basenames]

    missions_exhaustively_tested: List[RoutableFilename] = []
    missions_not_exhaustively_tested: List[RoutableFilename] = []
    mission_num: int = 1
    for avcl_mission_filename in all_mission_file_paths:
      try:
        completed_exhaustive_test: bool =  exhaustively_test_mission( avcl_mission_filename, validate, exhaustive_testing_max_runs_per_mission )
        if completed_exhaustive_test:  missions_exhaustively_tested.append(avcl_mission_filename)
        else:                          missions_not_exhaustively_tested.append(avcl_mission_filename)

      except FailedToStartRBM as e:
        # At this point, the error "e" will have already been printed, so we do not need to print it again.
        assert mission_num >= 1
        sys.exit(mission_num)  # Using the mission number of the failed mission as the return code, so if e.g. the program closes with return code of 5, you know that mission 5 is the one that had the issue.
    
      mission_num += 1    
    # end for avcl_mission_filename
    
    n_exhaustive_tests_aborted: int = len(missions_not_exhaustively_tested)
    print("\n\n")
    if len(missions_not_exhaustively_tested) == 0:
      print("============================ SUCCEEDED IN EXHAUSTIVELY TESTING ALL " + str(len(missions_exhaustively_tested)) + " OF THE STANDARD LIBRARY MISSIONS: =======================")
      for avcl_mission_filename in missions_exhaustively_tested:  print(str(avcl_mission_filename))
    else:
      print("============================== SUCCEEDED IN EXHAUSTIVELY TESTING " + str(len(missions_exhaustively_tested)) + " OF THE STANDARD LIBRARY MISSIONS: =========================")
      for avcl_mission_filename in missions_exhaustively_tested:  print(str(avcl_mission_filename))
      print("\n================ FAILED TO EXHAUSTIVELY TEST " + str(len(missions_not_exhaustively_tested)) + " OF THE STANDARD LIBRARY MISSIONS DUE TO TIMEOUT OR USER ABORT: ================")
      for avcl_mission_filename in missions_not_exhaustively_tested:  print(str(avcl_mission_filename))
      
  
  else:  # loop_over_all_standard_lib_missions == False
  
    if args.mission_file_path is not None:  avcl_mission_filename = args.mission_file_path
    else:  avcl_mission_filename = ask_user_for_mission_file()
    
    try:
      if exhaustive_test:
        exhaustively_test_mission(avcl_mission_filename, validate, exhaustive_testing_max_runs_per_mission)
      else:
        print("\nSelected mission:  " +    os.path.abspath(avcl_mission_filename) + "\n")

        mission_orders: MissionOrders  = load_mission_orders_from_avcl_file(avcl_mission_filename, validate)
        
        run_RBM_with_mission_orders(mission_orders                           =mission_orders,
                                    optional_tactical_level_engine_instance  =None,
                                    optional_execution_level_engine_instance =None,
                                    tactical_level_engine_class              =HumanInTheLoopTacticalEngine,
                                    execution_level_engine_class             =NullExecutionEngine)
        # Done running mission.
                                                                          
    except FailedToStartRBM as e:
      # At this point, the error will have already been printed, so we do not need to print it again.
      sys.exit(1)
  
  # end if/else loop_over_all_standard_lib_missions
  
  print("\n\n")
  sys.exit(0)
# end function main.


if __name__ == "__main__":  # <-- this line is a standard python idiom to see if we are running as a user-invoked shell script (rather than having been loaded as a submodule of some larger program)
  main()
 