# Sailor Overboard UAV Mission

Sailor overboard response is critical for any maritime vessel and can result in lethal outcomes if not performed successfully.  This is an exemplar robot mission corresponding to and complementing human procedures, implemented in the Prolog programming language.

* The [Sailor Overboard UAV Mission figure](SailorOverboardUavMission.2019Aug12b.png) shows mission logic using the ternary approach to goal definition, with each branch resulting from success, failure or exception.
* Robert B. McGhee, [Achieving Fail-Safe and Ethically Constrained Missions for Manned or Unmanned Vehicles Using Exhaustively Testable MEA Software Breakpoints](AchievingFailSafeEthicallyConstrainedMissionsUsingExhaustivelyTestableMeaSoftwareBreakpoints.pdf), Technical Memorandum, Naval Postgraduate School (NPS), August 2019.

Instructions follow for running mission tests.

## Prolog Prerequisites

1. [Allegro CL 10.1](https://franz.com/downloads/clp/survey) by [Franz](https://franz.com/downloads/clp/survey).  We are using the "Free Express Edition."

## Mission Execution Engine (MEE)

The [MissionExecutionEngine.cl](MissionExecutionEngine.cl) is a Prolog program that provides
a reusable set of constructs which, when invoked together with a separate set of 
mission orders, becomes actively executable following compilation by a Prolog compiler.

## Sailor Overboard Mission #1

[SailorOverboardUavMissionOrdersReducedState1.cl](SailorOverboardUavMissionOrdersReducedState1.cl) mission #1
uses a similar but simpler mission structure to demonstrate the basic state changes that occur
when running this mission.  Here are [sample outputs](SailorOverboardUavMissionOrdersReducedState.log.txt).

a. First load file [MissionExecutionEngine.cl](MissionExecutionEngine.cl), the RBM Strategic Level task sequencing algorithm.

b. Compile it (using the Dumptruck icon).  Warnings can be ignored.

c. Similarly load and run [SailorOverboardUavMissionOrdersReducedState1.cl](SailorOverboardUavMissionOrdersReducedState1.cl)

d. Now type in debug window: **(run)**

e. For each goal within the mission, type single-letter responses for (S)uccess, (F)ailure, or Abort(x).

f. Example output file [SailorOverboardUavMissionOrdersReducedState1.log.txt](SailorOverboardUavMissionOrdersReducedState1.log.txt) shows all 21 possible sets of interactions by a human operator.

## Sailor Overboard Mission #2
[SailorOverboardUavMissionOrdersReducedState2.cl](SailorOverboardUavMissionOrdersReducedState2.cl) mission #2
adds further detail to the minimalist mission #1, expanding phase 1 to phases 1.1, 1.2 and 1.3

(User note: you may need to restart Allegro CL for proper operation.)

a. First load file [MissionExecutionEngine.cl](MissionExecutionEngine.cl), the RBM Strategic Level task sequencing algorithm.

b. Compile it (using the Dumptruck icon).  Warnings can be ignored.

c. Similarly load and run [SailorOverboardUavMissionOrdersReducedState2.cl](SailorOverboardUavMissionOrdersReducedState2.cl)

d. Now type in debug window: **(run)**

e. For each goal within the mission, type single-letter responses for (S)uccess, (F)ailure, or Abort(x).

f. Example output file [SailorOverboardUavMissionOrdersReducedState2.log.txt](SailorOverboardUavMissionOrdersReducedState2.log.txt) shows a variety of interactions by a human operator.

3. TODO: full [Sailor Overboard Uav Mission](SailorOverboardUavMission.2019Aug12b.png) mission in Prolog.


## Sailor Overboard Mission #3
[SailorOverboardUavMissionOrdersReducedState3.cl](SailorOverboardUavMissionOrdersReducedState3.cl) mission #3
adds further detail to mission #2, expanding to an 8-phase with a chokepoint at State #5
i.e. single-node predecessor prior to States 6, 7, 8.

(User note: you may need to restart Allegro CL for proper operation.)

a. First load file [MissionExecutionEngine.cl](MissionExecutionEngine.cl), the RBM Strategic Level task sequencing algorithm.

b. Compile it (using the Dumptruck icon).  Warnings can be ignored.

c. Similarly load and run [SailorOverboardUavMissionOrdersReducedState3.cl](SailorOverboardUavMissionOrdersReducedState3.cl)

d. Now type in debug window: **(run)**

e. For each goal within the mission, type single-letter responses for (S)uccess, (F)ailure, or Abort(x).

f. Example output file [SailorOverboardUavMissionOrdersReducedState3.log.txt](SailorOverboardUavMissionOrdersReducedState3.log.txt) shows a variety of interactions by a human operator.

3. TODO: full [Sailor Overboard Uav Mission](SailorOverboardUavMission.2019Aug12b.png) mission in Prolog.

---

![Sailor Overboard UAV Mission](SailorOverboardUavMission.2019Aug12b.png "Sailor Overboard UAV Mission")
