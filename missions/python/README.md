```
usage: MissionExecutionEngine.py [-h] [--validate]
                                 [--exhaustive-testing-max-runs-per-mission EXHAUSTIVE_TESTING_MAX_RUNS_PER_MISSION]
                                 [--exhaustively-test]
                                 [--exhaustively-test-all-standard-library-missions]
                                 [mission_file_path]

Mission Execution Engine for the Rational Behavior Model system for ethical
control of autonomy. See project website at:
https://savage.nps.edu/EthicalControl/

positional arguments:
  mission_file_path

optional arguments:
  -h, --help            show this help message and exit
  --validate            Warn the user if the mission is dangerously under-
                        specified.
  --exhaustive-testing-max-runs-per-mission EXHAUSTIVE_TESTING_MAX_RUNS_PER_MISSION
                        Sets the exhaustive tester to abort testing a mission
                        after testing a certain number of runs through the
                        mission. This can be useful if your mission is large
                        and would take days, years, or decades to exhaustively
                        test.
  --exhaustively-test   Exhaustively test the selected mission file, using
                        depth-first search to exhaustively run through all
                        possible mission flows.
  --exhaustively-test-all-standard-library-missions
                        Exhaustively test every mission in the standard
                        mission library folder
                        /Volumes/NAVAL/EthicalControl/missions/avcl. If you
                        use this option you should not specify a specific
                        mission filename.

Example invocations:
python3 MissionExecutionEngine.py  # Start the program and open the menu, then run in interactive mode
python3 MissionExecutionEngine.py --exhaustively-test  # Use the menu to choose which mission to exhaustively test
python3 MissionExecutionEngine.py --exhaustively-test ../avcl/LifeboatTracking.xml
python3 MissionExecutionEngine.py --exhaustively-test-all-standard-library-missions
python3 MissionExecutionEngine.py --validate ../avcl/HospitalShipEmDecoy1.Opponent.xml  # The --validate flag provides some warnings about possible issues with the mission plan
```