<?xml version="1.0" encoding="UTF-8"?>
<!--
  <head>
   <meta name="filename"    content="AvclSchemaElementsToStringConstants.xslt" />
   <meta name="author"      content="Don Brutzman" />
   <meta name="created"     content="27 January 2020" />
   <meta name="description" content="XSLT stylesheet to convert AVCL mission scripts to Lisp source code." />
   <meta name="url"         content="auv/AuvWorkbench/Scripts/AvclSchemaElementsToStringConstants.xslt" />
   <meta name="invocation"  content="auv/AuvWorkbench> ant AvclSchemaElementsToStringConstants" />
  </head>
-->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <!-- Default parameter values can be overridden when invoking this stylesheet -->
    <xsl:param name="prologVersion">
        <!-- AllegroCommonLisp or ISO -->
        <xsl:text>AllegroCommonLisp</xsl:text>
    </xsl:param>
    
    <xsl:output method="text" encoding="UTF-8" indent="yes" media-type="text/plain" omit-xml-declaration="yes"/>
    <xsl:strip-space elements="*"/>
    <xsl:variable name="UNDEFINED">
        <xsl:text>'undefined'</xsl:text><!-- must be quoted string -->
    </xsl:variable>
    
    <xsl:template match="/">
        <!-- begin processing at root of AVCL mission document -->
        
        <!-- first define initial AVCL goal as XSLT variable initialGoal -->
        <!-- TODO make this a mission setting and prolog variable -->
        <xsl:variable name="initialGoal" select="//Goal[1]/@id"/>
        <xsl:message>
            <xsl:text>$initialGoal=</xsl:text>
            <xsl:value-of select="$initialGoal"/>
        </xsl:message>
        
        <!-- Header -->
        <xsl:choose>
            <xsl:when test="($prologVersion = 'ISO')">
        <xsl:text disable-output-escaping="yes"><![CDATA[
% This version was originally adapted by Duane Davis and tested under SWI Prolog.

% https://www.swi-prolog.org 

% Test conditions for goal termination states
goal_succeed(s).
goal_succeed(succeed).
goal_succeed(y).
goal_succeed(yes).
goal_fail(f).
goal_fail(fail).
goal_fail(n).
goal_fail(no).
constraint(e).
constraint(ethic).
constraint(ethics).
constraint(c).
constraint(constraint).

% Human external agent communications functions
report(X) :- write(X), write("."), nl.
command(X) :- write("Commence: "),
	      write(X), write("."), nl.
ask_end_state(A) :- repeat, write("Did goal Succeed (s), Fail (f), or end with a Constraint (c)? "),
                    read(A),(goal_succeed(A); goal_fail(A); constraint(A)).
random_end_state(Result, Fail_ratio, Constraint_ratio) :-
	random(X), ((X =< Fail_ratio, Result = fail, write("Goal execution failed!\n"));
		    (X =< Constraint_ratio, Result = constraint,
		     write("Goal execution constraint terminated!\n"));
		    (Result = succeed, write("Goal execution succeeded!\n"))).

% Mission execution ruleset (MEE)
execute_mission :- initialize_mission, repeat, execute_current_goal, done, !.
auto_execute_mission :- initialize_mission, repeat, auto_execute_current_goal, done, !.
initialize_mission :- retractall(current_goal(_)), asserta(current_goal(1)).
execute_current_goal :- current_goal(G), execute_goal(G), !.
auto_execute_current_goal :- current_goal(G), auto_execute_goal(G), !.
done :- current_goal(mission_complete), report("Mission Complete!").
done :- current_goal(mission_abort), report("Mission Abort!").
done :- current_goal(answer_succeed), report("Reporting Goal Success!").
done :- current_goal(answer_fail), report("Reporting Goal Failure!").
done :- current_goal(answer_constraint), report("Reporting Constraint Encountered!").

% Goal execution ruleset (MEE) for iteratively defined goals.
execute_goal :- initialize_goal, repeat, execute_current_goal, done, !.
initialize_goal :- retractall(current_goal(_)), asserta(current_goal(1.1)).

% Utility functions
change_goal(New) :- retractall(current_goal(_)), asserta(current_goal(New)).

% Mission Orders for Cannonical Mission (human operator execution)
execute_goal(1) :- command("Search Area A"), goal_completed(1).
goal_completed(1) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(2));
		       (goal_fail(Ending), change_goal(3));
		       (constraint(Ending), change_goal(4))).

execute_goal(2) :- command("Take environmental sample from Area A"),
	           goal_completed(2).
goal_completed(2) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(3));
		       (goal_fail(Ending), change_goal(5));
		       (constraint(Ending), change_goal(4))).

execute_goal(3) :- command("Search Area B"), goal_completed(3).
goal_completed(3) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(4));
		       (goal_fail(Ending), change_goal(4));
		       (constraint(Ending), change_goal(4))).

execute_goal(4) :- command("Rendezvous with vehicle 2 in Area C"),
                   goal_completed(4).
goal_completed(4) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(5));
		       (goal_fail(Ending), change_goal(5));
		       (constraint(Ending), change_goal(5))).

execute_goal(5) :- command("Return to Base"), goal_completed(5).
goal_completed(5) :- ask_end_state(Ending),
		      ((goal_succeed(Ending), change_goal(mission_complete));
		       (goal_fail(Ending), change_goal(mission_abort));
		       (constraint(Ending), change_goal(mission_abort))).

% Mission Orders for Cannonical Mission (autonomous execution)
auto_execute_goal(1) :- command("Search Area A"), goal_completed(1, auto).
goal_completed(1, auto) :- execute_ratios(1, Fail_ratio, Constraint_ratio),
	             random_end_state(Ending, Fail_ratio, Constraint_ratio),
	              ((goal_succeed(Ending), change_goal(2));
		       (goal_fail(Ending), change_goal(3));
		       (constraint(Ending), change_goal(4))).

auto_execute_goal(2) :- command("Take environmental sample from Area A"),
	           goal_completed(2, auto).
goal_completed(2, auto) :- execute_ratios(1, Fail_ratio, Constraint_ratio),
	             random_end_state(Ending, Fail_ratio, Constraint_ratio),
	              ((goal_succeed(Ending), change_goal(3));
		       (goal_fail(Ending), change_goal(5));
		       (constraint(Ending), change_goal(4))).

auto_execute_goal(3) :- command("Search Area B"), goal_completed(3, auto).
goal_completed(3, auto) :- execute_ratios(1, Fail_ratio, Constraint_ratio),
	             random_end_state(Ending, Fail_ratio, Constraint_ratio),
	              ((goal_succeed(Ending), change_goal(4));
		       (goal_fail(Ending), change_goal(4));
		       (constraint(Ending), change_goal(4))).

auto_execute_goal(4) :- command("Rendezvous with vehicle 2 in Area C"),
                   goal_completed(4, auto).
goal_completed(4, auto) :- execute_ratios(1, Fail_ratio, Constraint_ratio),
	             random_end_state(Ending, Fail_ratio, Constraint_ratio),
	              ((goal_succeed(Ending), change_goal(5));
		       (goal_fail(Ending), change_goal(5));
		       (constraint(Ending), change_goal(5))).

auto_execute_goal(5) :- command("Return to Base"), goal_completed(5, auto).
goal_completed(5, auto) :- execute_ratios(1, Fail_ratio, Constraint_ratio),
	             random_end_state(Ending, Fail_ratio, Constraint_ratio),
		      ((goal_succeed(Ending), change_goal(mission_complete));
		       (goal_fail(Ending), change_goal(mission_abort));
		       (constraint(Ending), change_goal(mission_abort))).

% Autonomous execution goal termination ratios
execute_ratios(1, 0.25,0.5).
execute_ratios(2, 0.25,0.5).
execute_ratios(3, 0.25,0.5).
execute_ratios(4, 0.25,0.5).
execute_ratios(5, 0.25,0.5).

% MEA orders for a Depth-First search of Area A (human operator execution).
execute_goal(1.1) :- command("Initialize Search Area A"), goal_completed(1.1).
goal_completed(1.1) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(1.2));
		       (goal_fail(Ending), write("Depth First Search:  "),
			change_goal(answer_fail));
		       (constraint(Ending), write("Depth First Search:  "),
			change_goal(answer_constraint))).

execute_goal(1.2) :- command("Move Forward"), goal_completed(1.2).
goal_completed(1.2) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(1.3));
		       (goal_fail(Ending), change_goal(1.4));
		       (constraint(Ending), write("Depth First Search:  "),
			change_goal(answer_constraint))).

execute_goal(1.3) :- command("Observe Environment, Test Goal Found"), goal_completed(1.3).
goal_completed(1.3) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), write("Depth First Search:  "),
			change_goal(answer_succeed));
		       (goal_fail(Ending), change_goal(1.2));
		       (constraint(Ending), write("Depth First Search:  "),
			change_goal(answer_constraint))).

execute_goal(1.4) :- command("Backtrack"), goal_completed(1.4).
goal_completed(1.4) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(1.5));
		       (goal_fail(Ending), write("Depth First Search:  "),
			change_goal(answer_fail));
		       (constraint(Ending), write("Depth First Search:  "),
			change_goal(answer_constraint))).

execute_goal(1.5) :- command("Observe Environment, Test Available Cell"), goal_completed(1.5).
goal_completed(1.5) :- ask_end_state(Ending),
	              ((goal_succeed(Ending), change_goal(1.2));
		       (goal_fail(Ending), change_goal(1.4));
		       (constraint(Ending),write("Depth First Search:  "),
			 change_goal(answer_constraint))).

]]></xsl:text>
            </xsl:when>
            <xsl:otherwise>
        <xsl:text disable-output-escaping="yes"><![CDATA[;This code written in Franz Allegro Prolog (matching Lisp-style syntax)
;
;Original design pattern: MissionExecutionEngine.pl
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.

; Description: this is the RBM Strategic Level task sequencing algorithm that is the basis
; of the Mission Execution Engine (MEE), see Figure 2 in the IEEE JOE paper (TODO link this).

;Start Prolog.
(require :prolog) (shadowing-import '(prolog:==)) (use-package :prolog)
]]></xsl:text>
        <xsl:text>

;Mission specification
</xsl:text>
        <xsl:choose>
            <xsl:when test="(count(//Goal) > 0)">
                <!-- mission header -->
                <xsl:text>&#10;</xsl:text>
                <xsl:text>;;;;;;;;;;;;;;;;;;;;;; Begin </xsl:text>
                <xsl:value-of select="//AVCL/head/meta[@name='title']/@content"/>
                <xsl:text> Mission Orders ;;;;;;;;;;;;;;;;;;;;;;</xsl:text>
                <xsl:text>&#10;</xsl:text>
                <xsl:text>&#10;</xsl:text>
            
                <xsl:for-each select="//Goal">
                    <xsl:variable name="isTerminal"
                                  select="(string-length(@nextOnSuccess)   = 0) and
                                          (string-length(@nextOnFailure)   = 0) and
                                          (string-length(@nextOnException) = 0)"/>
        <!-- master version of this ruleset appears in AvclToTurtle.xslt -->
        <!-- see annotated key table in mission diagrams for rules regarding default values for failure and exception if not otherwise defined:
		     - Goal Success condition must be defined for all transitions
             - If no Failure condition defined, then Failure matches Success
             - If no Exception defined, then Exception condition matches Global Exception or else Failure -->
        <xsl:variable name="nextOnSuccess">
            <xsl:choose>
                <xsl:when test="$isTerminal">
                    <xsl:text>mission_complete</xsl:text>
                </xsl:when>
                <xsl:when test="(string-length(normalize-space(@nextOnSuccess)) > 0)">
                    <xsl:value-of select="normalize-space(@nextOnSuccess)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$UNDEFINED"/>
                    <xsl:message>
                        <xsl:text>*** [error] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' nextOnSuccess </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                        <xsl:text> which is illegal for non-terminal AVCL mission goal</xsl:text>
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
		<xsl:variable name="nextOnFailure">
            <xsl:choose>
                <xsl:when test="$isTerminal">
                    <xsl:text>mission_abort</xsl:text>
                </xsl:when>
                <xsl:when test="(string-length(normalize-space(@nextOnFailure)) > 0)">
                    <xsl:value-of select="normalize-space(@nextOnFailure)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$nextOnSuccess"/>
                    <xsl:message>
                        <xsl:text>*** [warn] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' nextOnFailure </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                        <xsl:text> and set to @nextOnSuccess='</xsl:text>
                        <xsl:value-of select="$nextOnSuccess"/>
                        <xsl:text>'</xsl:text>
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
		<xsl:variable name="nextOnException">
            <xsl:choose>
                <xsl:when test="$isTerminal">
                    <xsl:text>mission_abort</xsl:text>
                </xsl:when>
                <xsl:when test="(string-length(normalize-space(@nextOnException)) > 0)">
                    <xsl:value-of select="normalize-space(@nextOnException)"/>
                </xsl:when>
                <xsl:when test="(string-length($nextOnFailure) > 0)">
                    <xsl:value-of select="$nextOnFailure"/>
                    <xsl:message>
                        <xsl:text>*** [warn] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' nextOnException </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                        <xsl:text> and set to @nextOnFailure </xsl:text>
                        <xsl:value-of select="$nextOnFailure"/>
                    </xsl:message>
                </xsl:when>
                <!-- TODO global exception not yet implemented -->
                <xsl:otherwise>
                    <xsl:value-of select="$UNDEFINED"/>
                    <xsl:message>
                        <xsl:text>*** [error] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' nextOnException </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                        <xsl:text> which is currently illegal since Global Exception is not yet supported in AVCL</xsl:text><!-- TODO -->
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
                    <!-- begin source code -->
                    <xsl:text disable-output-escaping="yes"><![CDATA[(<-]]></xsl:text>
                    <xsl:if test="(position() = 1)">
                        <xsl:text>-</xsl:text>
                    </xsl:if>
                    <xsl:text> (execute_goal </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text>) (command </xsl:text>
                    <xsl:text>"</xsl:text>
                    <xsl:if test="not(starts-with(lower-case(@id),'goal'))">
                        <xsl:text>Goal </xsl:text>
                    </xsl:if>
                    <xsl:value-of select="@id"/>
                    <xsl:text>: </xsl:text>
                    <xsl:value-of select="@title"/>
                    <xsl:if test="(string-length(@description) > 0)">
                        <xsl:if test="not(ends-with(normalize-space(@title),'?'))">
                            <xsl:text> -</xsl:text>
                        </xsl:if>
                        <xsl:text> </xsl:text>
                        <xsl:value-of select="@description"/>
                    </xsl:if>
                    <xsl:text>"</xsl:text>
                    <xsl:text>) (update_outcome)</xsl:text>
                    <xsl:if test="(position() = 1)">
                        <xsl:text> ; initial goal, by default</xsl:text>
                    </xsl:if>
                    
                    <xsl:text>
                           (current_goal_outcome s) (change_goal </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text> </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="$nextOnSuccess"/>
                    <xsl:text>))</xsl:text>
                    <xsl:text>&#10;</xsl:text>
                    
                    <xsl:text disable-output-escaping="yes"><![CDATA[(<- (execute_goal ]]></xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text>) (current_goal_outcome f) (change_goal </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text> </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="$nextOnFailure"/>
                    <xsl:text>))</xsl:text>
                    <xsl:text>&#10;</xsl:text>
                    
                    <xsl:text disable-output-escaping="yes"><![CDATA[(<- (execute_goal ]]></xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text>) (current_goal_outcome x) (change_goal </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text> </xsl:text>
                    <xsl:text>'</xsl:text>
                    <xsl:value-of select="$nextOnException"/>
                    <xsl:text>))</xsl:text>
                    <xsl:text>&#10;</xsl:text>
                    
                </xsl:for-each>
                <xsl:text>&#10;</xsl:text>

                <!-- footer -->
                <xsl:text>;;;;;;;;;;;;;;;;;;;;;; End </xsl:text>
                <xsl:value-of select="//AVCL/head/meta[@name='title']/@content"/>
                <xsl:text> Mission Orders ;;;;;;;;;;;;;;;;;;;;;;</xsl:text>
                <xsl:text>&#10;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>; AVCL mission has no Goal definitions</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        <!-- now iterate through the AVCL mission and produce lisp functions for each goal encountered -->
        <xsl:apply-templates select="//AVCL/body/MissionPreparation/AgendaMission/GoalList/Goal"/>
        

        <!-- Mission Execution Engine -->
        <xsl:text disable-output-escaping="yes"><![CDATA[
;Facts
(<-- (current_goal ']]></xsl:text>
        <xsl:value-of select="$initialGoal"/>
        <xsl:text disable-output-escaping="yes"><![CDATA[)) ; initialGoal
(<-- (current_goal_outcome s))
     
;Mission execution rule set  
(<-- (execute_mission) (initialize_mission) (repeat) (execute_current_goal) (done) !)
(<-- (initialize_mission) (abolish current_goal ']]></xsl:text>
        <xsl:value-of select="$initialGoal"/>
        <xsl:text disable-output-escaping="yes"><![CDATA[) (asserta ((current_goal ']]></xsl:text>
        <xsl:value-of select="$initialGoal"/>
        <xsl:text disable-output-escaping="yes"><![CDATA[)))) ; initialGoal
(<-- (execute_current_goal) (current_goal ?x) (execute_goal ?x) !)
(<-- (done) (current_goal 'mission_complete))
(<- (done) (current_goal 'mission_abort))

; Human external agent communication functions

(<-- (negative nil)) (<- (negative n))
(<-- (affirmative ?x) (not (negative ?x)))
(<-- (report ?C) (princ ?C) (princ ".") (nl))
(<-- (command ?C) (princ ?C) (princ "!") (nl))
(<-- (ask ?Q ?A) (princ ?Q) (princ "?") (read ?A))
(<-- (ask_outcome ?A) (ask "Did goal succeed (s), fail (f), or abort (x)" ?A))

; Utility functions
(<-- (change_goal ?old ?new) (retract ((current_goal ?old))) (asserta ((current_goal ?new))))
(<-- (update_outcome) (ask_outcome ?A) (abolish current_goal_outcome 1) (asserta ((current_goal_outcome ?A))))

; Test functions (illustrate format for calling predicates from Lisp)
(defun run () (?- (execute_mission)))
(defun update () (?- (update_outcome)))
(defun mission-goal () (?- (current_goal ?X)))
(defun outcome () (?- (current_goal_outcome ?X)))
]]></xsl:text>

            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>
    
    <xsl:template match="*">
        
        <xsl:if test="(local-name() = 'hide')">
            <xsl:text>&#10;</xsl:text>

            <xsl:text>; </xsl:text>
            <xsl:value-of select="local-name()"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="@id"/>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
        
        
        <xsl:apply-templates select="*"/><!-- recurse downward -->
        
    </xsl:template>
    
</xsl:stylesheet>

