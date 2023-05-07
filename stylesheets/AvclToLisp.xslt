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
    
    <xsl:output method="text" encoding="UTF-8" indent="yes" media-type="text/plain" omit-xml-declaration="yes"/>
    <xsl:strip-space elements="*"/>
    <xsl:variable name="UNDEFINED">
        <xsl:text>'undefined'</xsl:text><!-- must be quoted string -->
    </xsl:variable>
    
    <xsl:template match="/">
        <!-- begin processing at root of AVCL mission document -->
        
        <xsl:variable name="missionName" select="//AVCL/head/meta[@name='title']/@content"/>
        <!-- first define initial AVCL goal as XSLT variable initialGoal -->
        <xsl:variable name="initialGoal" select="concat('goal',//Goal[1]/@id)"/>
        <xsl:message>
            <xsl:text>Mission </xsl:text>
            <xsl:value-of select="$missionName"/>
            <xsl:text>: set $initialGoal=</xsl:text>
            <xsl:value-of select="$initialGoal"/>
            <xsl:text> (i.e. initial AVCL Goal)</xsl:text>
        </xsl:message>
        
        <xsl:text>;This code written in ANSI Common Lisp
;
;Original design pattern:
;Prof. Robert B. McGhee (robertbmcghee@gmail.com) at the Naval Postgraduate School, Monterey, CA 93943.
            
; Generator: AvclToLisp.xslt

;This code facilitates human-robot cooperation and involves three entities. At
;the top level, the "mission commander" is always human, at least at this point in the
;evolution of RBM software. The "mission controller" is usually assumed to be electronic,
;but could be a human "executive officer" in the case of a manned vehicle. The mission
;agent is always a vehicle. If the vehicle has an onboard computer incorporating the
;mission controller, then the vehicle is a robot. If not, it is an "ROV" or a "drone".

;;;;;;;;;;;;;;;;;;;;;;;;;Begin Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;

(defclass mission-goal ()
  ((command :accessor command)
   (successor-list :accessor successor-list)));Elements are (outcome next-goal).

(defclass mission-execution-engine ()
  ((external-agent-response :accessor external-agent-response :initform nil)
   (current-execution-goal :accessor current-execution-goal :initform '</xsl:text><xsl:value-of select="$initialGoal"/><xsl:text>)
   (successor-list-index :accessor successor-list-index :initform 0)))

(defmethod initialize-goal ((goal mission-goal) new-command new-successor-list)
  (setf (command goal) new-command
    (successor-list goal) new-successor-list))

(defmethod issue-command ((MEE mission-execution-engine))
  (let* ((goal (current-execution-goal MEE))
         (new-command (command (eval goal))))
    (issue-order new-command)))

(defmethod ask-result ((MEE mission-execution-engine))
  (let* ((result (ask-outcome)))
    (setf (external-agent-response MEE) result)))

(defmethod set-next-goal ((MEE mission-execution-engine))
  (let* ((goal (current-execution-goal MEE))
         (new-successor-list (successor-list (eval goal)))
         (new-index (successor-list-index MEE))
         (next-goal (second (nth new-index new-successor-list))))
    (setf (current-execution-goal MEE) next-goal)))

(defmethod set-successor-list-index ((MEE mission-execution-engine))
  (let* ((index (convert-outcome-to-index (external-agent-response MEE))))
    (setf (successor-list-index MEE) index)))

(defvar mission-controller (make-instance 'mission-execution-engine)) ; instantiate the class as an object

; discussion for future work: naming conventions

(defun execute-goal ()
  (issue-command mission-controller)
  (ask-result mission-controller)
  (set-successor-list-index mission-controller)
  (set-next-goal mission-controller))

(defun execute-terminal-goal ()
  (issue-command mission-controller))

(defun create-mission-orders ()
  (initialize-mission)
  (setf (current-execution-goal mission-controller) '</xsl:text><xsl:value-of select="$initialGoal"/><xsl:text>))

(defun issue-order (command)
  (format t "~A" command)
  (terpri)) ; terminate printing, newline

(defun ask-outcome ()
  (format t "... determine goal execution: success (s), failure (f), or exception (x)? ")
  (terpri)
  (let ((value (read)))
       (if (not(member value '(s f x)))
           (format t " ? unknown response, please retry" (terpri)))
       (if (member value '(s f x))
           value
           (ask-outcome))))

(defun run ()
  (create-mission-orders)
  (execute-mission))

; execute starting at a new goal, also enables testing and refinement resuming in the middle of a mission
(defun rerun (new-start-goal)
  (setf (current-execution-goal mission-controller) new-start-goal)
  (execute-mission))

(defun convert-outcome-to-index (r)
  (cond ((equal r 's) (format t " =success")   (terpri) 0)
        ((equal r 'f) (format t " =failure")   (terpri) 1)
        ((equal r 'x) (format t " =exception") (terpri) 2)))

(defvar terminal-goal-list nil)

(defun execute-mission ()
  (cond ((member (current-execution-goal mission-controller) terminal-goal-list) (execute-terminal-goal))
        (t (execute-goal)(execute-mission))))

;;;;;;;;;;;;;;;;;;;;;;;;;End Universal Mission Execution Engine;;;;;;;;;;;;;;;;;;;;;;;
</xsl:text>
        <xsl:choose>
            <xsl:when test="(count(//Goal) > 0)">
                <!-- mission header -->
                <xsl:text>&#10;</xsl:text>
                <xsl:text>;;;;;;;;;;;;;;;;;;;;;;Begin </xsl:text>
                <xsl:value-of select="//AVCL/head/meta[@name='title']/@content"/>
                <xsl:text> Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;</xsl:text>
                <xsl:text>&#10;</xsl:text>
                <xsl:text>&#10;</xsl:text>
            
                <xsl:for-each select="//Goal">
                    <xsl:text>  (defvar goal</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text> (make-instance 'mission-goal))</xsl:text>
                    <xsl:if test="(position() = 1)">
                        <xsl:text> ; initial goal, by default</xsl:text>
                    </xsl:if>
                    <xsl:text>&#10;</xsl:text>
                </xsl:for-each>
                <xsl:text>&#10;</xsl:text>
                
                <!--  function definition -->
                <xsl:text>(defun initialize-mission ()</xsl:text>
                
                <!-- list terminal goals -->
                <xsl:text>&#10;</xsl:text>
                <xsl:text>  (setf terminal-goal-list '(</xsl:text>
                <xsl:for-each select="//Goal">
                    <xsl:variable name="isTerminal"
                                  select="(string-length(@nextOnSuccess)   = 0) and
                                          (string-length(@nextOnFailure)   = 0) and
                                          (string-length(@nextOnException) = 0)"/>
                    <!-- local diagnostic, repeated when computing fallback values
                    <xsl:if test="$isTerminal">
                        <xsl:message>
                            <xsl:text>*** [info] Goal </xsl:text>
                            <xsl:value-of select="@id"/>
                            <xsl:text> isTerminal</xsl:text>
                        </xsl:message>
                    </xsl:if> -->
                    <xsl:if test="$isTerminal">
                        <xsl:text>goal</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:if test="not(position() = last())">
                            <xsl:text> </xsl:text>
                        </xsl:if>
                    </xsl:if>
                </xsl:for-each>
                <xsl:text>))</xsl:text>
                <xsl:text>&#10;</xsl:text>
                <xsl:text>&#10;</xsl:text>
                
                <!-- initialize-goal code blocks -->
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
                    <!-- keep empty -->
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
                    <!-- keep empty -->
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
                    <!-- keep empty -->
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
                    <xsl:text>  (initialize-goal </xsl:text>
                    <xsl:text>goal</xsl:text>
                    <xsl:value-of select="@id"/>
                    <xsl:text> "</xsl:text>
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
                    <xsl:choose>
                        <xsl:when test="not($isTerminal)">
                            <xsl:text>&#10;</xsl:text>
                            <xsl:text>                    </xsl:text>
                            <xsl:text>'(("Success." goal</xsl:text>
                            <xsl:value-of select="$nextOnSuccess"/>
                            <xsl:text>) ("Failed." goal</xsl:text>
                            <xsl:value-of select="$nextOnFailure"/>
                            <xsl:text>) ("Exception." goal</xsl:text>
                            <xsl:value-of select="$nextOnException"/>
                            <xsl:text>)))</xsl:text>
                            <xsl:text>&#10;</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text> nil)</xsl:text>
                            <xsl:text>&#10;</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                    <xsl:if test="not(position() = last())">
                        <xsl:text>&#10;</xsl:text>
                    </xsl:if>
                </xsl:for-each>
                <xsl:text>)</xsl:text>
                <xsl:text>&#10;</xsl:text>
<!--
  (initialize-goal goal1.1 "Choose Tube and Launch!"  
                    '(("Success." goal1.2) ("Failed." goal8) ("Exception." goal8)))
-->
                <!-- footer -->
                <xsl:text>;;;;;;;;;;;;;;;;;;;;;;End </xsl:text>
                <xsl:value-of select="//AVCL/head/meta[@name='title']/@content"/>
                <xsl:text> Mission Orders;;;;;;;;;;;;;;;;;;;;;;;;;</xsl:text>
                <xsl:text>&#10;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>; AVCL mission has no Goal definitions</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
        <!-- now iterate through the AVCL mission and produce lisp functions for each goal encountered -->
        <xsl:apply-templates select="//AVCL/body/MissionPreparation/AgendaMission/GoalList/Goal"/>
        
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

