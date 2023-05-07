<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/xpath-functions" xmlns:math="http://www.w3.org/2005/xpath-functions/math" xmlns:array="http://www.w3.org/2005/xpath-functions/array" xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:err="http://www.w3.org/2005/xqt-errors" exclude-result-prefixes="array fn map math xhtml xs err" version="3.0">
	<!-- AVCL3ToTurtle.xslt -->
    <!-- This stylesheet transforms an XML document conforming to the Autonomous Vehicle Command Language (AVCL) XML schema into an OWL ontology (rendered in Turtle syntax) conforming to the Mission Execution Ontology OWL specification -->
	<xsl:output method="text" encoding="utf-8" media-type="text/plain"/>
    <xsl:variable name="UNDEFINED">
        <xsl:text>'undefined'</xsl:text><!-- must be quoted string -->
    </xsl:variable>
    
    <!-- ### Namespace declarations -->
	<xsl:template match="/" name="xsl:initial-template"><xsl:text>@prefix :     &lt;https://savage.nps.edu/EthicalControl/missions#> .
@prefix meo:  &lt;https://savage.nps.edu/EthicalControl/ontologies/MissionExecutionOntology#> .
@prefix owl:  &lt;http://www.w3.org/2002/07/owl#> .
@prefix rdf:  &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: &lt;http://www.w3.org/2000/01/rdf-schema#> .
@prefix xml:  &lt;http://www.w3.org/XML/1998/namespace> .
@prefix xsd:  &lt;http://www.w3.org/2001/XMLSchema#> .

@base &lt;https://savage.nps.edu/EthicalControl/missions> . # TODO why is "missions/" appended?

### Import base ontology
&lt;https://savage.nps.edu/EthicalControl/missions> rdf:type owl:Ontology ;
owl:imports &lt;https://savage.nps.edu/EthicalControl/ontologies/MissionExecutionOntology> .
</xsl:text>
<xsl:text>&#10;</xsl:text>
		<xsl:text>#################################################################</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>#    Individuals</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>#################################################################</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:apply-templates select="AVCL/body/MissionPreparation/AgendaMission"/>
		<xsl:apply-templates select="AVCL/body/MissionPreparation/AgendaMission/GoalList/Goal"/>
	</xsl:template>
	<!-- TEMPLATE FOR TRANSFORMING MISSION INFORMATION -->
	<xsl:template match="AVCL/body/MissionPreparation/AgendaMission">
		<!-- output mission individual in turtle -->
		<xsl:variable name="missionIndividual" select="../../../head/meta[@name='title']"/>
		<xsl:text>:</xsl:text><xsl:value-of select="fn:substring-before(../../../head/meta[@name='title']/@content, '.xml')"/><xsl:text> rdf:type owl:NamedIndividual , owl:Thing, meo:Mission ;</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>  meo:startsWith :</xsl:text><xsl:value-of select="GoalList/Goal[1]/@id"/><xsl:text> ;</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>  rdfs:comment "</xsl:text><xsl:value-of select="../../../head/meta[@name='description']/@content"/><xsl:text>" .</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>
	
	<!-- TEMPLATE FOR TRANSFORMING GOAL INFORMATION -->
	<xsl:template match="AVCL/body/MissionPreparation/AgendaMission/GoalList/Goal">
        <!-- rules for default values - might be replacable by ontology relationships so that terse form is functionally satisfactory -->
        
        <xsl:variable name="isTerminal"
             select="(string-length(normalize-space(@nextOnSuccess))   = 0) and
                     (string-length(normalize-space(@nextOnFailure))   = 0) and
                     (string-length(normalize-space(@nextOnException)) = 0)"/>
        <xsl:if test="$isTerminal"><!-- diagnostic message -->
            <xsl:message>
                <xsl:text>*** [info] Goal id='</xsl:text>
                <xsl:value-of select="@id"/>
                <xsl:text>' isTerminal (no nextOnSuccess, nextOnFailure, nextOnException)</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:variable name="title">
            <xsl:choose>
                <xsl:when test="(string-length(normalize-space(@title)) > 0)">
                    <xsl:value-of select="normalize-space(@title)"/>
                </xsl:when>
                <xsl:otherwise>
                    <!-- no default value, just diagnostic -->
                    <xsl:message>
                        <xsl:text>*** [warn] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' title </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
		<xsl:variable name="description">
            <xsl:choose>
                <xsl:when test="(string-length(normalize-space(@description)) > 0)">
                    <xsl:value-of select="normalize-space(@description)"/>
                </xsl:when>
                <xsl:otherwise>
                    <!-- no default value, just diagnostic -->
                    <xsl:message>
                        <xsl:text>*** [warn] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' description </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
		<xsl:variable name="phase">
            <xsl:choose>
                <xsl:when test="(string-length(normalize-space(@phase)) > 0)">
                    <xsl:value-of select="normalize-space(@phase)"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$UNDEFINED"/>
                    <xsl:message>
                        <xsl:text>*** [warn] Goal id='</xsl:text>
                        <xsl:value-of select="@id"/>
                        <xsl:text>' phase </xsl:text>
                        <xsl:value-of select="$UNDEFINED"/>
                    </xsl:message>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
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
		
        <!-- output goal individual in turtle -->
		<xsl:text>:</xsl:text><xsl:value-of select="./@id"/><xsl:text> </xsl:text>
		<xsl:text>rdf:type owl:NamedIndividual ,</xsl:text>
		<xsl:text> owl:Thing , meo:Goal</xsl:text>
		<xsl:if test="(string-length($nextOnSuccess) > 0) and not($isTerminal)">
			<xsl:text> ;</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>  meo:hasNextOnSuccess :</xsl:text><xsl:value-of select="$nextOnSuccess"/>
		</xsl:if>
		<xsl:if test="(string-length($nextOnFailure) > 0) and not($isTerminal)">
			<xsl:text> ;</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>  meo:hasNextOnFailure :</xsl:text><xsl:value-of select="$nextOnFailure"/>
		</xsl:if>
		<xsl:if test="(string-length($nextOnException) > 0) and not($isTerminal)">
			<xsl:text> ;</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>  meo:hasNextOnException :</xsl:text><xsl:value-of select="$nextOnException"/>
		</xsl:if>
		<xsl:text> ;</xsl:text>
		<xsl:text>&#10;</xsl:text>
        <xsl:if test="$phase">
            <xsl:text>  meo:isPartOfPhase "</xsl:text><xsl:value-of select="normalize-space($phase)"/>
            <xsl:text>" ;</xsl:text>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
		<xsl:text>  rdfs:comment "</xsl:text><xsl:value-of select="$title"/>
        <xsl:if test="$description">
            <xsl:if test="not(ends-with($title,'?'))">
                  <xsl:text>:</xsl:text>
            </xsl:if>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$description"/>
        </xsl:if>
        <xsl:text>"</xsl:text>
		<xsl:text> .</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>
	
</xsl:stylesheet>
