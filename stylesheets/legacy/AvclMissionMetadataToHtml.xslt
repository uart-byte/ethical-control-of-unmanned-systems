<?xml version="1.0" encoding="UTF-8"?>
<!--
<head>
    <meta name="filename"    content="AvclMissionMetadataToHtml.xslt"/>
    <meta name="author(s)"   content="Don Brutzman, Ron Hemmelgarn, Nathan Conger"/>
    <meta name="created"     content="08 JAN 2008"/>
    <meta name="description" content="XSLT stylesheet, converts telemetry and mission script output into xhtml format"/>
    <meta name="version"     content="$Id$"/>
</head>
-->
<xsl:stylesheet version="2.0"
                xmlns="http://www.w3.org/1999/xhtml"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:java="http://xml.apache.org/xslt/java"
                xmlns:xalan="http://xml.apache.org/xslt"
                xmlns:pre="xalan://org.apache.xalan.Version"
                xmlns:html="http://www.w3.org/1999/xhtml">
    <!--
    -->
    <xsl:strip-space elements="*"/>
    <xsl:output method="xml"
                encoding="UTF-8"
                omit-xml-declaration="no"
                doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
                doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
                cdata-section-elements="Script"
                indent="yes"
                media-type="text/html"/>
    
    <xsl:variable name="avclSchemaUrl">https://savage.nps.edu/AuvWorkbench/documentation/schemas/AVCL.2.1/AVCL.2.1_</xsl:variable>

    <!-- Path of MissionPreparation -->
    <xsl:variable name="missionPreparationPath" select="/AVCL/body/MissionPreparation"/>

    <!-- Path of MissionResults -->
    <xsl:variable name="missionResultsPath" select="/AVCL/body/MissionResults"/>
    
    <!-- Vehicle name, e.g. OjoDelMar, SeaFox, Aries, etc. -->
    <xsl:variable name="vehicleName" select="/AVCL/@vehicleName"/>
    
    <!-- Vehicle type, e.g. USV, UAV, UUV or UGV -->
    <xsl:variable name="vehicleType" select="/AVCL/@vehicleType"/>

    <!-- Variable to identify AVCL type from output telemetry file -->
    <xsl:variable name="avclType">
	<xsl:value-of select="local-name($missionPreparationPath/Configuration/*)"/>
    </xsl:variable>
    
    <xsl:variable name="commandScriptType">
        <xsl:choose>
            <xsl:when test="$missionPreparationPath/AgendaMission">
                <xsl:text>AgendaMission</xsl:text>
            </xsl:when>
            <xsl:when test="$avclType = 'UnmannedAerialVehicle'">
                <xsl:text>UAVCommandScript</xsl:text>
            </xsl:when>
            <xsl:when test="$avclType = 'UnmannedGroundVehicle'">
                <xsl:text>UGVCommandScript</xsl:text>
            </xsl:when>
            <xsl:when test="$avclType = 'UnmannedSurfaceVehicle'">
                <xsl:text>USVCommandScript</xsl:text>
            </xsl:when>
            <xsl:when test="$avclType = 'UnmannedUnderwaterVehicle'">
                <xsl:text>UUVCommandScript</xsl:text>
            </xsl:when>
            <xsl:when test="$avclType = 'UnmannedUnderwaterVehicle'">
                <xsl:text>UUVCommandScript</xsl:text>
            </xsl:when>
            <!-- TODO munition -->
        </xsl:choose>
    </xsl:variable>
    
    <!-- SetPosition type per vehicle -->
    <xsl:variable name="setPositionType">
	<xsl:value-of select="concat('SetPosition', $vehicleType)"/>
    </xsl:variable>

    <!-- Loiter type per vehicle -->
    <xsl:variable name="loiterType">
	<xsl:value-of select="concat('Loiter', $vehicleType)"/>
    </xsl:variable>

    <!-- Waypoint type per vehicle -->
    <xsl:variable name="waypointType">
	<xsl:value-of select="concat('Waypoint', $vehicleType)"/>
    </xsl:variable>

    <!-- Attempt to establish an incremental counter variable. TODO avoid use of java class, stick with pure XSLT. -->
    <!-- From: http://osdir.com/ml/text.xml.xalan.java.user/2006-05/msg00014.html -->
    <xsl:variable name="javaCounter" select="java:java.util.ArrayList.new()"/>

    <xsl:variable name="dateTimeGroup" select="java:org.web3d.xmsf.xsbc.util.DateTimeGroupStamp.getCurrentDateTimeGroup()"/>

    <!-- Global variables for the metadata report corresponding to a mission output telemetry file -->
    <xsl:variable name="missionFileName"  select="//head/meta[@name='title']/@content"/>
    <xsl:variable name="metadataFileName" select="concat(substring-before($missionFileName,'.xml'),'.MissionMetadata.xml')"/>

    <xsl:variable name="metadataInput" select="document($metadataFileName)">
        <!-- document resolver in AUVW looks for metadataFile in directory relative to missionResults directory, rather than in Scripts directory -->
    </xsl:variable>

    <xsl:variable name="figureWidth">
        <!--  55% fits 3 images per page when printing to .pdf -->
        <xsl:text>55%</xsl:text>
    </xsl:variable>

    <xsl:variable name="onlineDirectoryTelemetryPlotHelp">
        <xsl:text>http://xmsf.svn.sourceforge.net/viewvc/xmsf/trunk/AuvWorkbench/src/plots/</xsl:text>
    </xsl:variable>

    <xsl:template match="/">
	<!--
        <xsl:message>
	    <xsl:text>waypointType is: </xsl:text>
	    <xsl:value-of select="$waypointType"/>
	</xsl:message>
        -->

	<xsl:message>
	    <xsl:text>missionFileName is: </xsl:text>
	    <xsl:value-of select="$missionFileName"/>
	</xsl:message>
	<xsl:message>
	    <xsl:text>metadataFileName is: </xsl:text>
	    <xsl:value-of select="$metadataFileName"/>
            <xsl:choose>
                <xsl:when test="string-length($metadataInput) > 0">
                    <xsl:text>, data found</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>, data not found</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
	</xsl:message>
    
        <!-- Debug messages -->
        <!--
	<xsl:message>
	    <xsl:text>metadataInput is: </xsl:text>
	    <xsl:value-of select="$metadataInput"/>
	</xsl:message>
        <!xsl:message>
            Processor:
            <xsl:value-of select="system-property('xsl:vendor')"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="pre:getVersion()"/>
        </xsl:message>
	<!xsl:message>
	    <xsl:text>$avclType is: </xsl:text>
	    <xsl:value-of select="$avclType"/>
	</xsl:message>
        -->
	<xsl:element name="html">
	    <xsl:element name="head">
		<xsl:element name="title">
                    <!-- provide unique name for browser-tab tooltip and .pdf export filename -->
		    <xsl:text>AUV Workbench Mission Report: </xsl:text>
                    <xsl:value-of select="$missionFileName"/>
		    <xsl:text> </xsl:text>
                    <xsl:value-of select="$metadataInput/MissionReport/id/date"/>
		</xsl:element>
		<xsl:element name="meta">
		    <xsl:attribute name="http-equiv">
			<xsl:text>Content-Language</xsl:text>
		    </xsl:attribute>
		    <xsl:attribute name="content">
			<xsl:text>en-us</xsl:text>
		    </xsl:attribute>
		</xsl:element>
		<xsl:element name="meta">
		    <xsl:attribute name="http-equiv">
			<xsl:text>Content-Type</xsl:text>
		    </xsl:attribute>
		    <xsl:attribute name="content">
			<xsl:text>text/html; charset=windows-1252</xsl:text>
		    </xsl:attribute>
		</xsl:element>
                <meta name="reference" content="https://savage.nps.edu/AuvWorkbench"/>
                <meta name="generator" content="AvclMissionMetadataToHtml.xslt"/>
                <xsl:comment>
                    <xsl:text>Results generated using XSLT processor </xsl:text>
                </xsl:comment>
                <meta name="generator" content="{system-property('xsl:vendor')}"/>

                <!-- The icon will only work if project is in
                /auv/AuvWorkbench/MyAuvwProjects/${projectname}/missionResults/${missionname} -->
		<xsl:element name="link">
		    <xsl:attribute name="rel">
			<xsl:text>shortcut icon</xsl:text>
		    </xsl:attribute>
		    <xsl:attribute name="href">
			<xsl:text>../../../../run.ico</xsl:text>
		    </xsl:attribute>
		    <xsl:attribute name="type">
			<xsl:text>image/x-icon</xsl:text>
		    </xsl:attribute>
		</xsl:element>
                <style type="text/css"><![CDATA[
span.element {color: navy}
span.attribute {color: green}
span.value {color: teal}
span.plain {color: black}
span.gray  {color: gray}
span.idName {color: maroon}
a.idName {color: maroon}
div.center {text-align: center}
right-align  {text-align: right}
div.indent {margin-left: 25px}
right-indent {text-align: right; margin-left: 25px}
th.bgcolor {background-color: #DDDDFF}
td.bgcolor {background-color: #DDDDFF}
th.subheading {text-align:left; background-color: #EEFFFF}
td.subheading {text-align:left; background-color: #EEFFFF}
span.prototype {color: purple}
a.prototype {color: purple; visited: black}
span.route {color: red}
b.warning {color: #CC5500}
b.error {color: #CC0000}]]>
                </style>
	    </xsl:element>
            
            <!-- TODO consider limiting the width of the entire HTML page to be printer friendly, if creating .pdf:  class="width: 1024px" -->
	    <body>
                <!-- Link anchor for the top of the page -->
                <a name="top"/>

                <!-- ============================================================ -->
		<h2 style="text-align:center">
                    <xsl:value-of select="substring-before($missionFileName,'.xml')"/>
                    <xsl:if test="not(contains($missionFileName,'Mission'))">
                        <xsl:text>Mission</xsl:text>
                    </xsl:if>
                    Report
                </h2>
                <!-- ============================================================ -->

                <!-- Bookmarks for TOC -->
                <blockquote style="text-align:center;font-size:small">
                        <a href="#Metadata">Mission Metadata</a> |
                        <a href="#Header">AVCL Header</a> |
                        <a href="#Details">AVCL Mission Details</a> |
                        <a href="#MissionCommands">AVCL Mission Commands</a> |
                        <!-- <a href="#X3D">X3D Scene</a> | -->
                        <a href="#Snapshots">Screen Snapshots</a> |
                        <a href="#Plots">Telemetry Plots</a> |
                        <a href="#Credits">Credits</a>
                </blockquote>
                <!-- ============================================================ -->

                <p>
                    <a name="Metadata"/>
                </p>
                <table align="center" border="1" summary="Background information for this AVCL mission" title="Mission Metadata" width="90%" cellpadding="2">
                    <thead>
                        <tr>
                            <th align="center" colspan="2" class="bgcolor">
                                <xsl:value-of select="$metadataInput/MissionReport/@name"/>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td align="right">
                                <xsl:text>AVCL Mission File</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:element name="a">
                                    <xsl:attribute name="href">
                                        <xsl:text>../../missions/</xsl:text>
                                        <xsl:value-of select="$missionFileName"/>
                                    </xsl:attribute>
                                    <xsl:value-of select="$missionFileName"/>
                                </xsl:element>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:value-of select="$metadataInput/MissionReport/statement/@name"/>
                            </td>
                            <td align="left">
                                <xsl:value-of select="$metadataInput/MissionReport/statement/text"/>
                            </td>
                        </tr>
                      <!-- ============================================================ -->
                        <tr>
                            <td colspan="2" class="subheading">
                                <b>
                                    <xsl:value-of select="$metadataInput/MissionReport/id/@name"/>
                                </b>
                            </td>
                        </tr>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/date) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/date/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/date"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/time) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/time/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/time"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/location) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/location/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/location"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/personnel) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/personnel/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/personnel"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/author) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/author/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/author"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/id/live) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/live/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/id/live"/>
                                </td>
                            </tr>
                        </xsl:if>
                      <!-- ============================================================ -->
                        <tr>
                            <td colspan="2" class="subheading">
                                <b>
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/@name"/>
                                </b>
                            </td>
                        </tr>
                        <xsl:if test="string-length($metadataInput/MissionReport/environment/SeaState) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/SeaState/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/SeaState"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/environment/SetDrift) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/SetDrift/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/SetDrift"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/environment/wind) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/wind/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/wind"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/environment/navfix) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/navfix/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/environment/navfix"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <!-- ============================================================ -->
                        <xsl:variable name="objectivesMission" select="string-length($metadataInput/MissionReport/objectives/mission)"/>
                <!-- Debug message
                <xsl:message>
                    <xsl:text>objectivesMission=</xsl:text>
                    <xsl:value-of select="$objectivesMission"/>
                </xsl:message>
                -->
                        <xsl:if test="$objectivesMission">
                        </xsl:if>
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/objectives/@name"/>
                                    </b>
                                </td>
                            </tr>
                        <xsl:if test="$objectivesMission">
                        </xsl:if>
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/mission/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/mission"/>
                                </td>
                            </tr>
                        <xsl:if test="string-length($metadataInput/MissionReport/objectives/hardware) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/hardware/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/hardware"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/objectives/software) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/software/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/software"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/objectives/communications) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/communications/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/objectives/communications"/>
                                </td>
                            </tr>
                        </xsl:if>
                      <!-- ============================================================ -->
                        <xsl:if test="(string-length($metadataInput/MissionReport/hardware/mechanical) > 0)
                                   or (string-length($metadataInput/MissionReport/hardware/electrical) > 0)
                                   or (string-length($metadataInput/MissionReport/hardware/sensors) > 0)
                                   or (string-length($metadataInput/MissionReport/hardware/communications) > 0)
                                   or (string-length($metadataInput/MissionReport/hardware/other) > 0)">
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/hardware/@name"/>
                                    </b>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/hardware/mechanical) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/mechanical/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/mechanical"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/hardware/electrical) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/electrical/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/electrical"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/hardware/sensors) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/sensors/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/sensors"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/hardware/communications) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/communications/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/communications"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/hardware/other) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/other/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/hardware/other"/>
                                </td>
                            </tr>
                        </xsl:if>
                      <!-- ============================================================ -->
                        <xsl:if test="(string-length($metadataInput/MissionReport/software/versions) > 0)
                                   or (string-length($metadataInput/MissionReport/software/control) > 0)
                                   or (string-length($metadataInput/MissionReport/software/sensors) > 0)
                                   or (string-length($metadataInput/MissionReport/software/communications) > 0)
                                   or (string-length($metadataInput/MissionReport/software/archiveDirectory) > 0)
                                   or (string-length($metadataInput/MissionReport/software/archiveUrl) > 0)">
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/software/@name"/>
                                    </b>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/software/versions) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/versions/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/versions"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/software/control) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/control/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/control"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/software/sensors) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/sensors/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/sensors"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/software/communications) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/communications/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/communications"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/software/other) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/other/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/software/other"/>
                                </td>
                            </tr>
                        </xsl:if>
                      <!-- ============================================================ -->
                        <xsl:if test="$metadataInput/MissionReport/TroubleLog/problem[string-length(.) > 0]">
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/TroubleLog/@name"/>
                                    </b>
                                </td>
                            </tr>
                            <xsl:for-each select="$metadataInput/MissionReport/TroubleLog/problem">
                                <xsl:if test="string-length(.) > 0">
                                    <tr>
                                        <td align="right">
                                            <xsl:value-of select="./@name"/>
                                        </td>
                                        <td align="left">
                                            <xsl:value-of select="."/>
                                        </td>
                                    </tr>
                                </xsl:if>
                            </xsl:for-each>
                        </xsl:if>
                      <!-- ============================================================ -->
                        <xsl:if test="(string-length($metadataInput/MissionReport/DataCollection/narratives) > 0)
                                   or (string-length($metadataInput/MissionReport/DataCollection/files) > 0)
                                   or (string-length($metadataInput/MissionReport/DataCollection/photos) > 0)
                                   or (string-length($metadataInput/MissionReport/DataCollection/missions) > 0)
                                   or (string-length($metadataInput/MissionReport/DataCollection/other) > 0)">
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/DataCollection/@name"/>
                                    </b>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/narratives) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/narratives/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/narratives"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/files) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/files/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/files"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/photos) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/photos/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/photos"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/missions) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/missions/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/missions"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/archiveDirectory) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/archiveDirectory/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/archiveDirectory"/>
                                </td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="string-length($metadataInput/MissionReport/DataCollection/archiveUrl) > 0">
                            <tr>
                                <td align="right">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/archiveUrl/@name"/>
                                </td>
                                <td align="left">
                                    <xsl:value-of select="$metadataInput/MissionReport/DataCollection/archiveUrl"/>
                                </td>
                            </tr>
                        </xsl:if>

                      <!-- ============================================================ -->
                        <xsl:if test="$metadataInput/MissionReport/relatedmissions/mission[string-length(.) > 0]">
                            <tr>
                                <td colspan="2" class="subheading">
                                    <b>
                                        <xsl:value-of select="$metadataInput/MissionReport/relatedmissions/@name"/>
                                    </b>
                                </td>
                            </tr>
                            <xsl:for-each select="$metadataInput/MissionReport/relatedmissions/mission">
                                <xsl:if test="string-length(.) > 0">
                                    <tr>
                                        <td align="right">
                                            <xsl:value-of select="./@name"/>
                                        </td>
                                        <td align="left">
                                            <xsl:value-of select="."/>
                                        </td>
                                    </tr>
                                </xsl:if>
                            </xsl:for-each>
                        </xsl:if>

                      <!-- ============================================================ -->
                        <tr>
                            <td colspan="2" class="subheading">
                                <b>
                                    <xsl:value-of select="$metadataInput/MissionReport/conclusion/@name"/>
                                </b>
                            </td>
                        </tr>
                        <xsl:for-each select="$metadataInput/MissionReport/conclusion/text">
                            <xsl:if test="string-length(.) > 0">
                                <tr>
                                    <td align="right">
                                        <xsl:value-of select="./@name"/>
                                    </td>
                                    <td align="left">
                                        <xsl:value-of select="."/>
                                    </td>
                                </tr>
                            </xsl:if>
                        </xsl:for-each>
                    </tbody>
                </table>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>
                <!-- ============================================================ -->

                <p>
                    <a name="Header"> </a>
                </p>
                <table align="center" border="1" summary="Header information for this AVCL mission" title="AVCL Mission Header" width="90%" cellpadding="2">
                    <thead>
                        <tr>
                            <th align="center" colspan="2" class="bgcolor">
                                <xsl:text>AVCL Mission Header</xsl:text>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <xsl:for-each select="/AVCL/head/meta">
                            <tr>
                                <td width="25%" align="right">
                                    <xsl:value-of select="@name"/>
                                </td>
                                <td align="left">
                                    <xsl:choose>
                                        <xsl:when test="(@name='identifier')">
                                            <xsl:element name="a">
                                                <xsl:attribute name="href">
                                                    <xsl:value-of select="@content"/>
                                                </xsl:attribute>
                                                <xsl:value-of select="@content"/>
                                            </xsl:element>
                                        </xsl:when>
                                        <xsl:when test="contains(@content,'.xml') and (@name='title') or (@name='mission') or (@name='MissionMetadata') or (@name='reference') or (@name='image') or (@name='photo') or (@name='video')">
                                            <xsl:element name="a">
                                                <xsl:attribute name="href">
                                                    <xsl:text>../../missions/</xsl:text>
                                                    <xsl:value-of select="@content"/>
                                                </xsl:attribute>
                                                <xsl:value-of select="@content"/>
                                            </xsl:element>
                                        </xsl:when>
                                        <xsl:when test="(@name='generator')">
                                            <xsl:variable name="urlPrefix"  select="substring-before(@content,'http')"/>
                                            <xsl:variable name="urlAddress" select="substring-after (@content,$urlPrefix)"/>
                                            <xsl:value-of select="$urlPrefix"/>
                                            <xsl:element name="a">
                                                <xsl:attribute name="href">
                                                    <xsl:value-of select="$urlAddress"/>
                                                </xsl:attribute>
                                                <xsl:value-of select="$urlAddress"/>
                                            </xsl:element>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:value-of select="@content"/>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </td>
                            </tr>
                        </xsl:for-each>
                    </tbody>
                </table>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>

                <p>
                    <a name="Details"/>
                </p>
                <table align="center" border="1" summary="Mission Details information for this AVCL mission" title="Mission Metadata" width="90%" cellpadding="2">
                    <thead>
                        <tr>
                            <th align="center" colspan="2" class="bgcolor">
                                <xsl:text>AVCL Mission Details</xsl:text>
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Vehicle Type</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="$vehicleType"/>
                            </td>
                        </tr>
                          <tr>
                            <td width="25%" align="right">
                                <xsl:text>Vehicle Name</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="$vehicleName"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Vehicle ID</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/@vehicleID"/>
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" class="subheading">
                                <b>
                                    <xsl:text>Units of Measure</xsl:text>
                                </b>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Angle</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/UnitsOfMeasure/@angle"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Distance</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/UnitsOfMeasure/@distance"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Mass</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/UnitsOfMeasure/@mass"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Time</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/UnitsOfMeasure/@time"/>
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" align="left" class="subheading">
                                <b>
                                    <xsl:text>Geographic Origin</xsl:text>
                                </b>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Latitude</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/GeoOrigin/@latitude"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Longitude</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionPreparation/GeoOrigin/@longitude"/>
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2" align="left" class="subheading">
                                <b>
                                    <xsl:text>Time Units</xsl:text>
                                </b>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Start Time</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@day"/>
                                <xsl:text> </xsl:text>
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@month"/>
                                <xsl:text> </xsl:text>
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@year"/>
                                <xsl:text> </xsl:text>
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@hour"/>
                                <xsl:text>:</xsl:text>
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@minute"/>
                                <xsl:text>:</xsl:text>
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@second"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Time Zone</xsl:text>
                            </td>
                            <td align="left">
                                <xsl:value-of select="/AVCL/body/MissionResults/MissionStartTime/@timeZone"/>
                            </td>
                        </tr>
                        <tr>
                            <td width="25%" align="right">
                                <xsl:text>Sample Time Interval</xsl:text>
                            </td>
                            <td align="left">
                                <!-- TODO: Something is not right with this value's derivation -->
                                <xsl:value-of select="round(count(/AVCL/body/MissionResults/SampledResults/@timeStamp) div 60)"/>
                                <xsl:text> seconds</xsl:text>
                            </td>
                        </tr>
                    </tbody>
                </table>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>

                <a name="MissionCommands">
                    <table align="center" border="1" summary="Snapshots" title="Screen Snapshots" width="90%" cellpadding="2">
                        <thead>
                            <tr>
                                <th align="center" colspan="2" class="bgcolor">
                                    <xsl:text>AVCL Mission Commands</xsl:text>
                                </th>
                            </tr>
                        </thead>
                    </table>
                </a>
                <p/>
                    
		<xsl:call-template name="command-script"/>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>
                
		<p>
                    <a name="Images"/>
                </p>
                <xsl:call-template name="mission-images"/>
                <xsl:element name="hr"/>

                <!-- Debug message -->
                <!--xsl:message>
                    <xsl:text>Current DTG is: </xsl:text>
                    <xsl:value-of select="$dateTimeGroup"/>
                </xsl:message-->
                
                <h2> <a name="Credits">Credits</a> </h2>

                <blockquote>
                    This report was generated by the
                    <a href="https://savage.nps.edu/AuvWorkbench">Autonomous Unmanned Vehicle (AUV) Workbench</a>,
                    an open-source tool
                    for robot-mission planning, rehearsal, execution, and playback visualization.
                    <br />
                    Available online at
                    <a href="https://savage.nps.edu/AuvWorkbench">https://savage.nps.edu/AuvWorkbench</a>.
                </blockquote>
                <blockquote>
                    This report was generated on
                    <xsl:value-of select="$dateTimeGroup"/>.
                </blockquote>
	    </body>
	</xsl:element>
    </xsl:template>

    <!-- Get help filename matching image filename -->
    <xsl:template name="getChartHelpFileName">
        <xsl:param name="imageFileName"/>
        
        <xsl:value-of select="$onlineDirectoryTelemetryPlotHelp"/>
        <xsl:choose>
            <xsl:when test="($imageFileName='chartXy')">
                <xsl:text>HelpChartXy.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartXyz')">
                <xsl:text>HelpChartXyz.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartZ')">
                <xsl:text>HelpChartVertical.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartOrientations')">
                <xsl:text>HelpChartOrientations.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartLinearVelocities')">
                <xsl:text>HelpChartLinearVelocities.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartRotationalVelocities')">
                <xsl:text>HelpChartRotationalVelocities.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartPropellers')">
                <xsl:text>HelpChartPropellers.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartRudder')">
                <xsl:text>HelpCharthartRudder.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartPlaneSurfaces')">
                <xsl:text>HelpChartPlanes.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartBodyThrusters')">
                <xsl:text>HelpChartBodyThrusters.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartLateralThrustersCourse')">
                <xsl:text>HelpChartLateralThrustersAndCourse.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartVerticalThrustersDepth')">
                <xsl:text>HelpChartVerticalThrustersAndDepth.html</xsl:text>
            </xsl:when>
            <xsl:when test="($imageFileName='chartRemainingBatteryPower')">
                <xsl:text>HelpChartRemainingBatteryPower.html</xsl:text>
            </xsl:when>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template name="command-script">
        <xsl:element name="table">
            <xsl:attribute name="border">
                <xsl:text>5</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="cellpadding">
                <xsl:text>10</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="cellspacing">
                <xsl:text>2</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="align">
                <xsl:text>center</xsl:text>
            </xsl:attribute>
            <xsl:element name="thead">
                <xsl:element name="tr">
                    <xsl:element name="th">
                        <xsl:attribute name="colspan">
                            <xsl:text>6</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="class">
                            <xsl:text>bgcolor</xsl:text>
                        </xsl:attribute>
                        <xsl:text>AVCL Mission Command List: </xsl:text>
                        <xsl:element name="a">
                            <xsl:attribute name="href">
                                <xsl:text>../../missions/</xsl:text>
                                <xsl:value-of select="$missionFileName"/>
                            </xsl:attribute>
                            <xsl:value-of select="$missionFileName"/>
                        </xsl:element>
                    </xsl:element>
                </xsl:element>
                <xsl:element name="tr">
                    <xsl:element name="th">
                        <xsl:text>#</xsl:text>
                    </xsl:element>
                    <xsl:element name="th">
                        <xsl:text>Command</xsl:text>
                    </xsl:element>
                    <xsl:element name="th">
                        <xsl:text>X Position</xsl:text>
                    </xsl:element>
                    <xsl:element name="th">
                        <xsl:text>Y Position</xsl:text>
                    </xsl:element>
                    <xsl:element name="th">
                        <xsl:text>Z Position</xsl:text>
                    </xsl:element>
                    <xsl:element name="th">
                        <xsl:text>Description</xsl:text>
                    </xsl:element>
                </xsl:element>
            </xsl:element> <!-- thead -->
                    <xsl:message>
                        <xsl:text>$commandScriptType=</xsl:text>
                        <xsl:value-of select="$commandScriptType"/>
                    </xsl:message>
            
            <xsl:element name="tbody">
                <!-- Solution from: http://www.stylusstudio.com/xsllist/200209/post80690.html -->
                <xsl:for-each select="//AVCL/body/MissionPreparation/*[name()=$commandScriptType]/*">

                    <!-- Helpful debug messages for this type of 'for loop' -->
                    <xsl:message>
                        <xsl:text>node name is: </xsl:text>
                        <xsl:value-of select="local-name()"/>
                    </xsl:message>
                    <!--
                    <xsl:message>
                        <xsl:text>expression is: </xsl:text>
                        <xsl:value-of select="local-name()=$setPositionType"/>
                    </xsl:message>
                    -->
                    
                    <xsl:choose>

                        <xsl:when test="(local-name()='LaunchPosition') or (local-name()='RecoveryPosition')">
                            <xsl:call-template name="LaunchRecoveryPosition"/>
                        </xsl:when>
                        <xsl:when test="(local-name()='GoalList')">
                            <!-- table entry handled by RecoveryPosition -->

                            <!-- process child agenda items-->

                            <xsl:for-each select="*">

                                <!-- Helpful debug messages for this type of 'for loop' -->
                                <!--xsl:message>
                                    <xsl:text>node name is: </xsl:text>
                                    <xsl:value-of select="local-name()"/>
                                </xsl:message>
                                <xsl:message>
                                    <xsl:text>expression is: </xsl:text>
                                    <xsl:value-of select="local-name()=$setPositionType"/>
                                </xsl:message-->
                                
                                <!-- alternate row background colors for readability -->
                                <xsl:variable name="backgroundColor">
                                    <xsl:choose>
                                        <xsl:when test="(position() mod 2) = 1">
                                            <xsl:text>#F4F4F4;</xsl:text>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:text>#FFFFFF;</xsl:text>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:variable>
                                <xsl:choose>
                                    <xsl:when test="(local-name()='Goal')">
                                        <tr style="background-color:{$backgroundColor}">
                                            <td><xsl:value-of select="position()+1"/></td>
                                            <td>
                                                <!-- <xsl:text>Goal: </xsl:text> --> <!-- note trailing underscore character -->
                                                <xsl:variable name="goalName" select="local-name(*[1])"/>
                                                <a href="{concat($avclSchemaUrl,$goalName,'.html')}" target="_AVCL">
                                                    <xsl:value-of select="$goalName"/>
                                                </a>
                                                <xsl:if test="(@alert = 'true')">
                                                    <!-- TODO tooltip -->
                                                    <span class="color:orange;"><xsl:text> (alert)</xsl:text></span>
                                                </xsl:if>
                                            </td>
                                            <td><xsl:value-of select="@id"/></td>
                                            <td>
                                                <span style="color:green;">
                                                    <xsl:choose>
                                                        <xsl:when test="string-length(@nextOnSucceed) > 1">
                                                            <xsl:value-of select="@nextOnSucceed"/>
                                                        </xsl:when>
                                                        <xsl:otherwise>
                                                            <xsl:text>(Recovery Point)</xsl:text>
                                                        </xsl:otherwise>
                                                    </xsl:choose>
                                                </span>
                                            </td>
                                            <td>
                                                <span style="color:red;">
                                                    <xsl:choose>
                                                        <xsl:when test="string-length(@nextOnFail) > 1">
                                                            <xsl:value-of select="@nextOnFail"/>
                                                        </xsl:when>
                                                        <xsl:otherwise>
                                                            <xsl:text>(Recovery Point)</xsl:text>
                                                        </xsl:otherwise>
                                                    </xsl:choose>
                                                </span>
                                            </td>
                                            <td rowspan="3" style="vertical-align:text-top;">
                                                <xsl:value-of select="@description"/>
                                            </td>
                                            <!-- TODO properly show @alert attribute -->
                                        </tr>
                                        <tr style="background-color:{$backgroundColor}">
                                            <td></td>
                                            <td colspan="4">
                                                <xsl:for-each select="*[1]/@*">
                                                    <!-- output each goal attribute -->
                                                    <xsl:value-of select="local-name()"/>
                                                    <xsl:text>=</xsl:text>
                                                    <xsl:value-of select="."/>
                                                    <xsl:if test="not(position()=last())">
                                                        <xsl:text>, </xsl:text>
                                                    </xsl:if>
                                                </xsl:for-each>
                                                <xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text><!-- consistent vertical height for this row even if no attributes -->
                                            </td>
                                        </tr>
                                        <tr style="background-color:{$backgroundColor}">
                                            <td></td>
                                            <td colspan="4">
                                                <xsl:text>Operating Area: </xsl:text>
                                                <xsl:value-of select="local-name(OperatingArea/*[1])"/>
                                                <xsl:text>, duration=</xsl:text>
                                                <xsl:value-of select="Duration/@value"/>
                                                <xsl:text> seconds </xsl:text>
                                                <!-- TODO convert to readable time -->
                                            </td>
                                        </tr>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:call-template name="otherCommand"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:for-each>
                        </xsl:when>
                        <xsl:when test="(local-name()='ConstraintList')">

                        <tr style="font-weight:bold;">
                            <td></td>
                            <td align="center">
                                <xsl:text>Constraint List</xsl:text>
                            </td>
                            <td>id</td>
                            <td></td>
                            <td></td>
                            <td></td> <!-- Description header already at top of column -->
                        </tr>
            
                            <!-- process child constraint items-->
                            <xsl:for-each select="*">

                                <!-- Helpful debug messages for this type of 'for loop' -->
                                <!--xsl:message>
                                    <xsl:text>node name is: </xsl:text>
                                    <xsl:value-of select="local-name()"/>
                                </xsl:message>
                                <xsl:message>
                                    <xsl:text>expression is: </xsl:text>
                                    <xsl:value-of select="local-name()=$setPositionType"/>
                                </xsl:message-->
                                
                                <!-- alternate row background colors for readability -->
                                <xsl:variable name="backgroundColor">
                                    <xsl:choose>
                                        <xsl:when test="(position() mod 2) = 1">
                                            <xsl:text>#F4F4F4;</xsl:text>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:text>#FFFFFF;</xsl:text>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:variable>
                                <xsl:choose>
                                    <xsl:when test="(local-name()='AvoidArea')">
                                        <tr style="background-color:{$backgroundColor}">
                                            <td><xsl:value-of select="position()+count(//GoalList/*)+1"/></td>
                                            <td>
                                                <!-- <xsl:text>Goal: </xsl:text> --> <!-- note trailing underscore character -->
                                                <xsl:variable name="avoidAreaName" select="local-name()"/>
                                                <a href="{concat($avclSchemaUrl,$avoidAreaName,'.html')}" target="_AVCL">
                                                    <xsl:value-of select="$avoidAreaName"/>
                                                </a>
                                                <xsl:if test="(@alert = 'true')">
                                                    <!-- TODO tooltip -->
                                                    <span class="color:orange;"><xsl:text> (alert)</xsl:text></span>
                                                </xsl:if>
                                            </td>
                                            <td><xsl:value-of select="@id"/></td>
                                            <td></td>
                                            <td></td>
                                            <td rowspan="3" style="vertical-align:text-top;">
                                                <xsl:value-of select="@description"/>
                                            </td>
                                            <!-- TODO properly show @alert attribute -->
                                        </tr>
                                        <tr style="background-color:{$backgroundColor}">
                                            <td></td>
                                            <td colspan="4">
                                                <xsl:text>Avoidance Area: </xsl:text>
                                                <!-- no intervening OperatingArea element -->
                                                <xsl:value-of select="local-name(*[1])"/>
                                                <xsl:text>, duration=</xsl:text>
                                                <xsl:value-of select="Duration/@value"/>
                                                <xsl:text> seconds </xsl:text>
                                                <!-- TODO convert to readable time -->
                                            </td>
                                        </tr>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:call-template name="otherCommand"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:for-each>
                        </xsl:when>

                        <xsl:when test="(local-name()=$setPositionType)">
                            <xsl:call-template name="setPosition"/>
                        </xsl:when>

                        <xsl:when test="local-name()=$loiterType">
                            <xsl:call-template name="loiter"/>
                        </xsl:when>

                        <xsl:when test="local-name()=$waypointType">
                            <xsl:call-template name="waypoint"/>
                        </xsl:when>

                        <xsl:when test="local-name()='MakeKnots'">
                            <xsl:call-template name="makeKnots"/>
                        </xsl:when>

                        <xsl:when test="local-name()='SendMessage'">
                            <xsl:call-template name="SendMessage"/>
                        </xsl:when>

                        <xsl:when test="local-name()='MakeDepth'">
                             <xsl:call-template name="MakeDepth"/>
                        </xsl:when>
                        <xsl:when test="local-name()='MakeSpeed'">
                             <xsl:call-template name="MakeSpeed"/>
                        </xsl:when>
                        <xsl:when test="local-name()='Meta'">
                             <xsl:call-template name="Meta"/>
                        </xsl:when>
                        <xsl:when test="local-name()='SetPlanes'">
                             <xsl:call-template name="SetCommandValuePercent"/>
                        </xsl:when>
                        <xsl:when test="local-name()='SetPower'">
                             <xsl:call-template name="SetCommandValuePercent"/>
                        </xsl:when>
                        <xsl:when test="local-name()='Wait'">
                             <xsl:call-template name="Wait"/>
                        </xsl:when>
                        <xsl:when test="local-name()='SetRudder'">
                             <xsl:call-template name="SetRudder"/>
                        </xsl:when>
                         <xsl:when test="local-name()='Thrusters'">
                             <xsl:call-template name="SetCommandValueBoolean"/>
                        </xsl:when>

                        <xsl:otherwise>
                            <xsl:call-template name="otherCommand"/>
                        </xsl:otherwise>
                    </xsl:choose>

                </xsl:for-each>

                <xsl:element name="tr">
                    <xsl:element name="th">
                        <xsl:attribute name="colspan">
                            <xsl:text>6</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="class">
                            <xsl:text>bgcolor</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Reference: </xsl:text>
                        <a href="https://savage.nps.edu/Savage/AuvWorkbench/AVCL/AVCL.html">Autonomous Vehicle Control Language (AVCL)</a>
                    </xsl:element>
                </xsl:element>

            </xsl:element> <!-- tbody -->
        </xsl:element> <!-- table -->
    </xsl:template>

    <xsl:template name="setPosition">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="$setPositionType"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@x"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@y"/>
            </xsl:element>
            <xsl:element name="td">
                    <xsl:text/>
                </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template name="LaunchRecoveryPosition">
        <xsl:variable name="backgroundColor"><xsl:text>#F4F4F4;</xsl:text></xsl:variable>
        <tr style="background-color:{$backgroundColor}">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <a href="{concat($avclSchemaUrl,local-name(),'.html')}" target="_AVCL">
                    <xsl:value-of select="local-name()"/>
                </a>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@x"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@y"/>
            </xsl:element>
            <xsl:element name="td">
                    <xsl:text/>
                </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </tr>
        <xsl:if test="(local-name()='RecoveryPosition')">
            <tr style="font-weight:bold;">
                <td></td>
                <td align="center">
                    <xsl:text>Goal List</xsl:text>
                </td>
                <td>id</td>
                <td style="">nextOnSucceed</td>
                <td style="">nextOnFail</td>
                <td></td> <!-- Description header already at top of column -->
            </tr>
        </xsl:if>
    </xsl:template>

    <xsl:template name="loiter">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="$loiterType"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@x"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@y"/>
            </xsl:element>
            <xsl:element name="td">
                    <xsl:text/>
                </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template name="waypoint">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="$waypointType"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@x"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="XYPosition/@y"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="Speed/@value"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template name="makeKnots">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text>MakeKnots</xsl:text>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="@value"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="MakeDepth">
        <tr>
            <td align="right">
                <xsl:value-of select="position()-1"/>
            </td>
            <td>
                <xsl:text>MakeDepth</xsl:text>
            </td>
            <td align="left" colspan="3">
                <xsl:value-of select="@value"/>
                <xsl:text> meters </xsl:text>
            </td>
            <td align="left">
                <xsl:value-of select="@description"/>
            </td>
        </tr>
    </xsl:template>
    
    <xsl:template name="MakeSpeed">
        <tr>
            <td align="right">
                <xsl:value-of select="position()-1"/>
            </td>
            <td>
                <xsl:text>MakeSpeed</xsl:text>
            </td>
            <td align="left" colspan="3">
                <xsl:value-of select="@value"/>
                <xsl:text> meters/second </xsl:text>
            </td>
            <td align="left">
                <xsl:value-of select="@description"/>
            </td>
        </tr>
    </xsl:template>
    
    <xsl:template name="Meta">
        <tr>
            <td align="right">
                <xsl:value-of select="position()-1"/>
            </td>
            <td>
                <xsl:text>Meta</xsl:text>
            </td>
            <td>
                <xsl:value-of select="@name"/>
            </td>
            <td align="left" colspan="2">
                <xsl:value-of select="@content"/>
            </td>
            <td align="left">
                <xsl:value-of select="@description"/>
            </td>
        </tr>
    </xsl:template>
    
    <xsl:template name="SendMessage">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text>SendMessage</xsl:text>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text>Sender=</xsl:text>
                <xsl:value-of select="AVCLMessage/head/Sender/@value"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:text>Recipient=</xsl:text>
                <xsl:value-of select="AVCLMessage/head/Recipient/@value"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:for-each select="AVCLMessage/body/*">
                    <!-- TODO more work here -->
                    <xsl:value-of select="local-name()"/>
                    <xsl:value-of select="."/>
                </xsl:for-each>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="AVCLMessage/@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="SetCommandValuePercent">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="local-name()"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="local-name(*[1])"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="colspan">
                    <xsl:text>2</xsl:text>
                </xsl:attribute>
           <!-- <xsl:text>value </xsl:text> -->
                <xsl:value-of select="*[1]/@value"/>
                <xsl:text>%</xsl:text>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="SetCommandValueBoolean">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="local-name()"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="colspan">
                    <xsl:text>3</xsl:text>
                </xsl:attribute>
                <xsl:text>enabled="</xsl:text>
                <xsl:value-of select="@value"/>
                <xsl:text>"</xsl:text>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
    
    <xsl:template name="Wait">
        <tr>
            <td align="right">
                <xsl:value-of select="position()-1"/>
            </td>
            <td>
                <xsl:text>Wait</xsl:text>
            </td>
            <td align="left" colspan="3">
                <xsl:value-of select="@value"/>
                <xsl:text> seconds</xsl:text>
            </td>
            <td align="left">
                <xsl:value-of select="@description"/>
            </td>
        </tr>
    </xsl:template>
    
    <xsl:template name="SetRudder">
        <tr>
            <td align="right">
                <xsl:value-of select="position()-1"/>
            </td>
            <td>
                <xsl:text>SetRudder</xsl:text>
            </td>
            <td align="left" colspan="3">
                <xsl:choose>
                    <!-- TODO verify direction -->
                    <xsl:when test="@value > 0">
                        <xsl:text> left </xsl:text>
                    </xsl:when>
                    <xsl:when test="0 > @value">
                        <xsl:text> right </xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text> amidships </xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:value-of select="@value"/>
                <xsl:text> % </xsl:text>
            </td>
            <td align="left">
                <xsl:value-of select="@description"/>
            </td>
        </tr>
    </xsl:template>

    <xsl:template name="otherCommand">
        <xsl:element name="tr">
            <xsl:element name="td">
                <xsl:attribute name="align">
                    <xsl:text>right</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="position()-1"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="local-name()"/>
            </xsl:element>
            <xsl:element name="td">
                <xsl:attribute name="colspan">
                    <xsl:text>3</xsl:text>
                </xsl:attribute>
                <xsl:attribute name="align">
                    <xsl:text>left</xsl:text>
                </xsl:attribute>
                <xsl:for-each select="@*[local-name()!='description']">
                    <xsl:value-of select="local-name()"/>
                    <xsl:text>="</xsl:text>
                    <xsl:value-of select="."/>
                    <xsl:text>"</xsl:text>
                    <br />
                </xsl:for-each>
            </xsl:element>
            <xsl:element name="td">
                <xsl:value-of select="@description"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template name="mission-images">

	<!-- ===========================
        <p>
            <a name="X3D"/>
        </p>
        <table align="center" border="1" summary="X3D Scene" title="X3D Scene" width="90%" cellpadding="2">
            <thead>
                <tr>
                    <th align="center" colspan="2" class="bgcolor">
                        <xsl:text>X3D Scene</xsl:text>
                    </th>
                </tr>
            </thead>
        </table>
        <p/>
	<p>
            <xsl:apply-templates select="$metadataInput/MissionReport/scene/x3d"/>
        </p>
         -->

        <!-- TODO add links to supporting component scenes -->

	<!-- =========================== -->
        <p>
            <a name="Snapshots"/>
        </p>
        <table align="center" border="1" summary="Snapshots" title="Screen Snapshots" width="90%" cellpadding="2">
            <thead>
                <tr>
                    <th align="center" colspan="2" class="bgcolor">
                        <xsl:text>Screen Snapshots</xsl:text>
                    </th>
                </tr>
            </thead>
        </table>
        <table align="center" border="0" summary="Plots" title="Mission Telemetry Plot Charts" width="90%" cellpadding="2">
            <tbody>
                <tr border="0">
                    <th align="right" colspan="2">
                        <xsl:element name="a">
                            <xsl:attribute name="href">
                                <xsl:text>#top</xsl:text>
                            </xsl:attribute>
                            <xsl:attribute name="style">
                                <xsl:text>font-size:small;color:#990000</xsl:text>
                            </xsl:attribute>
                            <xsl:text>Back to top</xsl:text>
                        </xsl:element>
                    </th>
                </tr>
            </tbody>
        </table>
        <!-- Screen Snapshots table of contents (TOC) -->
        <table align="center" border="1" summary="Screen Snapshots" title="Screen Snapshots" cellpadding="2">
            <tbody>
                <xsl:for-each select="$metadataInput/MissionReport/images/missionview">
                    <tr>
                        <td align="center">
                            <a href="#{@name}">
                                <xsl:text>Figure </xsl:text>
                                <xsl:number value="java:size($javaCounter)+ position()" format="1"/>
                            </a>
                        </td>
                        <td>
                            <a href="#{@name}">
                                <xsl:value-of select="@name"/>
                            </a>
                        </td>
                    </tr>
                </xsl:for-each>
            </tbody>
        </table>
        <p/>

        <xsl:apply-templates select="$metadataInput/MissionReport/images/missionview"/>

	<!-- =========================== -->
        <p>
            <a name="Plots"/>
        </p>
        <table align="center" border="1" summary="Plots" title="Mission Telemetry Plot Charts" width="90%" cellpadding="2">
            <thead>
                <tr>
                    <th align="center" colspan="2" class="bgcolor">
                        <xsl:text>Mission Telemetry Plot Charts</xsl:text>
                    </th>
                </tr>
            </thead>
        </table>
        <table align="center" border="0" summary="Plots" title="Mission Telemetry Plot Charts" width="90%" cellpadding="2">
            <tbody>
                <tr>
                    <th align="right" colspan="2">
                        <xsl:element name="a">
                            <xsl:attribute name="href">
                                <xsl:text>#top</xsl:text>
                            </xsl:attribute>
                            <xsl:attribute name="style">
                                <xsl:text>font-size:small;color:#990000</xsl:text>
                            </xsl:attribute>
                            <xsl:text>Back to top</xsl:text>
                        </xsl:element>
                    </th>
                </tr>
            </tbody>
        </table>
        <!-- Plots table of contents (TOC) -->
        <table align="center" border="1" summary="Plots" title="Mission Telemetry Plot Charts" cellpadding="2">
            <tbody>
                <xsl:for-each select="$metadataInput/MissionReport/plots/jFreeChart">
                    <tr>
                        <td align="center">
                            <a href="#{@name}">
                                <xsl:text>Figure </xsl:text>
                                <xsl:number value="java:size($javaCounter)+ position()" format="1"/>
                            </a>
                        </td>
                        <td>
                            <a href="#{@name}">
                                <xsl:value-of select="@name"/>
                            </a>
                        </td>
                        <td>
                            <xsl:variable name="helpAddress">
                                <xsl:call-template name="getChartHelpFileName">
                                    <xsl:with-param name="imageFileName" select="@name"/>
                                </xsl:call-template>
                            </xsl:variable>
                            <a href="{$helpAddress}"> <!--  target="_plotHelp" -->
                                <xsl:text>description</xsl:text>
                            </a>
                        </td>
                    </tr>
                </xsl:for-each>
            </tbody>
        </table>
        <p/>

	<xsl:apply-templates select="$metadataInput/MissionReport/plots/jFreeChart"/>
        
	<!-- =========================== -->

    </xsl:template>

    <xsl:template match="x3d">
	<xsl:element name="div">
	    <xsl:attribute name="class">
		<xsl:text>center</xsl:text>
	    </xsl:attribute>

            <object data="{.//text()}" type="model/x3d+xml" height="400" width="600">
                <param name="src" value=".//text()"/>
                <!-- the remaining HTML that follows should not need further modification -->
                <param name="DASHBOARD" value="FALSE"/>
                <param name="SPLASHSCREEN" value="FALSE"/>
                <!-- the following anchor-link text is only displayed to user if no X3D plugin is already installed -->
                <div class="noX3dPluginInstalled">
                    <a href="http://www.web3d.org/x3d/content/examples/X3dResources.html#Applications" target="helpPage">Select an X3D plugin to see this scene...</a>
                    <!-- alternate url: http://www.web3d.org/tools/viewers_and_browsers -->
                </div>
            </object>

            <!-- add an index to the array for counting purposes -->
	    <xsl:variable name="addIndex" select="java:add($javaCounter, '1')"/>
	    <xsl:element name="p">
		<xsl:text>Figure </xsl:text>
		<xsl:number value="java:size($javaCounter)" format="1"/>
		<xsl:text>: Mission X3D scene from </xsl:text>
		<xsl:value-of select="@name"/>
	    </xsl:element>
	</xsl:element>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>
    </xsl:template>

    <xsl:template match="missionview">
	<xsl:element name="div">
	    <xsl:attribute name="class">
		<xsl:text>center</xsl:text>
	    </xsl:attribute>
            
            <a name="{@name}">
                <xsl:element name="a">
                    <xsl:attribute name="href">
                        <xsl:value-of select=".//text()"/>
                    </xsl:attribute>
                    <xsl:attribute name="style">
                        <xsl:text>border:0</xsl:text>
                    </xsl:attribute>
                    <xsl:element name="img">
                        <!-- border shows that img is linked -->
                        <xsl:attribute name="border">
                            <xsl:text>1</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="width">
                            <xsl:value-of select="$figureWidth"/>
                        </xsl:attribute>
                        <xsl:attribute name="src">
                            <xsl:value-of select=".//text()"/>
                        </xsl:attribute>
                        <xsl:attribute name="alt">
                            <xsl:text>Mission image view</xsl:text>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </a>

            <!-- add an index to the array for counting purposes -->
	    <xsl:variable name="addIndex" select="java:add($javaCounter, '1')"/>
	    <xsl:element name="p">
		<xsl:text>Figure </xsl:text>
		<xsl:number value="java:size($javaCounter)" format="1"/>
		<xsl:text>: Resulting mission image from </xsl:text>
		<xsl:value-of select="@name"/>
	    </xsl:element>
	</xsl:element>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#Snapshots</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to snapshot index</xsl:text>
                    </xsl:element>
                    
                    <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
                    
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>
    </xsl:template>

    <xsl:template match="jFreeChart">
        
	<xsl:element name="div">
	    <xsl:attribute name="class">
		<xsl:text>center</xsl:text>
	    </xsl:attribute>
	    <xsl:attribute name="valign">
		<xsl:text>top</xsl:text>
	    </xsl:attribute>
            
            <a name="{@name}">
                <xsl:element name="a">
                    <xsl:attribute name="href">
                        <xsl:value-of select=".//text()"/>
                    </xsl:attribute>
                    <xsl:attribute name="style">
                        <xsl:text>border:0</xsl:text>
                    </xsl:attribute>
                    <xsl:element name="img">
                        <!-- border shows that img is linked -->
                        <xsl:attribute name="border">
                            <xsl:text>1</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="width">
                            <xsl:value-of select="$figureWidth"/>
                        </xsl:attribute>
                        <xsl:attribute name="src">
                            <xsl:value-of select=".//text()"/>
                        </xsl:attribute>
                        <xsl:attribute name="alt">
                            <xsl:text>Mission plot chart</xsl:text>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </a>
        
            <!-- add an index to the array for counting purposes -->
	    <xsl:variable name="addIndex" select="java:add($javaCounter, '1')"/>
            
            <xsl:variable name="helpAddress">
                <xsl:call-template name="getChartHelpFileName">
                    <xsl:with-param name="imageFileName" select="@name"/>
                </xsl:call-template>
            </xsl:variable>
                
	    <xsl:element name="p">
		<xsl:text>Figure </xsl:text>
		<xsl:number value="java:size($javaCounter)" format="1"/>
		<xsl:text>: Telemetry plot </xsl:text>
		<xsl:value-of select="@name"/>
		<xsl:text> (</xsl:text>
                <a href="{$helpAddress}"> <!--  target="_plotHelp" -->
                    <xsl:text>description</xsl:text>
                </a>
		<xsl:text>)</xsl:text>
	    </xsl:element>
        
            <!-- TODO also show help in the document
            <xsl:call-template name="getHtmlBody" mode="copy">
                <xsl:with-param name="pageUrl" select="$helpAddress"/>
            </xsl:call-template>
            -->            

	</xsl:element>

                <p style="text-align:right;width:95%">
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#Plots</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to plot index</xsl:text>
                    </xsl:element>
                    
                    <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
                    
                    <xsl:element name="a">
                        <xsl:attribute name="href">
                            <xsl:text>#top</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="style">
                            <xsl:text>font-size:small;color:#990000</xsl:text>
                        </xsl:attribute>
                        <xsl:text>Back to top</xsl:text>
                    </xsl:element>
                </p>
    </xsl:template>
    
    <!-- still under development, see http://www.dpawson.co.uk/xsl/sect2/N4554.html#d5768e300 -->
    
    <xsl:template name="getHtmlBody">
        <xsl:param name="pageUrl"/>
            
	<xsl:value-of select="$pageUrl"/>
        <xsl:value-of select="document($pageUrl)"/>
            
    </xsl:template>
    
    <xsl:template match="*" mode="copy">
        <xsl:element name="{local-name()}">
            <xsl:copy-of select="@*" />
            <xsl:apply-templates mode="copy" />
        </xsl:element>
    </xsl:template>

    <xsl:template match="text()" mode="copy">
        <xsl:value-of select="." />
    </xsl:template>

</xsl:stylesheet>