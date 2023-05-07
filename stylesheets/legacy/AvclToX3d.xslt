<?xml version="1.0" encoding="UTF-8"?>
<!--
  <head>
   <meta name="title"       content="AvclToX3d.xslt" />
   <meta name="creator"     content="Dieter Jahn, Mounir Sidhom, Duane Davis, Nick Polys, Don Brutzman" />
   <meta name="created"     content="12 March 2005" />
   <meta name="description" content="XSLT stylesheet to convert AVCL mission scripts to X3D files." />
   <meta name="url"         content="/auv/AuvWorkbench/Scripts/AvclToX3d.xslt" />
   <meta name="version"     content="$Id$"/>
  </head>

    TODO questions for further development:
    _____________________________________

    1. Altitude not revised yet
    2. X3D upper viewpoint needs centering
    3. Clean up min/max code

-->
<!-- Coordinate conversion:

    DEFAULT X3D           GeoLocation              AVCL/DIS
                          GeoTransform

     ^ y up                ^ y up                   ^ -z up
     |                     |                        |
     |                     |                        |
     |______x              |______ -z               |______ x
    /       nose          /         north          /        north
   /                     /                        /
  z                     x                        y
  right-hand side       east                     east

(GeoLocation and GeoTransform are in the X3D Geospatial component)
-->
<!--
Copyright (c) 1995-2012 held by the author(s).  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other materials provided with the
      distribution.
    * Neither the names of the Naval Postgraduate School (NPS)
      Modeling Virtual Environments and Simulation (MOVES) Institute
      (http://www.nps.edu and http://www.movesinstitute.org)
      nor the names of its contributors may be used to endorse or
      promote products derived from this software without specific
      prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-->
<!-- todo add xml default namespace to be X3D -->
<xsl:stylesheet version="2.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:strip-space elements="*"/>
    <xsl:output method="xml"
                encoding="UTF-8"
                omit-xml-declaration="no"
                cdata-section-elements="Script"
                indent="yes"
                media-type="model/x3d+xml"/>
		
    <!-- pass any parameters from AUVW -->
    <xsl:param name="missionFilename">
        <xsl:text>AVCL-Mission</xsl:text>
    </xsl:param>
    <xsl:param name="createdDate"></xsl:param>
    <xsl:param name="revisedDate"></xsl:param>
    <!-- TODO hook up -->
    <xsl:param name="DIS_or_Interpolators">
        <xsl:text>DIS</xsl:text>
    </xsl:param>
    <xsl:param name="waypointsColor">
        <xsl:text>0.8 0.8 0.1</xsl:text>
    </xsl:param>
    <xsl:param name="includeWaypointViewpoints">
        <xsl:text>true</xsl:text>
    </xsl:param>
    <xsl:param name="telemetryColor">
        <xsl:text>0.5 0.5 0.04</xsl:text>
    </xsl:param>
    <xsl:param name="SavageRootDirectory">
        <xsl:text>../../../www.web3d.org/x3d/content/examples</xsl:text>
    </xsl:param>
    <!-- (TODO confirm) AUVW anchor point, decimal degrees -->
    <xsl:param name="applicationGeoOriginLatitude">
        <xsl:text></xsl:text>
    </xsl:param>
    <xsl:param name="applicationGeoOriginLongitude">
        <xsl:text></xsl:text>
    </xsl:param>

    <!-- Need this if you are given a DIS Z value other than zero -->
    <!-- For the GeoLocation's geoCoords Z value only -->
    <xsl:param name="tangentPlaneNormal">
        <xsl:text>0.0</xsl:text>
    </xsl:param>
    <!-- TODO get X3D GeoOrigin for terrain file, update hardwired GeoOrigin tag below -->
       <!-- Viewing parameters -->
    <!--
    maxC is a max coordinate
    minC is the minimum x coordinate for the XYPosition element
    zoomCoordinate are x, y, z of a region in the path
    zoomOrientation is the orientation of the detail viewpoint
    direction is the direction of the top viewpoint
	-->
    <!-- todo if no maxC minC passed to stylesheet or computed, then reset to +- 100. or somesuch -->
    <!-- Path of MissionPreparation -->
    <xsl:variable name="missionPrepPath" select="/AVCL/body/MissionPreparation"/>

    <!-- Variable to identify AVCL type from output telemetry file -->
    <xsl:variable name="avclType">
        <xsl:value-of select="local-name($missionPrepPath/Configuration/*)"/>
    </xsl:variable>

    <!-- Vehicle type, i.e. USV, UAV, UUV or UGV -->
    <xsl:variable name="vehicleType">
        <xsl:if test="$avclType = 'UnmannedAerialVehicle'">
            <xsl:text>UAV</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedGroundVehicle'">
            <xsl:text>UGV</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedSurfaceVehicle'">
            <xsl:text>USV</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedUnderwaterVehicle'">
            <xsl:text>UUV</xsl:text>
        </xsl:if>
    </xsl:variable>

    <!-- CommandScript type per vehicle -->
    <xsl:variable name="commandScriptType">
        <xsl:if test="$avclType = 'UnmannedAerialVehicle'">
            <xsl:text>UAVCommandScript</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedGroundVehicle'">
            <xsl:text>UGVCommandScript</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedSurfaceVehicle'">
            <xsl:text>USVCommandScript</xsl:text>
        </xsl:if>
        <xsl:if test="$avclType = 'UnmannedUnderwaterVehicle'">
            <xsl:text>UUVCommandScript</xsl:text>
        </xsl:if>
    </xsl:variable>

    <!-- Waypoint type per vehicle -->
    <xsl:variable name="waypointType">
        <xsl:value-of select="concat('Waypoint', $vehicleType)"/>
    </xsl:variable>

    <!-- SetPosition type per vehicle -->
    <xsl:variable name="setpositionType">
        <xsl:value-of select="concat('SetPosition', $vehicleType)"/>
    </xsl:variable>

    <!-- Telemetry type per vehicle -->
    <xsl:variable name="telemetryType">
        <xsl:value-of select="concat($vehicleType, 'Telemetry')"/>
    </xsl:variable>

    <!-- Loiter type per vehicle -->
    <xsl:variable name="loiterType">
        <xsl:value-of select="concat('Loiter', $vehicleType)"/>
    </xsl:variable>

    <xsl:param name="maxC">
        <xsl:call-template name="getMaxXPosition">
            <xsl:with-param name="values" select="/AVCL/body/MissionPreparation/*[contains(local-name(), $commandScriptType)]/*[local-name()=$setpositionType or contains(local-name(), $waypointType) or local-name='Hover']/XYPosition"/>
        </xsl:call-template>
    </xsl:param>
    <xsl:param name="minC">
        <xsl:call-template name="getMinXPosition">
            <xsl:with-param name="values" select="/AVCL/body/MissionPreparation/*[contains(local-name(), $commandScriptType)]/*[local-name()=$setpositionType or contains(local-name(), $waypointType) or local-name='Hover']/XYPosition"/>
        </xsl:call-template>
    </xsl:param>
    <xsl:param name="zoomCoordinate">
        <xsl:text>100 5 80</xsl:text>
    </xsl:param>
    <xsl:param name="zoomOrientation">
        <xsl:text>1.0 0.0 0.0 -0.3</xsl:text>
    </xsl:param>
    <xsl:param name="direction">
        <xsl:text>1 0 0 -1.57</xsl:text>
    </xsl:param> <!-- -1.0 0.0 0.0 1.59 -->

  <!-- offset for x position of text -->
    <xsl:variable name="billboardOffsetX">
        <xsl:text>0</xsl:text>
    </xsl:variable>
    <!-- offset for x position of text; TODO added 1m to overcome Xj3D bug in Text node display -->
    <xsl:variable name="billboardOffsetY">
        <xsl:text>4</xsl:text>
    </xsl:variable>

    <xsl:output method="xml" encoding="UTF-8" indent="yes" media-type="model/x3d+xml" omit-xml-declaration="no"/>
    <xsl:strip-space elements="*"/>

    <xsl:variable name="missionName">
        <xsl:choose>
            <xsl:when test="($missionFilename='AVCL-Mission')">
                <xsl:value-of select="substring-before(local-name(),$commandScriptType)"/>
                <xsl:text> Mission</xsl:text>
            </xsl:when>
            <xsl:when test="contains($missionFilename,'.xml')">
                <xsl:value-of select="substring-before(local-name(),$commandScriptType)"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="substring-before($missionFilename,'.xml')"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="substring-before(local-name(),$commandScriptType)"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="$missionFilename"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    <!-- sequence of node design -->
    <xsl:template match="/">
        <!-- diagnostic:
        <xsl:message>
            <xsl:text>AvclToX3d.xslt invoked</xsl:text>
        </xsl:message>
        -->
        <!--
        <xsl:message>
            <xsl:text>$applicationGeoOriginLatitude=</xsl:text><xsl:value-of select="$applicationGeoOriginLatitude"/><xsl:text>, </xsl:text>
            <xsl:text>$applicationGeoOriginLongitude=</xsl:text><xsl:value-of select="$applicationGeoOriginLongitude"/>
        </xsl:message>
        -->

        <!-- setup global variables -->
        <xsl:variable name="vehicleName">
            <xsl:choose>
                <xsl:when test="/AVCL/@vehicleType">
                    <xsl:message>
                        <xsl:text>AVCL/@vehicleType=</xsl:text>
                        <xsl:value-of select="/AVCL/@vehicleType"/>
                    </xsl:message>
                    <xsl:value-of select="/AVCL/@vehicleType"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:message>[AvclToX3d.xslt] AVCL/@vehicleType not found, using Aries instead</xsl:message>
                    <xsl:text>AriesExample</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="vehicleID">
            <xsl:choose>
                <xsl:when test="/AVCL/@vehicleID">
                    <xsl:value-of select="/AVCL/@vehicleID"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:message>[AvclToX3d.xslt] AVCL vehicleID not found, using 0</xsl:message>
                    <xsl:text>0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- precomputed in meters -->
        <xsl:variable name="missionOffsetDeltaNorth">
            <xsl:choose>
                <xsl:when test="//meta[@name='missionOffsetDeltaNorth']/@content">
                    <xsl:value-of select="//meta[@name='missionOffsetDeltaNorth']/@content"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="missionOffsetDeltaEast">
            <xsl:choose>
                <xsl:when test="//meta[@name='missionOffsetDeltaEast']/@content">
                    <xsl:value-of select="//meta[@name='missionOffsetDeltaEast']/@content"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <!-- diagnostic:
        <xsl:message>
            <xsl:text>$missionOffsetDeltaNorth=</xsl:text><xsl:value-of select="$missionOffsetDeltaNorth"/><xsl:text>, </xsl:text>
            <xsl:text>$missionOffsetDeltaEast=</xsl:text><xsl:value-of select="$missionOffsetDeltaEast"/><xsl:text>, </xsl:text>
        </xsl:message>
        -->

        <xsl:variable name="hostSavage">
            <xsl:text>https://savage.nps.edu/</xsl:text>
        </xsl:variable>

        <xsl:variable name="hostSavageDefense">
            <xsl:text>https://savagedefense.nps.navy.mil/</xsl:text>
        </xsl:variable>

        <xsl:variable name="localSavageRootUrl">
            <!-- really the examples root  -->
            <!-- first insert prefix -->
            <xsl:choose>
                <xsl:when test="contains($SavageRootDirectory,':')">
                    <!-- presumably windows -->
                    <xsl:text>file:/</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>file:</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
            <!-- then append cleaned-up passed parameter -->
            <xsl:value-of select="translate($SavageRootDirectory,'\','/')"/>
            <!-- TODO:  only add if string doesn't end in / already <xsl:text>/</xsl:text> -->
        </xsl:variable>

        <xsl:variable name="modelDirectoryPath">
            <!-- may include X3D models from Savage or SavageDefense -->
            <!-- $vehicleName matches the AUVWorkbenchConfiguration.xml modelName -->
            <xsl:choose>
                <!-- UAV tests -->
                <xsl:when test="($vehicleName='Rascal') or
                                ($vehicleName='ScanEagle') or
                                ($vehicleName='Predator')">
                    <xsl:text>Savage/Robots/UnmannedAirVehicles/</xsl:text>
                </xsl:when>
                <!-- USV tests -->
                <xsl:when test="($vehicleName='Rhib') or
                                ($vehicleName='SeaFox') or
                                ($vehicleName='AMN')">
                    <xsl:text>Savage/Robots/UnmannedSurfaceVehicles/</xsl:text>
                </xsl:when>
                <!-- ROV tests -->
                <xsl:when test="($vehicleName='OjoDelMarROV') or
                                ($vehicleName='BeetleROV')">
                    <xsl:text>Savage/Robots/RemotelyOperatedVehicles/</xsl:text>
                </xsl:when>
                <!-- UUV tests -->
                <xsl:when test="($vehicleName='Aries') or
                                ($vehicleName='Cetus') or
                                ($vehicleName='MARV') or
                                ($vehicleName='Phoenix') or
                                ($vehicleName='Remus') or
                                ($vehicleName='SeaDiver') or
                                ($vehicleName='SolarAuv')">
                    <xsl:text>Savage/Robots/UnmannedUnderwaterVehicles/</xsl:text>
                </xsl:when>
                <xsl:when test="($vehicleName='Ohio')">
                    <xsl:text>Savage/Submarines/SSGN-Ohio-UnitedStates/</xsl:text>
                </xsl:when>
                <xsl:when test="($vehicleName='EMATT') or
                                ($vehicleName='SeaHorse')">
                    <xsl:text>SavageDefense/Robots/UnmannedUnderwaterVehicles/</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <!-- default is AriesExample -->
                    <xsl:text>Savage/Robots/UnmannedUnderwaterVehicles/</xsl:text>
                    <!-- output diagnostic message -->
                    <xsl:variable name="warningString">
                        <xsl:text>[AvclToX3d.xslt] Unknown </xsl:text>
                        <xsl:value-of select="local-name(/AVCL/Configuration/*[1])"/>
                        <xsl:text>, $vehicleName='</xsl:text>
                        <xsl:value-of select="$vehicleName"/>
                        <xsl:text>'</xsl:text>
                    </xsl:variable>
                    <xsl:message>
                        <xsl:value-of select="$warningString"/>
                    </xsl:message>
                    <xsl:comment>
                        <xsl:value-of select="$warningString"/>
                    </xsl:comment>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- TODO: This is not completely accurate if we are using model prototypes -->
        <xsl:variable name="urlList">
            <xsl:choose>
                <!-- first .x3d then .wrl -->
                <xsl:when test="not(contains($modelDirectoryPath,'SavageDefense'))">
                    <xsl:text>"</xsl:text>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$localSavageRootUrl"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$hostSavage"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$localSavageRootUrl"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.wrl" "</xsl:text>
                    <xsl:value-of select="$hostSavage"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.wrl"</xsl:text>
                </xsl:when>
                <xsl:when test="contains($modelDirectoryPath,'SavageDefense')">
                    <xsl:text>"</xsl:text>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$localSavageRootUrl"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$hostSavageDefense"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.x3d" "</xsl:text>
                    <xsl:value-of select="$localSavageRootUrl"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.wrl" "</xsl:text>
                    <xsl:value-of select="$hostSavageDefense"/>
                    <xsl:value-of select="$modelDirectoryPath"/>
                    <xsl:value-of select="$vehicleName"/>
                    <xsl:text>.wrl"</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>

        <!-- X3D header, scene start, vehicle of interest -->
        <!-- final DOCTYPE -->
        <xsl:text disable-output-escaping="yes"><![CDATA[
<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.2//EN" "http://www.web3d.org/specifications/x3d-3.2.dtd">]]>
        </xsl:text>
        <xsl:text>&#10;</xsl:text>

        <X3D version="3.2" profile="Immersive"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema-instance" xsd:noNamespaceSchemaLocation="http://www.web3d.org/specifications/x3d-3.2.xsd">
            <head>
                <!--
                <xsl:message><xsl:text>Processing </xsl:text><xsl:value-of select="$missionFilename"/></xsl:message>
                -->
                <component level="1" name="DIS"/>
                <component level='1' name='Geospatial'/>
                <xsl:comment>Mission metadata</xsl:comment>
                <meta content="{concat(substring-before($missionFilename,'.xml'),'.x3d')}" name="title"/>
                <xsl:for-each select="//head/meta">
                    <xsl:element name="meta">
                        <xsl:attribute name="name">
                            <xsl:value-of select="@name"/>
                        </xsl:attribute>
                        <xsl:attribute name="content">
                            <xsl:value-of select="@content"/>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:for-each>
                <xsl:comment>AVCL to X3D autoconversion stylesheet metadata</xsl:comment>
                <meta content="Autogenerated X3D scene visualizing AVCL mission waypoints for use in the AUV Workbench." name="description"/>
                <xsl:if test="$createdDate">
                    <meta name="created" content="{$createdDate}"/>
                </xsl:if>
                <xsl:if test="$revisedDate">
                    <meta name="modified" content="{$revisedDate}"/>
                </xsl:if>
                <meta content="http://www.web3d.org/x3d/content/examples/license.html" name="license"/>
                <!--	<meta content="No automatic detail viewpoint processing" name="warning"/> -->
                <meta name="subject" content="AVCL, XML to X3D Conversion, viewpoint calculation"/><!-- keywords -->
                <meta name="generator" content="https://savage.nps.edu/Savage/AuvWorkbench/AVCL/AvclToX3d.xslt"/>
                <!--	<meta name="generator-authors" content="Dieter Jahn, Sidhom Mounir, Duane Davis and Don Brutzman"/> -->
            </head>
            <Scene>
                <!-- TODO different NavInfo nodes for up-close or distant-view navigation, probably using ProximitySensor to detect high altitude -->

                <!-- insert DIS-aware vehicle into this scene, positioned at the head of the track, wrapping X3D Savage model in EspduTransform -->
                <!-- TODO: further distinguish model selection using $vehicleName -->
                <!-- TODO: self-adjusting uniform scale to keep vehicle viewable even when distant, alternatively switch to symbolic icon -->
                <!-- TODO: adjustable multicast address, port -->
                <!-- diagnostic comments in the autogenerated X3D scene: -->
                <xsl:comment>
                    <xsl:text>$vehicleName=</xsl:text>
                    <xsl:value-of select="$vehicleName"/>
                </xsl:comment>
                <xsl:comment>
                    <xsl:text>$localSavageRootUrl=</xsl:text>
                    <xsl:value-of select="$localSavageRootUrl"/>
                </xsl:comment>
                <xsl:comment>
                    <xsl:text>$modelDirectoryPath=</xsl:text>
                    <xsl:value-of select="$modelDirectoryPath"/>
                </xsl:comment>
                <xsl:comment>
                    <xsl:text>$urlList=</xsl:text>
                    <xsl:value-of select="$urlList"/>
                </xsl:comment>
                <xsl:comment>
                    <xsl:text>$DIS_or_Interpolators=</xsl:text>
                    <xsl:value-of select="$DIS_or_Interpolators"/>
                </xsl:comment>

                <!-- TODO: provide switch/test for non-georeferenced locations to avoid X3DToGeoLocationCoordinateSystemRotation -->
                <GeoLocation geoCoords='{$applicationGeoOriginLatitude} {$applicationGeoOriginLongitude} 0.0' geoSystem='"GD" "WE"'>    
                    <xsl:comment> TODO GeoOrigin deprecated for X3D v3.3 </xsl:comment>
                    <GeoOrigin DEF='ORIGIN' geoCoords='{$applicationGeoOriginLatitude} {$applicationGeoOriginLongitude} 0.0'/>         
                    <xsl:comment> TODO Bad Xj3D bug? Correction rotation to bring the Background node to correct orientation under a GeoLocation node </xsl:comment>
                    <Transform rotation='0 1 0 2.815'>
                        <Background DEF='SandyShallowBottomBackground' groundAngle='0.05 1.52 1.56 1.571' groundColor='0.2 0.2 0 0.3 0.3 0 0.5 0.5 0.3 0.1 0.3 0.4 0 0.2 0.4' skyAngle='0.05 0.1 1.309 1.571' skyColor='0.8 0.8 0.2 0.1 0.1 0.6 0.1 0.1 0.6 0.1 0.25 0.8 0.6 0.6 0.9'/>
                    </Transform>
                </GeoLocation>
                <GeoLocation geoCoords='{$applicationGeoOriginLatitude} {$applicationGeoOriginLongitude} 0.0' geoSystem='"GD" "WE"'>
                    <GeoOrigin USE='ORIGIN'/>
                    <!-- Must specify a rotation about the Y-axis to get a Savage model's coordinate system back to the X3D GeoLocation coordinate system for proper DIS operation when networked -->
                    <!-- Current default Savage model nose coordinate system is X-axis (east), must rotate about the Y-axis 90 degress (1.5708 radians) to align with north -->
                    <!-- Consult para. 25.3.3 GeoLocation for the Geospatial component's adjustment of X3D to GeoLoc coordinates -->
                    <Transform rotation="0 1 0 1.5708" DEF="RotateSavageDefaultToX3DNorth">

                        <xsl:if test="($maxC != 0) and ($maxC != 0) and ($maxC != NaN) and ($maxC != NaN)">
                            <Group DEF="Viewpoints">
                                <Viewpoint DEF="{concat(translate($missionName,' ',''),'MainView')}"   description="{concat($missionName,' main view')}"   position="{($maxC + $minC) div 2} 0 {$maxC * 2}"/>
                                <Viewpoint DEF="{concat(translate($missionName,' ',''),'UpperView')}"  description="{concat($missionName,' upper view')}"  position="{($maxC + $minC) div 2} {$maxC * 2} 0" orientation="{$direction}"/>
                                <!--	<Viewpoint DEF="{concat(translate($missionName,' ',''),'DetailView')}" description="{concat($missionName,' detail view')}" position="{$zoomCoordinate}" orientation="{$zoomOrientation}"/> -->
                            </Group>
                        </xsl:if>
                        
                        <xsl:variable name="initialPosition">
                            <xsl:choose>
                                <!-- Coordinate system conversion: AVCL (@x Depth @y) to X3D (X -Y Z) -->
                                <xsl:when           test="//*[contains(local-name(),'CommandScript')]//XYPosition">
                                    <xsl:value-of select="//*[contains(local-name(),'CommandScript')]//XYPosition[1]/@x"/>
                                    <xsl:text> </xsl:text>
                                    <xsl:choose>
                                        <xsl:when           test="//*[contains(local-name(),'CommandScript')]//Depth">
                                            <!-- ensure negative X3D for depth -->
                                            <xsl:text>-</xsl:text>
                                            <xsl:value-of select="//*[contains(local-name(),'CommandScript')]//Depth[1]/@value"/>
                                        </xsl:when>
                                        <xsl:when           test="//*[contains(local-name(),'CommandScript')]//MSLAltitude">
                                            <xsl:value-of select="//*[contains(local-name(),'CommandScript')]//MSLAltitude[1]/@value"/>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <xsl:text>0</xsl:text>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                    <xsl:text> </xsl:text>
                                    <xsl:value-of         select="//*[contains(local-name(),'CommandScript')]//XYPosition[1]/@y"/>
                                </xsl:when>
                                <xsl:otherwise>
                                    <!-- TODO consider different defaults for different vehicle types -->
                                    <xsl:text>0 0 0</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
    
                        <Transform DEF="MissionOffsetDelta" translation="{$missionOffsetDeltaEast} 0 {$missionOffsetDeltaNorth}" >
                            <xsl:choose>
                                <xsl:when test="$DIS_or_Interpolators='Interpolators'">
                                    <!-- TODO:  animation without DIS -->
                                    <Transform DEF='AnimationTransform'>
                                        <Inline url="{$urlList}"/>
                                    </Transform>
                                </xsl:when>
                                <xsl:when test="$DIS_or_Interpolators='DIS'">
                                    <xsl:choose>
                                        <!-- UAV tests -->
                                        <xsl:when test="(//Configuration/UnmannedAerialVehicle) and ($vehicleName='Rascal')">
                                            <EspduTransform marking="Rascal{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="{$urlList}"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <xsl:when test="(//Configuration/UnmannedAerialVehicle) and ($vehicleName='ScanEagle')">
                                            <EspduTransform marking="ScanEagle{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="{$urlList}"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- Predator is default UAV -->
                                        <xsl:when test="(//Configuration/UnmannedAerialVehicle)">
                                            <!-- TODO:  possible AVCL problem why isn't this called UnmannedAirVehicle instead of UnmannedAerialVehicle -->
                                            <ExternProtoDeclare name="PredatorEspdu" url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedAirVehicles/PredatorEspduPrototype.x3d#PredatorEspdu&quot; &quot;{$hostSavage}/Savage/Robots/UnmannedAirVehicles/PredatorEspduPrototype.x3d#PredatorEspdu&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedAirVehicles/PredatorEspduPrototype.wrl#PredatorEspdu&quot; &quot;{$hostSavage}/Savage/Robots/UnmannedAirVehicles/PredatorEspduPrototype.wrl#PredatorEspdu&quot;">
                                                <field accessType="inputOutput" name="marking" type="SFString"/>
                                                <field accessType="inputOutput" name="translation" type="SFVec3f"/>
                                                <field accessType="inputOutput" name="rotation" type="SFRotation"/>
                                                <field accessType="inputOutput" name="siteID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="applicationID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="entityID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="readInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="writeInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="networkMode" type="SFString"/>
                                                <field accessType="inputOutput" name="address" type="SFString"/>
                                                <field accessType="inputOutput" name="port" type="SFInt32"/>
                                            </ExternProtoDeclare>
                                            <!-- TODO:  handle multiple instances of a given vehicle via vehicleID counter -->
                                            <ProtoInstance DEF="Predator{$vehicleID}" containerField="children" name="PredatorEspdu">
                                                <fieldValue name="marking" value="Predator{$vehicleID}"/>
                                                <fieldValue name="siteID" value="0"/>
                                                <fieldValue name="applicationID" value="1"/>
                                                <fieldValue name="entityID" value="{$vehicleID}"/>
                                                <fieldValue name="readInterval" value="0.5"/>
                                                <fieldValue name="networkMode" value="networkReader"/>
                                                <fieldValue name="address" value="239.255.5.8"/>
                                                <fieldValue name="port" value="62040"/>
                                                <fieldValue name="translation" value="{$initialPosition}"/>
                                            </ProtoInstance>
                                        </xsl:when>
                                        <!-- USV tests -->
                                        <xsl:when test="(//Configuration/UnmannedSurfaceVehicle) and ($vehicleName='SeaFox' or $vehicleName='AMN')">
                                            <EspduTransform marking="SeaFox{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedSurfaceVehicles/SeaFox.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedSurfaceVehicles/SeaFox.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedSurfaceVehicles/SeaFox.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedSurfaceVehicles/SeaFox.wrl&quot;"/>
                                                <!-- Navigation improvements when Viewpoint is near vehicle:
                                                     slow down navigation speed,
                                                     fix view frustum (near culling plane) as defined in avatarSize[0],
                                                     and (possibly, if browser agrees) override navigation mode (type).
                                                     TODO what about centerOfRotation?
                                                     TODO what about visibilityLimit? -->
                                                <NavigationInfo DEF="VehicleNavInfo" speed="10" type='"EXAMINE" "ANY"' avatarSize='0.5 1.6 0.755'/>
                                                <ProximitySensor DEF="VehicleCenter" size='2000 2000 2000'/>
                                                <ROUTE fromField='isActive' fromNode='VehicleCenter' toField='set_bind' toNode='VehicleNavInfo'/>
                                                <Script DEF='ProximityScript' directOutput='true'>
                                                    <field accessType='initializeOnly' name='LocalNavInfo' type='SFNode'>
                                                        <NavigationInfo USE="VehicleNavInfo"/>
                                                    </field>
                                                    <field accessType='inputOnly' name='set_active' type='SFBool'/>
                                                    <xsl:text>
ecmascript:

function set_active (value)
{
    Browser.println ('ProximityScript active=' + value);
    if (value==true)
    {
         Browser.println ('</xsl:text>
                                                    <xsl:value-of select='$vehicleName'/>
                                                    <xsl:text> VehicleNavInfo' +
             ' avatarSize=' + LocalNavInfo.avatarSize.toString() +
             ' speed='      + LocalNavInfo.speed.toString() +
             ' type='       + LocalNavInfo.type.toString());
    }
    else Browser.println ('VehicleNavInfo</xsl:text>
                                                    <xsl:value-of select='$vehicleName'/>
                                                    <xsl:text> unbound');
}
                                                    </xsl:text>
                                                    <xsl:text>&#10;</xsl:text>
                                                </Script>
                                                <ROUTE fromField='isActive' fromNode='VehicleCenter' toField='set_active' toNode='ProximityScript'/>

                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- Rigid Hull Inflatable Boat (Rhib) is default USV -->
                                        <xsl:when test="(//Configuration/UnmannedSurfaceVehicle)">
                                            <EspduTransform marking="Rhib{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/ShipsMilitary/RHIB-UnitedStates/RhibOutboard.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/ShipsMilitary/RHIB-UnitedStates/RhibOutboard.x3d&quot; &quot;{$localSavageRootUrl}Savage/ShipsMilitary/RHIB-UnitedStates/RhibOutboard.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/ShipsMilitary/RHIB-UnitedStates/RhibOutboard.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- ROV models (tolerates UnmannedUnderwaterVehicle) -->
                                        <xsl:when test="(((//Configuration/RemotelyOperatedVehicles) or (//Configuration/UnmannedUnderwaterVehicle)) and ($vehicleName='OjoDelMarROV'))">
                                            <EspduTransform marking="OjoDelMar-{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/RemotelyOperatedVehicles/OjoDelMarROV.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/RemotelyOperatedVehicles/OjoDelMarROV.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/RemotelyOperatedVehicles/OjoDelMarROV.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/RemotelyOperatedVehicles/OjoDelMarROV.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <xsl:when test="(((//Configuration/RemotelyOperatedVehicles) or (//Configuration/UnmannedUnderwaterVehicle)) and ($vehicleName='BeetleROV'))">
                                            <EspduTransform marking="BeetleROV-{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/RemotelyOperatedVehicles/BeetleROV.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/RemotelyOperatedVehicles/BeetleROV.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/RemotelyOperatedVehicles/BeetleROV.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/RemotelyOperatedVehicles/BeetleROV.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- UUV tests -->
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='Cetus')">
                                            <EspduTransform marking="Cetus{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/Cetus.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/Cetus.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/Cetus.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/Cetus.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='Phoenix')">
                                            <!-- Phoenix is predecessor of Aries with similar body shape -->
                                            <ExternProtoDeclare name="AriesEspdu" url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.x3d#AriesEspdu&quot; &quot;{$hostSavage}/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.x3d#AriesEspdu&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.wrl#AriesEspdu&quot; &quot;{$hostSavage}/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.wrl#AriesEspdu&quot;">
                                                <field accessType="inputOutput"
                                                       appinfo="up to 11 characters maps to EspduTransform marking"
                                                       name="hullName" type="SFString"/>
                                                <field accessType="inputOutput" name="hullColor" type="SFColor"/>
                                                <field accessType="inputOutput" name="planeColor" type="SFColor"/>
                                                <field accessType="inputOutput" name="translation" type="SFVec3f"/>
                                                <field accessType="inputOutput" name="rotation" type="SFRotation"/>
                                                <field accessType="inputOutput" name="siteID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="applicationID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="entityID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="readInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="writeInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="networkMode" type="SFString"/>
                                                <field accessType="inputOutput" name="address" type="SFString"/>
                                                <field accessType="inputOutput" name="port" type="SFInt32"/>
                                            </ExternProtoDeclare>
                                            <ProtoInstance DEF="Phoenix{$vehicleID}" name="AriesEspdu" containerField="children">
                                                <fieldValue name="hullName" value="Phoenix {$vehicleID}"/>
                                                <fieldValue name='hullColor' value='0 0 0.8'/>
                                                <fieldValue name='planeColor' value='.6 .4 .1'/>
                                                <fieldValue name="siteID" value="0"/>
                                                <fieldValue name="applicationID" value="1"/>
                                                <fieldValue name="entityID" value="{$vehicleID}"/>
                                                <fieldValue name="readInterval" value="0.5"/>
                                                <fieldValue name="networkMode" value="networkReader"/>
                                                <fieldValue name="address" value="239.255.5.8"/>
                                                <fieldValue name="port" value="62040"/>
                                                <fieldValue name="translation" value="{$initialPosition}"/>
                                            </ProtoInstance>
                                        </xsl:when>
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='Remus')">
                                            <EspduTransform marking="Remus{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/Remus.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/Remus.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/Remus.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/Remus.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='SeaDiver')">
                                            <EspduTransform marking="SeaDiver{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/SeaDiver.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/SeaDiver.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/SeaDiver.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/SeaDiver.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='SolarAuv')">
                                            <EspduTransform marking="SolarAuv{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <Inline url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/SolarAuv.x3d&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/SolarAuv.x3d&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/SolarAuv.wrl&quot; &quot;https://www.web3d.org/x3d/content/examples/Savage/Robots/UnmannedUnderwaterVehicles/SolarAuv.wrl&quot;"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- For Ohio SSGN -->
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle) and ($vehicleName='Ohio')">
                                            <EspduTransform marking="Ohio{$vehicleID}"
                                                            address="239.255.5.8"
                                                            port="62040"
                                                            applicationID="1"
                                                            entityID="{$vehicleID}"
                                                            networkMode="networkReader"
                                                            readInterval="0.5"
                                                            siteID="0"
                                                            translation="{$initialPosition}">
                                                <!-- TODO: incorrect??  translation="0 {$tangentPlaneNormal} 0"> -->
                                                <Inline url="{$urlList}"/>
                                                <Viewpoint DEF="VP_FAR-{$vehicleID}" description="ENTITY-Ohio{$vehicleID} Far" orientation=".124 -.946 -.301 -2.395" position="125 125 -125"/>
                                                <Viewpoint DEF="VP_FP-{$vehicleID}" description="ENTITY-Ohio{$vehicleID} First Person" orientation="0 1 0 -1.57075" position="-85.35 35.1 0"/>
                                                <Viewpoint description="OHIO SSGN from 1000 meters above" position="0 1000 0" orientation="1 0 0 -1.5708"/>
                                                <Viewpoint description="OHIO SSGN from 10000 meters above" position="0 10000 0" orientation="1 0 0 -1.5708"/>
                                                <NavigationInfo DEF="NAVINFO_CLOSE-{$vehicleID}"/>
                                                <BooleanFilter DEF="FILTER_NAVINFO_CLOSE-{$vehicleID}"/>
                                                <ROUTE fromNode="VP_FP-{$vehicleID}" fromField="isBound" toNode="FILTER_NAVINFO_CLOSE-{$vehicleID}" toField="set_boolean"/>
                                                <ROUTE fromNode="VP_FAR-{$vehicleID}" fromField="isBound" toNode="FILTER_NAVINFO_CLOSE-{$vehicleID}" toField="set_boolean"/>
                                                <ROUTE fromNode="FILTER_NAVINFO_CLOSE-{$vehicleID}" fromField="inputTrue" toNode="NAVINFO_CLOSE-{$vehicleID}" toField="set_bind"/>
                                            </EspduTransform>
                                        </xsl:when>
                                        <!-- Aries is default UUV -->
                                        <xsl:when test="(//Configuration/UnmannedUnderwaterVehicle)">
                                            <ExternProtoDeclare name="AriesEspdu" url="&quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.x3d#AriesEspdu&quot; &quot;{$hostSavage}/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.x3d#AriesEspdu&quot; &quot;{$localSavageRootUrl}Savage/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.wrl#AriesEspdu&quot; &quot;{$hostSavage}/Robots/UnmannedUnderwaterVehicles/AriesEspduPrototype.wrl#AriesEspdu&quot;">
                                                <field accessType="inputOutput"
                                                       appinfo="up to 11 characters maps to EspduTransform marking"
                                                       name="hullName" type="SFString"/>
                                                <field accessType="inputOutput" name="hullColor" type="SFColor"/>
                                                <field accessType="inputOutput" name="planeColor" type="SFColor"/>
                                                <field accessType="inputOutput" name="translation" type="SFVec3f"/>
                                                <field accessType="inputOutput" name="rotation" type="SFRotation"/>
                                                <field accessType="inputOutput" name="siteID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="applicationID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="entityID" type="SFInt32"/>
                                                <field accessType="inputOutput" name="readInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="writeInterval" type="SFTime"/>
                                                <field accessType="inputOutput" name="networkMode" type="SFString"/>
                                                <field accessType="inputOutput" name="address" type="SFString"/>
                                                <field accessType="inputOutput" name="port" type="SFInt32"/>
                                            </ExternProtoDeclare>
                                            <ProtoInstance DEF="Aries{$vehicleID}" name="AriesEspdu" containerField="children">
                                                <fieldValue name="hullName" value="Aries {$vehicleID}"/>
                                                <fieldValue name="siteID" value="0"/>
                                                <fieldValue name="applicationID" value="1"/>
                                                <fieldValue name="entityID" value="{$vehicleID}"/>
                                                <fieldValue name="readInterval" value="0.5"/>
                                                <fieldValue name="networkMode" value="networkReader"/>
                                                <fieldValue name="address" value="239.255.5.8"/>
                                                <fieldValue name="port" value="62040"/>
                                                <fieldValue name="translation" value="{$initialPosition}"/>
                                            </ProtoInstance>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <!-- output diagnostic message -->
                                            <xsl:variable name="warningString">
                                                <xsl:text>[AvclToX3d.xslt] Unknown Configuration/VehicleType </xsl:text>
                                                <xsl:value-of select="local-name(//Configuration/*[1])"/>
                                                <xsl:text>, $vehicleName='</xsl:text>
                                                <xsl:value-of select="$vehicleName"/>
                                                <xsl:text>'</xsl:text>
                                            </xsl:variable>
                                            <xsl:message>
                                                <xsl:value-of select="$warningString"/>
                                            </xsl:message>
                                            <xsl:comment>
                                                <xsl:value-of select="$warningString"/>
                                            </xsl:comment>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:when>
                            </xsl:choose>
                            <!-- <xsl:message><xsl:text>missionFilename=</xsl:text><xsl:value-of select="$missionFilename"/></xsl:message>
                            <xsl:message><xsl:text>missionName=</xsl:text><xsl:value-of select="$missionName"/></xsl:message>
                            <xsl:message><xsl:text>missionName (no space) =</xsl:text><xsl:value-of select="translate($missionName,' ','')"/></xsl:message>
                            -->
                            <xsl:apply-templates select="/AVCL/body/MissionPreparation/*[contains(local-name(),$commandScriptType)]"/>
                            <xsl:apply-templates select="/AVCL/body/MissionResults"/>
                        </Transform>
                    </Transform>
                </GeoLocation>
            </Scene>
        </X3D>
        <!-- output-complete diagnostic message on console -->
        <xsl:variable name="completionMessage">
            <xsl:text>[AvclToX3d.xslt] Created </xsl:text>
            <xsl:value-of select="$vehicleName"/>
            <xsl:text> vehicleID='</xsl:text>
            <xsl:value-of select="$vehicleID"/>
            <xsl:text>'</xsl:text>
            <xsl:text> from </xsl:text>
            <xsl:value-of select="$missionFilename"/>
        </xsl:variable>
        <xsl:message>
            <xsl:value-of select="$completionMessage"/>
        </xsl:message>
    </xsl:template>

    <!-- The CommandScript element contains the individual mission commands -->
    <xsl:template match="*[contains(local-name(),$commandScriptType)]">

        <xsl:comment>Commanded waypoints from AVCL mission script</xsl:comment>
        <Group DEF='CommandedWaypoints'>
            <Shape>
                <Appearance>
                    <Material emissiveColor="{$waypointsColor}"/>
                </Appearance>
                <xsl:element name="IndexedLineSet">
                    <xsl:attribute name="coordIndex">
                        <!-- <xsl:text>0 </xsl:text> -->
                        <xsl:for-each select="*[local-name()='Hover' or local-name()=$setpositionType or contains(local-name(), $waypointType)]">
                            <!-- <xsl:number count="*[local-name()=$waypointType]" format="1"/> -->
                            <xsl:value-of select="position() - 1"/>
                            <xsl:text> </xsl:text>
                            <!-- repeat index if only one is present to avoid Xj3D bug -->
                            <xsl:if test="count(../*[local-name()='Hover' or local-name()=$setpositionType or contains(local-name(), $waypointType)])=1">
                                <xsl:text>0</xsl:text>
                            </xsl:if>
                        </xsl:for-each>
                    </xsl:attribute>
                    <!-- TODO:  color map
                    <xsl:attribute name="colorPerVertex">
                        <xsl:text>true</xsl:text>
                    </xsl:attribute>
                    <xsl:attribute name="colorIndex">
                        <xsl:for-each select="*[local-name()='Hover' or local-name()=$setpositionType or contains(local-name(),$waypointType)]">
                            <xsl:text>0 </xsl:text>
                        </xsl:for-each>
                    </xsl:attribute>
                    <xsl:element name="Color">
                        <xsl:attribute name="color">
                            <xsl:value-of select="$waypointsColor"/>
                        </xsl:attribute>
                    </xsl:element>
                    -->
                    <xsl:element name="Coordinate">
                        <xsl:attribute name="point">
                            <!-- Call the template for the first command with default parameters -->
                            <xsl:apply-templates select="*[1]">
                                <xsl:with-param name="priorDepth" select="0"/>
                            </xsl:apply-templates>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </Shape>
            <xsl:comment>These defaults are are saved for DEF/USE purposes, and so are nonrendering</xsl:comment>
            <Shape DEF="DefaultStyleDEFs">
                <Appearance DEF="BillboardAppearance">
                    <Material diffuseColor="{$waypointsColor}"/>
                </Appearance>
                <!-- blank string to avoid X3D Schematron QA warnings -->
                <Text string='" "'>
                    <FontStyle DEF="MiddleJustifyFontStyle" justify='"MIDDLE" "MIDDLE"'/>
                </Text>
            </Shape>
            <Switch whichChoice="-1">
                <Shape DEF="MilestoneMarkerShape">
                    <Box size="0.2 1 0.2"/>
                    <Appearance USE="BillboardAppearance"/>
                </Shape>
                <Shape DEF="WaypointStandoffCylinder">
                    <Cylinder bottom='false' height='10' radius='1' solid='false' top='false'/>
                    <Appearance DEF="BillboardAppearanceSemiTransparent">
                        <Material diffuseColor="{$waypointsColor}" transparency="0.95"/>
                    </Appearance>
                </Shape>
            </Switch>
        </Group>
        <Collision enabled="false" DEF="AllWaypointBillboards">
            <!--  TODO:  ViewpointGroup to make billboard viewpoint entries less onerous
            -->
            <xsl:call-template name="BuildBillboards">
                <xsl:with-param name="allCommands" select="*"/>
                <xsl:with-param name="defaultDepth" select="0"/>
                <xsl:with-param name="missionLabel">
                    <xsl:value-of select="$missionName"/>
                </xsl:with-param>
            </xsl:call-template>
               <!--	<xsl:with-param name="allHoverPositionWaypoints" select="*[local-name()='Hover' or local-name()=$setpositionType or contains(local-name(),$waypointType)]"/> -->
        </Collision>
    </xsl:template>

    <!-- The SampledResults element contains the individual telemetry data points -->
    <xsl:template match="MissionResults">

        <xsl:comment>Mission telemetry results</xsl:comment>

        <xsl:variable name="telemetryPointList">
            <xsl:for-each select="SampledResults/*[contains(local-name(), $telemetryType)]">
                <!-- note coordinate conversion -->
                <xsl:value-of select="GeographicPosition/XYPosition/@x"/>
                <xsl:text> </xsl:text>
                <xsl:text>0</xsl:text> <!-- TODO bad AVCL bug:  telemetry missing z values; be sure to negate -->
                <xsl:text> </xsl:text>
                <xsl:value-of select="GeographicPosition/XYPosition/@y"/>
                <xsl:text> </xsl:text> <!-- space separated -->
            </xsl:for-each>
        </xsl:variable>

        <xsl:variable name="telemetryPointFraction">
            <xsl:value-of select="1 div count(SampledResults/*[contains(local-name(), $telemetryType)]/GeographicPosition/XYPosition)"/>
        </xsl:variable>
        <!-- diagnostic:
        <xsl:comment>
            <xsl:text>count(of GeographicPosition/XYPosition nodes)=</xsl:text><xsl:value-of select="count(SampledResults/*[contains(local-name(), $telemetryType)]/GeographicPosition/XYPosition)"/>
            <xsl:text>, </xsl:text>
            <xsl:text>$telemetryPointFraction=</xsl:text><xsl:value-of select="$telemetryPointFraction"/>
        </xsl:comment>
        -->

        <xsl:variable name="keyList">
            <xsl:for-each select="SampledResults/*[contains(local-name(), $telemetryType)]">
                <xsl:value-of select="(position() - 1) * $telemetryPointFraction"/>
                <xsl:text> </xsl:text>
            </xsl:for-each>
            <xsl:text> 1.0</xsl:text>
        </xsl:variable>
        <!-- diagnostic:
        <xsl:comment>
            <xsl:text>$keyList=</xsl:text><xsl:value-of select="$keyList"/>
        </xsl:comment>
        -->

        <xsl:variable name="telemetryRotations">
            <xsl:for-each select="SampledResults/*[contains(local-name(), $telemetryType)]">
                <!-- TODO:  compute proper SFRotations -->
                <xsl:text>0 -1 0 </xsl:text>
                <xsl:value-of select="((Orientation/@psi) mod 360.0) * 3.14159 div 180"/>
                <xsl:text> </xsl:text>
            </xsl:for-each>
        </xsl:variable>
        <!-- diagnostic:
        -->
        <xsl:comment>
            <xsl:text>$telemetryRotations=</xsl:text>
            <xsl:value-of select="$telemetryRotations"/>
        </xsl:comment>

        <xsl:choose>
            <xsl:when test="$DIS_or_Interpolators='Interpolators'">
                   <!-- animation minus DIS -->
                <Group DEF='AnimationGroup'>
                    <PositionInterpolator    DEF='AnimationPosition' key='{$keyList}' keyValue='{$telemetryPointList}'/>
                    <OrientationInterpolator DEF='AnimationRotation' key='{$keyList}' keyValue='{$telemetryRotations}'/>
                    <ROUTE fromField='value_changed' fromNode='AnimationPosition' toField='translation' toNode='AnimationTransform'/>
                    <ROUTE fromField='value_changed' fromNode='AnimationRotation' toField='rotation' toNode='AnimationTransform'/>
                    <TimeSensor DEF='AnimationClock' cycleInterval='30' loop='true'/>
                    <ROUTE fromField='fraction_changed' fromNode='AnimationClock' toField='set_fraction' toNode='AnimationPosition'/>
                    <ROUTE fromField='fraction_changed' fromNode='AnimationClock' toField='set_fraction' toNode='AnimationRotation'/>
                </Group>
            </xsl:when>
        </xsl:choose>

        <Switch DEF='MissionResultsTrack' whichChoice='0'>
            <Shape>
                <Appearance>
                    <Material DEF="telemetryColor" emissiveColor="{$telemetryColor}"/>
                </Appearance>
                <xsl:element name="LineSet">
                    <xsl:attribute name="vertexCount">
                        <xsl:value-of select="count(SampledResults/*[contains(local-name(), $telemetryType)])"/>
                    </xsl:attribute>
                    <!-- TODO:  per-vertex color map
                    <xsl:element name="Color">
                        <xsl:attribute name="color">
                            <xsl:value-of select="$telemetryColor"/>
                        </xsl:attribute>
                    </xsl:element>
                    -->
                    <xsl:element name="Coordinate">
                        <xsl:attribute name="DEF">
                            <xsl:text>TelemetryCoordinatePoints</xsl:text>
                        </xsl:attribute>
                        <xsl:attribute name="point">
                            <xsl:for-each select="SampledResults/*[contains(local-name(), $telemetryType)]">
                                <!-- note coordinate conversion -->
                                <xsl:value-of select="GeographicPosition/XYPosition/@x"/>
                                <xsl:text> </xsl:text>
                                <xsl:text>0</xsl:text> <!-- TODO bad AVCL bug:  telemetry missing z values; be sure to negate -->
                                <xsl:text> </xsl:text>
                                <xsl:value-of select="GeographicPosition/XYPosition/@y"/>
                                <xsl:text> </xsl:text> <!-- space separated -->
                            </xsl:for-each>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </Shape>
            <Shape>
                <Appearance>
                    <Material USE="telemetryColor"/>
                </Appearance>
                <xsl:element name="PointSet">
                    <!-- TODO:  per-vertex color map
                    <xsl:element name="Color">
                        <xsl:attribute name="color">
                            <xsl:value-of select="$telemetryColor"/>
                        </xsl:attribute>
                    </xsl:element>
                    -->
                    <xsl:element name="Coordinate">
                        <xsl:attribute name="USE">
                            <xsl:text>TelemetryCoordinatePoints</xsl:text>
                        </xsl:attribute>
                    </xsl:element>
                </xsl:element>
            </Shape>
        </Switch>

    </xsl:template>

    <!-- Adding Billboard nodes -->
    <xsl:template name="BuildBillboards">
        <!--	<xsl:param name="allHoverPositionWaypoints"/> -->
        <xsl:param name="allCommands"/>
        <xsl:param name="defaultDepth" select="0"/>
        <xsl:param name="position" select="0"/>
        <xsl:param name="missionLabel"/>
        <!-- the default value 0 depends on the schema  -->
        <xsl:variable name="first"     select="$allCommands [1]"/>
        <xsl:variable name="remainder" select="$allCommands [not(position()=1)]"/>
        <xsl:variable name="newDepth">
            <xsl:choose>
                <xsl:when test="$first/Depth/@value">
                    <xsl:value-of select="$first/Depth/@value"/>
                </xsl:when>
                <!-- note negation of AVCL altitude values to match X3D coordinates -->
                <xsl:when test="$first/AGLAltitude/@value">
                    <xsl:value-of select="-$first/AGLAltitude/@value"/>
                </xsl:when>
                <xsl:when test="$first/MSLAltitude/@value">
                    <xsl:value-of select="-$first/MSLAltitude/@value"/>
                </xsl:when>
                <xsl:when test="string-length($defaultDepth) > 0">
                    <xsl:value-of select="$defaultDepth"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0.0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:if test="local-name($first)='Hover' or local-name($first)=$loiterType or local-name($first)=$setpositionType or contains(local-name($first), $waypointType)">
            <xsl:element name="Transform">
                <xsl:attribute name="DEF">
                    <xsl:value-of select="concat(local-name($first),$position,'Transform')"/>
                </xsl:attribute>
                <xsl:variable name="xPosition">
                    <xsl:value-of select="$billboardOffsetX + $first/XYPosition/@x"/>
                </xsl:variable>
                <xsl:variable name="yPosition">
                    <xsl:value-of select="-$newDepth"/>
                </xsl:variable>
                <xsl:variable name="zPosition">
                    <xsl:value-of select="$first/XYPosition/@y"/>
                </xsl:variable>
                <xsl:attribute name="translation">
                    <xsl:value-of select="$billboardOffsetX + $first/XYPosition/@x"/>
                    <xsl:text> </xsl:text>
                    <!-- sign modified -->
                    <xsl:value-of select="-$newDepth"/>
                    <xsl:text> </xsl:text>
                    <xsl:value-of select="$first/XYPosition/@y"/>
                </xsl:attribute>

                <xsl:variable name="description">
                    <xsl:value-of select="$first/@description"/>
                </xsl:variable>

                <xsl:variable name="standoffValue">
                    <xsl:choose>
                        <xsl:when test="$first/Standoff/@value">
                            <xsl:value-of select="$first/Standoff/@value"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>0</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                
                <xsl:variable name="cylinderDepthOffset">
                    <xsl:choose>
                        <xsl:when test="string-length($newDepth) > 0">
                            <xsl:value-of select="$newDepth - 5"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>0</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>

                <Shape USE="MilestoneMarkerShape"/>
                
                <!-- Diagnostic: 
                -->
                <xsl:comment>
                        <xsl:text>Current command=</xsl:text>
                        <xsl:value-of select="local-name($first)"/>
                        <xsl:text>, $standoffValue=</xsl:text>
                        <xsl:value-of select="$standoffValue"/>
                        <xsl:text> (</xsl:text>
                        <xsl:value-of select="$first/Standoff/@value"/>
                        <xsl:text>), $newDepth=</xsl:text>
                        <xsl:value-of select="$newDepth"/>
                </xsl:comment>
                <xsl:if test="($standoffValue > 0)">
                    <!-- translate downward -->
                    <Transform scale="{$standoffValue} 1 {$standoffValue}" translation="0 {$cylinderDepthOffset} 0">
                        <Shape USE="WaypointStandoffCylinder"/>
                    </Transform>
                </xsl:if>

                <!-- if multiple waypoint or hover commands have same location, then upshift the translation to void overlap/superpositioning -->
                <xsl:variable name="precedingCoincidentLocationCount">
                    <xsl:value-of select="count(preceding-sibling::*[local-name()='Hover' or local-name()=$loiterType or local-name()=$setpositionType or contains(local-name(), $waypointType)][/XYPosition/@x = $first/XYPosition/@x][/XYPosition/@y = $first/XYPosition/@y])"/>
                </xsl:variable>
                <xsl:comment>
                        <xsl:text>$precedingCoincidentLocationCount=</xsl:text>
                        <xsl:value-of select="$precedingCoincidentLocationCount"/>
                </xsl:comment>
                <Transform translation="{concat('0 ',$billboardOffsetY*($precedingCoincidentLocationCount+1),' 0')}">
                    <!-- diagnostic:
                    <xsl:message>
                        <xsl:text>$includeWaypointViewpoints=</xsl:text>
                        <xsl:value-of select="$includeWaypointViewpoints"/>
                    </xsl:message>
                    -->
                    <xsl:if test="$includeWaypointViewpoints">
                        <Viewpoint position="0 -2 10" DEF="{concat(local-name($first),$position,'Viewpoint')}" description="{concat($missionLabel,' [',$position,'] ',local-name($first),' (',$xPosition,' ',$yPosition,' ',$zPosition,') ',$description)}"/>
                    </xsl:if>
                    <Billboard>
                        <Shape>
                            <xsl:element name="Text">
                                <xsl:attribute name="string">
                                    <xsl:if test="$missionLabel">
                                        <xsl:text>"</xsl:text>
                                        <xsl:value-of select="$missionLabel"/>
                                        <xsl:text>" </xsl:text>
                                    </xsl:if>
                                    <xsl:text>"</xsl:text>
                                    <xsl:text>[</xsl:text>
                                    <xsl:value-of select="$position"/>
                                    <xsl:text>] </xsl:text>
                                    <xsl:value-of select="local-name($first)"/>
                                    <xsl:text>" </xsl:text>
                                    <!-- output for descriptions, x, y and depth, and ids required  -->
                                    <xsl:if test="$first/@id">
                                        <xsl:text>"</xsl:text>
                                        <xsl:value-of select="$first/@id"/>
                                        <xsl:text>" </xsl:text>
                                    </xsl:if>
                                    <xsl:if test="$first/@description">
                                        <xsl:text>"</xsl:text>
                                        <xsl:value-of select="$first/@description"/>
                                        <xsl:text>" </xsl:text>
                                    </xsl:if>
                                    <xsl:if test="not(@description) or not($first/@id)">
                                        <!-- no output required -->
                                    </xsl:if>
                                    <xsl:text>"</xsl:text>
                                    <xsl:value-of select="$billboardOffsetX + $first/XYPosition/@x"/>
                                    <xsl:text> </xsl:text>
                                    <!-- sign modified -->
                                    <xsl:value-of select="$billboardOffsetY - $newDepth"/>
                                    <xsl:text> </xsl:text>
                                    <xsl:value-of select="$first/XYPosition/@y"/>
                                    <xsl:text>"</xsl:text>
                                </xsl:attribute>
                                <FontStyle USE="MiddleJustifyFontStyle"/>
                            </xsl:element>
                            <Appearance USE="BillboardAppearance"/>
                        </Shape>
                    </Billboard>
                </Transform>
            </xsl:element>
        </xsl:if>
        <!-- tail recursion -->
        <xsl:if test="$remainder">
            <xsl:call-template name="BuildBillboards">
                <xsl:with-param name="allCommands" select="$remainder"/>
                <xsl:with-param name="defaultDepth" select="$newDepth"/>
                <xsl:with-param name="position" select="$position+1"/>
                <xsl:with-param name="missionLabel">
                    <xsl:value-of select="$missionLabel"/>
                </xsl:with-param>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <!-- $waypointType elements via pattern matching.  Might have also used call-template by name. -->
    <!-- Feed-forward tail recursion pattern by Duane Davis. -->
    <xsl:template match="*[local-name()='Hover' or local-name()=$loiterType or local-name()=$setpositionType or contains(local-name(), $waypointType)]">
        <!--  here is the feed-forward parameter:  priorDepth, which may or may not be present -->
        <xsl:param name="priorDepth"/>
        <xsl:variable name="depthMode">
            <xsl:choose>
                <xsl:when test="Depth">depth</xsl:when>
                <xsl:when test="AGLAltitude or MSLAltitude">altitude</xsl:when>
            </xsl:choose>
        </xsl:variable>
        <!-- extract data of interest, if available.  otherwise use prior value. -->
        <xsl:variable name="currentDepth">
            <xsl:choose>
                <!-- use new value from current element, if present -->
                <xsl:when test="Depth">
                    <xsl:value-of select="Depth/@value"/>
                </xsl:when>
                <!-- note negation of AVCL altitude values to match X3D coordinates -->
                <!-- AGL = Above Ground Level -->
                <xsl:when test="AGLAltitude">
                    <xsl:value-of select="-AGLAltitude/@value"/>
                </xsl:when>
                <!-- MSL = Mean Sea Level -->
                <xsl:when test="MSLAltitude">
                    <xsl:value-of select="-MSLAltitude/@value"/>
                </xsl:when>
                <!-- otherwise use original value which was passed forward from prior recursion loop -->
                <xsl:otherwise>
                    <xsl:value-of select="$priorDepth"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <!-- all inputs now known OK, so next we process results -->
        <!-- First 2 elements of track.out line are waypoint x and y position (North and East), converted from feet to meters as required -->
        <xsl:value-of select="XYPosition/@x"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="-$currentDepth"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="XYPosition/@y"/>
        <xsl:text>, </xsl:text>

        <!-- results complete.  template repeats by recursing on self with all but first element.
                     pass forward priorDepth in case we need it again. -->
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="priorDepth" select="$currentDepth"/>
        </xsl:apply-templates>
    </xsl:template>

    <!-- MakeDepth command updates depth mode and depth parameters to forward through to the templates -->
    <xsl:template match="MakeDepth">
        <xsl:param name="priorDepth"/>
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="depthModeParam" select="'depth'"/>
            <xsl:with-param name="priorDepth" select="@value"/>
        </xsl:apply-templates>
    </xsl:template>

    <!-- MakeAltitude command updates depth mode and depth parameters to forward through to the templates -->
    <xsl:template match="MakeAltitude">
        <xsl:param name="priorDepth"/>
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="depthModeParam" select="'altitude'"/>
            <xsl:with-param name="priorDepth" select="@value"/>
        </xsl:apply-templates>
    </xsl:template>

    <!-- Other commands aren't supported by Aries, print warning and forward all the parameters along -->
    <xsl:template match="*">
        <xsl:param name="priorDepth"/>
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="priorDepth" select="$priorDepth"/>
        </xsl:apply-templates>
    </xsl:template>
    <xsl:template match="SetPosition">
        <xsl:param name="priorDepth"/>
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="priorDepth" select="$priorDepth"/>
        </xsl:apply-templates>
    </xsl:template>
    <xsl:template match="Hover">
        <xsl:param name="priorDepth"/>
        <xsl:apply-templates select="following-sibling::*[1]">
            <xsl:with-param name="priorDepth" select="$priorDepth"/>
        </xsl:apply-templates>
    </xsl:template>

    <!-- selecting the max xCoordinate -->
    <xsl:template name="getMaxXPosition">
        <xsl:param name="values"/>
        <xsl:variable name="max">
            <xsl:choose>
                <xsl:when test="$values/@x">
                    <xsl:for-each select="$values/@x">
                        <xsl:sort data-type="number" order="descending"/>
                        <xsl:if test="position() = 1">
                            <xsl:value-of select="."/>
                        </xsl:if>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$max"/>
    </xsl:template>

    <!-- selecting the min xCoordinate -->
    <xsl:template name="getMinXPosition">
        <xsl:param name="values"/>
        <xsl:variable name="min">
            <xsl:choose>
                <xsl:when test="$values/@x">
                    <xsl:for-each select="$values/@x">
                        <xsl:sort data-type="number" order="ascending"/>
                        <xsl:if test="position() = 1">
                            <xsl:value-of select="."/>
                        </xsl:if>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>0</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$min"/>
    </xsl:template>

</xsl:stylesheet>
