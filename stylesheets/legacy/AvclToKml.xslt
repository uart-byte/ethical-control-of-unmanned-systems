<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/xpath-functions" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.opengis.net/kml/2.2">

<!-- Title: Autonomous Vehicle Control Language (AVCL) to Keyhole Markup Language (KML) Transformation -->
<!-- Author: Curtis Blais and Don Brutzman, Naval Postgraduate School MOVES Institute -->
<!-- Creation Date: 29 July 2009 -->
<!-- File Name: AvclToKml.xslt -->
<!-- Description: This Extensible Stylesheet Language Transformation (XSLT) file reads position information from the mission preparation (plan), event log, and mission results of a source AVCL file to generate a KML file for visualization in applications such as Google Earth. AVCL files are required to contain mission preparation data, consisting of command scripts or a mission agenda (launch/recovery data) and may contain event logs, telemetry data, or both. The logic creates red "push-pin" icons for locations of interest, including planned waypoints, and yellow "push-pin" icons for telemetry points. -->
<!-- Change History -->
<!-- 
Blais: 29 July 2009 Loaded AVCL transformation directory to Tactical Data Conversion Suite.
Blais: 12 August 2009 Simplified transformation logic.
Brutzman: 14 August 2009 Renamed to follow CamelCaseNamingConventions; .xslt extension.
Brutzman: 14 August 2009 Fixed error from duplicated variable name.
Brutzman: 21 August 2009 Added TODO comments.
McGredo: 31 August 2009 Renamed directory.
McGredo: 5 October 2009 Moved name.
Blais: 11 February 2010 Modified with changes discussed in MV4250 class: (1) styleUrl capitalization; (2) date conversion logic; (3) order of child elements in LookAt.
Blais: 17 March 2010 Replaced with Sotiris version of the transformation file. Changed "gt" to ">"; changed <xsl:value-of> to <xsl:text> when evaluating a literal value.
Brutzman: 19 July 2010 Matched director structure in TrackDataConversionSuiteDefense.
Brutzman: 24 July 2012 Copied stylesheet from TrackDataConversionSuite subversion to AUV Workbench; will need to reconcile later.
Brutzman: 31 July 2012 Better handling of coordinates, telemetry for all vehicles.
Brutzman: 24 August 2012 Improved altitude.
Brutzman: 8 October 2012 Added line breaks.
Brutzman: 17 October 2012 Corrected computation of time in seconds and added comments.
Blais: 3 January 2013 Incorporated new logic from changes/testing in December 2012: (1) added comments in header describing file history; (2) inserted declaration of xsi namespace and added the schemaLocation attribute to <kml> root element in result file for XML validation. 
Blais: 3 January 2013 Incorporated new logic from changes/testing in December 2012: (3) added logic to generate comments from meta tags in the source file; (4) added a stub for generating KML for the AVCL EventLog elements; (5) inserted separator comments between major portions of the logic (MissionPreparation, EventLog, SampledResults). 
Blais: 3 January 2013 Incorporated new logic from changes/testing in December 2012: (6) created a template for generating the KML LookAt element for Placemarks and replaced explicit generation with calls to the template; (7) modified LookAt child element values for better positioning of the viewer when the KML file is opened in Google-Earth; (8) added a separator comment for templates providing utility logic (e.g., time conversion, LookAt logic).
Blais: 11 January 2013 Incorporated new logic from changes/testing in December 2012: (9) added logic for converting from XY position (relative to an origin given in lat-long) to lat-long to scale the longitude calculation based on the latitude value and updated all lat-long calculations accordingly.
Blais: 11 January 2013 Incorporated new logic from changes/testing in December 2012: (10) added explicit template stubs and partial logic for AVCL content that can provide geographic points and shapes for the KML file, replacing previous Waypoint and Hover logic; (11) inserted comments to describe transformation logic.
Blais: 11 January 2013 In SampledResults template, replaced generic telemetry point logic with logic particular to the kind of vehicle (UAV, UGV, USV, UUV) in order to store depth or altitude values in the KML point data; add postTelemetryPoint template.
Blais: 13 January 2013 In AgendaMission, add template stubs and partial logic for producing KML content from GoalList and ConstraintList elements, including geometrical shapes for operating areas (goals) and avoid areas (constraints).
Blais: 13 January 2013 In AgendaMission, added logic for creating KML Point from AVCL Point for GoalList/Goal/Operating Area and ConstraintList/AvoidArea geometries. Provided initial logic for generating a KML polygon from the AVCL rectangle definition (need to include rotation of the polygon).
Blais: 21 February 2013 Added stubs for processing AVCL content that has not been implemented yet.
-->

    <!-- Set variables storing the GeoOrigin location -->
    <!-- TODO include description of GeoOrigin location in AVCL, KML -->
    <xsl:variable name="originLongitude" select="AVCL/body/MissionPreparation/GeoOrigin/@longitude"/>
    <xsl:variable name="originLatitude"  select="AVCL/body/MissionPreparation/GeoOrigin/@latitude"/>

	<!-- Define a conversion factor for meters to degrees (at the equator) -->
    <xsl:variable name="latitudeMetersToDegrees" select="0.0000089831207"/>
    <!-- Assuming a spherical earth, latitudeToDegrees is accurate for latitude values anywhere -->
    <!-- Linear scaling of the longitude computation based on latitude is given by latitudeMetersToDegrees * (1 - abs(latitude)/90), where abs is the absolute value function -->
	<!-- TODO also need to test with scenarios with the geoOrigin on one side of the equator and mission points above and below the equator -->
    <xsl:variable name="longitudeMetersToDegrees" select="$latitudeMetersToDegrees * (1.0 - (fn:abs($originLatitude) div 90.0))"/>
    
    <xsl:output method="xml" indent="yes" exclude-result-prefixes="xsl fo xs fn"/>
    
    <xsl:template match="/">
        <xsl:text>&#10;</xsl:text>
        <kml xsi:schemaLocation="http://www.opengis.net/kml/2.2 http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd">
 		
			<!-- Read meta tags from the source AVCL file and output the content as comments in the KML file -->
			<xsl:apply-templates select="AVCL/head/meta"/>
			
           <Document>
                <Style id="redPushpin">
                    <IconStyle>
                        <Icon>
                            <href>http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png</href>
                        </Icon>
                    </IconStyle>
                </Style>
				
				<!-- Generate Placemarks for the GeoOrigin and other mission data -->
                <xsl:apply-templates select="AVCL/body/MissionPreparation"/>
				<xsl:apply-templates select="AVCL/body/EventLog"/>
                <xsl:apply-templates select="AVCL/body/MissionResults/SampledResults"/>
            </Document>
        </kml>
    </xsl:template>
	
	<xsl:template match="meta">
		<!-- Capture the content of meta tags from the source AVCL document as XML comments in the KML result document -->
		<xsl:comment>
			<xsl:text>meta name="</xsl:text>
			<xsl:value-of select="./@name"/>
			<xsl:text>" content="</xsl:text>
			<xsl:value-of select="./@content"/>
			<xsl:text>"</xsl:text>
		</xsl:comment>
	</xsl:template>
	
	<!-- ****** TEMPLATES FOR MISSION PREPARATION ****** -->
	
    <xsl:template match="MissionPreparation">
		<!-- Create a PlaceMark for the GeoOrigin of the mission -->
		<!-- Note that although every AVCL file is required to have a MissionPreparation element, the element is only required to contain UnitsOfMeasure content; GeoOrigin, Configuration, and MissionSpecification are not mandatory child elements -->
        <Placemark>
            <name>
                <xsl:text>GeoOrigin</xsl:text>
            </name>
            <xsl:call-template name="generateLookAt">
				<xsl:with-param name="lookAtLongitude" select="$originLongitude"/>
				<xsl:with-param name="lookAtLatitude" select="$originLatitude"/>
				<xsl:with-param name="lookAtAltitude" select="100"/>
				<xsl:with-param name="lookAtHeading" select="0"/>
				<xsl:with-param name="lookAtTilt" select="45"/>
				<xsl:with-param name="lookAtRange" select="5000"/>
				<xsl:with-param name="lookAtAltitudeMode" select="'absolute'"/>
            </xsl:call-template>
            <styleUrl>#redPushpin</styleUrl>
            <Point>
                <coordinates>
                    <xsl:value-of select="$originLongitude"/>
                    <xsl:text>,</xsl:text>
                    <xsl:value-of select="$originLatitude"/>
                </coordinates>
            </Point>
        </Placemark>
		
		<!-- Generate Placemarks for points of interest from the mission planning -->
		<!-- Note: The MissionPreparation element may have at most one of these child elements -->
		<xsl:choose>
			<xsl:when test="UAVCommandScript">
				<xsl:apply-templates select="UAVCommandScript"/>
			</xsl:when>
			<xsl:when test="UGVCommandScript">
				<xsl:apply-templates select="UGVCommandScript"/>
			</xsl:when>
			<xsl:when test="USVCommandScript">
				<xsl:apply-templates select="USVCommandScript"/>
			</xsl:when>
			<xsl:when test="UUVCommandScript">
				<xsl:apply-templates select="UUVCommandScript"/>
			</xsl:when>
			<xsl:when test="AgendaMission">
				<xsl:apply-templates select="AgendaMission"/>
			</xsl:when>
			<xsl:when test="GuidedMunitionMission">
				<xsl:apply-templates select="GuidedMunitionMission"/>
			</xsl:when>
			<xsl:otherwise>
				<!-- no other Mission Preparation data are available in the source AVCL document -->
			</xsl:otherwise>
		</xsl:choose>
		
    </xsl:template>
	
	<xsl:template match="UAVCommandScript | UAVEvent">
		<xsl:apply-templates select="CompositeWaypointUAV"/>
		<xsl:apply-templates select="LoiterUAV"/>
		<xsl:apply-templates select="SetPositionUAV"/>
		<xsl:apply-templates select="WaypointUAV"/>
		<!-- TODO add logic for CompositeWaypointUAV and other commands with position data -->
	</xsl:template>

	<xsl:template match="CompositeWaypointUAV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="LoiterUAV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="SetPositionUAV">
		<!-- SetPositionUAV can contain an AGLAltitude or an MSLAltitude (not both) -->
		<!-- TODO logic is same as for WaypointUAV; can be consolidated -->
		<xsl:choose>
			<xsl:when test="AGLAltitude">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="AGLAltitude/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="MSLAltitude">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="MSLAltitude/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<!-- no altitude data provided in the AVCL source file for this waypoint -->
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="0"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="WaypointUAV">
		<!-- a WaypointUAV can contain an AGLAltitude or an MSLAltitude (not both) -->
		<xsl:choose>
			<xsl:when test="AGLAltitude">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="AGLAltitude/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="MSLAltitude">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="MSLAltitude/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<!-- no altitude data provided in the AVCL source file for this waypoint -->
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="0"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="UGVCommandScript | UGVEvent">
		<xsl:apply-templates select="CompositeWaypointUGV"/>
		<xsl:apply-templates select="LoiterUGV"/>
		<xsl:apply-templates select="SetPositionUGV"/>
		<xsl:apply-templates select="WaypointUGV"/>
		<!-- TODO add logic for CompositeWaypointUGV and other commands with position data -->
	</xsl:template>

	<xsl:template match="CompositeWaypointUGV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="LoiterUGV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="SetPositionUGV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="WaypointUGV">
		<xsl:call-template name="postWaypoint">
			<xsl:with-param name="waypoint" select="."/>
			<!-- no altitude is provided in a WaypointUGV element -->
			<xsl:with-param name="altitude" select="0"/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="USVCommandScript | USVEvent">
		<xsl:apply-templates select="CompositeWaypointUSV"/>
		<xsl:apply-templates select="LoiterUSV"/>
		<xsl:apply-templates select="SetPositionUSV"/>
		<xsl:apply-templates select="WaypointUSV"/>
		<!-- TODO add logic for CompositeWaypointUSV and other commands with position data -->
	</xsl:template>

	<xsl:template match="CompositeWaypointUSV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="LoiterUSV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="SetPositionUSV">
		<!-- TODO note logic is same as WaypointUSV -->
		<xsl:call-template name="postWaypoint">
			<xsl:with-param name="waypoint" select="."/>
			<!-- no altitude or depth is provided in a WaypointUSV element -->
			<xsl:with-param name="altitude" select="0"/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="WaypointUSV">
		<xsl:call-template name="postWaypoint">
			<xsl:with-param name="waypoint" select="."/>
			<!-- no altitude or depth is provided in a WaypointUSV element -->
			<xsl:with-param name="altitude" select="0"/>
		</xsl:call-template>
	</xsl:template>
	
	<xsl:template match="UUVCommandScript | UUVEvent">
		<xsl:apply-templates select="CompositeWaypointUUV"/>
		<xsl:apply-templates select="Hover"/>
		<xsl:apply-templates select="LoiterUUV"/>
		<xsl:apply-templates select="SetPositionUUV"/>
		<xsl:apply-templates select="WaypointUUV"/>
		<!-- TODO add logic for CompositeWaypointUUV and other commands with position data -->
	</xsl:template>

	<xsl:template match="CompositeWaypointUUV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="Hover">
		<!-- a Hover element can contain a depth or an altitude (not both) -->
		<xsl:choose>
			<xsl:when test="Depth">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="Depth/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="Altitude">
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="Altitude/@value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<!-- no altitude data provided in the AVCL source file for this waypoint -->
				<xsl:call-template name="postWaypoint">
					<xsl:with-param name="waypoint" select="."/>
					<xsl:with-param name="altitude" select="0"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="LoiterUUV">
		<!-- TODO Select data to post to KML. -->
	</xsl:template>

	<xsl:template match="SetPositionUUV">
		<!-- TODO logic is the same for WaypointUUV; may be able to consolidate logic -->
		<xsl:call-template name="postWaypoint">
			<xsl:with-param name="waypoint" select="."/>
			<!-- TODO expand logic: when provided, either Depth or Altitude (not both) can be given in a SetPositionUUV element -->
			<xsl:with-param name="altitude" select="-Depth/@value"/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template match="WaypointUUV">
		<xsl:call-template name="postWaypoint">
			<xsl:with-param name="waypoint" select="."/>
			<!-- TODO expand logic: when provided, either Depth or Altitude (not both) can be given in a WaypointUUV element -->
			<xsl:with-param name="altitude" select="-Depth/@value"/>
		</xsl:call-template>
	</xsl:template>

	<xsl:template name="postWaypoint">
		<xsl:param name="waypoint"/>
		<xsl:param name="altitude"/>
		<Placemark>
			<name>
				<xsl:value-of select="$waypoint/@description"/>
			</name>
			
			<LookAt>
				<longitude>
					<xsl:value-of select="($waypoint/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
				</longitude>
				<latitude>
					<xsl:value-of select="($waypoint/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
				</latitude>
				<altitude>100</altitude>
				<heading>0</heading>
                <tilt>45</tilt>
				<range>10000</range>
			</LookAt>
			
			<styleUrl>#redPushpin</styleUrl>
			<Point>
				<altitudeMode>absolute</altitudeMode>
				<coordinates>
					<xsl:value-of select="($waypoint/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
					<xsl:text>,</xsl:text>
					<xsl:value-of select="($waypoint/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
					<xsl:text>,</xsl:text>
					<xsl:value-of select="$altitude"/>
				</coordinates>
			</Point>
		</Placemark>

	</xsl:template>

	<xsl:template match="AgendaMission">
		<!-- LaunchPosition -->
		<xsl:if test="LaunchPosition/XYPosition">
			<Placemark>
				<name>
					<xsl:text>LaunchPosition </xsl:text>
					<xsl:value-of select="LaunchPosition/@description"/>
				</name>
				
				<LookAt>
					<longitude>
						<xsl:value-of select="(LaunchPosition/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
					</longitude>
					<latitude>
						<xsl:value-of select="(LaunchPosition/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
					</latitude>
					<altitude>100</altitude>
					<heading>0</heading>
					<tilt>45</tilt>
					<range>10000</range>
				</LookAt>
				
				<styleUrl>#redPushpin</styleUrl>
				<Point>
					<altitudeMode>absolute</altitudeMode>
					<coordinates>
						<xsl:value-of select="(LaunchPosition/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
						<xsl:text>,</xsl:text>
						<xsl:value-of select="(LaunchPosition/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
					</coordinates>
				</Point>
			</Placemark>
		</xsl:if>
		<!-- RecoveryPosition -->
		<xsl:if test="RecoveryPosition/XYPosition">
			<Placemark>
				<name>
					<xsl:text>RecoveryPosition </xsl:text>
					<xsl:value-of select="RecoveryPosition/@description"/>
				</name>
				
				<LookAt>
					<longitude>
						<xsl:value-of select="(RecoveryPosition/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
					</longitude>
					<latitude>
						<xsl:value-of select="(RecoveryPosition/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
					</latitude>
					<altitude>100</altitude>
					<heading>0</heading>
					<tilt>45</tilt>
					<range>10000</range>
				</LookAt>
				
				<styleUrl>#redPushpin</styleUrl>
				<Point>
					<altitudeMode>absolute</altitudeMode>
					<coordinates>
						<xsl:value-of select="(RecoveryPosition/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
						<xsl:text>,</xsl:text>
						<xsl:value-of select="(RecoveryPosition/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
					</coordinates>
				</Point>
			</Placemark>
		</xsl:if>
		<xsl:apply-templates select="GoalList"/>
		<xsl:apply-templates select="ConstraintList"/>
	</xsl:template>
	
	<xsl:template match="GoalList">
		<xsl:apply-templates select="Goal"/>
	</xsl:template>
	
	<xsl:template match="Goal">
		<xsl:apply-templates select="Reposition"/>
		<xsl:apply-templates select="OperatingArea"/>
	</xsl:template>

	<xsl:template match="Reposition">
		<!-- Child element Routing has XYPosition (one to many) and depth or altitude data; same structure as for ConstraintList/IngressRouting and ConstraintList/EgressRouting -->
		<xsl:apply-templates select="Routing"/>
	</xsl:template>

	<xsl:template match="OperatingArea | AvoidArea">
		<!-- TODO Translate area geometries to KML; note the AVCL structures for GoalList/Goal/OperatingArea and ConstraintList/AvoidArea are identical -->
		<!-- AVCL area geometries are defined as Circle, LimitingLine, Point, Polygon, or Rectangle (select one) -->
		<xsl:apply-templates select="Circle"/>
		<xsl:apply-templates select="LimitingLine"/>
		<xsl:apply-templates select="Point"/>
		<xsl:apply-templates select="Polygon"/>
		<xsl:apply-templates select="Rectangle"/>
	</xsl:template>
	
	<xsl:template match="Circle">
		<!-- TODO AVCL defines a circle by a center and a radius. Need to determine appropriate KML geometry to generate in the result document (e.g., it is not possible to achieve this by scaling a circular icon). -->
		<!-- TODO Determine how to include vertical block data (depth/altitude) from the AVCL source document -->
	</xsl:template>

	<xsl:template match="LimitingLine">
		<!-- TODO AVCL defines a limiting line by one to many XYPosition or LatitudeLongitude values. Construct a KML LineString with the AVCL limiting line points. -->
		<Placemark>
			<name>
				<!-- Construct the name by combining the description of the OperatingArea or AvoidArea (parent element) and the description of the geometric shape -->
				<xsl:value-of select="../@description"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="./@description"/>
			</name>
			<LineString>
				<xsl:call-template name="geometryPoints">
					<xsl:with-param name="shape" select="."/>
					<xsl:with-param name="isClosed" select="'false'"/>
					<!-- isClosed = false means this is not a closed shape (in contrast to a polygon, for example) -->
				</xsl:call-template>
			</LineString>
		</Placemark>
		<!-- TODO Determine how to include vertical block data (depth/altitude) from the AVCL source document -->
	</xsl:template>

	<xsl:template match="Point">
		<!-- TODO AVCL defines a point by an XYPosition or LatitudeLongitude. Need to determine appropriate KML geometry to generate in the result document. -->
		<Placemark>
			<name>
				<!-- Construct the name by combining the description of the OperatingArea or AvoidArea (parent element) and the description of the geometric shape -->
				<xsl:value-of select="../@description"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="./@description"/>
			</name>
			<Point>
				<xsl:call-template name="geometryPoints">
					<xsl:with-param name="shape" select="."/>
					<xsl:with-param name="isClosed" select="'false'"/>
					<!-- isClosed = false means this is not a closed shape (in contrast to a polygon, for example) -->
				</xsl:call-template>
			</Point>
		</Placemark>
		<!-- TODO Determine how to include vertical block data (depth/altitude) from the AVCL source document -->
	</xsl:template>

	<xsl:template match="Polygon">
		<!-- AVCL defines a polygon by one to many XYPosition or LatitudeLongitude values. Construct a KML Polygon/isOuterRing structure with the AVCL polygon vertices. -->
		<!-- TODO May need to pick up the description from the parent Goal or AvoidArea description -->
		<Placemark>
			<name>
				<!-- Construct the name by combining the description of the OperatingArea or AvoidArea (parent element) and the description of the geometric shape -->
				<xsl:value-of select="../@description"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="./@description"/>
			</name>
			<Polygon>
				<outerBoundaryIs>
					<LinearRing>
						<xsl:call-template name="geometryPoints">
							<xsl:with-param name="shape" select="."/>
							<xsl:with-param name="isClosed" select="'true'"/>
							<!-- isClosed = true means this is constructing a closed shape where the final point in the KML geometry needs to be identical to the first point -->
						</xsl:call-template>
					</LinearRing>
				</outerBoundaryIs>
			</Polygon>
		</Placemark>
		<!-- TODO Determine how to include vertical block data (depth/altitude) from the AVCL source document -->
	</xsl:template>
	
	<xsl:template name="geometryPoints">
		<!-- Output vertices of AVCL geometry to KML coordinates element -->
		<xsl:param name="shape"/>
		<xsl:param name="isClosed"/>
		<xsl:variable name="initialPointLat" select="($shape/XYPosition[1]/@x * $latitudeMetersToDegrees) + $originLatitude"/>
		<xsl:variable name="initialPointLong" select="($shape/XYPosition[1]/@y * $longitudeMetersToDegrees) + $originLongitude"/>
		<coordinates>
			<xsl:for-each select="XYPosition">
				<xsl:value-of select="(./@y * $longitudeMetersToDegrees) + $originLongitude"/>
				<xsl:text>,</xsl:text>
				<xsl:value-of select="(./@x * $latitudeMetersToDegrees) + $originLatitude"/>
				<xsl:text> </xsl:text>
				<xsl:if test="$isClosed = 'true' and position() = last()">
					<xsl:value-of select="$initialPointLong"/>
					<xsl:text>,</xsl:text>
					<xsl:value-of select="$initialPointLat"/>
				</xsl:if>
			</xsl:for-each>
		</coordinates>
	</xsl:template>

	<xsl:template match="Rectangle">
		<!-- AVCL defines a rectangle by a northwest corner, width, height, and rotation (around the northwest corner). -->
		<!-- As an initial capability, compute the vertices of a rectangular polygon from the AVCL data, without yet taking the rotation into account  (computing the rotated coordinates will require trigonometric functions not available in XSLT without extension) -->
		<!-- TODO Add logic to apply the rotation to compute the vertices of the rectangle -->
		<xsl:variable name="NWlat" select="(NorthwestCorner/XYPosition/@x * $latitudeMetersToDegrees) + $originLatitude"/>
		<xsl:variable name="NWlong" select="(NorthwestCorner/XYPosition/@y * $longitudeMetersToDegrees) + $originLongitude"/>
		<xsl:variable name="SWlat" select="((NorthwestCorner/XYPosition/@x + Height/@value) * $latitudeMetersToDegrees) + $originLatitude"/>
		<xsl:variable name="SWlong" select="$NWlong"/>
		<xsl:variable name="SElat" select="$SWlat"/>
		<xsl:variable name="SElong" select="((NorthwestCorner/XYPosition/@y + Width/@value) * $longitudeMetersToDegrees) + $originLongitude"/>
		<xsl:variable name="NElat" select="$NWlat"/>
		<xsl:variable name="NElong" select="$SElong"/>
		<Placemark>
			<name>
				<!-- Construct the name by combining the description of the OperatingArea or AvoidArea (parent element) and the description of the geometric shape -->
				<xsl:value-of select="../@description"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="./@description"/>
			</name>
			<Polygon>
				<outerBoundaryIs>
					<LinearRing>
						<coordinates>
							<xsl:value-of select="$NWlong"/>
							<xsl:text>,</xsl:text>
							<xsl:value-of select="$NWlat"/>
							<xsl:text> </xsl:text>
							<xsl:value-of select="$SWlong"/>
							<xsl:text>,</xsl:text>
							<xsl:value-of select="$SWlat"/>
							<xsl:text> </xsl:text>
							<xsl:value-of select="$SElong"/>
							<xsl:text>,</xsl:text>
							<xsl:value-of select="$SElat"/>
							<xsl:text> </xsl:text>
							<xsl:value-of select="$NElong"/>
							<xsl:text>,</xsl:text>
							<xsl:value-of select="$NElat"/>
							<xsl:text> </xsl:text>
							<xsl:value-of select="$NWlong"/>
							<xsl:text>,</xsl:text>
							<xsl:value-of select="$NWlat"/>
						</coordinates>
					</LinearRing>
				</outerBoundaryIs>
			</Polygon>
		</Placemark>
		<!-- TODO Determine how to include vertical block data (depth/altitude) from the AVCL source document -->
	</xsl:template>

	<xsl:template match="ConstraintList">
		<xsl:apply-templates select="IngressRouting"/>
		<xsl:apply-templates select="AvoidArea"/>
		<xsl:apply-templates select="EgressRouting"/>
	</xsl:template>

	<xsl:template match="Routing | IngressRouting | EgressRouting">
		<!-- TODO Translate routing points to KML; note the AVCL structures are identical -->
	</xsl:template>

	<xsl:template match="GuidedMunitionMission">
		<!-- TODO Select data to post to KML. -->
		<!-- LaunchPositionML -->
		<!-- AimPoint -->
	</xsl:template>


	
	<!-- ****** TEMPLATES FOR EVENT LOG ****** -->
	
	<xsl:template match="EventLog">
		<!-- Child elements of EventLog have the same structure as the respective vehicle CommandScript elements -->
		<xsl:apply-templates select="UUVEvent"/>
		<xsl:apply-templates select="UGVEvent"/>
		<xsl:apply-templates select="USVEvent"/>
		<xsl:apply-templates select="UAVEvent"/>
	</xsl:template>


	<!-- ****** TEMPLATES FOR SAMPLED RESULTS ****** -->
	
    <xsl:template match="SampledResults">
	
        <Placemark>
            <name>
                <xsl:text>Telemetry Point #</xsl:text>
                <xsl:value-of select="position()"/>
            </name>
            <xsl:if test="position()=1">
				<xsl:call-template name="generateLookAt">
					<xsl:with-param name="lookAtLongitude" select="$originLongitude"/>
					<xsl:with-param name="lookAtLatitude" select="$originLatitude"/>
					<xsl:with-param name="lookAtAltitude" select="100"/>
					<xsl:with-param name="lookAtHeading" select="0"/>
					<xsl:with-param name="lookAtTilt" select="45"/>
					<xsl:with-param name="lookAtRange" select="5000"/>
				<xsl:with-param name="lookAtAltitudeMode" select="'absolute'"/>
				</xsl:call-template>
            </xsl:if>
	
            <xsl:call-template name="generateTimeStamp"/>
            
			<!-- Determine which kind of telemetry (UUV, USV, UGV, or UAV) is in the SampledResults element -->
			<xsl:choose>
				<xsl:when test="UUVTelemetry">
					<xsl:choose>
						<!-- when VerticalPosition is present, the depth is in attribute depth (mandatory) and possibly attribute altitude (optional) -->
						<xsl:when test="UUVTelemetry/VerticalPosition">
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UUVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UUVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="-UUVTelemetry/VerticalPosition/@depth"/>
							</xsl:call-template>
						</xsl:when>
						<xsl:otherwise>
							<!-- no depth provided in the source file for this telemetry point -->
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UUVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UUVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="0"/>
							</xsl:call-template>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:when>
				<xsl:when test="USVTelemetry">
					<xsl:call-template name="postTelemetryPoint">
						<xsl:with-param name="latitude" select="$originLatitude + (USVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
						<xsl:with-param name="longitude" select="$originLongitude + (USVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
						<!-- no vertical position is given for USVTelemetry -->
						<xsl:with-param name="altitude" select="0"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:when test="UGVTelemetry">
					<!-- when AltitudeUGV is present, the altitude is in attribute altitudeMSL (mandatory) -->
					<xsl:choose>
						<xsl:when test="UGVTelemetry/AltitudeUGV">
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UGVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UGVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="UGVTelemetry/AltitudeUGV/@altitudeMSL"/>
							</xsl:call-template>
						</xsl:when>
						<xsl:otherwise>
							<!-- no altitude provided in the source file for this telemetry point --> 
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UGVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UGVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="0"/>
							</xsl:call-template>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:when>
				<xsl:when test="UAVTelemetry">
					<!-- when VerticalPositionUAV is present, the altitude is in attribute altitudeMSL (mandatory) and possibly attribute altitudeAGL (optional) -->
					<xsl:choose>
						<xsl:when test="UAVTelemetry/VerticalPositionUAV">
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UAVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UAVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="UAVTelemetry/VerticalPositionUAV/@altitudeMSL"/>
							</xsl:call-template>
						</xsl:when>
						<xsl:otherwise>
							<!-- no altitude provided in the source file for this telemetry point -->
							<xsl:call-template name="postTelemetryPoint">
								<xsl:with-param name="latitude" select="$originLatitude + (UAVTelemetry/GeographicPosition/XYPosition/@x * $latitudeMetersToDegrees)"/>
								<xsl:with-param name="longitude" select="$originLongitude + (UAVTelemetry/GeographicPosition/XYPosition/@y * $longitudeMetersToDegrees)"/>
								<xsl:with-param name="altitude" select="0"/>
							</xsl:call-template>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:when>
				<xsl:otherwise>
					<!-- There is no other geospatial data contained in the SampledResults element --> 
				</xsl:otherwise>
			</xsl:choose>
        </Placemark>
    </xsl:template>
	
	<xsl:template name="postTelemetryPoint">
		<xsl:param name="latitude"/>
		<xsl:param name="longitude"/>
		<xsl:param name="altitude"/>
		<styleUrl>#msn_ylw-pushpin</styleUrl>
		<Point>
			<altitudeMode>absolute</altitudeMode>
			<coordinates>
				<xsl:value-of select="$longitude"/>
				<xsl:text>,</xsl:text>
				<xsl:value-of select="$latitude"/>
				<xsl:text>,</xsl:text>
				<xsl:value-of select="$altitude"/>
			</coordinates>
		</Point>
	</xsl:template>
	
	
	<!-- ****** TEMPLATES FOR UTILITY COMPUTATIONS ****** -->

	<xsl:template name="generateLookAt">
		<!-- LookAt defines a virtual camera. The LookAt element positions the "camera" in relation to the object that is being viewed. In Google Earth, the view "flies to" this LookAt viewpoint when the user double-clicks an item in the Places panel or double-clicks an icon in the 3D viewer. Information here and in the comments below is extracted from the KML Reference page: https://developers.google.com/kml/documentation/kmlreference#lookat -->
		<xsl:param name="lookAtLongitude"/>
		<!-- Longitude of the point the camera is looking at. Angular distance in degrees, relative to the Prime Meridian. Values west of the Meridian range from -180 to 0 degrees. Values east of the Meridian range from 0 to 180 degrees. -->
		<xsl:param name="lookAtLatitude"/>
		<!-- Latitude of the point the camera is looking at. Degrees north or south of the Equator (0 degrees). Values range from -90 degrees to 90 degrees. -->
		<xsl:param name="lookAtAltitude"/>
		<!-- Altitude: Distance from the earth's surface, in meters. Interpreted according to the LookAt's altitude mode. -->
		<xsl:param name="lookAtHeading"/>
		<!-- Heading: Direction (that is, North, South, East, West), in degrees. Default = 0 (North). Values range from 0 to 360 degrees. -->
		<xsl:param name="lookAtTilt"/>
		<!-- Tilt: Angle between the direction of the LookAt position and the normal to the surface of the earth. Values range from 0 to 90 degrees. A value of 0 degrees indicates fiewing from directly above. A value of 90 degrees indicates viewing along the horizon. -->
		<xsl:param name="lookAtRange"/>
		<!-- Range (required): Distance in meters from the point specified by the longitude, latitude, and altitude values to the camera position. -->
		<xsl:param name="lookAtAltitudeMode"/>
		<!-- Altitude Mode: Specifies how the altitude value is interpreted. Possible values are clampToGround (default, ignores the altitude value and places the LookAt position on the ground), relativeToGround (interprets the altitude as a value in meters above the ground), and absolute (interprets the altitude as a value in meters above sea level). -->
		<LookAt>
			<longitude>
				<xsl:value-of select="$lookAtLongitude"/>
			</longitude>
			<latitude>
				<xsl:value-of select="$lookAtLatitude"/>
			</latitude>
			<altitude>
				<xsl:value-of select="$lookAtAltitude"/>
			</altitude>
			<heading>
				<xsl:value-of select="$lookAtHeading"/>
			</heading>
			<tilt>
				<xsl:value-of select="$lookAtTilt"/>
			</tilt>
			<range>
				<xsl:value-of select="$lookAtRange"/>
			</range>
			<altitudeMode>
				<xsl:value-of select="$lookAtAltitudeMode"/>
			</altitudeMode>
		</LookAt>
	</xsl:template>

    <xsl:template name="generateTimeStamp">
        <TimeStamp>
            <when>
                <!-- the values for mission starting point -->
                <xsl:variable name="initialSeconds" select="number(../MissionStartTime/@second)"/>
                <xsl:variable name="initialMinutes" select="number(../MissionStartTime/@minute)"/>
                <xsl:variable name="initialHours" select="number(../MissionStartTime/@hour)"/>
                <xsl:variable name="initialDays" select="../MissionStartTime/@day"/>
                <xsl:variable name="initialMonths">
                    <xsl:call-template name="getMonthValue">
                        <xsl:with-param name="month" select="../MissionStartTime/@month"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:variable name="initialYears" select="../MissionStartTime/@year"/>
			
                <!-- the current telemetry point's timestamp -->
                <xsl:variable name="currentTimeStamp" select="@timeStamp"/>
                <!-- compute the number of hours in the timestamp -->
                <xsl:variable name="currentHours" select="floor($currentTimeStamp div 3600)"/>
                <!-- compute the number of seconds left over after computing the number of hours in the timestamp -->
                <xsl:variable name="currentLeftover" select="$currentTimeStamp mod 3600"/>
                <!-- compute the number of minutes in the timestamp -->
                <xsl:variable name="currentMinutes" select="floor($currentLeftover div 60)"/>
                <!-- compute the number of seconds left over after computing the number of minutes left in the timestamp -->
                <xsl:variable name="currentSeconds" select="$currentLeftover mod 60"/>
				
                <!-- variable holding the display value for seconds, can have fractional value -->
                <xsl:variable name="displaySeconds">
                    <xsl:choose>
                        <!-- TODO confirm computation... what if greater than 120? -->
                        <xsl:when test="$currentSeconds + $initialSeconds >= 60">
                            <xsl:value-of select="$currentSeconds + $initialSeconds - 60"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$currentSeconds + $initialSeconds"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the value of minutes to be carried on
            				if the number of seconds is greater than 60, takes the value of 1 -->
                <xsl:variable name="carryonMinutes">
                    <xsl:choose>
                        <xsl:when test="$currentSeconds + $initialSeconds &gt; 59">
                            <xsl:value-of select="1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="0"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the display value for minutes -->
                <xsl:variable name="displayMinutes">
                    <xsl:choose>
                        <xsl:when test="$currentMinutes + $initialMinutes + $carryonMinutes > 59">
                            <xsl:value-of select="$currentMinutes + $initialMinutes + $carryonMinutes - 60"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$currentMinutes + $initialMinutes + $carryonMinutes"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the value of hours to be carried on
            				if the number of minutes is greater than 60, takes the value of 1 -->
                <xsl:variable name="carryonHours">
                    <xsl:choose>
                        <xsl:when test="$currentMinutes + $initialMinutes + $carryonMinutes > 59">
                            <xsl:value-of select="1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="0"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the display value for hours -->
                <xsl:variable name="displayHours">
                    <xsl:choose>
                        <xsl:when test="$currentHours + $initialHours + $carryonHours > 23">
                            <xsl:value-of select="$currentHours + $initialHours + $carryonHours - 24"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$currentHours + $initialHours + $carryonHours"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the value of days to be carried on
            				if the number of hours is greater than 24, takes the value of 1 -->
                <xsl:variable name="carryonDays">
                    <xsl:choose>
                        <xsl:when test="$currentHours + $initialHours + $carryonHours > 23">
                            <xsl:value-of select="1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="0"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the duration of the initial month -->
                <xsl:variable name="monthLength">
                    <xsl:call-template name="getMonthLength">
                        <xsl:with-param name="month" select="../MissionStartTime/@month"/>
                        <xsl:with-param name="year" select="$initialYears"/>
                    </xsl:call-template>
                </xsl:variable>
				
                <!-- variable holding the display value for days -->
                <xsl:variable name="displayDays">
                    <xsl:choose>
                        <xsl:when test="$initialDays + $carryonDays > number($monthLength)">
                            <xsl:value-of select="$initialDays + $carryonDays - number($monthLength)"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$initialDays + $carryonDays"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the value of months to be carried on
            				if the number of days is greater than the initial month duration, takes the value of 1 -->
                <xsl:variable name="carryonMonths">
                    <xsl:choose>
                        <xsl:when test="$initialDays + $carryonDays > number($monthLength)">
                            <xsl:value-of select="1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="0"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the display value for days -->
                <xsl:variable name="displayMonths">
                    <xsl:choose>
                        <xsl:when test="$initialMonths + $carryonMonths > 12">
                            <xsl:value-of select="$initialMonths + $carryonMonths - 12"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$initialMonths + $carryonMonths"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the value of years to be carried on
            				if the number of months is greater than 12, takes the value of 1 -->
                <xsl:variable name="carryonYears">
                    <xsl:choose>
                        <xsl:when test="$initialMonths + $carryonMonths > 12">
                            <xsl:value-of select="1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="0"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- variable holding the display value for years -->
                <xsl:variable name="displayYears">
                    <xsl:choose>
                        <xsl:when test="$initialMonths + $carryonMonths > 12">
                            <xsl:value-of select="$initialYears + 1"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="$initialYears"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
				
                <!-- display the first half of the timestamp (yyyy-mm-ddT) -->
                <xsl:value-of select="$displayYears"/>
                <xsl:text>-</xsl:text>
                <xsl:if test="$displayMonths &lt; 10">
                    <!-- leading zero -->
                    <xsl:text>0</xsl:text>
                </xsl:if>
                <xsl:value-of select="$displayMonths"/>
                <xsl:text>-</xsl:text>
                <xsl:if test="$displayDays &lt; 10">
                    <!-- leading zero -->
                    <xsl:text>0</xsl:text>
                </xsl:if>
                <xsl:value-of select="$displayDays"/>
                <xsl:text>T</xsl:text>
				
                <!-- display second half of timestamp (hh:mm:ssZ) -->
                <xsl:if test="$displayHours &lt; 10">
                    <!-- leading zero -->
                    <xsl:text>0</xsl:text>
                </xsl:if>
                <xsl:value-of select="$displayHours"/>
                <xsl:text>:</xsl:text>
				
                <xsl:if test="$displayMinutes &lt; 10">
                    <!-- leading zero -->
                    <xsl:text>0</xsl:text>
                </xsl:if>
                <xsl:value-of select="$displayMinutes"/>
                <xsl:text>:</xsl:text>
				
                <xsl:if test="$displaySeconds &lt; 10">
                    <!-- leading zero -->
                    <xsl:text>0</xsl:text>
                </xsl:if>
                <xsl:value-of select="$displaySeconds"/>
                <xsl:text>Z</xsl:text>
				
            </when>
        </TimeStamp>
    </xsl:template>
	
    <xsl:template name="getMonthValue">
        <xsl:param name="month"/>
        <xsl:choose>
            <xsl:when test="$month='January'">
                <xsl:text>01</xsl:text>
            </xsl:when>
            <xsl:when test="$month='February'">
                <xsl:text>02</xsl:text>
            </xsl:when>
            <xsl:when test="$month='March'">
                <xsl:text>03</xsl:text>
            </xsl:when>
            <xsl:when test="$month='April'">
                <xsl:text>04</xsl:text>
            </xsl:when>
            <xsl:when test="$month='May'">
                <xsl:text>05</xsl:text>
            </xsl:when>
            <xsl:when test="$month='June'">
                <xsl:text>06</xsl:text>
            </xsl:when>
            <xsl:when test="$month='July'">
                <xsl:text>07</xsl:text>
            </xsl:when>
            <xsl:when test="$month='August'">
                <xsl:text>08</xsl:text>
            </xsl:when>
            <xsl:when test="$month='September'">
                <xsl:text>09</xsl:text>
            </xsl:when>
            <xsl:when test="$month='October'">
                <xsl:text>10</xsl:text>
            </xsl:when>
            <xsl:when test="$month='November'">
                <xsl:text>11</xsl:text>
            </xsl:when>
            <xsl:when test="$month='December'">
                <xsl:text>12</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>00</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
	
    <xsl:template name="getMonthLength">
        <xsl:param name="month"/>
        <xsl:param name="year"/>
        <xsl:choose>
            <xsl:when test="$month='January'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='February' and $year mod 4=0">
                <xsl:text>29</xsl:text>
            </xsl:when>
            <xsl:when test="$month='February' and $year mod 4 > 0">
                <xsl:text>28</xsl:text>
            </xsl:when>
            <xsl:when test="$month='March'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='April'">
                <xsl:text>30</xsl:text>
            </xsl:when>
            <xsl:when test="$month='May'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='June'">
                <xsl:text>30</xsl:text>
            </xsl:when>
            <xsl:when test="$month='July'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='August'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='September'">
                <xsl:text>30</xsl:text>
            </xsl:when>
            <xsl:when test="$month='October'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:when test="$month='November'">
                <xsl:text>30</xsl:text>
            </xsl:when>
            <xsl:when test="$month='December'">
                <xsl:text>31</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>00</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
