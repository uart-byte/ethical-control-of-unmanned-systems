<?xml version="1.0" encoding="UTF-8"?>
<!--
  <head>
   <meta name="filename"    content="MeoDadmNamingVerification.xslt" />
   <meta name="author"      content="Don Brutzman" />
   <meta name="created"     content="8 May 2022" />
   <meta name="description" content="XSLT stylesheet to check naming constraints of Dimensions Autonomous Decision Making (DADM)  triples." />
   <meta name="url"         content="EthicalControl/stylesheets/MeoDadmNamingVerification.xslt" />
   <meta name="invocation"  content="EthicalControl> ant MeoDadmNamingVerification" />
  </head>
-->
<xsl:stylesheet version="2.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                xmlns:owl="http://www.w3.org/2002/07/owl#">
    
    <xsl:output method="xml" encoding="UTF-8" indent="yes" media-type="text/plain" omit-xml-declaration="yes"/>
    <xsl:strip-space elements="*"/>
    
    <xsl:variable name='indent'> <xsl:text>  </xsl:text></xsl:variable>
    <xsl:variable name='missing'><xsl:text>(missing)</xsl:text></xsl:variable>
    <xsl:variable name='namespacePrefix'>
        <xsl:value-of select="//owl:Prefix[(string-length(@name)= 0)]/@IRI"/>
    </xsl:variable>
    
    <xsl:template match="/">
        <!-- begin processing at root of OWL document containing Dimensions of Autonomous Decision Making (DADM)
             constructs, currently as part of Mission Execution Ontology (MEO)  -->
             
        <!--
        <xsl:message>
            <xsl:text></xsl:text>
            <xsl:text>&#10;</xsl:text>
        </xsl:message>
        -->
        <avclSchemaInsertBlock>
            <xsd:simpleType name="dadmRiskElementNames">
                <xsd:annotation>
                    <xsd:documentation>DADM Risk Elements</xsd:documentation>
                </xsd:annotation>
                <xsd:restriction base="xsd:string">

                    <xsl:apply-templates select="//*[contains(local-name(),'Class')][starts-with(@IRI,'#RE')]"><!-- depth-first search recurse downward -->
                        <xsl:sort select="translate(@IRI,'.','')"/>
                    </xsl:apply-templates>

                </xsd:restriction>
            </xsd:simpleType>
        </avclSchemaInsertBlock>
        
    </xsl:template>
    
    <xsl:template match="@*[(local-name() = 'xmlns:xsd') or ends-with(local-name(), 'xsd') or (. = 'http://www.w3.org/2001/XMLSchema')]">
        <xsl:message>
            <xsl:text>*** found </xsl:text>
            <xsl:value-of select="local-name()"/>
        </xsl:message>
        <!-- ignore -->
    </xsl:template>
    
    <xsl:template match="owl:Class">
        
        <xsl:variable name='className' select='local-name()'/>
        <xsl:variable name='name' select='@IRI'/>

        <xsl:if test="not(preceding::*/@IRI = $name)">
            
            <xsl:variable name="subClassOf">
                <xsl:choose>
                    <xsl:when           test="(@IRI = '#DimensionsAutonomousDecisionMaking')">
                        <xsl:text>(DADM is a top-level class, not a subclass)</xsl:text>
                    </xsl:when>
                    <xsl:when           test="//owl:SubClassOf[owl:Class[1][@IRI = $name]]">
                        <xsl:value-of select="//owl:SubClassOf[owl:Class[1][@IRI = $name]]/owl:Class[2][not(@IRI = $name)]/@IRI"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            
            <xsl:variable name="annotationRdfsLabel">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:label']][owl:IRI/. = $name]/owl:Literal) > 0)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:label']][owl:IRI/. = $name]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            
            <xsl:variable name="annotationVersionInfo">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'owl:versionInfo']][owl:IRI/. = $name]/owl:Literal) = 1)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'owl:versionInfo']][owl:IRI/. = $name]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            
            <xsl:variable name="annotationDescription">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:comment']][owl:IRI/. = $name]/owl:Literal) = 1)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:comment']][owl:IRI/. = $name]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            
            <xsl:if test="starts-with($annotationRdfsLabel,'RE')">
                <xsl:element name="xsd:enumeration">
                    <xsl:attribute name="value">
                        <xsl:value-of select="$annotationRdfsLabel"/>
                    </xsl:attribute>
                    <xsl:element name="xsd:annotation">
                        <xsl:element name="xsd:appinfo">
                            <xsl:value-of select="$annotationDescription"/>
                        </xsl:element>
                        <!-- overkill output
                        <xsl:element name="xsd:documentation">
                            <xsl:value-of select="$namespacePrefix"/>
                            <xsl:value-of select="@IRI"/>
                        </xsl:element>
                        -->
                    </xsl:element>
                </xsl:element>
            </xsl:if>
            
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
        
    </xsl:template>
    
</xsl:stylesheet>

