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
    
    <xsl:output method="text" encoding="UTF-8" indent="yes" media-type="text/plain" omit-xml-declaration="yes"/>
    <xsl:strip-space elements="*"/>
    
    <xsl:variable name='indent'> <xsl:text>  </xsl:text></xsl:variable>
    <xsl:variable name='missing'><xsl:text>(missing)</xsl:text></xsl:variable>
    
    <xsl:template match="/">
        <!-- begin processing at root of OWL document containing Dimensions of Autonomous Decision Making (DADM)
             constructs, currently as part of Mission Execution Ontology (MEO)  -->
             
        <xsl:message>
            <xsl:text>MeoDadmNamingVerification.xslt checks for MEO DADM .owl ontology:</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>- naming conventions for DADM and Risk Element (RE) classes</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>- rdfs:label short names match, corresponding to truncation prefix of longer name</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>- owl:versionInfo includes relevant page number(s) in master DADM document</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>- rdfs:comment includes description of categories and risk elements</xsl:text>
            <xsl:text>&#10;</xsl:text>
            <xsl:text>- TODO check and avoid duplication errors</xsl:text>
            <xsl:text>&#10;</xsl:text>
        </xsl:message>
        
        <xsl:apply-templates select="//*[contains(local-name(),'Class')][starts-with(@IRI,'#DimensionsAutonomousDecisionMaking')]"/>
        
        <xsl:apply-templates select="//*[contains(local-name(),'Class')][starts-with(@IRI,'#DADM') or starts-with(@IRI,'#RE')]"><!-- depth-first search recurse downward -->
            <xsl:sort select="translate(@IRI,'.','')"/>
        </xsl:apply-templates>
        
    </xsl:template>
    
    <xsl:template match="owl:Class">
        
        <!-- <xsl:if test="not(position()=1)">
            <xsl:text>&#10;</xsl:text>
            <xsl:text>; </xsl:text>
        </xsl:if> -->
        <xsl:variable name='className' select='local-name()'/>
        <xsl:variable name='IRI' select='@IRI'/>

        <xsl:if test="not(preceding::*/@IRI = $IRI)">
            <xsl:value-of select="local-name()"/>
            <xsl:text> IRI='</xsl:text>
            <xsl:value-of select="@IRI"/>
            <xsl:text>'</xsl:text>
            <xsl:text>&#10;</xsl:text>
            
            <xsl:variable name="subClassOf">
                <xsl:choose>
                    <xsl:when           test="(@IRI = '#DimensionsAutonomousDecisionMaking')">
                        <xsl:text>(DADM is a top-level class, not a subclass)</xsl:text>
                    </xsl:when>
                    <xsl:when           test="//owl:SubClassOf[owl:Class[1][@IRI = $IRI]]">
                        <xsl:value-of select="//owl:SubClassOf[owl:Class[1][@IRI = $IRI]]/owl:Class[2][not(@IRI = $IRI)]/@IRI"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:if test="not($className = '#DADM_DimensionsAutonomousDecisionMaking')">
                <xsl:value-of select='$indent'/>
                <xsl:text> owl:subClassOf   </xsl:text>
                <xsl:value-of select='$subClassOf'/>
                <xsl:text>&#10;</xsl:text>
            </xsl:if>
            
            <xsl:variable name="annotationRdfsLabel">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:label']][owl:IRI/. = $IRI]/owl:Literal) > 0)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:label']][owl:IRI/. = $IRI]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:value-of select='$indent'/>
            <xsl:text> rdfs:label       </xsl:text>
            <xsl:value-of select='$annotationRdfsLabel'/>
            <xsl:choose>
                <xsl:when test="not($annotationRdfsLabel = $missing) and ($IRI = concat('#',$annotationRdfsLabel))">
                    <xsl:text> *** error, IRI </xsl:text>
                    <xsl:value-of select="$IRI"/>
                    <xsl:text> identical to rdfs:label </xsl:text>
                    <xsl:value-of select="$annotationRdfsLabel"/>
                    <xsl:text>, expected more-descriptive label</xsl:text>
                    <xsl:text> ***</xsl:text>
                </xsl:when>
                <xsl:when test="not($annotationRdfsLabel = $missing) and (string-length($IRI) > string-length($annotationRdfsLabel) + 1)">
                    <xsl:text> *** error, IRI </xsl:text>
                    <xsl:value-of select="$IRI"/>
                    <xsl:text> is longer than rdfs:label </xsl:text>
                    <xsl:value-of select="$annotationRdfsLabel"/>
                    <xsl:text>, instead expected more-descriptive label</xsl:text>
                    <xsl:text> ***</xsl:text>
                </xsl:when>
                <xsl:when test="not($annotationRdfsLabel = $missing) and not($IRI = '#DADM') and not(starts-with(concat('#',$annotationRdfsLabel),$IRI))">
                    <xsl:text> *** error, mismatch with IRI</xsl:text>
                    <xsl:if test="starts-with($IRI,'#') and contains($IRI,'_')">
                        <xsl:text>, expected </xsl:text>
                        <xsl:value-of select="substring-before(substring-after($IRI,'#'),'_')"/>
                    </xsl:if>
                    <xsl:text> ***</xsl:text>
                </xsl:when>
                <xsl:when test="($annotationRdfsLabel = $missing)">
                    <xsl:if test="starts-with($IRI,'#') and contains($IRI,'_')">
                        <xsl:text>, expected </xsl:text>
                        <xsl:value-of select="substring-before(substring-after($IRI,'#'),'_')"/>
                    </xsl:if>                    
                </xsl:when>
            </xsl:choose>

            <xsl:text>&#10;</xsl:text>
            
            <xsl:variable name="annotationVersionInfo">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'owl:versionInfo']][owl:IRI/. = $IRI]/owl:Literal) = 1)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'owl:versionInfo']][owl:IRI/. = $IRI]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:value-of select='$indent'/>
            <xsl:text> owl:versionInfo  </xsl:text>
            <xsl:value-of select='$annotationVersionInfo'/>
            <xsl:if test="not($annotationVersionInfo = $missing) and not(@IRI = '#DimensionsAutonomousDecisionMaking') and
                          not(starts-with($annotationVersionInfo,'page ') or starts-with($annotationVersionInfo,'pages '))">
                <xsl:text> *** error, incorrect page number(s) ***</xsl:text>
            </xsl:if>
            <xsl:text>&#10;</xsl:text>
            
            <xsl:variable name="annotationDescription">
                <xsl:choose>
                    <xsl:when test="(   count(//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:comment']][owl:IRI/. = $IRI]/owl:Literal) = 1)">
                        <xsl:value-of select="//owl:AnnotationAssertion[owl:AnnotationProperty[@abbreviatedIRI = 'rdfs:comment']][owl:IRI/. = $IRI]/owl:Literal"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select='$missing'/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:value-of select='$indent'/>
            <xsl:text> rdfs:comment    "</xsl:text>
            <xsl:value-of select='$annotationDescription'/>
            <xsl:text>"</xsl:text>
            <xsl:if test="($annotationDescription = $missing) or (string-length($annotationDescription) = 0)">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, missing rdfs:comment description ***</xsl:text>
            </xsl:if>
            <xsl:if test="contains($annotationDescription ,'`')">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, replace back-apostrophe ` with apostrophe ' ***</xsl:text>
            </xsl:if>
            <xsl:if test="contains($annotationDescription ,'“')">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, replace left-quote “ with quotation mark " ***</xsl:text>
            </xsl:if>
            <xsl:if test="contains($annotationDescription ,'”')">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, replace right-quote ” with quotation mark " ***</xsl:text>
            </xsl:if>
            <xsl:if test="contains($annotationDescription ,'—')">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, replace m-dash — with hyphen - ***</xsl:text>
            </xsl:if>
            <xsl:if test="contains($annotationDescription ,'�?')">
                <xsl:text>&#10;</xsl:text>
                <xsl:text> *** error, replace special character �? ***</xsl:text>
            </xsl:if>
                
            <xsl:text>&#10;</xsl:text>
            <xsl:text>&#10;</xsl:text>
        </xsl:if>
        
    </xsl:template>
    
</xsl:stylesheet>

