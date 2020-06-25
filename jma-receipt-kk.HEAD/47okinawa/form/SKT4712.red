<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="NO" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="4"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SRYYM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="16"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SEIYMD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="22"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="14"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HKNJANUM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="12"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="ZAI1" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="2"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="ZAI2" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="2"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="RECEKA" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="20"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TOKUBETU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="10"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="KOH-TBL" occurs="9">
      <dia:element name="HBTNUM" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="2"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="HKNBETU-TBL" occurs="3">
        <dia:element name="HKNBETU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="2"/>
          </dia:appinfo>
        </dia:element>
      </dia:element>
      <dia:element name="KOHNYUGAI-TBL" occurs="2">
        <dia:element name="KKENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="5"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="KNISSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="5"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="KTENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="9"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="KITBFTN" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="9"/>
          </dia:appinfo>
        </dia:element>
      </dia:element>
      <dia:element name="K-SKENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="K-SNISSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="K-SKINGK" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="K-SFTNGK" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
  </dia:dictionarydata>
  <dia:diagramdata>
    <dia:attribute name="background">
      <dia:color val="#ffffff"/>
    </dia:attribute>
    <dia:attribute name="paper">
      <dia:composite type="paper">
        <dia:attribute name="name">
          <dia:string>#A4#</dia:string>
        </dia:attribute>
        <dia:attribute name="pswidth">
          <dia:real val="21"/>
        </dia:attribute>
        <dia:attribute name="psheight">
          <dia:real val="29.7"/>
        </dia:attribute>
        <dia:attribute name="tmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="bmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="lmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="rmargin">
          <dia:real val="0"/>
        </dia:attribute>
        <dia:attribute name="is_portrait">
          <dia:boolean val="true"/>
        </dia:attribute>
        <dia:attribute name="scaling">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="fitto">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:composite>
    </dia:attribute>
  </dia:diagramdata>
  <dia:layer name="背景" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49,1.99;16.51,3.31"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="4.5,2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="12"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.3"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49,2.49;16.51,2.51"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,2.5"/>
        <dia:point val="16.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="5.25,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.24,2.49;5.26,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.25,2.5"/>
        <dia:point val="5.25,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="6,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.99,2.49;6.01,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,2.5"/>
        <dia:point val="6,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="6.75,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.74,2.49;6.76,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.75,2.5"/>
        <dia:point val="6.75,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="7.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.49,2.49;7.51,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.5,2.5"/>
        <dia:point val="7.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="8.25,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.24,2.49;8.26,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.25,2.5"/>
        <dia:point val="8.25,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="9,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.99,2.49;9.01,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,2.5"/>
        <dia:point val="9,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O18" connection="5"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="9.75,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.74,2.49;9.76,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.75,2.5"/>
        <dia:point val="9.75,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.49,1.99;10.51,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,2"/>
        <dia:point val="10.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O0" connection="1"/>
        <dia:connection handle="1" to="O0" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.49,2.49;10.51,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,2.5"/>
        <dia:point val="10.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O0" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.24,2.49;11.26,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.25,2.5"/>
        <dia:point val="11.25,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O18" connection="7"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="12,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.99,2.49;12.01,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,2.5"/>
        <dia:point val="12,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="12.75,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.74,2.49;12.76,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.75,2.5"/>
        <dia:point val="12.75,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="13.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.49,2.49;13.51,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.5,2.5"/>
        <dia:point val="13.5,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="14.25,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.24,2.49;14.26,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.25,2.5"/>
        <dia:point val="14.25,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="15,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.99,2.49;15.01,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,2.5"/>
        <dia:point val="15,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="15.75,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.74,2.49;15.76,3.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.75,2.5"/>
        <dia:point val="15.75,3.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="9,2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.98,1.98;11.27,3.32"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="9,2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="2.25"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.3"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.04"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,3.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,3.44;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.5,3.45"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="18.6"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="23.5"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,4.44;20.11,4.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,4.45"/>
        <dia:point val="20.1,4.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,6.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,6.94;20.11,6.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,6.95"/>
        <dia:point val="20.1,6.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,9.44;20.11,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,9.45"/>
        <dia:point val="20.1,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,11.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,11.94;20.11,11.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,11.95"/>
        <dia:point val="20.1,11.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,26.94;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,26.95"/>
        <dia:point val="20.1,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O19" connection="5"/>
        <dia:connection handle="1" to="O19" connection="7"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,3.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.49,3.44;3.51,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,3.45"/>
        <dia:point val="3.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.49,3.94;20.11,3.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,3.95"/>
        <dia:point val="20.1,3.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="12.5,3.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,3.44;12.51,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.5,3.45"/>
        <dia:point val="12.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="5,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99,3.94;5.01,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,3.95"/>
        <dia:point val="5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="6.5,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.49,3.94;6.51,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.5,3.95"/>
        <dia:point val="6.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="9.5,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.49,3.94;9.51,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.5,3.95"/>
        <dia:point val="9.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,3.94;13.91,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,3.95"/>
        <dia:point val="13.9,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,3.94;15.31,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,3.95"/>
        <dia:point val="15.3,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,3.94;17.71,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,3.95"/>
        <dia:point val="17.7,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,4.44;2.51,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,4.45"/>
        <dia:point val="2.5,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="2,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,4.44;2.01,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,4.45"/>
        <dia:point val="2,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="2,5.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,5.94;20.11,5.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,5.95"/>
        <dia:point val="20.1,5.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,6.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,6.44;20.11,6.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,6.45"/>
        <dia:point val="20.1,6.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,5.19;20.11,5.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,5.2"/>
        <dia:point val="20.1,5.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,5.19;20.11,5.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,5.2"/>
        <dia:point val="17.7,5.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,5.19;17.71,5.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,5.2"/>
        <dia:point val="15.3,5.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,5.19;13.91,5.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,5.2"/>
        <dia:point val="12.5,5.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,5.19;15.31,5.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,5.2"/>
        <dia:point val="13.9,5.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="2,8.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,8.44;20.11,8.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,8.45"/>
        <dia:point val="20.1,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,8.94;20.11,8.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,8.95"/>
        <dia:point val="20.1,8.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,7.69;20.11,7.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,7.7"/>
        <dia:point val="20.1,7.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,7.69;20.11,8.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,7.7"/>
        <dia:point val="17.7,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,7.69;17.71,8.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,7.7"/>
        <dia:point val="15.3,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,7.69;13.91,8.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,7.7"/>
        <dia:point val="12.5,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,7.69;15.31,8.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,7.7"/>
        <dia:point val="13.9,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="2,10.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,10.94;20.11,10.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,10.95"/>
        <dia:point val="20.1,10.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,11.44;20.11,11.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,11.45"/>
        <dia:point val="20.1,11.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,10.19;20.11,10.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,10.2"/>
        <dia:point val="20.1,10.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,10.19;20.11,10.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,10.2"/>
        <dia:point val="17.7,10.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,10.19;17.71,10.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,10.2"/>
        <dia:point val="15.3,10.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,10.19;13.91,10.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,10.2"/>
        <dia:point val="12.5,10.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,10.19;15.31,10.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,10.2"/>
        <dia:point val="13.9,10.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="2,13.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,13.44;20.11,13.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,13.45"/>
        <dia:point val="20.1,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,13.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,13.94;20.11,13.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,13.95"/>
        <dia:point val="20.1,13.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,12.69;20.11,12.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,12.7"/>
        <dia:point val="20.1,12.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,12.69;20.11,13.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,12.7"/>
        <dia:point val="17.7,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,12.69;17.71,13.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,12.7"/>
        <dia:point val="15.3,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,12.69;13.91,13.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,12.7"/>
        <dia:point val="12.5,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,12.69;15.31,13.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,12.7"/>
        <dia:point val="13.9,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,6.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,6.44;20.11,6.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,6.45"/>
        <dia:point val="17.7,6.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,6.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,6.44;17.71,6.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,6.45"/>
        <dia:point val="15.3,6.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,6.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,6.44;13.91,6.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,6.45"/>
        <dia:point val="12.5,6.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,6.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,6.44;15.31,6.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,6.45"/>
        <dia:point val="13.9,6.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,8.94;20.11,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,8.95"/>
        <dia:point val="17.7,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,8.94;17.71,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,8.95"/>
        <dia:point val="15.3,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,8.94;13.91,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,8.95"/>
        <dia:point val="12.5,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,8.94;15.31,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,8.95"/>
        <dia:point val="13.9,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,11.44;20.11,11.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,11.45"/>
        <dia:point val="17.7,11.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,11.44;17.71,11.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,11.45"/>
        <dia:point val="15.3,11.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,11.44;13.91,11.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,11.45"/>
        <dia:point val="12.5,11.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,11.44;15.31,11.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,11.45"/>
        <dia:point val="13.9,11.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,13.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,13.94;20.11,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,13.95"/>
        <dia:point val="17.7,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,13.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,13.94;17.71,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,13.95"/>
        <dia:point val="15.3,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,13.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,13.94;13.91,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,13.95"/>
        <dia:point val="12.5,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,13.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,13.94;15.31,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,13.95"/>
        <dia:point val="13.9,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,14.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,14.44;20.11,14.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,14.45"/>
        <dia:point val="20.1,14.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,16.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,16.94;20.11,16.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,16.95"/>
        <dia:point val="20.1,16.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,19.44;20.11,19.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,19.45"/>
        <dia:point val="20.1,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,21.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,21.94;20.11,21.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,21.95"/>
        <dia:point val="20.1,21.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,24.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,24.44;20.11,24.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,24.45"/>
        <dia:point val="20.1,24.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,14.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,14.44;2.51,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,14.45"/>
        <dia:point val="2.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="2,14.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,14.44;2.01,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,14.45"/>
        <dia:point val="2,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="2,15.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,15.94;20.11,15.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,15.95"/>
        <dia:point val="20.1,15.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,16.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,16.44;20.11,16.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,16.45"/>
        <dia:point val="20.1,16.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,15.19;20.11,15.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,15.2"/>
        <dia:point val="20.1,15.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,15.19;20.11,15.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,15.2"/>
        <dia:point val="17.7,15.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,15.19;17.71,15.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,15.2"/>
        <dia:point val="15.3,15.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,15.19;13.91,15.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,15.2"/>
        <dia:point val="12.5,15.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,15.19;15.31,15.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,15.2"/>
        <dia:point val="13.9,15.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="2,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,18.44;20.11,18.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,18.45"/>
        <dia:point val="20.1,18.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,18.94;20.11,18.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,18.95"/>
        <dia:point val="20.1,18.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,17.69;20.11,17.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,17.7"/>
        <dia:point val="20.1,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,17.69;20.11,18.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,17.7"/>
        <dia:point val="17.7,18.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,17.69;17.71,18.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,17.7"/>
        <dia:point val="15.3,18.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,17.69;13.91,18.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,17.7"/>
        <dia:point val="12.5,18.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,17.69;15.31,18.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,17.7"/>
        <dia:point val="13.9,18.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="2,20.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,20.94;20.11,20.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,20.95"/>
        <dia:point val="20.1,20.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,21.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,21.44;20.11,21.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,21.45"/>
        <dia:point val="20.1,21.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,20.19;20.11,20.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,20.2"/>
        <dia:point val="20.1,20.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,20.19;20.11,20.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,20.2"/>
        <dia:point val="17.7,20.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,20.19;17.71,20.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,20.2"/>
        <dia:point val="15.3,20.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,20.19;13.91,20.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,20.2"/>
        <dia:point val="12.5,20.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,20.19;15.31,20.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,20.2"/>
        <dia:point val="13.9,20.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="2,23.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,23.44;20.11,23.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,23.45"/>
        <dia:point val="20.1,23.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,23.94;20.11,23.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,23.95"/>
        <dia:point val="20.1,23.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,22.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,22.69;20.11,22.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,22.7"/>
        <dia:point val="20.1,22.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,22.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,22.69;20.11,23.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,22.7"/>
        <dia:point val="17.7,23.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,22.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,22.69;17.71,23.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,22.7"/>
        <dia:point val="15.3,23.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,22.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,22.69;13.91,23.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,22.7"/>
        <dia:point val="12.5,23.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,22.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,22.69;15.31,23.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,22.7"/>
        <dia:point val="13.9,23.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,16.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,16.44;20.11,16.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,16.45"/>
        <dia:point val="17.7,16.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,16.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,16.44;17.71,16.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,16.45"/>
        <dia:point val="15.3,16.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,16.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,16.44;13.91,16.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,16.45"/>
        <dia:point val="12.5,16.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,16.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,16.44;15.31,16.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,16.45"/>
        <dia:point val="13.9,16.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,18.94;20.11,19.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,18.95"/>
        <dia:point val="17.7,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,18.94;17.71,19.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,18.95"/>
        <dia:point val="15.3,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,18.94;13.91,19.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,18.95"/>
        <dia:point val="12.5,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,18.94;15.31,19.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,18.95"/>
        <dia:point val="13.9,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,21.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,21.44;20.11,21.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,21.45"/>
        <dia:point val="17.7,21.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,21.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,21.44;17.71,21.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,21.45"/>
        <dia:point val="15.3,21.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,21.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,21.44;13.91,21.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,21.45"/>
        <dia:point val="12.5,21.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,21.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,21.44;15.31,21.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,21.45"/>
        <dia:point val="13.9,21.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,23.94;20.11,24.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,23.95"/>
        <dia:point val="17.7,24.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,23.94;17.71,24.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,23.95"/>
        <dia:point val="15.3,24.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,23.94;13.91,24.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,23.95"/>
        <dia:point val="12.5,24.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,23.94;15.31,24.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,23.95"/>
        <dia:point val="13.9,24.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,26.94;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,26.95"/>
        <dia:point val="20.1,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,26.94;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,26.95"/>
        <dia:point val="20.1,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O19" connection="5"/>
        <dia:connection handle="1" to="O19" connection="7"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.49,26.94;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,26.95"/>
        <dia:point val="20.1,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="2,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.99,25.94;20.11,25.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,25.95"/>
        <dia:point val="20.1,25.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,26.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,26.44;20.11,26.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,26.45"/>
        <dia:point val="20.1,26.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.49,25.19;20.11,25.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,25.2"/>
        <dia:point val="20.1,25.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,25.19;20.11,25.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,25.2"/>
        <dia:point val="17.7,25.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,25.19;17.71,25.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,25.2"/>
        <dia:point val="15.3,25.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,25.19;13.91,25.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,25.2"/>
        <dia:point val="12.5,25.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,25.19;15.31,25.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,25.2"/>
        <dia:point val="13.9,25.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="20.1,26.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,26.44;20.11,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20.1,26.45"/>
        <dia:point val="17.7,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,26.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.29,26.44;17.71,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,26.45"/>
        <dia:point val="15.3,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,26.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.49,26.44;13.91,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,26.45"/>
        <dia:point val="12.5,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="15.3,26.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.89,26.44;15.31,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.3,26.45"/>
        <dia:point val="13.9,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="0" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="6.2,0.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2,0.631481;11.856,1.03148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療報酬請求書（医科・歯科）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.2,0.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="5.4,0.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4,0.631481;5.804,1.03148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.4,0.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="1.575,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.575,3.16111;3.393,3.46111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公費負担医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.575,3.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="6.75,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.841,2.11333;7.659,2.47333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険者番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.75,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="9.75,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2046,2.11333;10.2954,2.47333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#県番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.75,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="10.85,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4864,2.11333;11.2136,2.47333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点区#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.85,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="13.9,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.6274,2.11333;15.1726,2.47333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.9,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="9,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9,2.55278;10.515,3.30278"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.937500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4545"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.37875"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5,2.55278;11.2575,3.30278"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.937500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.5,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4545"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.37875"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Ellipse" version="0" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,0.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.24,0.54;10.41,1.06"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="9.25,0.55"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="1.15"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.5"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="12.65,0.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.65,0.631481;13.862,1.03148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#その３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.65,0.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,27.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,27.1611;3.156,27.4611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#注意#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,27.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="3.35,27.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.35,27.1611;7.289,27.4611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※印の欄は記入しないこと。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.35,27.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="8,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.091,3.51333;8.909,3.87333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療養の給付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8,3.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="16.3,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.6638,3.51333;17.9362,3.87333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事療養・生活療養#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.450000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.3,3.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.21816"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1818"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="4.25,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.947,4.06111;4.553,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.25,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.3864,3.95889;6.1136,4.43889"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療
実日数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,4.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="8,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.697,4.06111;8.303,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="11,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2425,4.06111;11.7575,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一部負担金#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="12.6,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.994,4.24074;12.6,4.44074"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（円）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.250000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.6,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1212"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.101"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="13.2,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.897,4.06111;13.503,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.2,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="14.625,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.322,4.06111;14.928,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#回数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.625,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="16.5,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.197,4.06111;16.803,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="17.8,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.194,4.24074;17.8,4.44074"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（円）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.250000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.8,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1212"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.101"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="18.9,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1425,4.06111;19.6575,4.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#標準負担額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.9,4.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="20.2,4.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.594,4.24074;20.2,4.44074"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（円）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.250000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20.2,4.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1212"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.101"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,4.91111;2.4015,5.51111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,6.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,6.01111;2.4015,6.91111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,6.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="3,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,4.71111;3.303,5.01111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="3,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,5.46111;3.4545,5.76111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,5.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="3,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,6.06111;3.303,6.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,6.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="3,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,6.56111;3.4545,6.86111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,6.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,7.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,7.41111;2.4015,8.01111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,7.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,8.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,8.51111;2.4015,9.41111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,8.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="3,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,7.21111;3.303,7.51111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="3,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,7.96111;3.4545,8.26111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,8.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="3,8.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,8.56111;3.303,8.86111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,8.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="3,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,9.06111;3.4545,9.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,9.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,10.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,9.91111;2.4015,10.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,10.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,11.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,11.0111;2.4015,11.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,11.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="3,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,9.71111;3.303,10.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="3,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,10.4611;3.4545,10.7611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="3,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,11.0611;3.303,11.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,11.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="3,11.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,11.5611;3.4545,11.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,11.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,12.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,12.4111;2.4015,13.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,12.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,13.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,13.5111;2.4015,14.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,13.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="3,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,12.2111;3.303,12.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="3,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,12.9611;3.4545,13.2611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="3,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,13.5611;3.303,13.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="3,14.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,14.0611;3.4545,14.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,14.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,5.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,4.86111;1.9318,5.16111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,5.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,5.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,5.61111;1.9318,5.91111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,5.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,6.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,6.36111;1.9318,6.66111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,6.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,7.36111;1.9318,7.66111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,7.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,8.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,8.11111;1.9318,8.41111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,8.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,9.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,8.86111;1.9318,9.16111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,9.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,10.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,9.86111;1.9318,10.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,10.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,10.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,10.6111;1.9318,10.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,10.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,11.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,11.3611;1.9318,11.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,11.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,12.3611;1.9318,12.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,13.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,13.1111;1.9318,13.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,13.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,14.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,13.8611;1.9318,14.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,14.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,15.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,14.9111;2.4015,15.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,15.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,16.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,16.0111;2.4015,16.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,16.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="3,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,14.7111;3.303,15.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="3,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,15.4611;3.4545,15.7611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,15.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="3,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,16.0611;3.303,16.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,16.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="3,16.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,16.5611;3.4545,16.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,16.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,17.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,17.4111;2.4015,18.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,17.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,18.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,18.5111;2.4015,19.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,18.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="3,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,17.2111;3.303,17.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="3,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,17.9611;3.4545,18.2611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,18.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="3,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,18.5611;3.303,18.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="3,19.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,19.0611;3.4545,19.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,19.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,19.9111;2.4015,20.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,20.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,21.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,21.0111;2.4015,21.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,21.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="3,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,19.7111;3.303,20.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="3,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,20.4611;3.4545,20.7611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="3,21.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,21.0611;3.303,21.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,21.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="3,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,21.5611;3.4545,21.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,21.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,22.4111;2.4015,23.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,22.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,23.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,23.5111;2.4015,24.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,23.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="3,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,22.2111;3.303,22.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="3,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,22.9611;3.4545,23.2611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,23.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="3,23.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,23.5611;3.303,23.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,23.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="3,24.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,24.0611;3.4545,24.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,24.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,15.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,14.8611;1.9318,15.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,15.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,15.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,15.6111;1.9318,15.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,15.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,16.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,16.3611;1.9318,16.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,16.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,17.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,17.3611;1.9318,17.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,17.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,18.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,18.1111;1.9318,18.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,18.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,19.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,18.8611;1.9318,19.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,19.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,20.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,19.8611;1.9318,20.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,20.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,20.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,20.6111;1.9318,20.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,20.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,21.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,21.3611;1.9318,21.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,21.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,22.3611;1.9318,22.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,22.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,23.1111;1.9318,23.4111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,24.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,23.8611;1.9318,24.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,24.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,25.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,24.9111;2.4015,25.5111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請
求#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,25.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,26.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.0985,26.0111;2.4015,26.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※
決
定#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,26.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="3,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,24.7111;3.303,25.0111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="3,25.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,25.4611;3.4545,25.7611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,25.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="3,26.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.697,26.0611;3.303,26.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,26.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="3,26.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5455,26.5611;3.4545,26.8611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,26.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,25.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,24.8611;1.9318,25.1611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#08#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,25.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,25.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,25.6111;1.9318,25.9111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,25.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,26.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5682,26.3611;1.9318,26.6611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,26.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="5.35,0.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.118,0.631481;5.35,1.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SRYYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成９９年９９月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.35,0.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SRYYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="16"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,2.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.117,2.16111;4.45,2.46111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成９９年９９月９９日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,2.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,2.55278;16.5525,3.30278"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#８２３４５６７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.937500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4545"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.37875"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.5,2.55278;9.045,3.30278"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#８８８８８８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.937500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.5,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4545"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.37875"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="7.975,1.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.975,1.17222;8.581,1.77222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAI1#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#在#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.975,1.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.3636"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.303"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ZAI1#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="7.775,1.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.775,1.0037;8.785,2.0037"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAI2#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1.250000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.775,1.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.606"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.505"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ZAI2#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,5.73148;1.4348,6.13148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,6.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,4.63148;4.95,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,4.63148;6.4,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,4.63148;9.4,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,4.63148;12.4,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,4.63148;13.8,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,4.63148;15.2,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O265">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,4.63148;17.6,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="20,4.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,4.63148;20,5.03148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,4.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,5.38148;4.95,5.78148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,5.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O268">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,5.38148;6.4,5.78148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,5.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,5.38148;9.4,5.78148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,5.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,5.38148;12.4,5.78148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,5.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,8.23148;1.4348,8.63148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,8.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,7.13148;4.95,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,7.13148;6.4,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,7.13148;9.4,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,7.13148;12.4,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,7.13148;13.8,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O277">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,7.13148;15.2,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,7.13148;17.6,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="20,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,7.13148;20,7.53148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,7.88148;4.95,8.28148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,8.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,7.88148;6.4,8.28148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,8.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,7.88148;9.4,8.28148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,8.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,7.88148;12.4,8.28148"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,8.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,10.7315;1.4348,11.1315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,11.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,9.63148;4.95,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,9.63148;6.4,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,9.63148;9.4,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,9.63148;12.4,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O289">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,9.63148;13.8,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,9.63148;15.2,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,9.63148;17.6,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="20,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,9.63148;20,10.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,10.3815;4.95,10.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,10.3815;6.4,10.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,10.3815;9.4,10.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,10.3815;12.4,10.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,13.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,13.2315;1.4348,13.6315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,13.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O298">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,12.1315;4.95,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,12.1315;6.4,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,12.1315;9.4,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,12.1315;12.4,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,12.1315;13.8,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,12.1315;15.2,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,12.1315;17.6,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="20,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,12.1315;20,12.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,12.8815;4.95,13.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,12.8815;6.4,13.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,12.8815;9.4,13.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,12.8815;12.4,13.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,13.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,15.7315;1.4348,16.1315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,14.6315;4.95,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,14.6315;6.4,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,14.6315;9.4,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,14.6315;12.4,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,14.6315;13.8,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,14.6315;15.2,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,14.6315;17.6,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="20,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,14.6315;20,15.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,15.3815;4.95,15.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,15.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,15.3815;6.4,15.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,15.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,15.3815;9.4,15.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,15.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,15.3815;12.4,15.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,15.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,18.2315;1.4348,18.6315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,17.1315;4.95,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,17.1315;6.4,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O326">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,17.1315;9.4,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,17.1315;12.4,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,17.1315;13.8,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O329">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,17.1315;15.2,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,17.1315;17.6,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="20,17.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,17.1315;20,17.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,17.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,17.8815;4.95,18.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,18.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,17.8815;6.4,18.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,18.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,17.8815;9.4,18.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,18.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,17.8815;12.4,18.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,18.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,20.7315;1.4348,21.1315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,21.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,19.6315;4.95,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O338">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,19.6315;6.4,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,19.6315;9.4,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,19.6315;12.4,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O341">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,19.6315;13.8,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,19.6315;15.2,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,19.6315;17.6,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="20,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,19.6315;20,20.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,19.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,20.3815;4.95,20.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,20.3815;6.4,20.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,20.3815;9.4,20.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,20.3815;12.4,20.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,23.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,23.2315;1.4348,23.6315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,23.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,22.1315;4.95,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,22.1315;6.4,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,22.1315;9.4,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,22.1315;12.4,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,22.1315;13.8,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,22.1315;15.2,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,22.1315;17.6,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="20,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,22.1315;20,22.5315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,22.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,22.8815;4.95,23.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,23.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,22.8815;6.4,23.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,23.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,22.8815;9.4,23.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,23.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O361">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,22.8815;12.4,23.2815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,23.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O362">
      <dia:attribute name="obj_pos">
        <dia:point val="0.95,26.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.95,25.7315;1.4348,26.1315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.95,26.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O363">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,24.6315;4.95,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O364">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,24.6315;6.4,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O365">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,24.6315;9.4,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O366">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,24.6315;12.4,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[0].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O367">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.588,24.6315;13.8,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].K-SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O368">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.988,24.6315;15.2,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].K-SNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O369">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4184,24.6315;17.6,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].K-SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O370">
      <dia:attribute name="obj_pos">
        <dia:point val="20,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.8184,24.6315;20,25.0315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="20,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].K-SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O371">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95,25.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.738,25.3815;4.95,25.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95,25.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O372">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4,25.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.188,25.3815;6.4,25.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,25.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KNISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O373">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,25.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2184,25.3815;9.4,25.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,25.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KTENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O374">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,25.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2184,25.3815;12.4,25.7815"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,25.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].KOHNYUGAI-TBL[1].KITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O375">
      <dia:attribute name="obj_pos">
        <dia:point val="19,1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2728,0.761111;19,1.06111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#NO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#0001#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19,1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#NO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="4"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O376">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,5.175"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5,4.77685;2.005,5.27685"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.5,5.175"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O377">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,5.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5,5.52685;2.005,6.02685"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.5,5.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O378">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.5,6.30185;2.005,6.80185"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.5,6.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[0].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O379">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,7.67341"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,7.27526;2,7.77526"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,7.67341"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O380">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,8.42341"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,8.02526;2,8.52526"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,8.42341"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O381">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,9.19841"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,8.80026;2,9.30026"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,9.19841"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[1].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O382">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,10.1734"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,9.77525;2,10.2753"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,10.1734"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O383">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,10.9234"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,10.5253;2,11.0253"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,10.9234"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O384">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,11.6984"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,11.3003;2,11.8003"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,11.6984"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[2].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O385">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,12.6753"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,12.2772;2,12.7772"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,12.6753"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O386">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,13.4253"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,13.0272;2,13.5272"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,13.4253"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O387">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,14.2003"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,13.8022;2,14.3022"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,14.2003"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[3].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O388">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,15.1753"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,14.7772;2,15.2772"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,15.1753"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O389">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,15.9253"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,15.5272;2,16.0272"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,15.9253"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O390">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,16.7003"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,16.3022;2,16.8022"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,16.7003"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[4].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O391">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,17.6784"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,17.2803;2,17.7803"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,17.6784"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O392">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,18.4284"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,18.0303;2,18.5303"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,18.4284"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O393">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,19.2034"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,18.8053;2,19.3053"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,19.2034"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[5].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O394">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,20.1534"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,19.7553;2,20.2553"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,20.1534"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O395">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,20.9034"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,20.5053;2,21.0053"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,20.9034"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O396">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,21.6784"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,21.2803;2,21.7803"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,21.6784"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[6].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O397">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,22.6735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,22.2754;2,22.7754"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,22.6735"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O398">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,23.4235"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,23.0254;2,23.5254"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,23.4235"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O399">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,24.1985"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,23.8004;2,24.3004"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,24.1985"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[7].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O400">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,25.1735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,24.7754;2,25.2754"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,25.1735"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[0].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O401">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,25.9235"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,25.5254;2,26.0254"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,25.9235"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[1].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O402">
      <dia:attribute name="obj_pos">
        <dia:point val="1.495,26.6985"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,26.3004;2,26.8004"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.495,26.6985"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL[8].HKNBETU-TBL[2].HKNBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O403">
      <dia:attribute name="obj_pos">
        <dia:point val="4.58063,1.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.58063,1.61111;7.61063,1.91111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#RECEKA#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#内科外科精神科あああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.58063,1.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#RECEKA#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O404">
      <dia:attribute name="obj_pos">
        <dia:point val="14.7855,0.952184"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.7855,0.633666;16.8055,1.03367"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#TOKUBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#特別療養費#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.7855,0.952184"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2424"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKUBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
