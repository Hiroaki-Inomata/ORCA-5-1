<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="SEIYM" occurs="1">
      <appinfo>
        <embed object="text" length="16"/>
      </appinfo>
    </element>
    <element name="POST1" occurs="3">
      <appinfo>
        <embed object="text" length="1"/>
      </appinfo>
    </element>
    <element name="POST2" occurs="4">
      <appinfo>
        <embed object="text" length="1"/>
      </appinfo>
    </element>
    <element name="ADRS" occurs="1">
      <appinfo>
        <embed object="text" length="100"/>
      </appinfo>
    </element>
    <element name="HOSPNAME" occurs="1">
      <appinfo>
        <embed object="text" length="100"/>
      </appinfo>
    </element>
    <element name="HOSPCD" occurs="1">
      <appinfo>
        <embed object="text" length="20"/>
      </appinfo>
    </element>
    <element name="SRYKANAME" occurs="3">
      <appinfo>
        <embed object="text" length="30"/>
      </appinfo>
    </element>
    <element name="TOKUBETU" occurs="1">
      <appinfo>
        <embed object="text" length="10"/>
      </appinfo>
    </element>
    <element name="DATA-TBL" occurs="46">
      <element name="HKNJANUM" occurs="1">
        <appinfo>
          <embed object="text" length="8"/>
        </appinfo>
      </element>
      <element name="NYUIN" occurs="1">
        <element name="N-KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="6"/>
          </appinfo>
        </element>
        <element name="N-TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="11"/>
          </appinfo>
        </element>
      </element>
      <element name="GAIRAI" occurs="1">
        <element name="G-KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="6"/>
          </appinfo>
        </element>
        <element name="G-TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="11"/>
          </appinfo>
        </element>
      </element>
    </element>
    <element name="TOTAL" occurs="2">
      <element name="TL-NYUIN" occurs="1">
        <element name="TL-N-KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="6"/>
          </appinfo>
        </element>
        <element name="TL-N-TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="11"/>
          </appinfo>
        </element>
      </element>
      <element name="TL-GAIRAI" occurs="1">
        <element name="TL-G-KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="6"/>
          </appinfo>
        </element>
        <element name="TL-G-TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="11"/>
          </appinfo>
        </element>
      </element>
    </element>
  </dia:dictionarydata>
  <dia:diagramdata>
    <dia:attribute name="background">
      <dia:color val="#ffffff"/>
    </dia:attribute>
    <dia:attribute name="pagebreak">
      <dia:color val="#000099"/>
    </dia:attribute>
    <dia:attribute name="paper">
      <dia:composite type="paper">
        <dia:attribute name="name">
          <dia:string>#A4#</dia:string>
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
        <dia:attribute name="custom_width">
          <dia:real val="10"/>
        </dia:attribute>
        <dia:attribute name="custom_height">
          <dia:real val="10"/>
        </dia:attribute>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="grid">
      <dia:composite type="grid">
        <dia:attribute name="width_x">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="width_y">
          <dia:real val="1"/>
        </dia:attribute>
        <dia:attribute name="visible_x">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="visible_y">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:composite type="color"/>
      </dia:composite>
    </dia:attribute>
    <dia:attribute name="color">
      <dia:color val="#d8e5e5"/>
    </dia:attribute>
    <dia:attribute name="guides">
      <dia:composite type="guides">
        <dia:attribute name="hguides"/>
        <dia:attribute name="vguides"/>
      </dia:composite>
    </dia:attribute>
  </dia:diagramdata>
  <dia:layer name="線" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4969,24.1125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4769,24.0925;20.0169,27.215"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="10.4969,24.1125"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="9.5"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="3.0825"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="3.35,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34,4.14;3.36,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.35,4.15"/>
        <dia:point val="3.35,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="10.15,1.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.13,1.53;20.02,1.57"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.15,1.55"/>
        <dia:point val="20,1.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,5.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,5.24;3.36,5.26"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,5.25"/>
        <dia:point val="3.35,5.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O34" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="1,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,6.33;20.02,6.37"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,6.35"/>
        <dia:point val="20,6.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="12.85,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.84,7.09;20.01,7.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.85,7.1"/>
        <dia:point val="20,7.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="3.35,7.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34,7.14;10.51,7.16"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.35,7.15"/>
        <dia:point val="10.5,7.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="1,8.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,8.64;20.01,8.66"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,8.65"/>
        <dia:point val="20,8.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="1,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,9.44;20.01,9.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,9.45"/>
        <dia:point val="20,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="1,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,10.19;20.01,10.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,10.2"/>
        <dia:point val="20,10.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="1,11"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,10.99;20.01,11.01"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,11"/>
        <dia:point val="20,11"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="1,11.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,11.73;20.02,11.77"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,11.75"/>
        <dia:point val="20,11.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="1,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,12.49;20.01,12.51"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,12.5"/>
        <dia:point val="20,12.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="1,13.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,13.29;20.01,13.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,13.3"/>
        <dia:point val="20,13.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="1,14.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,14.04;20.01,14.06"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,14.05"/>
        <dia:point val="20,14.05"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="1,14.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,14.84;20.01,14.86"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,14.85"/>
        <dia:point val="20,14.85"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="1,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,15.58;20.02,15.62"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,15.6"/>
        <dia:point val="20,15.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="1,16.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,16.39;20.01,16.41"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,16.4"/>
        <dia:point val="20,16.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="1,17.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,17.14;20.01,17.16"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,17.15"/>
        <dia:point val="20,17.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="1,17.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,17.94;20.01,17.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,17.95"/>
        <dia:point val="20,17.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="1,18.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,18.69;20.01,18.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,18.7"/>
        <dia:point val="20,18.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="1,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,19.43;20.02,19.47"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,19.45"/>
        <dia:point val="20,19.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="1,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,20.24;20.01,20.26"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,20.25"/>
        <dia:point val="20,20.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="1,21"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,20.99;20.01,21.01"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,21"/>
        <dia:point val="20,21"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="1,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,21.79;20.01,21.81"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,21.8"/>
        <dia:point val="20,21.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="1,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,22.54;20.01,22.56"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,22.55"/>
        <dia:point val="20,22.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="1,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,23.28;20.02,23.32"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,23.3"/>
        <dia:point val="20,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="1,24.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,24.09;20.01,24.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,24.1"/>
        <dia:point val="20,24.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="1,26.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,26.39;10.51,26.41"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,26.4"/>
        <dia:point val="10.5,26.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="1.80502,2.25538"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.79502,2.24479;7.65496,2.26538"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.80502,2.25538"/>
        <dia:point val="7.64496,2.25479"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="1,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,4.13;20.02,4.17"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,4.15"/>
        <dia:point val="20,4.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.49,4.14;11.51,6.36"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.5,4.15"/>
        <dia:point val="11.5,6.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="13.5838,4.1469"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.5738,4.1369;13.5938,6.34473"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.5838,4.1469"/>
        <dia:point val="13.5838,6.33473"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,5.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.49,5.24;20.01,5.26"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.5,5.25"/>
        <dia:point val="20,5.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O31" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,4.14;1.71,6.36"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,4.15"/>
        <dia:point val="1.7,6.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="4.58438,7.14083"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.5654,7.13083;4.59438,27.2136"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.58438,7.14083"/>
        <dia:point val="4.5754,27.2036"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="6.95,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.94,6.34;6.96,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.95,6.35"/>
        <dia:point val="6.95,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="8.21732,7.12058"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.20365,7.11058;8.22732,27.2183"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.21732,7.12058"/>
        <dia:point val="8.21365,27.2083"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="10.55,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5339,6.34;10.56,24.1225"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.55,6.35"/>
        <dia:point val="10.5439,24.1125"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="12.85,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.84,6.34;12.86,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.85,6.35"/>
        <dia:point val="12.85,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.09,7.09;14.11,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.1,7.1"/>
        <dia:point val="14.1,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="16.45,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.44,6.34;16.46,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.45,6.35"/>
        <dia:point val="16.45,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="20,1.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.98,1.53;20.02,27.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="20,1.55"/>
        <dia:point val="20,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="17.7,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.69,7.08999;17.7219,27.2092"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.7,7.1"/>
        <dia:point val="17.7119,27.1992"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="1,4.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,4.13;1.02,27.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,4.15"/>
        <dia:point val="1,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1661,1.53766"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.1561,1.52765;14.1777,4.14941"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.1661,1.53766"/>
        <dia:point val="14.1677,4.1394"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="17.4278,1.54132"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4178,1.53132;17.4384,4.15565"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.4278,1.54132"/>
        <dia:point val="17.4284,4.14565"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="16.7402,1.534"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.7302,1.524;16.7509,4.1494"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.7402,1.534"/>
        <dia:point val="16.7409,4.1394"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="10.85,1.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.84,1.54;10.86,4.16"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.85,1.55"/>
        <dia:point val="10.85,4.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4623,1.53574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4523,1.52573;13.4739,4.1496"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.4623,1.53574"/>
        <dia:point val="13.4639,4.13959"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="10.15,1.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.13,1.53;10.17,4.17"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.15,1.55"/>
        <dia:point val="10.15,4.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4855,6.34;10.51,25.585"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,6.35"/>
        <dia:point val="10.4955,25.575"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O4" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="1,24.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99,24.84;10.5111,24.86"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,24.85"/>
        <dia:point val="10.5011,24.85"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="1,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,7.88;20.02,7.92"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,7.9"/>
        <dia:point val="20,7.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="1,25.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.989996,25.64;10.5069,25.6638"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,25.65"/>
        <dia:point val="10.4969,25.6538"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O0" connection="3"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="1,27.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.98,27.18;20.02,27.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1,27.2"/>
        <dia:point val="20,27.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="1.80556,2.31943"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.79556,2.30701;7.66062,2.32943"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.80556,2.31943"/>
        <dia:point val="7.65062,2.31701"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="10.86,25.66"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.85,25.6438;20.0069,25.67"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.86,25.66"/>
        <dia:point val="19.9969,25.6538"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O0" connection="4"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="10.86,24.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.85,24.09;10.87,27.21"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.86,24.1"/>
        <dia:point val="10.86,27.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="2.32654,2.06148"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.32654,1.70898;6.32654,2.11148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診 療 報 酬 総 括 表#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.32654,2.06148"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2208,27.7005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.2208,27.4355;8.4708,27.738"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１． ※印欄は、記入しないで下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.2208,27.7005"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2208,28.0346"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.2208,27.7696;9.0708,28.0721"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２． 県内分と県外分に分けて作成のこと。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.2208,28.0346"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2208,28.3807"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.2208,28.1157;9.3708,28.4182"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３． 保険者コード欄は番号で記入すること。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.2208,28.3807"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8146,2.21056"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.6646,1.94556;13.9646,3.74806"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#受

付

印#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8146,2.21056"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O64">
        <dia:attribute name="obj_pos">
          <dia:point val="17.0993,2.64494"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="16.9493,2.37994;17.2493,2.68244"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#査#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="17.0993,2.64494"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O65">
        <dia:attribute name="obj_pos">
          <dia:point val="17.0993,2.15737"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="16.9493,1.89237;17.2493,2.19487"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#審#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="17.0993,2.15737"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O66">
        <dia:attribute name="obj_pos">
          <dia:point val="17.0993,3.10751"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="16.9493,2.84251;17.2493,3.14501"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#済#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="17.0993,3.10751"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O67">
        <dia:attribute name="obj_pos">
          <dia:point val="17.0993,3.57004"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="16.9493,3.30504;17.2493,3.60754"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#印#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="17.0993,3.57004"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:object type="Standard - Text" version="1" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="1.37888,4.62456"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.22888,4.35956;1.52888,6.53706"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保
険
医
療
機
関#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.37888,4.62456"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="8.59112,6.80992"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.84112,6.54492;9.34112,6.84742"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入  院  外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.59112,6.80992"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55565,4.8445"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.10565,4.5795;3.00565,4.882"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所在地#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55565,4.8445"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="2.50261,5.87195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.05261,5.60695;2.95261,5.90945"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#名  称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.50261,5.87195"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="2.22373,7.19741"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.32373,6.93241;3.12373,7.23491"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険者コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.22373,7.19741"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="5.10029,6.80992"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.35029,6.54492;5.85029,6.84742"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入      院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.10029,6.80992"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="5.71648,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.19148,7.37486;6.24148,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.71648,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="3.97635,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.60135,7.37486;4.35135,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.97635,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="9.41455,7.63687"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.88955,7.37187;9.93955,7.67437"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.41455,7.63687"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="7.5569,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1819,7.37486;7.9319,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.5569,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="15.2183,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.6933,7.37486;15.7433,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.2183,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4565,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.0815,7.37486;13.8315,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4565,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="18.7758,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2508,7.37486;19.3008,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.7758,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="17.0741,7.63986"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6991,7.37486;17.4491,7.67736"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.0741,7.63986"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="18.2286,6.80992"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4786,6.54492;18.9786,6.84742"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入  院  外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.2286,6.80992"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="14.7378,6.80992"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9878,6.54492;15.4878,6.84742"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入      院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.7378,6.80992"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="11.667,7.23492"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.767,6.96992;12.567,7.27242"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険者コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.667,7.23492"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O85">
        <dia:attribute name="obj_pos">
          <dia:point val="12.5781,4.6489"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.9781,4.3839;13.1781,4.6864"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#医療機関#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.5781,4.6489"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O86">
        <dia:attribute name="obj_pos">
          <dia:point val="12.0734,4.9624"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.9234,4.6974;12.2234,4.9999"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#コ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.0734,4.9624"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O87">
        <dia:attribute name="obj_pos">
          <dia:point val="12.6239,4.9624"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.4739,4.6974;12.7739,4.9999"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ー#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.6239,4.9624"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O88">
        <dia:attribute name="obj_pos">
          <dia:point val="13.0739,4.9624"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.9239,4.6974;13.2239,4.9999"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ド#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.0739,4.9624"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="1"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:object type="Standard - Line" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="1.35501,2.77162"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.34499,2.7616;2.70903,2.78482"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.35501,2.77162"/>
        <dia:point val="2.69901,2.7748"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="1.35585,2.78195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.34464,2.77194;1.36586,3.60696"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.35585,2.78195"/>
        <dia:point val="1.35465,3.59695"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2628,2.78195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25148,2.77193;2.27282,3.60697"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2628,2.78195"/>
        <dia:point val="2.2615,3.59695"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7003,2.78195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.68898,2.77193;2.71032,3.60697"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.7003,2.78195"/>
        <dia:point val="2.699,3.59695"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="3.16277,2.78292"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.15244,2.77291;3.17278,3.38811"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.16277,2.78292"/>
        <dia:point val="3.16245,3.3781"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="3.53501,2.7708"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.52336,2.76077;3.54504,3.40587"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.53501,2.7708"/>
        <dia:point val="3.53339,3.39584"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O108" connection="0"/>
        <dia:connection handle="1" to="O97" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="3.92666,2.77726"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.91563,2.76724;3.93668,3.38812"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.92666,2.77726"/>
        <dia:point val="3.92565,3.3781"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="4.28249,2.77508"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.27029,2.76505;4.29252,3.40413"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.28249,2.77508"/>
        <dia:point val="4.28032,3.3941"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="2.77577,3.40088"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7657,3.38073;4.30108,3.41095"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.77577,3.40088"/>
        <dia:point val="4.29101,3.3908"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8187,2.77629"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.80749,2.76628;1.82871,3.6013"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8187,2.77629"/>
        <dia:point val="1.8175,3.59129"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="1.34925,3.58677"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.33925,3.57638;2.71708,3.59677"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.34925,3.58677"/>
        <dia:point val="2.70708,3.58638"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="1.0923,3.30528"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.9423,3.04028;1.2423,3.34278"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#〒#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.0923,3.30528"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Ellipse" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="0.874259,2.98439"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.859259,2.96939;1.29655,3.38971"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="0.874259,2.98439"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="0.40729399999999999"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.39032299999999998"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="14.3836,1.80622"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2586,1.58622;14.5086,1.83872"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.3836,1.80622"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6244,1.80622"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4994,1.58622;17.7494,1.83872"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.6244,1.80622"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="19.7,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.55,5.635;19.85,5.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#科#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.7,5.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0624,1.81152"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.9374,1.59152;11.1874,1.84402"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.0624,1.81152"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="12.5913,5.88107"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.9913,5.61607;13.1913,5.91857"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#標榜科名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.5913,5.88107"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="2.41079,27.6914"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.41079,27.4264;3.01079,27.7289"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(注)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.41079,27.6914"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="2.77901,2.7748"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.76896,2.75675;4.30106,2.78485"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.77901,2.7748"/>
        <dia:point val="4.29101,2.7668"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="2.78878,2.764"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.77658,2.75397;2.79881,3.39305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.78878,2.764"/>
        <dia:point val="2.78661,3.38302"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.20000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="5.1,1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1,0.6475;5.5,1.05"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.1,1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3609,2.1515"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3609,1.8865;10.6609,4.439"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#書

留

番

号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3609,2.1515"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="1.04,3.74"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04,3.475;6.14,4.1525"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#
(国民健康保険  ・  後期高齢者医療)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.04,3.74"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0425,24.9664"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0425,24.7264;12.6625,24.9989"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国民健康保険#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.33750000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.0425,24.9664"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="10.9175,26.5164"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.9175,26.2764;12.8075,26.5489"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#後期高齢者医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.33750000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.9175,26.5164"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="10.575,25.125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.575,24.885;10.845,26.5075"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合



計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.33750000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.575,25.125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="差し込み" visible="true" active="true">
    <dia:object type="Embed - Text" version="1" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="5.1,1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,0.6475;5.1,1.05"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成99年99月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.1,1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,8.0025;2.9,8.455"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[0].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="14.2,4.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2,4.5475;16.2,4.95"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.2,4.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="1.74972,3.35015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.43972,2.80265;1.74972,3.42515"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.77500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.74972,3.35015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST1[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="3.45856,4.74088"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.45856,4.47588;10.9586,5.15338"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああああああああああああ
ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.45856,4.74088"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ADRS#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="3.45856,5.77624"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.45856,5.51124;9.45856,6.18874"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああああああああああああ
ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.45856,5.77624"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="14.2,5.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2,5.335;18.7,5.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.2,5.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SRYKANAME[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="2.19972,3.35015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.88972,2.80265;2.19972,3.42515"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.77500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.19972,3.35015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST1[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="2.64972,3.35015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.33972,2.80265;2.64972,3.42515"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.77500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.64972,3.35015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST1[2]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="3.09972,3.25015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.84972,2.81015;3.09972,3.31265"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.09972,3.25015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST2[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="3.48305,3.25015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23305,2.81015;3.48305,3.31265"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.48305,3.25015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST2[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="3.86639,3.25015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.61639,2.81015;3.86639,3.31265"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.86639,3.25015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST2[2]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="4.24972,3.25015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.99972,2.81015;4.24972,3.31265"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.24972,3.25015"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#POST2[3]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="14.2,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2,5.635;18.7,5.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.2,5.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SRYKANAME[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="14.2,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2,5.935;18.7,6.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.2,6.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SRYKANAME[2]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,8.7525;2.45,9.205"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[1].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,9.5525;2.45,10.005"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[2].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,10.3525;2.45,10.805"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[3].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,11.1025;2.45,11.555"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[4].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,11.8525;2.45,12.305"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[5].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,12.6525;2.45,13.105"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[6].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,13.4025;2.45,13.855"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[7].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,14.2025;2.45,14.655"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[8].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,14.9525;2.45,15.405"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[9].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,15.7025;2.45,16.155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[10].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,16.5025;2.45,16.955"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[11].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,17.3025;2.45,17.755"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[12].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,18.0525;2.45,18.505"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[13].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,18.8025;2.45,19.255"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[14].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,19.6025;2.45,20.055"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[15].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,20.3525;2.45,20.805"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[16].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,21.1525;2.45,21.605"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[17].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,21.9025;2.45,22.355"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[18].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,22.6525;2.45,23.105"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[19].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,23.4525;2.45,23.905"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[20].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,24.2025;2.45,24.655"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[21].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,25.0025;2.45,25.455"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,25.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[22].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,25.7525;2.45,26.205"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[23].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="1.1,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.1,26.5525;2.45,27.005"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.1,26.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[24].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,8.0025;12.4,8.455"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[25].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,8.7525;11.95,9.205"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[26].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,9.5525;11.95,10.005"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[27].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,10.3525;11.95,10.805"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[28].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,11.1025;11.95,11.555"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[29].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,11.8525;11.95,12.305"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[30].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,12.6525;11.95,13.105"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[31].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,13.4025;11.95,13.855"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[32].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,14.2025;11.95,14.655"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[33].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,14.9525;11.95,15.405"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[34].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,15.7025;11.95,16.155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[35].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,16.5025;11.95,16.955"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[36].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,17.3025;11.95,17.755"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[37].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,18.0525;11.95,18.505"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[38].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,18.8025;11.95,19.255"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[39].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,19.6025;11.95,20.055"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[40].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,20.3525;11.95,20.805"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[41].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,21.1525;11.95,21.605"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[42].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,21.9025;11.95,22.355"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[43].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,22.6525;11.95,23.105"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[44].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="10.6,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,23.4525;11.95,23.905"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[45].HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,8.1175;6.9,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[0].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,8.1175;4.55,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[0].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,8.1175;8.1375,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[0].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,8.1175;10.4375,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[0].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,24.9748"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,24.6923;16.4,25.0148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,24.9748"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[0].TL-NYUIN.TL-N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,24.9748"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,24.6923;14.05,25.0148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,24.9748"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[0].TL-NYUIN.TL-N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,24.9748"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,24.6923;17.65,25.0148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,24.9748"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[0].TL-GAIRAI.TL-G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,24.9748"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,24.6923;19.95,25.0148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,24.9748"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[0].TL-GAIRAI.TL-G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,8.8675;6.9,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[1].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,8.8675;4.55,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[1].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,8.8675;8.1375,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[1].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,8.8675;10.4375,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[1].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,9.6675;6.9,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[2].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,9.6675;4.55,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[2].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,9.6675;8.1375,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[2].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,9.6675;10.4375,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[2].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,10.4675;6.9,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[3].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,10.4675;4.55,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[3].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,10.4675;8.1375,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[3].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,10.4675;10.4375,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[3].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,11.2175;6.9,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[4].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,11.2175;4.55,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[4].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,11.2175;8.1375,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[4].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,11.2175;10.4375,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[4].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,11.9675;6.9,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[5].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,11.9675;4.55,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[5].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,11.9675;8.1375,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[5].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,11.9675;10.4375,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[5].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,12.7675;6.9,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[6].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,12.7675;4.55,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[6].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,12.7675;8.1375,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[6].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,12.7675;10.4375,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[6].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,13.5175;6.9,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[7].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,13.5175;4.55,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[7].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,13.5175;8.1375,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[7].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,13.5175;10.4375,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[7].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,14.3175;6.9,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[8].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,14.3175;4.55,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[8].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,14.3175;8.1375,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[8].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,14.3175;10.4375,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[8].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,15.0675;6.9,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[9].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,15.0675;4.55,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[9].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,15.0675;8.1375,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[9].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,15.0675;10.4375,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[9].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,15.8175;6.9,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[10].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,15.8175;4.55,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[10].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,15.8175;8.1375,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[10].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,15.8175;10.4375,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[10].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,16.6175;6.9,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[11].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,16.6175;4.55,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[11].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,16.6175;8.1375,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[11].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,16.6175;10.4375,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[11].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,17.4175;6.9,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[12].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,17.4175;4.55,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[12].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,17.4175;8.1375,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[12].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,17.4175;10.4375,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[12].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,18.1675;6.9,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[13].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,18.1675;4.55,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[13].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,18.1675;8.1375,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[13].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,18.1675;10.4375,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[13].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,18.9175;6.9,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[14].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,18.9175;4.55,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[14].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,18.9175;8.1375,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[14].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,18.9175;10.4375,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[14].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,19.7175;6.9,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[15].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,19.7175;4.55,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[15].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,19.7175;8.1375,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[15].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,19.7175;10.4375,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[15].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,20.4675;6.9,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[16].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,20.4675;4.55,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[16].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,20.4675;8.1375,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[16].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,20.4675;10.4375,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[16].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,21.2675;6.9,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[17].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,21.2675;4.55,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[17].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,21.2675;8.1375,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[17].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,21.2675;10.4375,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[17].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,22.0175;6.9,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[18].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,22.0175;4.55,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[18].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,22.0175;8.1375,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[18].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,22.0175;10.4375,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[18].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,22.7675;6.9,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[19].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,22.7675;4.55,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[19].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,22.7675;8.1375,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[19].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,22.7675;10.4375,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[19].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,23.5675;6.9,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[20].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,23.5675;4.55,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[20].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,23.5675;8.1375,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[20].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,23.5675;10.4375,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[20].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,24.3175;6.9,24.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[21].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O265">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,24.3175;4.55,24.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[21].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,24.3175;8.1375,24.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[21].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,24.3175;10.4375,24.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[21].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O268">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,25.1175;6.9,25.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,25.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[22].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,25.1175;4.55,25.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,25.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[22].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,25.1175;8.1375,25.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,25.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[22].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,25.1175;10.4375,25.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,25.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[22].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,25.8675;6.9,26.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[23].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,25.8675;4.55,26.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[23].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,25.8675;8.1375,26.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[23].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,25.8675;10.4375,26.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[23].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="6.9,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.14,26.6675;6.9,26.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,26.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[24].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O277">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.59,26.6675;4.55,26.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,26.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[24].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="8.1375,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1775,26.6675;8.1375,26.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.1375,26.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[24].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4375,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.6775,26.6675;10.4375,26.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4375,26.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[24].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,8.1175;16.4,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[25].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,8.1175;14.05,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[25].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,8.1175;17.65,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[25].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,8.1175;19.95,8.44"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,8.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[25].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,8.8675;16.4,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[26].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,8.8675;14.05,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[26].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,8.8675;17.65,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[26].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,8.8675;19.95,9.19"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[26].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,9.6675;16.4,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[27].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O289">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,9.6675;14.05,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[27].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,9.6675;17.65,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[27].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,9.6675;19.95,9.99"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,9.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[27].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,10.4675;16.4,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[28].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,10.4675;14.05,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[28].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,10.4675;17.65,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[28].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,10.4675;19.95,10.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,10.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[28].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,11.2175;16.4,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[29].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,11.2175;14.05,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[29].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O298">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,11.2175;17.65,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[29].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,11.2175;19.95,11.54"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[29].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,11.9675;16.4,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[30].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,11.9675;14.05,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[30].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,11.9675;17.65,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[30].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,12.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,11.9675;19.95,12.29"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,12.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[30].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,12.7675;16.4,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[31].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,12.7675;14.05,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[31].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,12.7675;17.65,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[31].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,12.7675;19.95,13.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[31].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,13.5175;16.4,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[32].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,13.5175;14.05,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[32].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,13.5175;17.65,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[32].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,13.5175;19.95,13.84"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[32].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,14.3175;16.4,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[33].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,14.3175;14.05,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[33].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,14.3175;17.65,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[33].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,14.3175;19.95,14.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[33].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,15.0675;16.4,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[34].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,15.0675;14.05,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[34].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,15.0675;17.65,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[34].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,15.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,15.0675;19.95,15.39"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,15.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[34].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,15.8175;16.4,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[35].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,15.8175;14.05,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[35].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,15.8175;17.65,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[35].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,15.8175;19.95,16.14"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,16.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[35].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,16.6175;16.4,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[36].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,16.6175;14.05,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[36].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O326">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,16.6175;17.65,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[36].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,16.6175;19.95,16.94"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[36].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,17.4175;16.4,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[37].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O329">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,17.4175;14.05,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[37].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,17.4175;17.65,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[37].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,17.4175;19.95,17.74"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,17.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[37].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,18.1675;16.4,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[38].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,18.1675;14.05,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[38].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,18.1675;17.65,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[38].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,18.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,18.1675;19.95,18.49"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,18.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[38].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,18.9175;16.4,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[39].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,18.9175;14.05,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[39].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O338">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,18.9175;17.65,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[39].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,18.9175;19.95,19.24"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,19.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[39].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,19.7175;16.4,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[40].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O341">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,19.7175;14.05,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[40].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,19.7175;17.65,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[40].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,19.7175;19.95,20.04"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[40].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,20.4675;16.4,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[41].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,20.4675;14.05,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[41].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,20.4675;17.65,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[41].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,20.4675;19.95,20.79"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,20.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[41].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,21.2675;16.4,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[42].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,21.2675;14.05,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[42].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,21.2675;17.65,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[42].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,21.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,21.2675;19.95,21.59"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,21.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[42].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,22.0175;16.4,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[43].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,22.0175;14.05,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[43].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,22.0175;17.65,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[43].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,22.0175;19.95,22.34"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[43].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,22.7675;16.4,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[44].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,22.7675;14.05,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[44].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,22.7675;17.65,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[44].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,22.7675;19.95,23.09"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[44].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,23.5675;16.4,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[45].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O361">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,23.5675;14.05,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[45].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O362">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,23.5675;17.65,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[45].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O363">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,23.5675;19.95,23.89"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA-TBL[45].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O364">
      <dia:attribute name="obj_pos">
        <dia:point val="7.15918,3.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.15918,3.31;8.65918,3.6125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#特別療養費#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.15918,3.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKUBETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O365">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,26.6298"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.64,26.3473;16.4,26.6698"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,26.6298"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[1].TL-NYUIN.TL-N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O366">
      <dia:attribute name="obj_pos">
        <dia:point val="14.05,26.5298"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.09,26.2473;14.05,26.5698"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,26.5298"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[1].TL-NYUIN.TL-N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O367">
      <dia:attribute name="obj_pos">
        <dia:point val="17.65,26.6298"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.69,26.3473;17.65,26.6698"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.65,26.6298"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[1].TL-GAIRAI.TL-G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O368">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,26.6298"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.19,26.3473;19.95,26.6698"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,26.6298"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOTAL[1].TL-GAIRAI.TL-G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
